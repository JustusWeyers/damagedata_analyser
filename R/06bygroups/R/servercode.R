
library(damageRF)

# Filepaths
output_path = Sys.getenv("output_path", unset = "/app/output/")
db_path     = Sys.getenv("db_path",     unset = "/app/data/fulldata_bast_clim.db")
r_files     = Sys.getenv("r_files",     unset = "/app/r/")

# Run parameters
bspid_code = Sys.getenv("bspid_code", unset = "B002")
run_id     = Sys.getenv("run_id",     unset = "run1205")

# Create output directory
run_dir = file.path(output_path, run_id, bspid_code)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

# Start logging
log_con = file(file.path(run_dir, "run.log"), open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message")

ts = function(...) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)

# Session info
si = capture.output(sessionInfo())
cat(strwrap(si, width = 90), sep = "\n")

ts("=== Run: ", run_id, " | BSPID: ", bspid_code, " ===")

# Source helper files
files = list.files(r_files, full.names = TRUE)
sapply(files[startsWith(basename(files), "hlp_")], source)
ts("Helper functions loaded")

# Read data
db = DBI::dbConnect(RSQLite::SQLite(), db_path)
workingdata_db = DBI::dbReadTable(db, "fulldata_bast_clim") |>
  dplyr::as_tibble() |>
  dplyr::rename(dplyr::any_of(setNames(names(col_translations), col_translations)))
DBI::dbDisconnect(db)
ts("Data read: ", nrow(workingdata_db), " rows")

# Split nach Verbesserungen
workingdata = workingdata_db |>
  dplyr::group_by(id) |>
  dplyr::arrange(age, .by_group = TRUE) |>
  dplyr::mutate(
    subgroup = cumsum(c(0, diff(rtg_d) < 0)),
    id   = paste(id, letters[subgroup + 1], sep = "_")
  ) |>
  dplyr::select(-subgroup) |>
  dplyr::ungroup()

# Nur Konstanz oder Verschlechterungen
workingdata = workingdata |>
  dplyr::group_by(id) |>
  dplyr::filter(any(diff(rtg_d) >= 0)) |>
  dplyr::ungroup()

# Initiale bspid
workingdata = workingdata |>
  dplyr::group_by(id) |>
  dplyr::arrange(age, .by_group = TRUE) |>
  dplyr::mutate(
    init_bsp_id = dplyr::first(bsp_id)
  ) |>
  dplyr::ungroup()
ts("Data wrangling done: ", nrow(workingdata), " rows remaining")

bspids = bspid_instantiation()
ts("BSPID objects instantiated")

# Binary classification
workingdata_binclass = workingdata |>
  dplyr::group_by(id) |>
  dplyr::arrange(age, .by_group = TRUE) |>
  dplyr::mutate(change = any(diff(rtg_d) > 0) | any(dplyr::lag(bsp_id) != bsp_id, na.rm = TRUE)) |>
  dplyr::mutate(change = as.numeric(change)) |>
  dplyr::ungroup()

# Binary classification
cp_binclass = file.path(run_dir, "checkpoint_binclass.rds")

if (file.exists(cp_binclass)) {
  ts("Binary classification: lade Checkpoint")
  bspid_binclass = readRDS(cp_binclass)
} else {
  bspid_binclass = set_data(bspids[[bspid_code]], workingdata_binclass)
  ts("Binary classification training started")
  bspid_binclass = binary_classification(
    self      = bspid_binclass,
    model     = damageRF::rf6_binary,
    target    = "change",
    row_limit = 1000,
    trees     = 1000,
    grid      = 3,
    n_boots   = 3
  )
  saveRDS(bspid_binclass, cp_binclass)
  ts("Binary classification done + Checkpoint gespeichert")
}

# Regression
workingdata_regression = workingdata_binclass |>
  dplyr::filter(change == 1) |>
  dplyr::select(-change)

cp_regression = file.path(run_dir, "checkpoint_regression.rds")

if (file.exists(cp_regression)) {
  ts("Regression: lade Checkpoint")
  bspid_regression = readRDS(cp_regression)
} else {
  bspid_regression = bspids[[bspid_code]]
  bspid_regression = set_data(bspid_regression, workingdata_regression)
  ts("Regression training started")
  bspid_regression = train_groups(
    self      = bspid_regression,
    model     = damageRF::rf7,
    target    = bspid_regression@target_col,
    row_limit = 1000,
    trees     = 500,
    grid      = 3,
    n_boots   = 3
  )
  saveRDS(bspid_regression, cp_regression)
  ts("Regression done + Checkpoint gespeichert")
}

# Save finale Ergebnisse
saveRDS(bspid_binclass,   file = file.path(run_dir, "bspid_binclass.rds"))
saveRDS(bspid_regression, file = file.path(run_dir, "bspid_regression.rds"))
ts("Saved to: ", run_dir)
ts("=== Finished ===")

# Close log
sink(type = "message")
sink()
close(log_con)

