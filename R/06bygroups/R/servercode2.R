library(damageRF)

# Filepaths
output_path = Sys.getenv("output_path", unset = "/app/output/")
db_path     = Sys.getenv("db_path",     unset = "/app/data/fulldata_bast_clim_red.db")
r_files     = Sys.getenv("r_files",     unset = "/app/r/")

# Run parameters
bspid_code = Sys.getenv("bspid_code", unset = "B006_03")
test_run   = as.logical(Sys.getenv("test_run", unset = "FALSE"))
run_id     = paste0("run", format(Sys.Date(), format = "%Y%m%d"))
run_id     = Sys.getenv("run_id", unset = run_id)

# Model parameters
shared_seed = 4242
shared_prop = 0.80

# Create output directory
run_path = file.path(output_path, run_id, bspid_code)
dir.create(run_path, recursive = TRUE, showWarnings = FALSE)

# Start logging
log_con = file(file.path(run_path, "run.log"), open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message")

ts = function(...) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)

si = capture.output(sessionInfo())
cat(strwrap(si, width = 90), sep = "\n")

ts("=== Run: ", run_id, " | BSPID: ", bspid_code, " | test_run: ", test_run, " ===")

# Source helper files
files = list.files(r_files, full.names = TRUE)
sapply(files[startsWith(basename(files), "hlp_")], source)
ts("Helper functions loaded")

# Read data
db = DBI::dbConnect(RSQLite::SQLite(), db_path)
workingdata_db = DBI::dbReadTable(db, "fulldata_bast_clim_red") |>
  dplyr::as_tibble() |>
  dplyr::rename(dplyr::any_of(setNames(names(translations), translations))) |>
  dplyr::select(-dplyr::any_of(c("construction_type", "structure_name", "district_office", "maint_unit"))) |>
  dplyr::mutate(
    bsp_id = sub("-+$", "", bsp_id),
    x      = as.numeric(x),
    y      = as.numeric(y)
  )
DBI::dbDisconnect(db)
ts("Data read: ", nrow(workingdata_db), " rows")

# BSPID objects
bspids = bspid_instantiation()
bspid  = bspids[[bspid_code]]
bspid  = set_data(bspid, workingdata_db)
ts("BSPID objects instantiated")

# Data wrangling - Split bei Verbesserungen
data = bspid@data |>
  dplyr::group_by(id) |>
  dplyr::arrange(age, .by_group = TRUE) |>
  dplyr::mutate(
    subgroup = cumsum(c(0, diff(rtg_d) < 0)),
    id       = paste(id, letters[subgroup + 1], sep = "_")
  ) |>
  dplyr::select(-subgroup) |>
  dplyr::ungroup()

# Nur Konstanz oder Verschlechterungen
data = data |>
  dplyr::group_by(id) |>
  dplyr::filter(any(diff(rtg_d) >= 0)) |>
  dplyr::ungroup()

# Initiale bsp_id, rtg_d, h3_rtg_d als Prädiktoren
data = data |>
  dplyr::group_by(id) |>
  dplyr::arrange(age, .by_group = TRUE) |>
  dplyr::mutate(
    init_bsp_id = dplyr::first(bsp_id),
    init_rtg_d  = dplyr::first(rtg_d),
    h3_rtg_d    = if (dplyr::n_distinct(age) < 2) {
      NA_real_
    } else {
      round(stats::approx(x = age, y = rtg_d, xout = age - 3, rule = 1, ties = mean)$y)
    }
  ) |>
  dplyr::ungroup()

bspid = set_data(bspid, data)
ts("Data wrangling done: ", nrow(bspid@data), " rows remaining")

# Hurdle object (temporaler Train/Test-Split bei split_year)
hurdle = methods::new("Hurdle", data = bspid@data, split_year = 2020)
ts("Hurdle object created, train rows: ", nrow(hurdle@train_data))

# Shared split (gleiche Brücken in Train/Test für Stage 1 + 2)
set.seed(shared_seed)
bspid = set_data(bspid, hurdle@train_data)

split_prep = bspid@data |>
  dplyr::mutate(gsplitvar = sapply(strsplit(id, "_"), `[`, 1)) |>
  dplyr::group_by(gsplitvar) |>
  dplyr::mutate(strata_split = factor(max(rtg_d))) |>
  dplyr::ungroup()

shared_split = rsample::group_initial_split(
  data   = split_prep,
  group  = "gsplitvar",
  strata = "strata_split",
  prop   = shared_prop
)

train_ids = unique(rsample::training(shared_split)$gsplitvar)
ts("Shared split created: ", length(train_ids), " train IDs")

# Stage 1 - Binary Classification
global_train_data_binclass = hurdle@train_data |>
  dplyr::group_by(id) |>
  dplyr::arrange(age, .by_group = TRUE) |>
  dplyr::mutate(
    change = as.numeric(
      any(diff(rtg_d) > 0) |
        any(dplyr::lag(bsp_id) != bsp_id, na.rm = TRUE)
    )
  ) |>
  dplyr::ungroup()

cp_binclass = file.path(run_path, "checkpoint_binclass.rds")

if (file.exists(cp_binclass)) {
  ts("Binary classification: lade Checkpoint")
  hurdle@classification_model = readRDS(cp_binclass)
} else {
  bspid_binclass = set_data(bspid, global_train_data_binclass)
  ts("Binary classification training started")
  hurdle@classification_model = binary_classification(
    self                = bspid_binclass,
    model               = damageRF::hurdle_rf6,
    target              = "change",
    train_ids           = train_ids,
    pred_threshold_mode = "standard",
    test_run            = test_run
  )
  saveRDS(hurdle@classification_model, cp_binclass)
  ts("Binary classification done + Checkpoint gespeichert")
}

# Stage 2 - Regression
global_train_data_regression = global_train_data_binclass |>
  dplyr::filter(change == 1) |>
  dplyr::select(-change)

cp_regression = file.path(run_path, "checkpoint_regression.rds")

if (file.exists(cp_regression)) {
  ts("Regression: lade Checkpoint")
  hurdle@regression_model = readRDS(cp_regression)
} else {
  bspid_regression = set_data(bspid, global_train_data_regression)
  ts("Regression training started")
  hurdle@regression_model = train_groups(
    self      = bspid_regression,
    model     = damageRF::hurdle_rf7,
    target    = bspid_regression@target_col,
    train_ids = train_ids,
    test_run  = test_run
  )
  saveRDS(hurdle@regression_model, cp_regression)
  ts("Regression done + Checkpoint gespeichert")
}

# Save
saveRDS(hurdle, file = file.path(run_path, "hurdle.rds"))
ts("Saved to: ", run_path)
ts("=== Finished ===")

# Close log
sink(type = "message")
sink()
close(log_con)
