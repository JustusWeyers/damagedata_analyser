# Catch console output
catch_console = function(o) {
  
  old_width = getOption("width")
  options(width = 1000)
  out = utils::capture.output(print(o))
  options(width = old_width)
  
  return(out)
}

save_dataset = function(dataset, name) {
  
  # Documentation
  old_width = getOption("width")
  options(width = 120)
  sink(gsub(".db", ".log", name))
  print(dataset |> skimr::skim())
  sink()
  options(width = old_width)
  
  # Speichern
  db_out = DBI::dbConnect(RSQLite::SQLite(), dbname = name)
  tb_name = tools::file_path_sans_ext(basename(name))
  DBI::dbWriteTable(db_out, name = tb_name, value = dataset, overwrite = TRUE)
  DBI::dbDisconnect(db_out)
}

clamp = function(x, minx, maxx) {
  pmax(minx, pmin(maxx, x))
}
