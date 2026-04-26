save_dataset = function(dataset, name) {
  
  # Documentation
  old_width = getOption("width")
  options(width = 120)
  sink(gsub(".db", ".log", name))
  print(dataset |> skimr::skim())
  sink()
  options(width = old_width)
  
  print(normalizePath(name))
  
  # Speichern
  db_out = DBI::dbConnect(RSQLite::SQLite(), dbname = normalizePath(name))
  tb_name = tools::file_path_sans_ext(basename(name))
  DBI::dbWriteTable(db_out, name = tb_name, value = dataset, overwrite = TRUE)
  DBI::dbDisconnect(db_out)
}

