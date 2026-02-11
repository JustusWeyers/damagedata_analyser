
### Recursive Feature Elimination

####################
### Dependencies ###
####################

require(ranger)

#############
### Setup ###
#############

# Print wd
print("Working directory:")
print(getwd())

# List /app/output
print("Contents of /app/output:")
print(list.files("output", recursive = TRUE))

# Fetch paths from env
db_path = Sys.getenv("DB_PATH", unset = "output/dataset_rfe.db")
out_path = Sys.getenv("OUTPUT_PATH", unset = "output/")

N = Sys.getenv("N", unset = NA)

# Check db_path
print("DB path:")
print(db_path)

# Check out_path
print("Output path:")
print(out_path)

# DB Connection
print("Connecting to database...")

if (!RSQLite::dbCanConnect(RSQLite::SQLite(), db_path)) {
  stop("Cannot connect to database at: ", db_path)
} else {
  db = RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  print("Successfully connected to database.")
}

############
### Data ###
############

# Fetch data
print("Fetch data")

tab = DBI::dbListTables(db)[1]

data = DBI::dbReadTable(db, tab) |> 
  dplyr::mutate(across(where(is.numeric),~ replace(., !is.finite(.), NA))) |> 
  dplyr::mutate(across(where(is.character), as.factor))

# Prepare data
print("Prepare Data")

data_one = data |>
  dplyr::select(c("schaden", "bsp_id")) |> 
  dplyr::group_by(schaden) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

bspids = sort(table(data_one$bsp_id), decreasing = TRUE)

###########
### RFE ###
###########

print("Start loop")

for (bspid in names(bspids)){
  
  print(bspid)
  
  tryCatch({
    
    prep = data |> 
      dplyr::filter(bsp_id == bspid) |> 
      dplyr::select(-c("bsp_id", "schaden", "ort", "tbwnr", "schad_id", "bauteil")) |> 
      dplyr::mutate(bewert_d = factor(bewert_d))
    
    # Comment out
    if (!is.na(N)) {
      prep = prep[1:N,]
    }
    
    rfRFE_functions = list(
      # Summary
      summary = caret::defaultSummary,
      fit = function(x, y, first, last, ...) {
        parsnip::rand_forest(trees = 500, mtry = min(ncol(x), ceiling(sqrt(ncol(x)))), min_n = 5) |>
          parsnip::set_mode("classification") |>
          parsnip::set_engine("ranger", importance = "impurity") |>
          parsnip::fit(y ~ ., data = cbind(x, y = y))
      },
      # Prediction
      pred = function(object, x) {
        stats::predict(object, new_data = x, type = "class")$.pred_class
      },
      # Ranking (variable importance)
      rank = function(object, x, y) {
        vimp = object$fit$fit$variable.importance
        vimp_df = data.frame(
          var = colnames(x),
          Overall = ifelse(colnames(x) %in% names(vimp), vimp[colnames(x)], 0)
        )
        vimp_df[order(vimp_df$Overall, decreasing = TRUE), ]
      },
      # Stuff
      selectSize = caret::pickSizeBest,
      selectVar = caret::pickVars
    )
    
    rfRFE = caret::rfe(
      x = prep |> dplyr::select(-bewert_d),
      y = prep$bewert_d,
      sizes = sort(c(1, 5, 10, 20, 30, 40, 50, ncol(prep)-1)),
      rfeControl = caret::rfeControl(
        functions = rfRFE_functions,
        method = "cv",
        index = caret::createFolds(
          prep$bewert_d, 
          k = min(5, nrow(prep)), 
          returnTrain = TRUE
        )
      )
    )
    
    print(paste("Save:", paste0(out_path, "rfe_", bspid, ".rds")))
    saveRDS(rfRFE, paste0(out_path, "rfe_", bspid, ".rds"))
  
  }, error = function(e) {
    print(e)
    return(e)
  })
  

}

