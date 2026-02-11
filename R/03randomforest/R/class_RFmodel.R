methods::setOldClass(c("gg", "ggplot"))
methods::setOldClass(c("rand_forest", "rand_forest"))
methods::setOldClass(c("workflow", "workflow"))
methods::setOldClass(c("recipe", "recipe"))
methods::setOldClass(c("tune_results", "tune_results"))
methods::setOldClass(c("manual_rset", "manual_rset"))

methods::setClass(
  "RF",
  slots = c(
    
    # Info
    bspid = "character",
    
    # Parameter
    min_max = "numeric",
    seed = "numeric",
    
    # Data
    # data = "data.frame", 
    train_data = "data.frame",
    # boots = "list",
    test_data = "data.frame",
    
    # Model
    # recipe = "recipe",
    # ranger_spec = "rand_forest",
    # ranger_workflow = "workflow",
    
    # Results
    # ranger_tune = "tune_results",
    best_params = "tbl_df",
    final_rf = "workflow",
    prediction = "list",
    metrics = "data.frame",
    
    # Plots
    hist_bewert_d = "gg",
    roc_plt = "gg",
    perm_importance = "gg",
    gini_importance = "gg"
  )
)

# setGeneric("showDetails", function(object) standardGeneric("showDetails"))

# setMethod("showDetails", "Person", function(object) {
#   paste("Person Details: Name:", object@name, ", Alter:", object@age)
# })
