# Usage of s3 classes
if (!methods::isClass("ggplot")) {
  methods::setOldClass(c("gg", "ggplot"))
}

if (!methods::isClass("rand_forest")) {
  methods::setOldClass(c("rand_forest", "rand_forest"))
}
if (!methods::isClass("ranger")) {
  methods::setOldClass(c("ranger", "ranger"))
}
if (!methods::isClass("vi")) {
  methods::setOldClass(c("vi", "vi"))
}

# Class RF definition
methods::setClass(
  "RF",
  slots = c(

    # Info
    bspid           = "character",
    start           = "POSIXct",
    end             = "POSIXct",

    # Model parameter
    seed            = "numeric",
    trees           = "numeric",
    min_max         = "numeric",
    n_boots         = "numeric",
    grid            = "numeric",
    tune_metric     = "character",

    param_txt       = "character",

    # Data
    target_var      = "character",
    train_data      = "data.frame",
    test_data       = "data.frame",

    # Model setup
    ranger_spec     = "rand_forest",
    recipe          = "character",

    # Model tuning
    tune_control    = "data.frame",
    tune_metrics    = "data.frame",

    # Results
    gini            = "numeric",
    best_params     = "data.frame",
    final_rf        = "character",
    prediction      = "list",
    metrics         = "data.frame",

    perm_importance = "vi",
    gini_importance = "vi",

    roc_df          = "data.frame",
    pr_df           = "data.frame",

    # Trained model
    trained_model   = "ranger",

    # Specifica
    size = "numeric"
  )
)

setMethod(
  "initialize",
  "RF",
  function(.Object, bspid, start, end, seed, trees, min_max, n_boots, grid,
           tune_metric, param_txt, target_var, train_data, test_data,
           ranger_spec, recipe, tune_control, tune_metrics, best_params,
           final_rf, gini, prediction, metrics, perm_importance,
           gini_importance, roc_df, pr_df, trained_model) {

    .Object@bspid <- bspid
    .Object@start <- start
    .Object@end <- end
    .Object@seed <- seed
    .Object@trees <- trees
    .Object@min_max <- min_max
    .Object@n_boots <- n_boots
    .Object@grid <- grid
    .Object@tune_metric <- tune_metric
    .Object@param_txt <- param_txt
    .Object@target_var <- target_var
    .Object@train_data <- train_data
    .Object@test_data <- test_data
    .Object@ranger_spec <- ranger_spec
    .Object@recipe <- recipe
    .Object@tune_control <- tune_control
    .Object@tune_metrics <- tune_metrics
    .Object@best_params <- best_params
    .Object@final_rf <- final_rf
    .Object@gini <- gini
    .Object@prediction <- prediction
    .Object@metrics <- metrics
    .Object@perm_importance <- perm_importance
    .Object@gini_importance <- gini_importance
    .Object@roc_df <- roc_df
    .Object@pr_df <- pr_df

    .Object@trained_model <- trained_model

    # Size of initialized object
    .Object@size <- sum(sapply(methods::slotNames(.Object), function(s) {
      utils::object.size(methods::slot(.Object, s))
    }))

    .Object <- methods::callNextMethod()

    .Object
  }
)
