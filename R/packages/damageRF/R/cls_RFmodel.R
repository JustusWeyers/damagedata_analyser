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
    identity        = "list",
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
    roles           = "list",
    target_var      = "character",
    train_data      = "data.frame",
    test_data       = "data.frame",

    # Model setup
    ranger_spec     = "ANY",
    recipe          = "ANY",

    # Model tuning
    tune_control    = "ANY",
    tune_metrics    = "ANY",

    # Results
    gini            = "numeric",
    best_params     = "data.frame",
    final_rf        = "ANY",
    prediction      = "list",
    metrics          = "data.frame",
    baseline_metrics = "data.frame",

    perm_importance = "ANY",
    gini_importance = "ANY",

    roc_df          = "data.frame",
    pr_df           = "data.frame",

    # Trained model
    trained_model   = "list",

    # Specifica
    size            = "numeric"
  ),

  prototype = list(

    # Info
    bspid       = NA_character_,
    start       = as.POSIXct(NA),
    end         = as.POSIXct(NA),

    # Model parameter
    seed        = NA_real_,
    trees       = NA_real_,
    min_max     = NA_real_,
    n_boots     = NA_real_,
    grid        = NA_real_,
    tune_metric = NA_character_,

    param_txt   = NA_character_,

    # Data
    roles       = list(),
    target_var  = NA_character_,
    train_data  = data.frame(),
    test_data   = data.frame(),

    # Model setup
    ranger_spec = NULL,
    recipe      = NULL,

    # Model tuning
    tune_control = NULL,
    tune_metrics = NULL,

    # Results
    gini            = NA_real_,
    best_params     = data.frame(),
    final_rf        = NULL,
    prediction      = list(),
    metrics          = data.frame(),
    baseline_metrics = data.frame(),

    perm_importance = NULL,
    gini_importance = NULL,

    roc_df          = data.frame(),
    pr_df           = data.frame(),

    # Trained model
    trained_model   = NULL,

    # Specifica
    size            = NA_real_
  )
)

setMethod(
  "initialize",
  "RF",
  function(.Object,
           bspid           = NA_character_,
           identity        = list(),
           start           = as.POSIXct(NA),
           end             = as.POSIXct(NA),
           seed            = NA_real_,
           trees           = NA_real_,
           min_max         = NA_real_,
           n_boots         = NA_real_,
           grid            = NA_real_,
           tune_metric     = NA_character_,
           param_txt       = NA_character_,
           roles           = list(),
           target_var      = NA_character_,
           train_data      = data.frame(),
           test_data       = data.frame(),
           ranger_spec     = NULL,
           recipe          = NULL,
           tune_control    = NULL,
           tune_metrics    = NULL,
           best_params     = data.frame(),
           final_rf        = NULL,
           gini            = NA_real_,
           prediction      = list(),
           metrics          = data.frame(),
           baseline_metrics = data.frame(),
           perm_importance  = NULL,
           gini_importance = NULL,
           roc_df          = data.frame(),
           pr_df           = data.frame(),
           trained_model   = NULL,
           size            = NA_real_,
           ...) {

    .Object@bspid           <- bspid
    .Object@start           <- start
    .Object@end             <- end
    .Object@seed            <- seed
    .Object@trees           <- trees
    .Object@min_max         <- min_max
    .Object@n_boots         <- n_boots
    .Object@grid            <- grid
    .Object@tune_metric     <- tune_metric
    .Object@param_txt       <- param_txt
    .Object@roles           <- roles
    .Object@target_var      <- target_var
    .Object@train_data      <- train_data
    .Object@test_data       <- test_data
    .Object@ranger_spec     <- ranger_spec
    .Object@recipe          <- recipe
    .Object@tune_control    <- tune_control
    .Object@tune_metrics    <- tune_metrics
    .Object@best_params     <- best_params
    .Object@final_rf        <- final_rf
    .Object@gini            <- gini
    .Object@prediction      <- prediction
    .Object@metrics          <- metrics
    .Object@baseline_metrics <- baseline_metrics
    .Object@perm_importance  <- perm_importance
    .Object@gini_importance <- gini_importance
    .Object@roc_df          <- roc_df
    .Object@pr_df           <- pr_df
    .Object@trained_model   <- trained_model

    .Object <- callNextMethod()

    .Object@size <- sum(
      sapply(slotNames(.Object), function(s) {
        object.size(slot(.Object, s))
      })
    )

    .Object
  }
)

