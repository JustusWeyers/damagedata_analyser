#' Title: Random Forest 6 - Multiclass Classification
#'
#' @description Multiclass classification with group folds, case weighting and probability output.
#' @param identity Optional: List with identity information
#' @param data A data.frame dataset
#' @param id Name of id variable
#' @param id_vars Names of id/grouping variables to exclude from predictors
#' @param target_vars Names of other target variables to exclude from predictors
#' @param target Name of target variable
#' @param time_ax Name of time axis variable
#' @param row_limit Optional: Limit number of rows in data
#' @param trees Optional: Number of trees
#' @param grid Optional: Tuning grid size
#' @param n_boots Optional: Number of resamples/folds
#' @param seed Optional: Seed
#' @param tune_metric Optional: Tuning metric "roc_auc", "accuracy", "f_meas", "pr_auc"
#' @param train_ratio Optional: Portion of train data. Default: 0.80
#' @param n Optional: Lower limit of entries per target class
#' @param randomize_target Optional: Randomize target variable (for baseline testing)
#' @param testrun Optional: Speed up for testing

#' @return Object of class RF

#' @export
rf6_multiclass = function(

  identity = list(), data, id, id_vars, target_vars, target, time_ax, row_limit = NA,
  trees = 1000, grid = 11, n_boots = 30, seed = NA, tune_metric = "pr_auc",
  train_ratio = 0.80, n = 30, randomize_target = FALSE, testrun = FALSE

  ) {

  ### -- Seed -----------------------

  print("Seed")

  # Take time
  start = Sys.time()

  # Create seed if necessary
  if (is.na(seed)) {
    seed = sample(1000:9999, size = 1)
  }

  # Set seed
  set.seed(seed)

  ### -- Testrun? -------------------

  # Speed things up
  if (testrun) {
    row_limit = 5000
    trees = 500
    grid = 3
    n_boots = 5
  }

  ### -- Variable/Column roles ------

  if ("target" %in% colnames(data)) {
    stop("Error in rf6_multiclass: Found 'target' in colnames(data)")
  }

  # 1. Rollen festlegen (mit original Spaltennamen)
  id_vars     = setdiff(intersect(colnames(data), id_vars), id)
  target_vars = setdiff(intersect(colnames(data), target_vars), target)
  pred_vars   = setdiff(colnames(data), c(id_vars, target_vars, target, id))

  # 2. Erst danach umbenennen
  colnames(data)[colnames(data) == target] = "target"
  colnames(data)[colnames(data) == id]     = "id"


  ### -- Data preparation -----------

  # Eventually limit for testing
  if (!is.na(row_limit)) {
    data = data[1:row_limit,]
  }

  if (randomize_target) {
    data$target = sample(data$target, size = length(data$target), replace = FALSE)
  }

  data = data |> tidyr::drop_na(target)

  # Factorize characters and make target numeric
  data = data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  # Delete target's with less than n entries
  data = data |>
    dplyr::group_by(target) |>
    dplyr::filter(dplyr::n() > n) |>
    dplyr::ungroup()

  # Calculate strata - factor
  data = data |>
    dplyr::mutate(strata = factor(target))

  freq_table <- data |>
    dplyr::count(strata, name = "freq")

  data <- data |>
    dplyr::left_join(freq_table, by = "strata") |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      strata = strata[order(freq, strata)][1]
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-freq)

  # Get gini coefficient for evaluation of imbalance
  gini = DescTools::Gini(prop.table(table(as.factor(data$target))))

  ### -- Case weights ---------------

  weights_df =
    data |>
    dplyr::count(target, name = "n_class") |>
    dplyr::mutate(
      K = dplyr::n(),
      N = sum(n_class)
    ) |>
    dplyr::mutate(weight = N / (K * n_class)) |>
    # dplyr::mutate(weight = sqrt(N / (K * n_class))) |>
    dplyr::select(target, weight)

  data = data |>
    dplyr::left_join(weights_df, by = "target") |>
    dplyr::mutate(.case_weights = hardhat::importance_weights(weight)) |>
    dplyr::select(-weight)

  ### -- Splitting ------------------

  print("Splitting")

  for (i in 1:1000) {

    # Initial split
    data_split = rsample::group_initial_split(
      data = data, group = "id", strata = "strata", prop = train_ratio
    )

    # Training (strata noch behalten für group_vfold_cv)
    train_data = rsample::training(data_split)

    # Testing
    test_data  = rsample::testing(data_split) |>
      dplyr::select(-strata)

    # Factorize target
    train_data_fac = train_data |>
      dplyr::mutate(target = factor(target))

    test_data_fac = test_data |>
      dplyr::mutate(target = factor(target))

    # Fetch levels
    train_levels = levels(train_data_fac$target)
    test_levels = levels(test_data_fac$target)

    if (identical(train_levels, test_levels)) {

      # Stop splitting
      break

    } else {

      # Repeat splitting
      if (i == 1000) {
        stop("Stopped splitting due to imbalanced stratification var")
      }
      if (i %% 100 == 0) {
        print(paste0("Redo Split ", i, "/100"))
      }
      seed = 1000 + i
      set.seed(seed)

    }
  }

  ### -- Resampling -----------------

  # Bootstraps
  boots = rsample::bootstraps(train_data_fac, strata = target, times = n_boots)

  # Folds (strata-Spalte ist noch vorhanden und gruppenintern konstant)
  folds = rsample::group_vfold_cv(train_data_fac, group = "id", strata = "strata", v = n_boots)

  # Boots or folds?
  resamples = folds

  # strata entfernen (wird nicht mehr benötigt)
  train_data     = train_data     |> dplyr::select(-strata)
  train_data_fac = train_data_fac |> dplyr::select(-strata)

  ### -- Recipe ---------------------

  print("Recipe")

  print(paste("  - rm ID-vars:", paste(id_vars, collapse = ", ")))
  print(paste("  - rm target-vars:", paste(target_vars, collapse = ", ")))

  rec = recipes::recipe(target ~ ., data = train_data_fac) |>
    # Entfernung von Identifikationsvariablen aus daten
    recipes::update_role(dplyr::all_of(id_vars), new_role = "id") |>
    recipes::step_rm(recipes::has_role("id")) |>
    # Entfernung möglicher Zielvariablen
    recipes::update_role(dplyr::all_of(target_vars), new_role = "target_var") |>
    recipes::step_rm(recipes::has_role("target_var")) |>
    # strata-Spalte entfernen falls in Fold-Daten vorhanden
    recipes::step_rm(tidyselect::any_of("strata")) |>
    # Viele seltene Variablen zusammenfassen
    recipes::step_other(
      recipes::all_factor_predictors(), threshold = 0.01, other = "other"
    ) |>
    # Umgang mit unbekannten Faktoren/neue Levels im Test setzen
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    # NA handling
    recipes::step_unknown(
      recipes::all_nominal_predictors(), new_level = "unknown"
    )

  ### -- Workflow -------------------

  print("Workflow")

  # Random forest for classification
  ranger_spec = parsnip::rand_forest(
    mtry = tune::tune(),
    min_n = tune::tune(),
    trees = trees
  ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger", probability = TRUE)

  # Define workflow
  ranger_workflow = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(ranger_spec) |>
    workflows::add_case_weights(.case_weights)

  ### -- Tuning ---------------------

  print("Tuning")

  f1_weighted <- function(data, truth, estimate, ...) {
    yardstick::f_meas(
      data = data,
      truth = !!rlang::enquo(truth),
      estimate = !!rlang::enquo(estimate),
      estimator = "macro_weighted",
      ...
    )
  }

  # Tune
  ranger_tune = tune::tune_grid(
    ranger_workflow,
    resamples = resamples,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE),
    metrics = yardstick::metric_set(
      yardstick::pr_auc,
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::f_meas
    )
  )

  ###############
  ### Fitting ###
  ###############

  print("Fitting")

  # Besten Parameter auswählen
  best_params = tune::select_best(x = ranger_tune, metric = tune_metric)

  # Out of Folds/Bag Predictions
  oofb = tune::collect_predictions(ranger_tune, parameters = best_params)

  # Finalize workflow with best params
  final_wf = tune::finalize_workflow(ranger_workflow, best_params)

  # Fit RF with best params
  final_wf_fitted = final_wf |>
    parsnip::fit(data = train_data_fac)

  final_rf = final_wf_fitted |>
    workflows::extract_fit_parsnip()

  prepped_recipe = workflows::extract_recipe(final_wf_fitted, estimated = TRUE)
  fit = final_rf$fit

  ### -- Predictions ----------------

  print("Predictions")

  probs = stats::predict(final_rf, new_data = test_data_fac, type = "prob") |>
    dplyr::bind_cols(truth = test_data_fac$target)

  preds = stats::predict(final_rf, new_data = test_data_fac) |>
    dplyr::bind_cols(truth = test_data_fac$target) |>
    dplyr::select(truth, .pred_class)

  ### -- Metrics --------------------

  print("Metrics")

  model_metrics = purrr::map_dfr(
    .x = levels(preds$truth),
    .f = metrics_df,
    pred = preds
  )

  ### -- ROC / PR Plot --------------

  # ROC/PR

  est_cols = grep("^\\.pred_", colnames(probs), value = TRUE)

  curve_to_df = function(curve_fn, x_expr, y_expr, type_label) {
    if (length(levels(probs$truth)) == 2) {
      df = purrr::map_dfr(est_cols, \(col) {
        lvl = sub("^\\.pred_", "", col)
        ev  = if (lvl == levels(probs$truth)[1]) "first" else "second"
        curve_fn(probs, truth = truth, !!rlang::sym(col), event_level = ev) |>
          dplyr::mutate(.level = lvl)
      })
    } else {
      df = curve_fn(probs, truth = truth, !!!rlang::syms(est_cols))
    }
    df |>
      dplyr::transmute(.level, x = {{x_expr}}, y = {{y_expr}}, type = type_label) |>
      data.frame()
  }

  roc_df = curve_to_df(yardstick::roc_curve, 1 - specificity, sensitivity, "ROC")
  pr_df  = curve_to_df(yardstick::pr_curve,  recall,           precision,  "PR")

  ### -- Variable Importance --------

  print("Variable Importance")

  # Permutation/Gini

  best_spec = ranger_spec |>
    tune::finalize_model(best_params)

  compute_vi = function(importance_type) {
    spec = best_spec |>
      parsnip::set_engine("ranger", importance = importance_type)
    workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(spec) |>
      workflows::add_case_weights(.case_weights) |>
      parsnip::fit(train_data_fac) |>
      workflows::extract_fit_parsnip() |>
      vip::vi()
  }

  perm_importance = compute_vi("permutation")
  gini_importance = compute_vi("impurity")

  # Take time
  end = Sys.time()

  ################################
  ### Instantiate model object ###
  ################################

  print("Instantiate model object")

  m = methods::new(

    Class = "RF",

    # Info
    identity = identity,                       # Identity (bspid/group of bspids)
    start = start,                             # Start timestamp
    end = end,                                 # End timestamp

    # Model parameter
    seed            = seed,                    # Used seed
    trees           = trees,                   # Number of trees
    min_max         = NaN,                     # Obsolete
    n_boots         = n_boots,                 # Number of boots for evaluation
    grid            = grid,                    # Tuning grid,
    tune_metric     = tune_metric,             # Tuning metric

    param_txt = c(
      txt_spacer("Seed:", seed,                   35),
      txt_spacer("Number of Resamples:", n_boots, 35),
      txt_spacer("Tuning metric:", tune_metric,   35),
      txt_spacer(
        paste0('Weights (Class 1-', nrow(weights_df), '):'),
        " ",
        35
      ),
      txt_spacer(
        " ",
        paste(round(weights_df$weight, 2), collapse = "/"),
        35
      ),

      txt_spacer("Best Params:", " ",             35),
      txt_spacer("    mtry:", best_params$mtry,   35),
      txt_spacer("    min_n:", best_params$min_n, 35),
      txt_spacer("Number of trees:", trees,       35)
    ),

    target_var      = target,                  # Name of target variable
    # Data
    train_data      = train_data_fac,          # Train data
    test_data       = test_data_fac,           # Test data

    # Model setup
    ranger_spec     = ranger_spec,             # Type of model
    recipe          = as.character(rec$steps), # Recipe (only steps)

    # Model tuning
    tune_metrics    = tune::collect_metrics(ranger_tune), # Tuning metrics

    # Results
    gini            = gini,                    # Gini imbalance coefficient
    best_params     = best_params,             # Best parameter data.frame
    final_rf        = catch_console(final_rf), # Final model (only console log)
    prediction      = list(                    # Predictions df
      pred           = preds,
      prob           = probs,
      OOFB           = oofb
    ),
    metrics         = model_metrics,           # Metrics data.frame

    roc_df          = roc_df,                  # ROC data.frame
    pr_df           = pr_df,                   # PR data.frame

    perm_importance = perm_importance,         # Vi Permutation-importance Obj
    gini_importance = gini_importance,         # Vi Gini-importance Obj

    # Trained model
    trained_model    = list(
      recipe    = prepped_recipe,
      fit       = fit
    )
  )

  # Return model object
  return(m)

}
