#' Title: Random Forest 4
#'
#' @description Simplified resampling, case weighting and general efficiency.
#' @param data A data.frame dataset
#' @param id Name of id variable
#' @param target Name of target variable
#' @param row_limit Optional: Limit number of rows in data
#' @param trees Optional: Number of trees
#' @param grid Optional: Tuning grid
#' @param n_boots Optional: Number of resamples
#' @param seed Optional: Seed
#' @param tune_metric Optional: One metric of "mae", "kap"
#' @param train_ratio Optional: Portion of train data. Default: 0.80
#' @param n Optional: Lower limit of entries per target class
#'
#' @return Object of class RF

#' @export
rf4 = function(

    data, id, bspid, target, row_limit = NA, trees = 1000, grid = 11, n_boots = 30,
    seed = NA, tune_metric = "mae", train_ratio = 0.80, n = 100, randomize_target = FALSE

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

  ### -- Target Variable ------------

  if ("target" %in% colnames(data)) {
    stop("Error in rf4: Found 'target' in colnames(data)")
  }

  # Set name of target and id column
  colnames(data)[colnames(data) == target] = "target"
  colnames(data)[colnames(data) == id] = "id"


  ### -- Data preparation -----------

  print("Data preparation")

  print("  - colnames():")

  print(colnames(data))

  # Eventually limit for testing
  if (!is.na(row_limit)) {
    data = data[1:row_limit,]
  }

  if (randomize_target) {
    data$target = sample(data$target, size = length(data$target), replace = FALSE)
  }

  # Factorize characters and make target numeric
  data = data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
    dplyr::mutate(target = as.numeric(target))

  # Delete target's with less than 100 entries
  data = data |>
    dplyr::group_by(target) |>
    dplyr::filter(dplyr::n() > n) |>
    dplyr::ungroup()

  # Calculate strata - Numeric
  if (is.numeric(data$target)) {
    data = data |>
      dplyr::group_by(id) |>
      dplyr::mutate(strata = factor(max(target))) |>
      dplyr::ungroup()
  } else if (is.factor(data$target)) {
    data = data |>
      dplyr::mutate(strata = factor(target))
  } else {
    data = data |>
      dplyr::mutate(strata = target)
  }

  # Get gini coefficient for evaluation of imbalance
  gini = DescTools::Gini(prop.table(table(as.factor(data$target))))

  ### -- Splitting ------------------

  print("Splitting")

  for (i in 1:1000) {

    # Initial split
    data_split = rsample::group_initial_split(
      data = data, group = "id", strata = "strata", prop = train_ratio
    )

    # Training
    train_data = rsample::training(data_split) |>
      dplyr::select(-strata)

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
  boots = rsample::bootstraps(train_data, strata = target, times = n_boots)

  ### -- Recipe ---------------------

  print("Recipe")

  id_vars = intersect(c("id", "schaden", "ort", "tbwnr", "schad_id", "bsp_id", "brucke"), colnames(data))
  print(paste("  - rm ID-Vars:", paste(id_vars, collapse = ", ")))

  rec = recipes::recipe(target ~ ., data = train_data) |>
    # Entfernung von Identifikationsvariablen aus daten
    recipes::update_role(dplyr::all_of(id_vars), new_role = "id") |>
    recipes::step_rm(recipes::has_role("id")) |>
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
    parsnip::set_mode("regression") |>
    parsnip::set_engine("ranger")

  # Define workflow
  ranger_workflow = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(ranger_spec)

  ### -- Tuning ---------------------

  print("Tuning")

  # Basic tuning
  ranger_tune = tune::tune_grid(
    ranger_workflow,
    resamples = boots,
    grid = grid,
    control = tune::control_grid(
      save_pred = TRUE,
      verbose = TRUE
    ),
    metrics = yardstick::metric_set(
      yardstick::mae,
      yardstick::rmse
    )
  )

  ### -- Fitting --------------------

  print("Fitting")

  # Choose best params
  best_params = tune::select_best(x = ranger_tune, metric = tune_metric)

  # Finalize workflow with best params
  final_wf = tune::finalize_workflow(ranger_workflow, best_params)

  # Fit RF with best params
  final_rf = final_wf |>
    parsnip::fit(data = train_data) |>
    workflows::extract_fit_parsnip()


  ### -- Predictions ----------------

  print("Predictions")

  levels_num = sort(unique(data$target))

  preds = predict(final_rf, new_data = test_data) |>
    dplyr::bind_cols(truth = test_data$target) |>
    # Round regression results
    dplyr::mutate(
      .pred_class = round(.pred),
      .pred_class = pmin(pmax(.pred_class, min(levels_num)), max(levels_num))
    ) |>
    # Factorize for classification metrics
    dplyr::mutate(
      .pred_class = factor(.pred_class, levels = levels_num, ordered = TRUE),
      truth       = factor(truth, levels = levels_num, ordered = TRUE)
    )

  ### -- Metrics --------------------

  print("Metrics")

  model_metrics = purrr::map_dfr(
    .x = levels(preds$truth),
    .f = metrics_df,
    pred = preds
  )

  # Additionally MAE ?
  # yardstick::mae(preds, truth = as.numeric(truth), estimate = .pred)

  ### -- Variable Importance --------

  print("Variable Importance")

  # Permutation based

  imp_spec = ranger_spec |>
    tune::finalize_model(best_params) |>
    parsnip::set_engine("ranger", importance = "permutation")

  perm_importance = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(imp_spec) |>
    parsnip::fit(train_data) |>
    workflows::extract_fit_parsnip() |>
    vip::vi()

  # Gini

  imp_spec = ranger_spec |>
    tune::finalize_model(best_params) |>
    parsnip::set_engine("ranger", importance = "impurity")

  gini_importance = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(imp_spec) |>
    parsnip::fit(train_data) |>
    workflows::extract_fit_parsnip() |>
    vip::vi()

  # Take time
  end = Sys.time()

  ### -- Instantiate model object ---

  print("Instantiate model object")

  m = methods::new(

    Class = "RF",

    # Info
    bspid = bspid,                                        # BSP ID of interest
    start = start,                                        # Start timestamp
    end = end,                                            # End timestamp

    # Model parameter
    seed            = seed,                               # Used seed
    trees           = trees,                              # Number of trees
    n_boots         = n_boots,                            # Number of boots for evaluation
    grid            = grid,                               # Tuning grid,
    tune_metric     = tune_metric,                        # Tuning metric

    # Text with characteristic
    param_txt = c(
      txt_spacer("Seed:", seed,                    35),
      txt_spacer("Number of Resamples:", n_boots,  35),
      txt_spacer("Tuning metric:", tune_metric,    35),
      txt_spacer("Best Params:", " ",              35),
      txt_spacer("    mtry:", best_params$mtry,    35),
      txt_spacer("    min_n:", best_params$min_n,  35),
      txt_spacer("Number of trees:", trees,        35)
    ),

    # Name of target variable
    target_var      = target,                             # Target

    # Data
    train_data      = train_data,                         # Train data
    test_data       = test_data,                          # Test data

    # Model setup
    ranger_spec     = ranger_spec,                        # Type of model
    recipe          = as.character(rec$steps),            # Recipe (only steps)

    # Model tuning
    tune_control    = tidyr::tibble(ranger_tune),         # Tuning parameter
    tune_metrics    = tune::collect_metrics(ranger_tune), # Tuning metrics

    # Results
    gini            = gini,                               # Gini imbalance coefficient
    best_params     = best_params,                        # Best parameter data.frame
    final_rf        = catch_console(final_rf),            # Final model (only console log)
    prediction      = list(pred = preds, prob = NULL),    # Predictions df
    metrics         = model_metrics,                      # Metrics data.frame

    perm_importance = perm_importance,                    # Vi Permutation-importance Obj
    gini_importance = gini_importance,                    # Vi Gini-importance Obj

    # Trained model
    trained_model   = final_rf$fit                        # Ranger-obj final model
  )

  ### -- Return model object --------

  return(m)

}
