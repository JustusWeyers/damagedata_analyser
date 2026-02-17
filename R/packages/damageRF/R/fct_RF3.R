#' Title: Random Forest 3
#'
#' @description Simplified resampling, case weighting and general efficiency.
#' @param bspid A character BSP-ID of Interest
#' @param prep A data.frame dataset
#' @param target Name of targe variable
#' @param n Optional: Number of rows to use
#' @param trees Optional: Number of trees
#' @param grid Optional: tuning grid
#' @param n_boots Optional: Number of resamples
#' @param seed Optional: seed
#' @param tune_metric Optional: Tuning metric "roc_auc", "pr_auc", "accuracy" etc.

#' @return Object of class RF

#' @export
rf3 = function(bspid, prep, target, n = NA, trees = 1000, grid = 11, n_boots = 30, seed = NA, tune_metric = "pr_auc") {

  ############
  ### Seed ###
  ############

  print("Seed")

  # Take time
  start = Sys.time()

  # Create seed if necessary
  if (is.na(seed)) {
    seed = sample(1000:9999, size = 1)
  }

  # Set seed
  set.seed(seed)

  #######################
  ### Target Variable ###
  #######################

  if ("target" %in% colnames(prep)) {
    stop("Error in rf2: Found 'target' in colnames(prep)")
  }

  # Set name of target variable to "target"
  colnames(prep)[colnames(prep) == target] <- "target"

  ########################
  ### Data preparation ###
  ########################

  print("Data preparation")

  # Filter for bspid of interest
  prep_1bspid = prep |>
    # dplyr::filter(bsp_id == bspid) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  # Eventually delete data
  if (!is.na(n)) {
    prep_1bspid = prep_1bspid[1:n,]
  }

  # Delete target's with less than two entries
  prep_1bspid = prep_1bspid |>
    dplyr::group_by(target) |>
    dplyr::filter(dplyr::n() > 2) |>
    dplyr::ungroup()

  # Calculate max_target
  prep_1bspid = prep_1bspid |>
    dplyr::group_by(schaden) |>
    dplyr::mutate(max_target = max(target)) |>
    dplyr::ungroup() |>
    dplyr::mutate(max_target = factor(max_target))

  # Get gini coefficient
  gini = DescTools::Gini(prop.table(table(as.factor(prep_1bspid$target))))

  ####################
  ### Case weights ###
  ####################

  weights_df =
    prep_1bspid |>
    dplyr::count(target, name = "n_class") |>
    dplyr::mutate(
      K = dplyr::n(),
      N = sum(n_class),
      weight = N / (K * n_class)
    ) |>
    dplyr::select(target, weight)

  prep_1bspid = prep_1bspid |>
    dplyr::left_join(weights_df, by = "target") |>
    dplyr::mutate(.case_weights = hardhat::importance_weights(weight)) |>
    dplyr::select(-weight)

  #################
  ### Splitting ###
  #################

  print("Splitting")

  for (i in 1:100) {

    # Initial split
    data_split = rsample::group_initial_split(
      data = prep_1bspid, group = "schaden", strata = "max_target", prop = 0.80
    )

    # Training
    train_data = rsample::training(data_split) |>
      dplyr::select(-max_target)

    # Testing
    test_data  = rsample::testing(data_split) |>
      dplyr::select(-max_target)

    # Factorize target variable
    train_data_fac = train_data |>
      dplyr::mutate(target = factor(target)) |>
      dplyr::ungroup()

    test_data_fac  = test_data  |>
      dplyr::mutate(target = factor(target)) |>
      dplyr::ungroup()

    if (identical(levels(train_data_fac$target), levels(test_data_fac$target))) {
      break
    } else {
      print(paste0("Redo Split ", i, "/100"))
      seed = 1000 + i
      set.seed(seed)
    }
  }

  print(table(train_data_fac$target))
  print(table(test_data_fac$target))

  ##################
  ### Resampling ###
  ##################

  # Bootstraps
  boots = rsample::bootstraps(train_data_fac, strata = target, times = n_boots)

  ##############
  ### Recipe ###
  ##############

  print("Recipe")

  rec = recipes::recipe(target ~ ., data = train_data_fac) |>
    # themis::step_upsample(target) |>
    # Entfernung von Identifikationsvariablen aus daten
    recipes::update_role(schaden, ort, tbwnr, schad_id, new_role = "id") |>
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
    ) |>
    # Normalize numeric predictors
    recipes::step_normalize(recipes::all_numeric_predictors())

  ################
  ### Workflow ###
  ################

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

  ##############
  ### Tuning ###
  ##############

  print("Tuning")

  # Tune
  ranger_tune = tune::tune_grid(
    ranger_workflow,
    resamples = boots,
    grid = grid,
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE),
    metrics = yardstick::metric_set(
      yardstick::pr_auc,
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::brier_class
    )
  )

  ###############
  ### Fitting ###
  ###############

  print("Fitting")

  # Besten Parameter auswählen
  best_params = tune::select_best(x = ranger_tune, metric = tune_metric)

  # Besten RF erstellen
  final_rf = tune::finalize_workflow(ranger_workflow, best_params) |>
    parsnip::fit(data = train_data_fac) |>
    workflows::extract_fit_parsnip()

  ###################
  ### Predictions ###
  ###################

  print("Predictions")

  probs = stats::predict(final_rf, new_data = test_data_fac, type = "prob") |>
    dplyr::bind_cols(truth = test_data_fac$target)

  preds = stats::predict(final_rf, new_data = test_data_fac) |>
    dplyr::bind_cols(truth = test_data_fac$target) |>
    dplyr::select(truth, .pred_class)

  ###############
  ### Metrics ###
  ###############

  print("Metrics")

  model_metrics = purrr::map_dfr(
    .x = levels(preds$truth),
    .f = metrics_df,
    pred = preds
  )

  #####################
  ### ROC / PR Plot ###
  #####################

  print("ROC / PR Plot")

  est_cols = grep("^\\.pred_", colnames(probs), value = TRUE)

  # ROC

  roc_df = yardstick::roc_curve(
    data = probs,
    truth = truth,
    !!!rlang::syms(est_cols)
  ) |>
    dplyr::mutate(type = "ROC") |>
    dplyr::mutate(x = 1 - specificity) |>
    dplyr::mutate(y = sensitivity) |>
    dplyr::select(.level, x, y) |>
    dplyr::mutate(type = "ROC") |>
    data.frame()

  # PR

  pr_df = yardstick::pr_curve(
    data = probs,
    truth = truth,
    !!!rlang::syms(est_cols)
  ) |>
    dplyr::mutate(type = "PR") |>
    dplyr::mutate(x = recall) |>
    dplyr::mutate(y = precision) |>
    dplyr::select(.level, x, y) |>
    dplyr::mutate(type = "PR") |>
    data.frame()

  ###########################
  ### Variable Importance ###
  ###########################

  print("Variable Importance")

  # Permutation

  imp_spec = ranger_spec |>
    tune::finalize_model(tune::select_best(ranger_tune, metric = tune_metric)) |>
    parsnip::set_engine("ranger", importance = "permutation")

  perm_importance = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(imp_spec) |>
    workflows::add_case_weights(.case_weights) |>
    parsnip::fit(train_data_fac) |>
    workflows::extract_fit_parsnip() |>
    vip::vi()

  # Gini

  imp_spec = ranger_spec |>
    tune::finalize_model(tune::select_best(ranger_tune, metric = tune_metric)) |>
    parsnip::set_engine("ranger", importance = "impurity")

  gini_importance = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(imp_spec) |>
    workflows::add_case_weights(.case_weights) |>
    parsnip::fit(train_data_fac) |>
    workflows::extract_fit_parsnip() |>
    vip::vi()

  # Take time
  end = Sys.time()

  ################################
  ### Instantiate model object ###
  ################################

  print("Instantiate model object")

  m = methods::new(

    Class = "RF",

    # Info
    bspid = bspid,                             # BSP ID of interest
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
      txt_spacer("Seed:", seed, 35),
      txt_spacer("Number of Resamples:", n_boots, 35),
      txt_spacer("Tuning metric:", tune_metric, 35),
      txt_spacer(
        paste0('Weights ("', paste(weights_df$target, collapse = '"/"'), '"):'),
        " ",
        35
      ),
      txt_spacer(
        " ",
        paste(round(weights_df$weight, 2), collapse = "/"),
        35
      ),
      txt_spacer("Best Params:", " ", 35),
      txt_spacer("    mtry:", best_params$mtry, 35),
      txt_spacer("    min_n:", best_params$min_n, 35),
      txt_spacer("Number of trees:", trees, 35)
    ),

    target_var      = target,                  # Name of target variable
    # Data
    train_data      = train_data_fac,          # Train data
    test_data       = test_data_fac,           # Test data

    # Model setup
    ranger_spec     = ranger_spec,             # Type of model
    recipe          = as.character(rec$steps), # Recipe (only steps)

    # Model tuning
    tune_control    = tidyr::tibble(ranger_tune),         # Tuning parameter
    tune_metrics    = tune::collect_metrics(ranger_tune), # Tuning metrics

    # Results
    gini            = gini,                    # Gini imbalance coefficient
    best_params     = best_params,             # Best parameter data.frame
    final_rf        = catch_console(final_rf), # Final model (only console log)
    prediction      = list(pred = preds, prob = probs),   # Predictions df
    metrics         = model_metrics,           # Metrics data.frame

    roc_df          = roc_df,                  # ROC data.frame
    pr_df           = pr_df,                   # PR data.frame

    perm_importance = perm_importance,         # Vi Permutation-importance Obj
    gini_importance = gini_importance,         # Vi Gini-importance Obj

    # Trained model
    trained_model    = final_rf$fit            # Ranger-obj final model
  )

  # Return model object
  return(m)

}
