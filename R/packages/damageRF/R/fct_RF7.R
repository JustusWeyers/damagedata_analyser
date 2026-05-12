#' Title: Random Forest 7 - Quantile Regression Forest
#'
#' @description Quantile regression forest based on RF5. Tuning uses standard
#'   regression (mae/rmse) to find optimal hyperparameters; the final model is
#'   refitted with quantreg = TRUE to produce full quantile predictions.
#' @param identity Identity (bspid / group of bspids)
#' @param data A data.frame dataset
#' @param id Name of id variable
#' @param id_vars Character vector of id-like variables to exclude
#' @param target_vars Character vector of target-like variables to exclude
#' @param target Name of target variable
#' @param row_limit Optional: Limit number of rows in data
#' @param trees Optional: Number of trees
#' @param grid Optional: Tuning grid size
#' @param n_boots Optional: Number of bootstrap resamples
#' @param seed Optional: Seed
#' @param tune_metric Optional: "mae" or "rmse"
#' @param train_ratio Optional: Proportion of train data. Default: 0.80
#' @param n Optional: Lower limit of entries per target value
#' @param quantiles Optional: Quantile levels to predict
#' @param randomize_target Optional: Shuffle target for null model
#' @param testrun Optional: Speed up for testing
#'
#' @return Object of class RF

#' @export
rf7 = function(

  identity, data, id, id_vars, target_vars, target, time_ax, row_limit = NA,
  trees = 1000, grid = 11,  n_boots = 30, seed = NA, tune_metric = "mae",
  train_ratio = 0.80, n = 30, randomize_target = FALSE, testrun = FALSE,
  quantiles = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)

  ) {

  ### -- Start ------------------------------

  # Start time
  start = Sys.time()

  # Start logs
  title = paste0("RF7 - Ordinal Regression via QRF (", target, ")")
  print(paste0(rep("-", nchar(title)), collapse = ""))
  print(title)
  print(start)
  print(paste0(rep("-", nchar(title)), collapse = ""))


  ### -- Do testrun? ------------------------

  # Eventually limit tuning parameters
  if (testrun) {

    print(">> This is a testrun")

    row_limit = 5000
    trees     = 500
    grid      = 3
    n_boots   = 3
  }

  ### -- 01. Seeding ------------------------

  print("01. Seeding")

  if (is.na(seed)) {
    seed = sample(1000:9999, size = 1)
  }

  set.seed(seed)

  ### -- 02. Variable/Column roles ----------

  print("02. Variable/Column roles")

  # Break if internal name "target" is already a column name in data
  if ("target" %in% colnames(data)) {
    stop("Error in rf7: Found 'target' in colnames(data)")
  }

  # Restrict id_vars and target_vars to columns actually present in data
  id_vars     = intersect(colnames(data), id_vars)
  target_vars = setdiff(intersect(colnames(data), target_vars), target)

  # Rename to internal names so code is independent of caller-supplied names
  colnames(data)[colnames(data) == target] = "target"
  colnames(data)[colnames(data) == id]     = "id"

  # Fetch names of predictors
  pred_vars   = setdiff(colnames(data), c("id", "target", id_vars, target_vars))

  ### -- 03. Data preparation ---------------

  print("03. Data preparation")

  # Eventually limit rows of given data (primarily for faster testing)
  if (!is.na(row_limit)) {
    n_ids = ceiling(row_limit / (nrow(data) / dplyr::n_distinct(data$id)))
    n_ids = min(n_ids, dplyr::n_distinct(data$id))
    sampled_ids = sample(unique(data$id), size = n_ids)
    data = data[data$id %in% sampled_ids, ]
  }

  # Eventually randomize target variable for model validation
  if (randomize_target) {
    data$target = sample(
      data$target, size = length(data$target), replace = FALSE
    )
  }

  # Factorize characters (excluding id) and make target numeric for regression
  data = data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character) & !dplyr::all_of("id"), as.factor)) |>
    dplyr::mutate(target = as.numeric(target)) |>
    tidyr::drop_na(target)

  # Drop target levels with too few observations to ensure meaningful splits
  data = data |>
    dplyr::group_by(target) |>
    dplyr::filter(dplyr::n() > n) |>
    dplyr::ungroup()

  # Gini-coefficient as metric for target imbalance
  gini = DescTools::Gini(prop.table(table(as.factor(data$target))))

  ### -- 04. Splitting ----------------------

  print("04. Splitting")

  # Make sure to group split only by first part of id (separated by "_")
  if (any(grepl("_", as.character(data$id)))) {
    data$gsplitvar = sapply(strsplit(data$id, "_"), `[`, 1)
  } else {
    data$gsplitvar = data$id
  }

  # Strata = worst observed target per gsplitvar group (must be constant within split group)
  data = data |>
    dplyr::group_by(gsplitvar) |>
    dplyr::mutate(strata = factor(max(target))) |>
    dplyr::ungroup()

  # Retry 1000 splits if test set is missing any target level present in train
  for (i in 1:1000) {

    # Rsample groupwise split over strata
    data_split = rsample::group_initial_split(
      data = data, group = "gsplitvar", strata = "strata", prop = train_ratio
    )

    # Create test and train dataset
    train_data = rsample::training(data_split) |>
      dplyr::select(-strata, -gsplitvar)
    test_data  = rsample::testing(data_split)  |>
      dplyr::select(-strata, -gsplitvar)

    # Test if split was succesfull
    if (
      identical(
        levels(factor(train_data$target)),
        levels(factor(test_data$target)))
      ) {
      break
    } else {
      if (i == 1000) stop("Stopped splitting (too imbalanced strata)")
      if (i %% 100 == 0) print(paste0("Try split ", i, "/1000"))
      seed = 1000 + i
      set.seed(seed)
    }
  }

  ### -- 05. Bootstrapping ------------------

  print("05. Bootstrapping")

  # Create bootstrapped resamples
  boots = rsample::bootstraps(train_data, strata = target, times = n_boots)

  ### -- 06. Recipe -------------------------

  print("06. Recipe")

  print(paste("  - rm id-vars:", paste(id_vars, collapse = ", ")))
  print(paste("  - rm pred-vars:", paste(pred_vars, collapse = ", ")))
  print(paste("  - rm target-vars:", paste(target_vars, collapse = ", ")))

  rec = recipes::recipe(target ~ ., data = train_data) |>
    recipes::update_role(dplyr::all_of(id_vars), new_role = "id") |>
    recipes::step_rm(recipes::has_role("id")) |>
    recipes::update_role(dplyr::all_of(target_vars), new_role = "target_var") |>
    recipes::step_rm(recipes::has_role("target_var")) |>
    recipes::step_other(recipes::all_factor_predictors(), threshold = 0.01, other = "other") |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_unknown(recipes::all_nominal_predictors(), new_level = "unknown")

  ### -- 07. Workflow (Tuning) --------------

  print("07. Workflow")

  # Standard regression spec for (without quantreg)
  ranger_spec = parsnip::rand_forest(
    mtry  = tune::tune(),
    min_n = tune::tune(),
    trees = trees
  ) |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("ranger")

  ### -- 08. Tuning -------------------------

  print("08. Tuning")

  ranger_tune = tune::tune_grid(
    workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(ranger_spec),
    resamples = boots,
    grid      = grid,
    control   = tune::control_grid(save_pred = FALSE, verbose = TRUE),
    metrics   = yardstick::metric_set(yardstick::mae, yardstick::rmse)
  )

  ### -- 09. Fitting (QRF) ------------------

  print("09. Fitting (QRF)")

  best_params = tune::select_best(x = ranger_tune, metric = tune_metric)

  # Final model with best hyperparams (and with quantreg)
  qrf_spec = ranger_spec |>
    tune::finalize_model(best_params) |>
    parsnip::set_engine("ranger", quantreg = TRUE)

  final_wf_fitted = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(qrf_spec) |>
    parsnip::fit(data = train_data)

  final_rf = final_wf_fitted |>
    workflows::extract_fit_parsnip()

  ### -- 10. Fitting (Normal RF) ------------

  print("10. Fitting (Normal RF)")

  levels_fac = levels(factor(data$target))

  train_data_fac <- train_data |>
    dplyr::mutate(target = factor(target, levels = levels_fac))
  test_data_fac  <- test_data  |>
    dplyr::mutate(target = factor(target, levels = levels_fac))

  rec_clf <- recipes::recipe(target ~ ., data = train_data_fac) |>
    recipes::update_role(dplyr::all_of(id_vars),     new_role = "id") |>
    recipes::step_rm(recipes::has_role("id")) |>
    recipes::update_role(dplyr::all_of(target_vars), new_role = "target_var") |>
    recipes::step_rm(recipes::has_role("target_var")) |>
    recipes::step_other(recipes::all_factor_predictors(), threshold = 0.01, other = "other") |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_unknown(recipes::all_nominal_predictors(), new_level = "unknown")

  clf_spec <- parsnip::rand_forest(
    mtry  = best_params$mtry,
    min_n = best_params$min_n,
    trees = trees
  ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger", probability = TRUE)

  clf_fitted <- workflows::workflow() |>
    workflows::add_recipe(rec_clf) |>
    workflows::add_model(clf_spec) |>
    parsnip::fit(data = train_data_fac)

  probs <- stats::predict(
    clf_fitted,
    new_data = test_data_fac,
    type = "prob"
  ) |>
    dplyr::bind_cols(truth = test_data_fac$target)

  est_cols <- grep("^\\.pred_", colnames(probs), value = TRUE)

  roc_df <- yardstick::roc_curve(probs, truth = truth, !!!rlang::syms(est_cols)) |>
    dplyr::mutate(x = 1 - specificity, y = sensitivity, type = "ROC") |>
    dplyr::select(.level, x, y, type) |>
    data.frame()

  pr_df <- yardstick::pr_curve(probs, truth = truth, !!!rlang::syms(est_cols)) |>
    dplyr::mutate(x = recall, y = precision, type = "PR") |>
    dplyr::select(.level, x, y, type) |>
    data.frame()


  ### -- 11. Predictions - Random Forest-----

  print("11. Predictions - Random Forest")

  levels_num = sort(unique(data$target))

  # Bake test data through the fitted recipe (ranger needs preprocessed input)
  prepped_recipe = workflows::extract_recipe(final_wf_fitted, estimated = TRUE)
  baked_test     = recipes::bake(prepped_recipe, new_data = test_data) |>
    dplyr::select(-target)

  # Quantile predictions directly from ranger (parsnip doesn't support type = "quantiles")
  quant_matrix = predict(
    final_rf$fit, data = baked_test, type = "quantiles", quantiles = quantiles
  )$predictions
  colnames(quant_matrix) = paste0("q", quantiles)

  quant_preds = as.data.frame(quant_matrix) |>
    dplyr::bind_cols(truth = test_data$target)

  ### -- 12. Predictions - Baseline models --

  print("12. Predictions - Baseline models")

  lag_n = function(id, time, truth, n = 1) {
    dplyr::tibble(.row = seq_along(id), id = id, time = time, truth = truth) |>
      dplyr::arrange(id, time) |>
      dplyr::group_by(id) |>
      dplyr::mutate(.pred_lag = dplyr::lag(truth, n = n)) |>
      dplyr::ungroup() |>
      dplyr::arrange(.row) |>
      dplyr::pull(.pred_lag)
  }

  # 12.1 Simply last value
  lag_1_val = lag_n(
    id = test_data[["id"]],
    time = test_data[[time_ax]],
    truth = quant_preds$truth,
    n = 1
  )
  # 12.2 Simply majority value
  majority_val = as.numeric(names(which.max(table(train_data$target))))
  # 12.3 Simply mean value
  mean_val     = mean(train_data$target, na.rm = TRUE)
  # 12.4 Simply a random value
  random_val = sample(train_data$target, size = nrow(quant_preds), replace = TRUE)

  # Append to quant_preds
  quant_preds = quant_preds |>
    dplyr::mutate(
      .pred_majority = majority_val,
      .pred_mean     = mean_val,
      .pred_lag_1    = lag_1_val,
      .pred_random = random_val
    )

  # Median (nearest quantile to 0.5) as point estimate for metric compatibility
  q_med = paste0("q", quantiles[which.min(abs(quantiles - 0.5))])

  preds = quant_preds |>
    dplyr::mutate(
      .pred       = .data[[q_med]],
      .pred_class = round(.data[[q_med]]),
      .pred_class = pmin(pmax(.pred_class, min(levels_num)), max(levels_num)),
      .pred_class = factor(.pred_class, levels = levels_num, ordered = TRUE),
      truth       = factor(truth,       levels = levels_num, ordered = TRUE)
    )

  ### -- 13. Metrics --------------------

  print("13. Metrics")

  model_metrics = purrr::map_dfr(
    .x   = levels(preds$truth),
    .f   = metrics_df,
    pred = preds
  )

  print("Baseline Metrics")

  truth_num = as.numeric(as.character(preds$truth))

  pinball_loss = function(truth, pred, q) {
    e = truth - pred
    mean(ifelse(e >= 0, q * e, (q - 1) * e), na.rm = TRUE)
  }

  point_pinball = function(pred_vec) {
    mean(sapply(quantiles, function(q) pinball_loss(truth_num, pred_vec, q)), na.rm = TRUE)
  }

  model_pinball = mean(sapply(quantiles, function(q) {
    pinball_loss(truth_num, preds[[paste0("q", q)]], q)
  }))

  q_lo = paste0("q", quantiles[1])
  q_hi = paste0("q", quantiles[length(quantiles)])

  point_preds = list(
    RF       = preds$.pred,
    majority = preds$.pred_majority,
    mean     = preds$.pred_mean,
    lag_1    = preds$.pred_lag_1,
    random   = preds$.pred_random
  )

  # skill_mae = 0: no better than mean; skill_mae = 1: perfect; negative: worse than mean
  mae_ref = mean(abs(truth_num - point_preds$mean), na.rm = TRUE)

  # RF uses the full quantile matrix for pinball; baselines use point predictions only
  baseline_metrics = purrr::map_dfr(names(point_preds), function(nm) {
    p        = point_preds[[nm]]
    mae      = mean(abs(truth_num - p), na.rm = TRUE)
    pred_ord = pmin(pmax(round(p), min(levels_num)), max(levels_num))
    data.frame(
      predictor = nm,
      mae       = round(mae, 3),
      rmse      = round(sqrt(mean((truth_num - p)^2, na.rm = TRUE)), 3),
      pinball   = round(if (nm == "RF") model_pinball else point_pinball(p), 3),
      coverage  = if (nm == "RF") round(mean(truth_num >= preds[[q_lo]] & truth_num <= preds[[q_hi]]), 3) else NA_real_,
      skill_mae = round(1 - mae / mae_ref, 3),
      adj_acc   = round(mean(abs(pred_ord - truth_num) <= 1, na.rm = TRUE), 3),
      kappa_w   = kappa_linear_weighted(truth_num, pred_ord)
    )
  })

  ### -- 14. Variable Importance --------

  print("14. Variable Importance")

  best_spec = ranger_spec |> tune::finalize_model(best_params)

  # Permutation: unbiased but slow; impurity: fast but biased toward high-cardinality features
  compute_vi = function(importance_type) {
    spec = best_spec |> parsnip::set_engine("ranger", importance = importance_type)
    workflows::workflow() |>
      workflows::add_recipe(rec) |>
      workflows::add_model(spec) |>
      parsnip::fit(train_data) |>
      workflows::extract_fit_parsnip() |>
      vip::vi()
  }

  perm_importance = compute_vi("permutation")
  gini_importance = compute_vi("impurity")

  end = Sys.time()

  ### -- 15. Instantiate model object ---

  print("15. Instantiate model object")

  m = methods::new(

    Class = "RF",

    # Info
    identity        = identity,
    start           = start,
    end             = end,

    # Model parameter
    seed            = seed,
    trees           = trees,
    n_boots         = n_boots,
    grid            = grid,
    tune_metric     = tune_metric,

    param_txt = c(
      txt_spacer("Seed:", seed,                                    35),
      txt_spacer("Number of Resamples:", n_boots,                  35),
      txt_spacer("Tuning metric:", tune_metric,                    35),
      txt_spacer("Best Params:", " ",                              35),
      txt_spacer("    mtry:", best_params$mtry,                    35),
      txt_spacer("    min_n:", best_params$min_n,                  35),
      txt_spacer("Number of trees:", trees,                        35)
    ),

    # Data

    target_var       = target,

    roles            = list(
      id_vars     = id_vars,
      target_vars = target_vars,
      pred_vars   = pred_vars
    ),

    train_data       = train_data,
    test_data        = test_data,

    # Model setup
    ranger_spec      = ranger_spec,
    recipe           = as.character(rec$steps),

    # Model tuning
    tune_metrics     = tune::collect_metrics(ranger_tune),

    # Results
    gini             = gini,
    best_params      = best_params,
    final_rf         = catch_console(final_rf),
    prediction       = list(pred = preds, prob = quant_preds),
    metrics          = model_metrics,
    baseline_metrics = baseline_metrics,

    roc_df           = roc_df,
    pr_df            = pr_df,

    perm_importance  = perm_importance,
    gini_importance  = gini_importance,

    # Trained model
    trained_model    = list(
      recipe    = prepped_recipe,
      fit       = final_rf$fit,
      quantiles = quantiles
    )
  )

  return(m)

}
