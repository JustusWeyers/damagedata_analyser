#' Title: Hurdle RF - Stage 1: Binary Classification
#'
#' @description Based on rf6_binary. Accepts an external train_ids vector to
#'   enforce a shared train/test split across the hurdle model pipeline.
#'   When train_ids is NULL the function behaves identically to rf6_binary.
#' @param train_ids Optional: character vector of bridge IDs (gsplitvar, i.e.
#'   the part of id before the first "_") assigned to training. When provided
#'   the internal splitting loop is skipped.
#'
#' @inheritParams rf6_binary
#' @export
hurdle_rf6 = function(

  identity = list(), data, id, id_vars, target_vars, target, time_ax,
  row_limit = NA, trees = 1000, grid = 11, n_boots = 30, seed = NA,
  tune_metric = "pr_auc", train_ratio = 0.80, n = 30,
  randomize_target = FALSE, test_run = FALSE,
  quantiles = NA, pred_threshold_mode = "standard", min_sensitivity = 0.95,
  train_ids = NULL

  ) {

  ### -- Seed -----------------------

  print("Seed")

  start = Sys.time()

  if (is.na(seed)) {
    seed = sample(1000:9999, size = 1)
  }

  set.seed(seed)

  ### -- Testrun? -------------------

  if (test_run) {
    row_limit = 5000
    trees = 500
    grid = 3
    n_boots = 3
  }

  ### -- Variable/Column roles ------

  if ("target" %in% colnames(data)) {
    stop("Error in hurdle_rf6: Found 'target' in colnames(data)")
  }

  id_vars     = setdiff(intersect(colnames(data), id_vars), id)
  target_vars = setdiff(intersect(colnames(data), target_vars), target)
  pred_vars   = setdiff(colnames(data), c(id_vars, target_vars, target, id))

  colnames(data)[colnames(data) == target] = "target"
  colnames(data)[colnames(data) == id]     = "id"

  ### -- Data preparation -----------

  if (!is.na(row_limit)) {
    data = data[1:row_limit,]
  }

  if (randomize_target) {
    data$target = sample(data$target, size = length(data$target), replace = FALSE)
  }

  data = data |> tidyr::drop_na(target)

  data = data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))

  data = data |>
    dplyr::group_by(target) |>
    dplyr::filter(dplyr::n() > n) |>
    dplyr::ungroup()

  data = data |>
    dplyr::mutate(strata = factor(target))

  freq_table = data |>
    dplyr::count(strata, name = "freq")

  data = data |>
    dplyr::left_join(freq_table, by = "strata") |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      strata = strata[order(freq, strata)][1]
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-freq)

  gini = DescTools::Gini(prop.table(table(as.factor(data$target))))

  ### -- Case weights ---------------

  weights_df =
    data |>
    dplyr::count(target, name = "n_class") |>
    dplyr::mutate(
      K = dplyr::n(),
      N = sum(n_class)
    ) |>
    dplyr::mutate(weight = sqrt(N / (K * n_class))) |>
    dplyr::select(target, weight)

  data = data |>
    dplyr::left_join(weights_df, by = "target") |>
    dplyr::mutate(.case_weights = hardhat::importance_weights(weight)) |>
    dplyr::select(-weight)

  ### -- Splitting ------------------

  print("Splitting")

  if (!is.null(train_ids)) {

    # Use externally provided training IDs (matched against first part of id before "_")
    split_id = sapply(strsplit(as.character(data$id), "_"), `[`, 1)
    train_data = data[split_id %in% train_ids, ] |> dplyr::select(-strata)
    test_data  = data[!split_id %in% train_ids, ] |> dplyr::select(-strata)

    train_data_fac = train_data |> dplyr::mutate(target = factor(target))
    test_data_fac  = test_data  |>
      dplyr::mutate(target = factor(target, levels = levels(train_data_fac$target)))

  } else {

    for (i in 1:1000) {

      data_split = rsample::group_initial_split(
        data = data, group = "id", strata = "strata", prop = train_ratio
      )

      train_data = rsample::training(data_split)
      test_data  = rsample::testing(data_split) |> dplyr::select(-strata)

      train_data_fac = train_data |> dplyr::mutate(target = factor(target))
      test_data_fac  = test_data  |> dplyr::mutate(target = factor(target))

      train_levels = levels(train_data_fac$target)
      test_levels  = levels(test_data_fac$target)

      if (identical(train_levels, test_levels)) {
        break
      } else {
        if (i == 1000) stop("Stopped splitting due to imbalanced stratification var")
        if (i %% 100 == 0) print(paste0("Redo Split ", i, "/100"))
        seed = 1000 + i
        set.seed(seed)
      }
    }

    train_data = train_data |> dplyr::select(-strata)
    train_data_fac = train_data_fac |> dplyr::select(-strata)

  }

  ### -- Resampling -----------------

  boots = rsample::bootstraps(train_data_fac, strata = target, times = n_boots)
  folds = rsample::group_vfold_cv(train_data_fac, group = "id", strata = "target", v = n_boots)

  resamples = folds

  ### -- Recipe ---------------------

  print("Recipe")

  print(paste("  - rm ID-vars:", paste(id_vars, collapse = ", ")))
  print(paste("  - rm pred-vars:", paste(pred_vars, collapse = ", ")))
  print(paste("  - rm target-vars:", paste(target_vars, collapse = ", ")))

  rec = recipes::recipe(target ~ ., data = train_data_fac) |>
    recipes::update_role(dplyr::all_of(id_vars), new_role = "id") |>
    recipes::step_rm(recipes::has_role("id")) |>
    recipes::update_role(dplyr::all_of(target_vars), new_role = "target_var") |>
    recipes::step_rm(recipes::has_role("target_var")) |>
    recipes::step_rm(tidyselect::any_of("strata")) |>
    recipes::step_other(recipes::all_factor_predictors(), threshold = 0.01, other = "other") |>
    recipes::step_novel(recipes::all_nominal_predictors()) |>
    recipes::step_unknown(recipes::all_nominal_predictors(), new_level = "unknown")

  ### -- Workflow -------------------

  print("Workflow")

  ranger_spec = parsnip::rand_forest(
    mtry  = tune::tune(),
    min_n = tune::tune(),
    trees = trees
  ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger", probability = TRUE)

  ranger_workflow = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(ranger_spec) |>
    workflows::add_case_weights(.case_weights)

  ### -- Tuning ---------------------

  print("Tuning")

  ranger_tune = tune::tune_grid(
    ranger_workflow,
    resamples = resamples,
    grid      = grid,
    control   = tune::control_grid(save_pred = TRUE, verbose = TRUE),
    metrics   = yardstick::metric_set(
      yardstick::pr_auc,
      yardstick::roc_auc,
      yardstick::accuracy,
      yardstick::f_meas
    )
  )

  ### -- Fitting --------------------

  print("Fitting")

  best_params = tune::select_best(x = ranger_tune, metric = tune_metric)

  oofb = tune::collect_predictions(ranger_tune, parameters = best_params)

  pred_col = ".pred_1"
  minority_class = "1"

  threshold_perf = data.frame()

  if (pred_threshold_mode == "standard") {

    best_threshold = 0.5

    threshold_perf = probably::threshold_perf(
      oofb, truth = target, estimate = pred_col,
      thresholds = seq(0.05, 0.95, by = 0.01),
      metrics = yardstick::metric_set(yardstick::sensitivity, yardstick::specificity)
    )

  } else if (pred_threshold_mode == "youden") {

    threshold_perf = probably::threshold_perf(
      oofb, truth = target, estimate = pred_col,
      thresholds = seq(0.05, 0.95, by = 0.01),
      metrics = yardstick::metric_set(yardstick::sensitivity, yardstick::specificity)
    )

    sens = threshold_perf |>
      dplyr::filter(.metric == "sensitivity") |>
      dplyr::select(.threshold, sens = .estimate)
    spec = threshold_perf |>
      dplyr::filter(.metric == "specificity") |>
      dplyr::select(.threshold, spec = .estimate)

    best_threshold = dplyr::inner_join(sens, spec, by = ".threshold") |>
      dplyr::mutate(youden_j = sens + spec - 1) |>
      dplyr::slice_max(youden_j, n = 1, with_ties = FALSE) |>
      dplyr::pull(.threshold)

  } else if (pred_threshold_mode == "best_sens") {

    threshold_perf = probably::threshold_perf(
      oofb, truth = target, estimate = pred_col,
      thresholds = seq(0.05, 0.95, by = 0.01),
      metrics = yardstick::metric_set(yardstick::f_meas, yardstick::sensitivity, yardstick::specificity)
    )

    threshold_candidates = threshold_perf |>
      dplyr::filter(.metric == "sensitivity", .estimate >= min_sensitivity)

    if (nrow(threshold_candidates) == 0) {
      warning(paste0(
        "min_sensitivity = ", min_sensitivity,
        " nicht erreichbar. Verwende Schwellenwert mit maximaler Sensitivity."
      ))
      best_threshold = threshold_perf |>
        dplyr::filter(.metric == "sensitivity") |>
        dplyr::slice_max(.estimate, n = 1, with_ties = FALSE) |>
        dplyr::pull(.threshold)
    } else {
      best_threshold = threshold_candidates |>
        dplyr::slice_max(.threshold, n = 1, with_ties = FALSE) |>
        dplyr::pull(.threshold)
    }

  } else {
    stop(paste0('pred_threshold_mode muss "standard", "youden" oder "best_sens" sein, nicht: "', pred_threshold_mode, '"'))
  }

  final_wf        = tune::finalize_workflow(ranger_workflow, best_params)
  final_wf_fitted = final_wf |> parsnip::fit(data = train_data_fac)
  final_rf        = final_wf_fitted |> workflows::extract_fit_parsnip()
  prepped_recipe  = workflows::extract_recipe(final_wf_fitted, estimated = TRUE)
  fit             = final_rf$fit

  ### -- Predictions ----------------

  print("Predictions")

  probs = stats::predict(final_rf, new_data = test_data_fac, type = "prob") |>
    dplyr::bind_cols(truth = test_data_fac$target)

  majority_class = setdiff(levels(test_data_fac$target), minority_class)

  preds = probs |>
    dplyr::mutate(
      .pred_class = factor(
        ifelse(.data[[pred_col]] >= best_threshold, minority_class, majority_class),
        levels = levels(test_data_fac$target)
      )
    ) |>
    dplyr::select(truth, .pred_class)

  ### -- Metrics --------------------

  print("Metrics")

  model_metrics = purrr::map_dfr(
    .x = levels(preds$truth),
    .f = metrics_df,
    pred = preds
  )

  ### -- Baseline Metrics -----------

  print("Baseline Metrics")

  baseline_pred_majority = preds |>
    dplyr::mutate(.pred_class = factor(majority_class, levels = levels(preds$truth)))

  prevalence = mean(train_data_fac$target == minority_class)
  set.seed(seed)
  baseline_pred_random = preds |>
    dplyr::mutate(.pred_class = factor(
      sample(
        c(minority_class, majority_class),
        size    = nrow(preds),
        replace = TRUE,
        prob    = c(prevalence, 1 - prevalence)
      ),
      levels = levels(preds$truth)
    ))

  clf_metrics_row = function(nm, pred_df) {
    m = metrics_df(minority_class, pred_df)
    data.frame(
      predictor   = nm,
      accuracy    = m$accuracy,
      sensitivity = m$TPR,
      specificity = m$TNR,
      ppv         = m$PPV,
      f1          = m$f1
    )
  }

  baseline_metrics = dplyr::bind_rows(
    clf_metrics_row("RF",                preds),
    clf_metrics_row("majority",          baseline_pred_majority),
    clf_metrics_row("random",            baseline_pred_random)
  )

  ### -- ROC / PR -------------------

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

  best_spec = ranger_spec |> tune::finalize_model(best_params)

  compute_vi = function(importance_type) {
    spec = best_spec |> parsnip::set_engine("ranger", importance = importance_type)
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

  end = Sys.time()

  ### -- Instantiate model object ---

  print("Instantiate model object")


  if (pred_threshold_mode == "best_sens") {
    txt_mode = c("Thres. mode:", paste0(pred_threshold_mode, " (min. sens = ", round(min_sensitivity, 2), ")"))
  } else {
    txt_mode = c("Thres. mode:", pred_threshold_mode)
  }

  m = methods::new(

    Class = "RF",

    identity        = identity,
    start           = start,
    end             = end,

    seed            = seed,
    trees           = trees,
    min_max         = NaN,
    n_boots         = n_boots,
    grid            = grid,
    tune_metric     = tune_metric,

    param_txt = c(
      txt_spacer("Seed:", seed,                   40),
      txt_spacer("Number of Resamples:", n_boots, 40),
      txt_spacer("Tuning metric:", tune_metric,   40),
      txt_spacer(paste0("Weights (Class 1-", nrow(weights_df), "):"), " ", 40),
      txt_spacer(" ", paste(round(weights_df$weight, 2), collapse = "/"), 40),
      txt_spacer("Best Params:", " ",             40),
      txt_spacer("    mtry:", best_params$mtry,   40),
      txt_spacer("    min_n:", best_params$min_n, 40),
      txt_spacer("Number of trees:", trees,       40),
      txt_spacer(txt_mode[1], txt_mode[2] , 40),
      txt_spacer("Threshold:", round(best_threshold, 2), 40)
    ),

    target_var      = target,
    train_data      = train_data_fac,
    test_data       = test_data_fac,
    ranger_spec     = ranger_spec,
    recipe          = as.character(rec$steps),
    tune_metrics    = tune::collect_metrics(ranger_tune),
    gini            = gini,
    best_params     = best_params,
    final_rf        = catch_console(final_rf),
    prediction      = list(
      pred           = preds,
      prob           = probs,
      OOFB           = oofb,
      threshold_perf = threshold_perf,
      best_threshold = best_threshold
    ),
    metrics          = model_metrics,
    baseline_metrics = baseline_metrics,
    roc_df           = roc_df,
    pr_df           = pr_df,
    perm_importance = perm_importance,
    gini_importance = gini_importance,
    trained_model   = list(
      recipe = prepped_recipe,
      fit    = fit
    )
  )

  return(m)

}
