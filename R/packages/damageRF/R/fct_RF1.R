#' Title: Brief description of rf1
#'
#' @description A longer explanation of what rf1 does.
#' @param bspid Description of parameter bspid.
#' @param prep Description of parameter prep
#' @param trees Description of parameter trees
#' @param grid Description of parameter grid
#' @param n_boots Description of parameter n_boots
#' @param seed Description of parameter seed
#' @param min_max Description of parameter min_max

#' @return What the function returns.

#' @export
rf1 = function(bspid, prep, trees = 1000, grid = 11, n_boots = NA, seed = NA, min_max = 1/100, tune_metric = "roc_auc") {

  ############
  ### Seed ###
  ############

  print("Seed")

  # Take time
  start = Sys.time()

  # Create seed if necessary
  if (is.na(seed)) {
    seed = sample(1:1000, size = 1)
  }

  # Set seed
  set.seed(seed)

  #############
  ### Start ###
  #############

  print("Start")

  # Filter for one bspid
  prep_1bspid = prep |>
    dplyr::filter(bsp_id == bspid)

  # prep_1bspid = prep_1bspid[1:100,] # <- Delete !!!

  gini = DescTools::Gini(prop.table(table(as.factor(prep_1bspid$bewert_d))))

  # Histogram of bewert_d
  hist_bewert_d = hist_bewert_d_plotter(prep_1bspid)

  # Calculate max_bewert_d
  prep_1bspid = prep_1bspid |>
    dplyr::group_by(schaden) |>
    dplyr::mutate(max_bewert_d = max(bewert_d)) |>
    dplyr::ungroup()

  # Delete bewert_d's with less than two entries
  prep_1bspid = prep_1bspid |>
    dplyr::group_by(bewert_d) |>
    dplyr::filter(dplyr::n() > 1)

  ########################
  ### Train Test Split ###
  ########################

  print("Train Test Split")

  # Initial split
  data_split = rsample::group_initial_split(
    prep_1bspid, group = schaden, strata = max_bewert_d, prop = 0.80
  )

  # Training
  train_data = rsample::training(data_split) |>
    dplyr::select(-max_bewert_d)

  # Testing
  test_data  = rsample::testing(data_split) |>
    dplyr::select(-max_bewert_d)

  ##################
  ### Resampling ###
  ##################

  print("Resampling")

  # Custom sampling

  min_max = 1/100

  split_by_bewert_d = split(train_data, f = train_data$bewert_d)

  counts = sapply(split_by_bewert_d, function(df) length(unique(df$schaden)))

  map = function(counts, min_max) {
    m = (min(max(counts), min(counts) * 1/min_max) - min(counts))/
      (max(counts) - min(counts)) * (counts - min(counts)) + min(counts)

    m = round(m, 0)

    return(m)
  }

  # Initial number of resamples
  mapping = map(counts, min_max)

  if (is.na(n_boots)) {

    n_boots = ceiling(max(counts)/max(mapping))

    # Eventually increase number of resamples if n_boots < 10
    while (n_boots < 10) {
      min_max = min_max * 2
      mapping = map(counts, min_max)
      n_boots = ceiling(max(counts)/max(mapping))
      print(min_max)
    }

  } else {
    n_boots = n_boots
  }

  splits = lapply(1:n_boots, FUN = function(id) {

    df = purrr::map_dfr(names(mapping), function(bd) {
      train_data |>
        dplyr::group_by(schaden) |>
        dplyr::mutate(max_bewert_d = max(bewert_d)) |>
        dplyr::filter(max_bewert_d == bd) |>
        dplyr::slice_head(n = 1) |>
        dplyr::slice_sample(n = mapping[[bd]]) |>
        dplyr::ungroup() |>
        dplyr::mutate(bewert_d = factor(bewert_d))
    })

    initial_sp = rsample::group_initial_split(
      data = df,
      group = schaden,
      prop = 0.632,
      strata = max_bewert_d
    )

    sp = rsample::make_splits(
      x = rsample::training(initial_sp) |> dplyr::select(-max_bewert_d),
      assessment = rsample::testing(initial_sp) |> dplyr::select(-max_bewert_d)
    )

    return(sp)
  })

  boots = rsample::manual_rset(
    splits = splits,
    ids = as.character(paste0("Bootstrap", 1:n_boots))
  )

  ####################################
  ### Factorise for classification ###
  ####################################

  print("Factorise for classification")

  train_data_fac = train_data |>
    dplyr::mutate(bewert_d = factor(bewert_d))

  test_data_fac  = test_data  |>
    dplyr::mutate(bewert_d = factor(bewert_d))

  ##############
  ### Recipe ###
  ##############

  print("Recipe")

  rec = recipes::recipe(bewert_d ~ ., data = train_data_fac) |>
    # Entfernung von Identifikationsvariablen aus daten
    recipes::update_role(schaden, ort, tbwnr, schad_id, new_role = "id") |>
    recipes::step_rm(recipes::has_role("id")) |>
    # Near-zero-variance Prädiktoren entfernen
    recipes::step_nzv(recipes::all_numeric_predictors()) |>
    # Viele seltene Variablen zusammenfassen
    recipes::step_other(
      bauteil, konstruktion, zwgruppe, threshold = 0.01, other = "other"
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
    parsnip::set_engine("ranger")

  # Define workflow
  ranger_workflow = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(ranger_spec)

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

  # Tuning resuls
  tune_plot = tune::autoplot(ranger_tune)

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
    dplyr::bind_cols(truth = test_data_fac$bewert_d)

  preds = stats::predict(final_rf, new_data = test_data_fac) |>
    dplyr::bind_cols(truth = test_data_fac$bewert_d) |>
    dplyr::select(truth, .pred_class)

  ###############
  ### Metrics ###
  ###############

  print("Metrics")

  yardstick::metrics(preds, truth = truth, .pred_class)
  yardstick::specificity(preds, truth = truth, estimate = .pred_class)

  mymetrics = purrr::map_dfr(
    .x = levels(preds$truth),
    .f = metrics_df,
    pred = preds
  )

  #####################
  ### ROC / PR Plot ###
  #####################

  print("ROC / PR Plot")

  # ROC
  roc_df = yardstick::roc_curve(probs, truth = truth, setdiff(colnames(probs), "truth")) |>
    dplyr::mutate(type = "ROC") |>
    dplyr::mutate(x = 1-specificity) |>
    dplyr::mutate(y = sensitivity) |>
    dplyr::select(.level, x, y) |>
    dplyr::mutate(type = "ROC")

  # PR
  pr_df = yardstick::pr_curve(probs, truth = truth, setdiff(colnames(probs), "truth")) |>
    dplyr::mutate(type = "PR") |>
    dplyr::mutate(x = recall) |>
    dplyr::mutate(y = precision) |>
    dplyr::select(.level, x, y) |>
    dplyr::mutate(type = "PR")

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
    bspid = bspid,
    start = start,
    end = end,

    # Model parameter
    seed            = seed,                    # Used seed
    trees           = trees,                   # Number of trees
    min_max         = min_max,                 # Min-Max ratio of bewert_d
    n_boots         = n_boots,                 # Number of boots for evaluation
    grid            = grid,                    # Tuning grid,
    tune_metric     = tune_metric,

    param_txt = c(
      txt_spacer("Seed:", seed, 35),
      txt_spacer("Ratio bewert_d:", min_max, 35),
      txt_spacer("Number of Resamples:", n_boots, 35),
      txt_spacer("Tuning metric:", tune_metric, 35),
      txt_spacer("Grid:", grid, 35),
      txt_spacer("Best Params:", " ", 35),
      txt_spacer("    mtry:", best_params$mtry, 35),
      txt_spacer("    min_n:", best_params$min_n, 35),
      txt_spacer("Number of trees:", trees, 35)
    ),

    # Data
    train_data      = train_data_fac,            # Train data
    test_data       = test_data_fac,             # Test data

    # Model setup
    ranger_spec     = ranger_spec,               # Type of model
    recipe          = unname(unlist(rec$steps)), # Recipe (only steps)

    # Model tuning
    tune_control    = tidyr::tibble(ranger_tune),         # Tuning parameter
    tune_metrics    = tune::collect_metrics(ranger_tune), # Tuning metrics

    # Results
    gini            = gini,
    best_params     = best_params,               # Best parameter data.frame
    final_rf        = catch_console(final_rf),   # Final model (only console log)
    prediction      = list(pred = preds, prob = probs),   # Predictions df
    metrics         = mymetrics,                 # Metrics data.frame

    perm_importance = perm_importance,
    gini_importance = gini_importance,

    roc_df          = roc_df,
    pr_df           = pr_df,

    # Trained model
    trained_model    = final_rf$fit             # Ranger-obj.
  )

  # Return model object
  return(m)

}
