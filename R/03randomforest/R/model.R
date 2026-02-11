model = function(bspid, prep, min_max = 1/100) {
  
  ###################
  ### Filter data ###
  ###################
  
  prep_1bspid = prep |> 
    dplyr::filter(bsp_id == bspid)
  
  hist_bewert_d = ggplot(prep_1bspid, aes(x = as.factor(bewert_d))) +
    geom_bar() +
    theme_minimal()

  prep_1bspid = prep_1bspid |>
    dplyr::group_by(schaden) |>
    dplyr::mutate(max_bewert_d = max(bewert_d)) |>
    dplyr::ungroup() |>
    dplyr::group_by(bewert_d) |>
    dplyr::filter(n() > 1)

  ########################
  ### Train Test Split ###
  ########################

  data_split = rsample::group_initial_split(
    prep_1bspid, group = schaden, strata = max_bewert_d, prop = 0.80
  )

  train_data = rsample::training(data_split) |>
    dplyr::select(-max_bewert_d)
  test_data  = rsample::testing(data_split) |>
    dplyr::select(-max_bewert_d)

  ##################
  ### Resampling ###
  ##################

  split_by_bewert_d = split(train_data, f = train_data$bewert_d)

  counts = sapply(split_by_bewert_d, function(df) length(unique(df$schaden)))

  mapped = (min(max(counts), min(counts) * 1/min_max) - min(counts))/
    (max(counts) - min(counts)) * (counts - min(counts)) + min(counts)
  
  mapped = round(mapped, 0)
  
  n_sampels = ceiling(max(counts)/max(mapped))
  
  n_sampels = max(10, min(20, n_sampels)) # <- comment out

  n_sampels = 2 # <- comment out

  splits = lapply(1:n_sampels, FUN = function(id) {

    df = purrr::map_dfr(names(mapped), function(bd) {
      train_data |>
        dplyr::group_by(schaden) |>
        dplyr::mutate(max_bewert_d = max(bewert_d)) |>
        dplyr::filter(max_bewert_d == bd) |>

        dplyr::slice_head(n = 1) |>
        dplyr::slice_sample(n = mapped[[bd]]) |>
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
    ids = as.character(paste0("Bootstrap", 1:n_sampels))
  )

  #####################################################
  ### Faktorisieren von bewert_d zur Klassifikation ###
  #####################################################

  train_data_fac = train_data |> dplyr::mutate(bewert_d = factor(bewert_d))
  
  test_data_fac  = test_data  |> dplyr::mutate(bewert_d = factor(bewert_d))

  ##############
  ### Recipe ###
  ##############

  rec = recipes::recipe(bewert_d ~ ., data = train_data_fac) |>
    # Entfernung von Identifikationsvariablen aus daten
    recipes::update_role(schaden, ort, tbwnr, schad_id, new_role = "id") |>
    recipes::step_rm(has_role("id")) |>
    # Entfernung von Zielvariablen aus daten
    recipes::update_role(
      zustandsnote, zustandsnotenklasse, substanzkennzahl, bewert_s, bewert_v,
      max_d, max_s, max_v, new_role = "bewert"
    ) |>
    recipes::step_rm(has_role("bewert")) |>
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

  ranger_spec = parsnip::rand_forest(
    mtry = tune::tune(), 
    min_n = tune::tune(), 
    trees = 1000
    ) |>
    parsnip::set_mode("classification") |>
    parsnip::set_engine("ranger")

  ranger_workflow = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(ranger_spec)

  ##############
  ### Tuning ###
  ##############

  ranger_tune = tune::tune_grid(
    ranger_workflow,
    resamples = boots,
    grid = 2, # <- e.g. 11
    control = tune::control_grid(save_pred = TRUE, verbose = TRUE)
  )

  # Tuning resuls
  tune_plot = autoplot(ranger_tune)
  
  ###############
  ### Fitting ###
  ###############

  # Besten Parameter auswählen
  best_params = tune::select_best(x = ranger_tune, metric = "roc_auc")

  # Besten RF erstellen
  final_rf = tune::finalize_workflow(ranger_workflow, best_params) |>
    fit(data = train_data_fac) |> 
    workflows::extract_fit_parsnip()

  
  ###################
  ### Predictions ###
  ###################
  
  probs = predict(final_rf, new_data = test_data_fac, type = "prob") |>
    dplyr::bind_cols(truth = test_data_fac$bewert_d)

  preds = predict(final_rf, new_data = test_data_fac) |>
    dplyr::bind_cols(truth = test_data_fac$bewert_d) |>
    dplyr::select(truth, .pred_class)
  
  ###############
  ### Metrics ###
  ###############

  yardstick::metrics(preds, truth = truth, .pred_class)
  yardstick::specificity(preds, truth = truth, estimate = .pred_class)

  mymetrics = purrr::map_dfr(
    .x = levels(preds$truth), 
    .f = my_class_metrics, 
    pred = preds
  )

  ###########
  ### ROC ###
  ###########

  roc_plt = probs |> 
    yardstick::roc_curve(truth = truth, setdiff(colnames(probs), "truth")) |>
    autoplot()

  ###########################
  ### Variable Importance ###
  ###########################
  
  # Permutation
  
  imp_spec = ranger_spec |>
    tune::finalize_model(select_best(ranger_tune, metric = "roc_auc")) |>
    set_engine("ranger", importance = "permutation")
  
  perm_importance = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(imp_spec) |>
    workflows::fit(train_data_fac) |>
    workflows::extract_fit_parsnip() |>
    vip::vip(geom = "point") +
    ggplot2::ylab("Importance ('permutation')") +
    ggplot2::theme_minimal()
  
  # Gini
  
  imp_spec = ranger_spec |>
    tune::finalize_model(select_best(ranger_tune, metric = "roc_auc")) |>
    set_engine("ranger", importance = "impurity")
  
  gini_importance = workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(imp_spec) |>
    workflows::fit(train_data_fac) |>
    workflows::extract_fit_parsnip() |>
    vip::vip(geom = "point") +
    ggplot2::ylab("Gini index ('impurity')") +
    ggplot2::theme_minimal()
  
  ###################################
  ### Initialize new model object ###
  ###################################
  
  m = methods::new(
    Class = "RF", 
    
    # Info
    bspid = bspid, 
    
    # Parameter
    min_max = min_max,
    
    # # Data
    # data = prep_1bspid,
    # train_data = train_data_fac,
    # test_data = test_data_fac,
    # boots = boots,
    
    # Model
    recipe = rec,
    ranger_spec = ranger_spec,
    ranger_workflow = ranger_workflow,
    
    # Results
    ranger_tune = ranger_tune,
    best_params = best_params,
    final_rf = final_rf,
    prediction = list(pred = preds, prob = probs),
    metrics = mymetrics,
    
    # Plots
    hist_bewert_d = hist_bewert_d,
    roc_plt = roc_plt,
    perm_importance = perm_importance,
    gini_importance = gini_importance
  
  )
  
  # Return model object
  return(m)
  
}
