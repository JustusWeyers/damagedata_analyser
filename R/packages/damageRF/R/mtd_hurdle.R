#' @export
# Für jedes Gruppenmodell ein eigener AR-Durchlauf.
# Rückgabe: benannte Liste, ein Data Frame pro Gruppenmodell + "lag_1" für statische Brücken.
# history/anchor/future werden aus der NA-Struktur von rtg_d abgeleitet — kein predict_from_yr nötig.
# - bspid_regression: S4-Modellobjekt mit @data und @models
# - dynamic_ids:      IDs der Brücken, die iterativ vorhergesagt werden sollen
ar = function(bspid_regression, dynamic_ids) {

  pred_list = lapply(bspid_regression@models, function(m) {

    grp = names(m@identity)

    # Brücken, die zu diesem Gruppenmodell gehören
    grp_dynamic_ids = bspid_regression@data |>
      dplyr::filter(
        id %in% dynamic_ids,
        as.character(init_bsp_id) %in% as.character(m@identity[[1]])
      ) |>
      dplyr::pull(id) |>
      unique()

    if (length(grp_dynamic_ids) == 0) return(dplyr::tibble())

    data = bspid_regression@data |>
      dplyr::filter(id %in% grp_dynamic_ids)

    # Bekannte Beobachtungen (rtg_d nicht NA) → History und Anker
    history_obs = data |>
      dplyr::filter(!is.na(rtg_d)) |>
      dplyr::select(id, age, rtg_d_ar = rtg_d)

    anchor_obs = data |>
      dplyr::filter(!is.na(rtg_d)) |>
      dplyr::group_by(id) |>
      dplyr::slice_max(inspection_yr, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()

    # Vorherzusagende Zeilen (rtg_d ist NA)
    future_obs = data |>
      dplyr::filter(is.na(rtg_d)) |>
      dplyr::arrange(id, inspection_yr)

    if (nrow(future_obs) == 0) return(dplyr::tibble())

    current_state = anchor_obs |>
      dplyr::select(id, rtg_d_ar = rtg_d)

    results = list()

    for (yr in sort(unique(future_obs$inspection_yr))) {

      step_rows = future_obs |>
        dplyr::filter(inspection_yr == yr) |>
        dplyr::inner_join(current_state |> dplyr::select(id), by = "id")

      if (nrow(step_rows) == 0) next

      step_rows = step_rows |>
        dplyr::rowwise() |>
        dplyr::mutate(
          h3_rtg_d = {
            hist_i = history_obs[history_obs$id == id, ]
            hist_i = hist_i[!is.na(hist_i$rtg_d_ar) & !is.na(hist_i$age), ]
            if (nrow(hist_i) < 1) {
              NA_real_
            } else if (nrow(hist_i) == 1) {
              hist_i$rtg_d_ar
            } else {
              round(stats::approx(
                x    = hist_i$age,
                y    = hist_i$rtg_d_ar,
                xout = age - 3,
                rule = 2,
                ties = mean
              )$y)
            }
          }
        ) |>
        dplyr::ungroup()

      pred_step = bspid_regression |>
        set_data(step_rows) |>
        predict(grp)

      # Ein Modell → eine Zeile pro Bridge → direkt round(q0.5) als AR-Feedback
      consensus = pred_step |>
        dplyr::select(id, rtg_d_ar = q0.5) |>
        dplyr::mutate(rtg_d_ar = round(rtg_d_ar))

      new_history = step_rows |>
        dplyr::select(id, age) |>
        dplyr::left_join(consensus, by = "id") |>
        dplyr::select(id, age, rtg_d_ar)

      history_obs   = dplyr::bind_rows(history_obs, new_history)
      current_state = dplyr::bind_rows(
        current_state |> dplyr::filter(!id %in% consensus$id),
        consensus
      )

      results[[length(results) + 1]] = pred_step |>
        dplyr::mutate(dynamic = TRUE)
    }

    dplyr::bind_rows(results)
  })

  # Statische Brücken: Ankerwert in alle Zukunftsjahre eingetragen (Lag-1-Baseline)
  static_data = bspid_regression@data |>
    dplyr::filter(!id %in% dynamic_ids)

  static_results = if (nrow(static_data) > 0) {

    anchor_static = static_data |>
      dplyr::filter(!is.na(rtg_d)) |>
      dplyr::group_by(id) |>
      dplyr::slice_max(inspection_yr, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(id, rtg_d_start = rtg_d)

    static_future = static_data |>
      dplyr::filter(is.na(rtg_d))

    static_future |>
      dplyr::left_join(anchor_static, by = "id") |>
      dplyr::mutate(
        rtg_d   = rtg_d_start,
        q0.5    = rtg_d_start,
        group   = "lag_1",
        dynamic = FALSE
      ) |>
      dplyr::select(-rtg_d_start)

  } else {
    dplyr::tibble()
  }

  if (nrow(static_results) > 0) pred_list$lag_1 = static_results

  pred_list
}


#' @export
eval_ar = function(pred_ar, bspid_regression, obs_data, pred_col = "q0.5") {

  ar_eval = pred_ar |>
    dplyr::select(id, inspection_yr, group, dplyr::starts_with("q")) |>
    dplyr::inner_join(
      obs_data |> dplyr::select(id, inspection_yr, age, rtg_d),
      by = c("id", "inspection_yr")
    )

  if (nrow(ar_eval) == 0) return(dplyr::tibble())

  truth_num  = ar_eval$rtg_d
  levels_num = sort(unique(truth_num))
  rf_pred    = ar_eval[[pred_col]]

  q_cols    = grep("^q[0-9]", colnames(ar_eval), value = TRUE)
  quantiles = as.numeric(sub("^q", "", q_cols))

  pinball_loss = function(truth, pred, q) {
    e = truth - pred
    mean(ifelse(e >= 0, q * e, (q - 1) * e), na.rm = TRUE)
  }

  model_pinball = mean(
    sapply(quantiles, \(q) pinball_loss(truth_num, ar_eval[[paste0("q", q)]], q)),
    na.rm = TRUE
  )

  q_lo = paste0("q", quantiles[1])
  q_hi = paste0("q", quantiles[length(quantiles)])

  train_all    = dplyr::bind_rows(lapply(bspid_regression@models, \(m) m@train_data))
  majority_val = as.numeric(names(which.max(table(round(train_all$target)))))
  mean_val     = mean(train_all$target, na.rm = TRUE)
  mae_ref      = mean(abs(truth_num - mean_val), na.rm = TRUE)

  anchor_obs_per_id = obs_data |>
    dplyr::group_by(id) |>
    dplyr::slice_min(inspection_yr, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(id, pred_lag1 = rtg_d)

  lag1_pred = ar_eval |>
    dplyr::left_join(anchor_obs_per_id, by = "id") |>
    dplyr::pull(pred_lag1)

  global_coef = stats::coef(stats::lm(target ~ age, data = train_all))

  id_trends = train_all |>
    dplyr::group_by(id) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::summarise(
      intercept = stats::lm(target ~ age)$coefficients["(Intercept)"],
      slope     = stats::lm(target ~ age)$coefficients["age"],
      .groups   = "drop"
    )

  linear_pred = ar_eval |>
    dplyr::left_join(id_trends, by = "id") |>
    dplyr::mutate(
      intercept = dplyr::if_else(is.na(intercept), unname(global_coef["(Intercept)"]), intercept),
      slope     = dplyr::if_else(is.na(slope),     unname(global_coef["age"]),         slope),
      .pred     = intercept + slope * age
    ) |>
    dplyr::pull(.pred)

  point_preds = list(
    RF           = rf_pred,
    lag_1        = lag1_pred,
    linear_trend = linear_pred,
    majority     = rep(majority_val, nrow(ar_eval)),
    mean         = rep(mean_val,     nrow(ar_eval))
  )

  purrr::map_dfr(names(point_preds), function(nm) {
    p        = point_preds[[nm]]
    mae      = mean(abs(truth_num - p), na.rm = TRUE)
    pred_ord = pmin(pmax(round(p), min(levels_num)), max(levels_num))
    data.frame(
      predictor = nm,
      n         = sum(!is.na(p)),
      mae       = round(mae, 3),
      rmse      = round(sqrt(mean((truth_num - p)^2, na.rm = TRUE)), 3),
      pinball   = if (nm == "RF") round(model_pinball, 3) else NA_real_,
      coverage  = if (nm == "RF") round(mean(truth_num >= ar_eval[[q_lo]] & truth_num <= ar_eval[[q_hi]], na.rm = TRUE), 3) else NA_real_,
      skill_mae = round(1 - mae / mae_ref, 3),
      adj_acc   = round(mean(abs(pred_ord - truth_num) <= 1, na.rm = TRUE), 3),
      kappa_w   = kappa_linear_weighted(truth_num, pred_ord)
    )
  })
}
