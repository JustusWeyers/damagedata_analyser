data_wrangling = function(df) {
  # Split nach Verbesserungen
  workingdata_prep <- workingdata |>
    dplyr::group_by(id) |>
    dplyr::arrange(age, .by_group = TRUE) |>
    dplyr::mutate(
      subgroup = cumsum(c(0, diff(rtg_d) < 0)),
      id   = paste(id, letters[subgroup + 1], sep = "_")
    ) |>
    dplyr::select(-subgroup) |> 
    dplyr::ungroup()
  
  # # Nur Verschlechterungen
  # workingdata_prep = workingdata_prep |> 
  #   dplyr::group_by(id) |>
  #   dplyr::filter(any(diff(rtg_d) > 0)) |> 
  #   dplyr::ungroup()
  
  # # Aufenthaltszeit berechnen
  # workingdata_prep <- workingdata_prep |>
  #   dplyr::group_by(id) |>
  #   dplyr::arrange(age, .by_group = TRUE) |>
  #   dplyr::mutate(
  #     run_id = cumsum(rtg_d != dplyr::lag(rtg_d, default = dplyr::first(rtg_d)))
  #   ) |>
  #   dplyr::group_by(id, run_id) |>
  #   dplyr::mutate(
  #     time_in_class = max(age) - min(age)
  #   ) |>
  #   dplyr::group_by(id) |>
  #   dplyr::mutate(
  #     time_in_class = dplyr::if_else(run_id == max(run_id), NA_real_, time_in_class)
  #   ) |>
  #   dplyr::select(-run_id) |>
  #   dplyr::ungroup()
  
  # # Letzte Bewertung bewert_d_h3
  # workingdata_prep <- workingdata_prep |>
  #   dplyr::group_by(id) |>
  #   dplyr::arrange(age, .by_group = TRUE) |>
  #   dplyr::mutate(
  #     bewert_d_3 = if (dplyr::n_distinct(age) < 2) {
  #       NA_real_
  #     } else {
  #       stats::approx(
  #         x    = age,
  #         y    = rtg_d,
  #         xout = age - 3,
  #         rule = 1,
  #         ties = mean
  #       )$y
  #     }
  #   ) |>
  #   dplyr::ungroup()
  
  # Initiale bspid
  workingdata_prep <- workingdata_prep |>
    dplyr::group_by(id) |>
    dplyr::arrange(age, .by_group = TRUE) |>
    dplyr::mutate(
      init_bsp_id = dplyr::first(bsp_id)
    ) |>
    dplyr::ungroup()
}
