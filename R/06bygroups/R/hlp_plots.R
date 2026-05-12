
plot_rtg_d_over_age = function(df, n = 100) {
  df |>
    dplyr::filter(id %in% sample(unique(id), n)) |>
    ggplot2::ggplot(ggplot2::aes(x = age, y = rtg_d, colour = id, group = id)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::labs(
      x      = "Age",
      y      = "Rating D",
      colour = "Damage"
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


plot_change_freqs = function(df) {
  
  df |>
    dplyr::mutate(
      rtg_d = factor(rtg_d),
      bsp_id = factor(bsp_id)
    ) |> 
    tidyr::pivot_longer(
      cols = c(bsp_id, rtg_d),
      names_to = "zustand_var",
      values_to = "zustand"
    ) |>
    dplyr::group_by(id, zustand_var) |>
    dplyr::summarise(
      n_distinct = dplyr::n_distinct(zustand),
      .groups = "drop"
    ) |>
    dplyr::count(zustand_var, n_distinct) |>
    ggplot2::ggplot(ggplot2::aes(x = n_distinct, y = n)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = n),
      vjust = 1.2,
      color = "white"
    ) +
    ggplot2::facet_wrap(~ zustand_var) +
    ggplot2::labs(
      x = "Number of distinct values per id",
      y = "Number of Entries",
      title = paste(bspid@bspid, "|", get_desc(bspid))
    ) +
    ggplot2::theme_minimal()
}