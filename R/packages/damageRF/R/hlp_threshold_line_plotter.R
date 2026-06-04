
threshold_line_plotter = function(threshold_df, best_threshold) {

  sens_df = threshold_df |>
    dplyr::filter(.metric == "sensitivity") |>
    dplyr::select(.threshold, value = .estimate) |>
    dplyr::mutate(metric = "sensitivity")

  spec_df = threshold_df |>
    dplyr::filter(.metric == "specificity") |>
    dplyr::select(.threshold, value = .estimate) |>
    dplyr::mutate(metric = "specificity")

  youden_df = dplyr::inner_join(
    sens_df |> dplyr::select(.threshold, sens = value),
    spec_df |> dplyr::select(.threshold, spec = value),
    by = ".threshold"
  ) |>
    dplyr::transmute(.threshold, value = sens + spec, metric = "youden_j+1")

  ggplot2::ggplot(
    rbind(sens_df, spec_df, youden_df),
    ggplot2::aes(x = .threshold, y = value, color = metric)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(
      xintercept = best_threshold,
      linetype = "dashed", color = "black"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Threshold",
      y = NULL,
      color = NULL,
      title = "Threshold Performance (OOF)"
    ) +
    ggplot2::theme(legend.position = "bottom", plot.title.position = "plot")

}
