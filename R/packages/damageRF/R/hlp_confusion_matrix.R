
confusion_matrix = function(cm) {
  cm_tbl <- as.data.frame(cm$table) |>
    dplyr::group_by(Truth) |>
    dplyr::mutate(pct = Freq / sum(Freq)) |>
    dplyr::mutate(Truth = forcats::fct_rev(Truth))

  p = ggplot2::ggplot(cm_tbl, ggplot2::aes(x = Prediction, y = Truth, fill = pct)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%d\n(%.0f%%)", Freq, pct * 100)), size = 3) +
    ggplot2::scale_fill_gradient(low = "white", high = "#2C7BB6", labels = scales::percent) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = "Prediction",
      y = "Truth",
      fill = "Anteil"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text  = ggplot2::element_text(size = 12),
      legend.position = "none"
    )

  return(p)
}

