
density_plotter = function(oofb, best_threshold) {

  ggplot2::ggplot(oofb, ggplot2::aes(x = .pred_1, fill = target)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::geom_vline(
      xintercept = best_threshold,
      linetype = "dashed"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "P(change = 1)",
      y = "Density",
      fill = "True class",
      title = "Prediction Distribution (OOF)"
    ) +
    ggplot2::theme(legend.position = "bottom", plot.title.position = "plot")

}
