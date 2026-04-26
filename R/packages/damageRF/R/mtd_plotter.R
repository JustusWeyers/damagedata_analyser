#' Plotter method
#'
#' @import patchwork
#'
#' @param self self
#' @export

setGeneric("plotter", function(self, target_name) standardGeneric("plotter"))

setMethod("plotter", "RF", function(self, target_name = NA) {

  info_txt = c(
    utils::capture.output(print(self@end - self@start)),
    paste0("Size: ", fs::fs_bytes(self@size))
  )

  info_txt_plt = cowplot::plot_grid(
    textplotter(self@param_txt),
    textplotter(info_txt),
    ncol = 1, nrow = 2, rel_heights = c(7, 2)
  )

  ltop = cowplot::plot_grid(
    hist_target_plotter(self, target_name),
    info_txt_plt,
    nrow = 1,
    ncol = 2
  )

  lbot = tableplotter(self@metrics)

  left = cowplot::plot_grid(
    ltop,
    lbot,
    nrow = 2,
    ncol = 1,
    rel_heights = c(2, 1)
  )

  perm_importance_plot = vip::vip(self@perm_importance, geom = "point") +
    ggplot2::ylab("Importance ('permutation')") +
    ggplot2::theme_minimal()

  gini_importance_plot = vip::vip(self@gini_importance, geom = "point") +
    ggplot2::ylab("Gini Impurity ('impurity')") +
    ggplot2::theme_minimal()

  middle = cowplot::plot_grid(
    perm_importance_plot,
    gini_importance_plot,
    nrow = 2,
    ncol = 1
  )

  if (nrow(self@roc_df) > 0 && nrow(self@pr_df) > 0 ) {
    right = combined_ROC_PR_plot(self)
  } else {
    right = NULL
  }

  plots = list(left, middle, right)

  p = cowplot::plot_grid(
    plotlist   = plots,
    nrow       = 1,
    ncol       = length(plots),
    rel_widths = c(7, 3, 3)[seq_along(plots)]
  ) |>
    ggplotify::as.ggplot()

  an = patchwork::plot_annotation(
    title = NULL,
    subtitle = NULL,
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
  )

  pc = p + an

  return(pc)

})

setMethod("plotter", "BSPID", function(self, target_name = NA) {

  desc = get_desc(self)

  plots = Map(function(model, group) {
    plotter(model, target_name = self@target_name) +
      ggplot2::labs(subtitle = paste0(self@bspid, "-group: '", group, "'")) +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(
          size = 8,
          hjust = 0.5
        )
      )
  }, self@models, names(self@groups))

  patchwork::wrap_plots(plots, ncol = 1) +
    patchwork::plot_annotation(
      title = paste0(self@bspid, " | ", desc[1]),
      subtitle = desc[2],
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14)
      )
    )

})
