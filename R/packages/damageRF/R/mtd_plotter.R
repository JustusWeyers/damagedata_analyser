#' Plotter method
#'
#' @import patchwork
#'
#' @param self self
#' @export

setGeneric("plotter", function(self, target_name) standardGeneric("plotter"))

setMethod("plotter", "RF", function(self, target_name = NA) {

  ltop = cowplot::plot_grid(
    hist_target_plotter(self, target_name),
    cowplot::plot_grid(
      textplotter(self@param_txt),
      textplotter(c(
        utils::capture.output(print(self@end - self@start)),
        paste0("Size: ", fs::fs_bytes(self@size))
      )),
      ncol = 1, rel_heights = c(7, 2)
    ),
    nrow = 1
  )

  middle = cowplot::plot_grid(
    vip::vip(self@perm_importance, geom = "point") +
      ggplot2::ylab("Importance ('permutation')") + ggplot2::theme_minimal(),
    vip::vip(self@gini_importance, geom = "point") +
      ggplot2::ylab("Gini Impurity ('impurity')") + ggplot2::theme_minimal(),
    nrow = 2
  )

  right = if (nrow(self@roc_df) > 0 && nrow(self@pr_df) > 0)
    combined_ROC_PR_plot(self) else NULL

  bottom_row = if (nrow(self@baseline_metrics) > 0)
    tableplotter(self@baseline_metrics) else NULL

  if ("pred" %in% names(self@prediction)) {
    if (all(c("truth", ".pred_class") %in% colnames(self@prediction$pred))) {
      cm_plot = confusion_matrix(
        yardstick::conf_mat(self@prediction$pred, truth = truth, estimate = .pred_class)
      )
      bottom_row = if (!is.null(bottom_row))
        cowplot::plot_grid(bottom_row, cm_plot, ncol = 2, rel_widths = c(7, 3))
      else
        cm_plot
    }
  }

  left_stack  = Filter(Negate(is.null), list(ltop, tableplotter(self@metrics), bottom_row))
  left_col    = cowplot::plot_grid(
    plotlist    = left_stack,
    nrow        = length(left_stack),
    ncol        = 1,
    rel_heights = c(5, 2.5, 3.5)[seq_along(left_stack)]
  )

  all_cols   = Filter(Negate(is.null), list(left_col, middle, right))
  col_widths = c(7, 3, 3)[seq_along(all_cols)]

  cowplot::plot_grid(
    plotlist   = all_cols,
    nrow       = 1,
    rel_widths = col_widths
  ) |>
    ggplotify::as.ggplot()

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

  if (!is.na(desc[1]) & !is.na(desc[2])) {
    p = patchwork::wrap_plots(plots, ncol = 1) +
      patchwork::plot_annotation(
        title = paste0(self@bspid, " | ", desc[1]),
        subtitle = desc[2],
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14)
        )
      )
  } else {
    p = patchwork::wrap_plots(plots, ncol = 1) +
      patchwork::plot_annotation(
        title = paste0(self@bspid, " | ", desc[1]),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14)
        )
      )
  }

  return(p)


})
