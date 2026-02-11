#' Plotter method
#'
#' @import patchwork
#'
#' @param self self
#' @export

setGeneric("plotter", function(self) standardGeneric("plotter"))

setMethod("plotter", "RF", function(self) {

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
    hist_target_plotter(self),
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

  right = combined_ROC_PR_plot(self)

  p = cowplot::plot_grid(
    left,
    middle,
    right,
    nrow = 1,
    ncol = 3,
    rel_widths = c(7, 3, 3)
  ) |> ggplotify::as.ggplot()

  an = patchwork::plot_annotation(
      title = self@bspid,
      subtitle = paste(
        get_desc(self@bspid, l[[1]], l[[2]])[1],
        get_desc(self@bspid, l[[1]], l[[2]])[2],
        collapse = ", "
      ),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14)
      )
  )

  pc = p + an

  return(pc)

})
