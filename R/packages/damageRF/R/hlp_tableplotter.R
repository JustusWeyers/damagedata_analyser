
tableplotter = function(df) {
  label_text = paste(catch_console(df), collapse = "\n")

  bg  = grid::roundrectGrob(
    r  = grid::unit(0.5, "lines"),
    gp = grid::gpar(fill = "#f0f0f0", col = "gray40", lwd = 1)
  )

  txt = grid::textGrob(
    label_text,
    gp = grid::gpar(fontfamily = "mono", fontsize = 11)
  )

  grob = grid::grobTree(bg, txt)

  p = ggplot2::ggplot() +
    ggplot2::annotation_custom(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
    )

  return(p)
}
