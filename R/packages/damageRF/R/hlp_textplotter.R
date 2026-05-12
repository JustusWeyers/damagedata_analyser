
textplotter = function(txt){
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.05, y = 0.95,
      label = paste(txt, collapse = "\n"),
      hjust = 0, vjust = 1,
      family = "mono",
      size = 4
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(expand = FALSE)
}
