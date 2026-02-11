# Catch console output
catch_console = function(o) {

  old_width = getOption("width")
  options(width = 1000)
  out = utils::capture.output(print(o))
  options(width = old_width)

  return(out)
}

metrics_df = function(class, pred) {

  pred$.pred_class <- factor(pred$.pred_class, levels = levels(pred$truth))

  # print(paste("### Class", class))

  # Real postives
  P = nrow(pred[pred$truth == class,])
  # Real neagatives
  N = nrow(pred[pred$truth != class,])

  # True positive
  tp = pred |>
    dplyr::filter(truth == class) |>
    dplyr::mutate(TP = truth == .pred_class)

  ## False negative
  fn = pred |>
    dplyr::filter(truth == class) |>
    dplyr::mutate(FN = .pred_class != truth)

  # True negative
  tn = pred |>
    dplyr::filter(truth != class) |>
    dplyr::mutate(TN = .pred_class != class)

  ## False positive
  fp = pred |>
    dplyr::filter(.pred_class == class) |>
    dplyr::mutate(FP = .pred_class != truth)


  df = data.frame(
    class = paste0('"', class, '"'),
    n = nrow(pred),
    P = P,
    N = N,
    TP = sum(tp$TP),
    FN = sum(fn$FN),
    TN = sum(tn$TN),
    FP = sum(fp$FP),
    TPR = round(sum(tp$TP)/P, 3),
    FNR = round(sum(fn$FN)/P, 3),
    TNR = round(sum(tn$TN)/N, 3),
    FPR = round(sum(fp$FP)/N, 3)
  )

  df$accuracy = round((df$TP + df$TN) / (df$P + df$N), 2)
  df$PPV = round(df$TP/(df$TP + df$FP), 2)
  df$f1 = round(2 * df$PPV * df$TPR / (df$PPV + df$TPR), 2)

  return(df)
}

txt_spacer = function(attr, val, len = 23) {
  paste0(
    attr,
    strrep(" ", len - nchar(as.character(attr)) - nchar(as.character(val))),
    val
  )
}

textplotter = function(txt){
  # Surrounding rectangle

  # label_grob <- grid::roundrectGrob(
  #   x = 0.5, y = 0.5,
  #   width = 1, height = 1,
  #   r = grid::unit(0.5, "line"),
  #   gp = grid::gpar(fill = "#ffffff", col = "black", lwd = 1)
  # )

  ggplot2::ggplot() +
    # ggplot2::annotation_custom(label_grob) +
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

tableplotter = function(df) {
  ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::annotate(
      "label",
      x = 0.0, y = 0.0,
      label = paste(catch_console(df), collapse = "\n"),
      family = "mono",
      size = 4,
      hjust = 0.5, vjust = 0.5,
      # label.size = 0.5,
      fill = "#f0f0f0",
      label.r = grid::unit(0.1, "lines")
    ) +
    ggplot2::theme_void()
}

