# Catch console output
catch_console = function(o) {

  old_width = getOption("width")
  options(width = 1000)
  out = utils::capture.output(print(o))
  options(width = old_width)

  return(out)
}

# Metrics data.frame from classification results
metrics_df = function(class, pred) {

  pred$.pred_class = factor(pred$.pred_class, levels = levels(pred$truth))

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

kappa_linear_weighted = function(truth_num, pred_num) {
  lvls = sort(unique(c(truth_num, pred_num)))
  if (length(lvls) < 2) return(NA_real_)
  n   = length(truth_num)
  obs = table(factor(truth_num, lvls), factor(pred_num, lvls))
  w   = outer(lvls, lvls, function(i, j) 1 - abs(i - j) / (max(lvls) - min(lvls)))
  exp = outer(rowSums(obs) / n, colSums(obs) / n) * n
  po  = sum(w * obs) / n
  pe  = sum(w * exp) / n
  round((po - pe) / (1 - pe), 3)
}

txt_spacer = function(attr, val, len = 23) {
  paste0(
    attr,
    strrep(" ", len - nchar(as.character(attr)) - nchar(as.character(val))),
    val
  )
}
