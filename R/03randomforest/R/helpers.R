my_class_metrics = function(class, pred) {
  
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

# ttsplit = function(data, prop) {
#   initial_sp = rsample::group_initial_split(
#     data = data, 
#     group = schaden, 
#     strata = max_bewert_d, 
#     prop = prop
#   )
#   
#   x = rsample::training(initial_sp) |> dplyr::select(-max_bewert_d)
#   a = rsample::testing(initial_sp) |> dplyr::select(-max_bewert_d) 
#   
#   return(list(x = x, a = a))
# }

# plotter = function(bspid, hist, tune_plt, metrics, vip, roc_plt) {
#   
#   table_grob <- gridExtra::tableGrob(metrics, theme = gridExtra::ttheme_minimal(base_size = 10))
#   
#   top = tune_plt | hist # ggplot() + theme_void()
#   btt = table_grob
#   
#   left = top/btt
#   middle = vip
#   right = roc_plt 
#   
#   p <- (left | middle | right) + patchwork::plot_layout(ncol = 3, widths = c(2, 1, 1))
#   
#   p = p + patchwork::plot_annotation(title = bspid)
# 
#   return(p)
# }

printer = function(filename, plots) {
  pdf(filename, width = 8.3, height = 11.7)
  
  plots_per_page <- 6
  n_pages <- ceiling(length(plots) / plots_per_page)
  
  for (i in 1:n_pages) {
    from <- (i - 1) * plots_per_page + 1
    to <- min(i * plots_per_page, length(plots))
    
    page_plots <- patchwork::wrap_plots(plots[from:to], ncol = 1, nrow = 6)
    
    print(page_plots)
  }
  
  dev.off()
}

combined_ROC_PR_plot = function(roc_df, pr_df) {
  dplyr::bind_rows(roc_df, pr_df) |> 
    ggplot2::ggplot() +
    
    ggplot2::geom_line(ggplot2::aes(x = x, y = y, color = type)) +
    ggplot2::scale_color_manual(values = c("ROC" = "blue", "PR" = "red")) +
    
    ggplot2::facet_wrap(
      ~ .level, 
      ncol = ceiling(sqrt(length(unique(dplyr::bind_rows(roc_df, pr_df)$.level)))), 
      nrow = ceiling(sqrt(length(unique(dplyr::bind_rows(roc_df, pr_df)$.level))))
    ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "grey70", linetype = "dashed") +
    ggplot2::xlab("ROC: 1 - Specificity (FPR) | PR: Recall (TPR)") +
    ggplot2::ylab("ROC: Sensitivity (TPR) | PR: Precision (PPV)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey80", color = "black", size = 0.5),
      strip.text = ggplot2::element_text(color = "black"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    ggplot2::coord_fixed(ratio = 1)
}
