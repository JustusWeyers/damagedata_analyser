metric_plot = function(m, metric_title, metric_shrt) {
  
  # Standardize incoming metric frame shape for plotting.
  colnames(m) = c("class", "n", "P", "metric", "bsp_id", "class1") 
  
  # Use a near-square facet grid based on available classes.
  facets = length(unique(m$class))
  cols = ceiling(sqrt(facets))
  rows = ceiling(facets / cols)
  
  # Panel 1: per-class bars with inline metric labels.
  p1 <- m |>
    dplyr::mutate(metric_sort = ifelse(is.na(metric), -Inf, metric)) |>
    ggplot2::ggplot(ggplot2::aes(
      y = tidytext::reorder_within(bsp_id, metric_sort, class), 
      x = metric, 
      fill = class1
    )) +
    ggplot2::scale_fill_discrete(name = "BSP-ID") +
    ggplot2::geom_bar(stat = "identity", na.rm = TRUE) +
    ggplot2::geom_text(
      stat = "identity", 
      ggplot2::aes(
        label = paste0(round(metric, 2), " (P/n = ", round(P/n, 2), ")"), 
        hjust = ifelse(metric <= 0.33, -0.1, 1.1),
        col = ifelse(metric <= 0.33, "low", "high")
      ), 
      vjust = 0.3, size = 1.6, show.legend = FALSE, na.rm = TRUE
    ) +
    ggplot2::labs(
      x = metric_shrt,
      y = "BSP-ID"
    ) +
    ggplot2::xlim(0, 1) +
    ggplot2::facet_wrap(~ class, scales = "free_y", nrow = rows, ncol = cols) +
    tidytext::scale_y_reordered() +
    ggplot2::scale_color_manual(values = c("low" = "gray60", "high" = "white")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      text = ggplot2::element_text(size = 8)
    )
  
  # Panel 2: ratio-vs-metric scatter with class markers.
  p2 = m |> 
    dplyr::filter(!is.na(metric) & !is.na(P/n)) |> 
    ggplot2::ggplot(ggplot2::aes(x = P/n, y = metric)) +
    ggplot2::geom_point(ggplot2::aes(pch = class)) +
    ggplot2::scale_shape(name = "Bewertung D")+
    ggplot2::geom_text(ggplot2::aes(label = bsp_id), cex = 2.3, col = "gray", hjust = -0.2, vjust = 0.1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, col = "gray", lty = "dashed") +
    ggplot2::labs(
      y = metric_shrt
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      text = ggplot2::element_text(size = 8)
    )
  
  # Combine both panels into a single report figure.
  p = patchwork::wrap_plots(p1, p2, ncol = 2) # +
    # patchwork::plot_annotation(metric_title)
  
  return(p)
}

printer = function(filename, plots) {
  # Render plots into a landscape PDF with two plots per page.
  pdf(filename, width = 15.0, height = 8.3)
  
  plots <- plots[!vapply(plots, is.null, logical(1))]
  
  plots_per_page <- 2
  n_pages <- ceiling(length(plots) / plots_per_page)
  
  for (i in seq_len(n_pages)) {
    from = (i - 1) * plots_per_page + 1
    to = min(i * plots_per_page, length(plots))
    
    page_plots <- cowplot::plot_grid(
      plotlist = plots[from:to],
      ncol = 1,
      nrow = 2,
      align = "v"
    )
    
    print(page_plots)
  }
  
  dev.off()
}
