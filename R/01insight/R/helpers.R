# Bewertungsmatrix
note = function(bewert_s, bewert_v, bewert_d) {
  
  mat = array(unlist(list(
    matrix(c(
      4.0, 4.0, 4.0, 4.0, 4.0,
      3.0, 3.2, 3.4, 3.6, 4.0,
      2.1, 2.2, 2.3, 2.7, 4.0,
      1.2, 1.3, 2.1, 2.6, 4.0,
      1.0, 1.1, 2.0, 2.5, 4.0
    ), nrow = 5, ncol = 5, byrow = TRUE),
    matrix(c(
      4.0, 4.0, 4.0, 4.0, 4.0,
      3.1, 3.3, 3.5, 3.7, 4.0,
      2.2, 2.3, 2.4, 2.8, 4.0,
      1.5, 1.7, 2.2, 2.7, 4.0,
      1.1, 1.3, 2.1, 2.6, 4.0
    ), nrow = 5, ncol = 5, byrow = TRUE),
    matrix(c(
      4.0, 4.0, 4.0, 4.0, 4.0,
      3.2, 3.4, 3.6, 3.8, 4.0,
      2.3, 2.5, 2.6, 2.9, 4.0,
      2.2, 2.3, 2.4, 2.8, 4.0,
      1.8, 2.1, 2.2, 2.7, 4.0
    ), nrow = 5, ncol = 5, byrow = TRUE),
    matrix(c(
      4.0, 4.0, 4.0, 4.0, 4.0,
      3.3, 3.5, 3.7, 3.9, 4.0,
      2.8, 3.0, 3.1, 3.2, 4.0,
      2.7, 2.8, 2.9, 3.0, 4.0,
      2.5, 2.6, 2.7, 2.8, 4.0
    ), nrow = 5, ncol = 5, byrow = TRUE),
    matrix(c(
      4.0, 4.0, 4.0, 4.0, 4.0,
      3.4, 3.6, 3.8, 4.0, 4.0,
      3.3, 3.5, 3.6, 3.7, 4.0,
      3.2, 3.3, 3.4, 3.5, 4.0,
      3.0, 3.1, 3.2, 3.3, 4.0
    ), nrow = 5, ncol = 5, byrow = TRUE)
  )), dim = c(5, 5, 5))
  
  return(mat[4-(bewert_s-1), bewert_v+1, bewert_d+1])
}

# Fetch bspid description
get_desc = function(bspid, mainclass = bspid_mainclass, df = bspid_classes) {
  
  " ----
  Build description for an two- or threepart bspid based on entries in df
  - JW 12.08.2025"
  
  x = as.character(as.numeric(strsplit(bspid, split = "-", fixed = TRUE)[[1]]))
  
  d1 = mainclass$description[mainclass$class1 == x[1]]
  
  if (length(x) == 2) {
    d2 = df$description[df$class1 == x[1] & df$class2 == x[2]]
    return(c(d1, d2))
  } else if (length(x) == 3) {
    d2 = paste(
      df$description[df$class1 == x[1] & is.na(df$class2) & df$class3 == x[3]]
    )
    return(c(d1, d2))
  } else {
    return("-")
  }
  
}

# Plot function
my_plotter = function(bspid, data_tbw, ylim = NA) {
  bar_df = data_tbw |> 
    dplyr::filter(bsp_id == bspid) |> 
    dplyr::group_by(prufjahr) |> 
    dplyr::count(prufjahr, Tbw)
  
  length_df = data_tbw |> 
    dplyr::filter(bsp_id == bspid) |> 
    dplyr::group_by(Tbw) |> 
    dplyr::summarise(difference = max(prufjahr) - min(prufjahr))
  
  # Plot 1
  hist = ggplot(bar_df, aes(x = prufjahr, y = n, fill = Tbw)) +
    geom_bar(stat = "identity") +
    ggplot2::scale_fill_grey() +
    theme_minimal() +
    labs(
      title = bspid,
      x = "Prüfjahr",
      y = "n ~ Tbw",
      subtitle = stringr::str_wrap(toString(get_desc(bspid, bspid_mainclass, bspid_classes)), width = 55)
    ) +
    theme(
      legend.position = "none",
      axis.text  = element_text(size = 6), 
      axis.title = element_text(size = 8),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 8)
    )
  
  if (is.na(ylim)) {
    # hist = hist + scale_y_continuous(breaks = integer_breaks(max(bar_df$n)))
  } else {
    hist = hist + ylim(0, ylim)
  }
  
  # Plot 2
  barplot = ggplot() +
    ggplot2::geom_histogram(
      data = length_df, 
      ggplot2::aes(y = difference), 
      bins = length(0:max(length_df$difference))
    ) +
    ggplot2::geom_text(
      stat = "bin", 
      ggplot2::aes(label = after_stat(count)),
      hjust = -0.1, 
      col = "red"
    ) +
    ggplot2::labs(
      subtitle = stringr::str_wrap("max. obs. length [a]", width = 10)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(angle = 90, hjust = 1, size = 6), 
      axis.text.y  = ggplot2::element_text(size = 6), 
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 8)
    )
  
  # Plot 1 + 2
  p <- hist + barplot + patchwork::plot_layout(widths = c(7, 1))
  
  return(p)
}

# Print to pdf
printer = function(filename, plots) {
  pdf(filename, width = 8.3, height = 11.7)
  
  plots_per_page <- 12
  n_pages <- ceiling(length(plots) / plots_per_page)
  
  for (i in 1:n_pages) {
    from <- (i - 1) * plots_per_page + 1
    to <- min(i * plots_per_page, length(plots))
    
    page_plots <- patchwork::wrap_plots(plots[from:to], ncol = 2, nrow = 6)
    
    print(page_plots)
  }
  
  dev.off()
}
