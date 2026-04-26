transition_martix_plots <- function(bspid, freq, fulldata){
  mat = freq[[bspid]]
  
  mat_df = reshape2::melt(mat)
  colnames(mat_df) = c("Row", "Col", "Value")
  
  # Diagonale nicht in Farbskala berücksichtigen
  mat_df$FillValue = mat_df$Value
  mat_df$FillValue[mat_df$Row == mat_df$Col] = NA
  
  
  # ---- Static Heatmap ----
  
  mat_plot = ggplot2::ggplot(mat_df, ggplot2::aes(x = Col, y = Row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = FillValue), color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = Value), size = 3) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "steelblue",
      na.value = "white"
    ) +
    ggplot2::scale_y_discrete(limits = rev(levels(mat_df$Row))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      legend.position = "none"
    )
  
  # ---- Interactive Heatmap ----
  mat_plot_ggiraph <- ggplot2::ggplot(mat_df, ggplot2::aes(x = Col, y = Row)) +
    ggiraph::geom_tile_interactive(
      ggplot2::aes(fill = FillValue, tooltip = NULL, data_id = paste(Row, Col, sep = "_")),
      color = "black"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = Value), size = 1.2) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue", na.value = "white") +
    ggplot2::scale_y_discrete(limits = rev(levels(mat_df$Row))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = -0.1, vjust = 0.5, size = 5),
      axis.text.y = ggplot2::element_text(size = 5),
      legend.position = "none"
    )
  
  # Wrap in ggiraph object
  heatmap_giraph <- ggiraph::girafe(ggobj = mat_plot_ggiraph, options = list(
    ggiraph::opts_selection(type = "single", only_shiny = TRUE),
    ggiraph::opts_tooltip(css = "background-color:lightgray;padding:5px;border-radius:4px;")
  ))
  
  # ---- Boxplot ----
  dta <- fulldata[startsWith(fulldata$bsp_id, bspid), c("bsp_id", "bewert_d")]
  dta <- na.omit(dta[dta$bsp_id %in% colnames(mat), ])
  
  box_plot <- ggplot2::ggplot(dta, ggplot2::aes(x = bsp_id, y = bewert_d)) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(
      fun = mean, geom = "crossbar", width = 0.6, color = "red"
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = -0.1, vjust = 0.5)
    )
  
  combined = cowplot::plot_grid(
    box_plot,  mat_plot, ncol = 1, align = "v", rel_heights = c(1, 3)
  )
  
  return(list(
    boxplot_static = box_plot,
    heatmap_static = mat_plot,
    combined_static = combined,
    heatmap_interactive = heatmap_giraph
  ))
  
}
