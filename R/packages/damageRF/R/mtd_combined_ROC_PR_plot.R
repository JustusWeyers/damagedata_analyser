#' combined_ROC_PR_plot method
#'
#' @param self self
#' @export

setGeneric("combined_ROC_PR_plot", function(self) standardGeneric("combined_ROC_PR_plot"))

setMethod("combined_ROC_PR_plot", "RF", function(self) {

  n = ceiling(sqrt(length(unique(dplyr::bind_rows(self@roc_df, self@pr_df)$.level))))

  dplyr::bind_rows(self@roc_df, self@pr_df) |>
    ggplot2::ggplot() +

    ggplot2::geom_abline(intercept = 0, slope = 1, color = "grey70", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y, color = type)) +
    ggplot2::scale_color_manual(values = c("ROC" = "blue", "PR" = "red")) +

    ggplot2::facet_wrap(
      ~ .level,
      ncol = n,
      nrow = n
    ) +
    ggplot2::xlab("ROC: 1 - Specificity (FPR) | PR: Recall (TPR)") +
    ggplot2::ylab("ROC: Sensitivity (TPR) | PR: Precision (PPV)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey80", color = "black", linewidth = 0.5),
      strip.text = ggplot2::element_text(color = "black"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(size = 8, angle = 55, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.title.x = ggplot2::element_text(size = 8),
      axis.title.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    ggplot2::coord_fixed(ratio = 1)
})
