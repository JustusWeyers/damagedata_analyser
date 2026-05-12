#' hist_target_plotter method
#' @import ggplot2
#'
#' @param self self

#' @export
setGeneric("hist_target_plotter", function(self, target_name) standardGeneric("hist_target_plotter"))

setMethod("hist_target_plotter", "RF", function(self, target_name) {

  df1 = data.frame(self@train_data)
  df2 = data.frame(self@test_data)

  df1$source = "df1"
  df2$source = "df2"

  df = rbind(df1, df2)

  label_df = df |>
    dplyr::count(target, source) |>
    dplyr::group_by(target) |>
    dplyr::summarise(
      label = paste0("train = ", n[1], "\ntest = ",  n[2]),
      y = sum(n)
    )

  p = ggplot2::ggplot(df, ggplot2::aes(x = as.factor(target), fill = source)) +
    ggplot2::geom_bar() +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(
        x = as.factor(target),
        y = y,
        label = label
      ),
      vjust = -0.3,
      size = 3,
      inherit.aes = FALSE
    ) +

    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.33))
    ) +
    ggplot2::xlab(self@target_var) +
    ggplot2::ylab(NULL) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2, size = 3,
      color = "gray", label = paste0(
        "Gini = ",
        format(self@gini, scientific = TRUE, digits = 2)
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_grey() +
    ggplot2::theme(legend.position = "none")

  if (!is.na(target_name)) {
    p = p + ggplot2::labs(x = target_name)
  }

  return(p)

})
