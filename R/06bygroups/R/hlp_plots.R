
plot_rtg_d_over_age = function(df, n = 100) {
  df |>
    dplyr::filter(id %in% sample(unique(id), n)) |>
    ggplot2::ggplot(ggplot2::aes(x = age, y = rtg_d, colour = id, group = id)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::labs(
      x      = "Age",
      y      = "Rating D",
      colour = "Damage"
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}


plot_change_freqs = function(df) {
  
  df |>
    dplyr::mutate(
      rtg_d = factor(rtg_d),
      bsp_id = factor(bsp_id)
    ) |> 
    tidyr::pivot_longer(
      cols = c(bsp_id, rtg_d),
      names_to = "zustand_var",
      values_to = "zustand"
    ) |>
    dplyr::group_by(id, zustand_var) |>
    dplyr::summarise(
      n_distinct = dplyr::n_distinct(zustand),
      .groups = "drop"
    ) |>
    dplyr::count(zustand_var, n_distinct) |>
    ggplot2::ggplot(ggplot2::aes(x = n_distinct, y = n)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = n),
      vjust = 1.2,
      color = "white"
    ) +
    ggplot2::facet_wrap(~ zustand_var) +
    ggplot2::labs(
      x = "Number of distinct values per id",
      y = "Number of Entries",
      title = paste(bspid@bspid, "|", get_desc(bspid))
    ) +
    ggplot2::theme_minimal()
}

.pw_core = function(df, ids, pred_col, ribbon_lo, ribbon_hi) {

  if (!is.null(pred_col) && "group" %in% colnames(df)) {
    all_groups   = sort(na.omit(unique(as.character(df$group))))
    other_groups = setdiff(all_groups, "lag_1")
    color_map = c(
      setNames(
        colorRampPalette(c("#1565C0", "#BBDEFB"))(max(1L, length(other_groups))),
        other_groups
      ),
      if ("lag_1" %in% all_groups) c(lag_1 = "#D43F28") else NULL
    )
    size_map = c(
      setNames(
        seq(2.5, 1, length.out = max(1L, length(other_groups))),
        other_groups
      ),
      if ("lag_1" %in% all_groups) c(lag_1 = 1.5) else NULL
    )
  }

  x_max    = max(df$inspection_yr, na.rm = TRUE)
  x_breaks = seq(2000, x_max, by = 5)

  plots = lapply(ids, function(i) {

    df_i  = dplyr::filter(df, id == i)
    df_obs = if ("group" %in% colnames(df_i)) dplyr::filter(df_i, is.na(group)) else df_i

    p = ggplot2::ggplot(df_i) +
      ggplot2::geom_line(data = df_obs, ggplot2::aes(x = inspection_yr, y = rtg_d), lwd = 0.8) +
      ggplot2::geom_point(data = df_obs, ggplot2::aes(x = inspection_yr, y = rtg_d)) +
      ggplot2::scale_y_reverse(limits = c(4, 0)) +
      ggplot2::scale_x_continuous(
        limits = c(2000, x_max),
        breaks = x_breaks,
        labels = function(x) paste0("'", substr(x, 3, 4))
      ) +
      ggplot2::geom_vline(xintercept = 2020, linetype = "dashed", color = "gray") +
      ggplot2::labs(
        title = {
          init_id = unique(na.omit(df_i$init_bsp_id))
          if (length(init_id) == 1L)
            paste0(i, " (init_bsp_id = ", init_id, ")")
          else
            i
        },
        x = NULL, y = NULL
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title           = ggplot2::element_text(face = "plain", size = 7),
        legend.position      = c(0.01, 0.99),
        legend.justification = c(0, 1),
        legend.title         = ggplot2::element_blank(),
        legend.background    = ggplot2::element_rect(
          fill      = scales::alpha("white", 0.7),
          color     = "black",
          linewidth = 0.3
        ),
        legend.key.size      = ggplot2::unit(0.35, "cm"),
        legend.key.spacing.y = ggplot2::unit(0, "pt"),
        legend.margin        = ggplot2::margin(0, 5, 0, 5),
        legend.text          = ggplot2::element_text(size = 6)
      )

    if (!is.null(pred_col) && "group" %in% colnames(df_i)) {
      df_pred    = dplyr::filter(df_i, !is.na(group))
      groups_i   = sort(na.omit(unique(as.character(df_i$group))))
      has_ribbon = all(c(ribbon_lo, ribbon_hi) %in% colnames(df_pred))

      p = p +
        ggplot2::guides(
          color = ggplot2::guide_legend(nrow = min(2L, length(groups_i))),
          size  = "none"
        ) +
        {if (has_ribbon)
          ggplot2::geom_ribbon(
            data = df_pred,
            ggplot2::aes(
              x    = inspection_yr,
              ymin = .data[[ribbon_lo]],
              ymax = .data[[ribbon_hi]],
              fill = group
            ),
            alpha    = 0.15,
            linetype = 0
          )
        } +
        ggplot2::geom_line(
          data = df_pred,
          ggplot2::aes(x = inspection_yr, y = .data[[pred_col]], color = group),
          linetype = "dashed"
        ) +
        ggplot2::geom_point(
          data = df_pred,
          ggplot2::aes(x = inspection_yr, y = .data[[pred_col]], color = group, size = group)
        ) +
        ggplot2::scale_color_manual(values = color_map[groups_i]) +
        ggplot2::scale_size_manual(values = size_map[groups_i], guide = "none") +
        {if (has_ribbon)
          ggplot2::scale_fill_manual(values = color_map[groups_i], guide = "none")
        }
    }

    p
  })

  result = patchwork::wrap_plots(plots, ncol = 4)
  suppressWarnings(print(result))
  invisible(result)
}


plot_held_out_pw = function(df, pred_col = NULL, ribbon_lo = "q0.05", ribbon_hi = "q0.95") {
  ids = if (exists("id_order_ext")) {
    intersect(id_order_ext, unique(df$id))
  } else {
    unique(df$id)
  }
  .pw_core(df, ids, pred_col, ribbon_lo, ribbon_hi)
}


plot_temporal_pw = function(df, pred_col = NULL, ribbon_lo = "q0.05", ribbon_hi = "q0.95", n = 16L, ids = NULL) {
  if (!is.null(ids)) {
    ids = intersect(ids, unique(df$id[!is.na(df$id)]))
  } else {
    all_ids = unique(df$id[!is.na(df$id)])
    ids     = sample(all_ids, min(n, length(all_ids)))
  }
  .pw_core(df, ids, pred_col, ribbon_lo, ribbon_hi)
}

