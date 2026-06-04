setClass(
  "BSPID",
  slots = list(
    bspid = "character",
    data = "data.frame",
    subs = "character",
    groups = "list",
    desc = "character",
    target_col = "character",
    age_col = "character",
    id_col = "character",
    bspid_col = "character",
    target_name = "character",
    age_name = "character",
    models = "list",
    bspid_codes = "data.frame",
    id_vars = "character",
    target_vars = "character"
  ),
  prototype = list(
    bspid = NA_character_,
    data = data.frame(),
    subs = NA_character_,
    groups = list(),
    desc = NA_character_,
    target_col = "rtg_d",
    age_col = "age",
    id_col = "id",
    bspid_col = "bsp_id",
    target_name = "Rating D [-]",
    age_name = "Age [a]",
    models = list(),
    bspid_codes = data.frame(),
    id_vars = c(
      "id", "location", "sub_struct_no", "damage_id", "structure_name",
      "damage", "X.Y.UTM.32N"
    ),
    target_vars = c(
      "condition_score", "condition_class", "substance_idx", "rtg_s",
      "rtg_v", "rtg_d", "max_s", "max_v", "max_d", "bsp_id"
    )
  )
)

setMethod("initialize", "BSPID", function(
    .Object, bspid, data = data.frame(), subs = c(), groups = list(), ...
) {

  .Object <- methods::callNextMethod()

  if (is.null(.Object@bspid_codes) || nrow(.Object@bspid_codes) == 0) {
    data("bspid_codes", package = "damageRF", envir = environment())
    .Object@bspid_codes <- bspid_codes
  }

  .Object@bspid <- bspid

  if ("bsp_id" %in% colnames(data)) {
    .Object@data <- data[startsWith(data$bsp_id, bspid),]
  } else {
    .Object@data <- data
  }

  sep <- as.numeric(strsplit(.Object@bspid, "-", fixed = TRUE)[[1]])

  if (length(sep) > 3) {
    stop("bspid of wrong format")
  }

  cols <- paste0("class", seq_along(sep))

  cond <- Reduce(`&`, Map(function(col, val) {
    x <- .Object@bspid_codes[[col]]

    if (col == "class2") {
      (x == val) | is.na(x)
    } else {
      x == val
    }
  }, cols, sep))

  cl1 <- .Object@bspid_codes[cond, c("class1", "class2", "class3")]

  c1 <- unique(na.omit(cl1$class1))
  c2 <- unique(na.omit(cl1$class2))
  c3 <- unique(na.omit(cl1$class3))

  if (length(c3) == 0) {
    exp <- expand.grid(
      class1 = sprintf("%03d", c1),
      class2 = sprintf("%02d", c2)
    )
  } else {
    exp <- expand.grid(
      class1 = sprintf("%03d", c1),
      class2 = sprintf("%02d", c2),
      class3 = sprintf("%02d", c3)
    )
  }

  .Object@subs <- sort(sapply(1:nrow(exp), function(i) {
    paste0(na.omit(unlist(exp[i, ])), collapse = "-")
  }))

  .Object@groups = lapply(groups, sort)

  .Object@bspid_codes <- .Object@bspid_codes[
    .Object@bspid_codes$class1 == as.numeric(sep[1]),
  ]

  return(.Object)
})

#' @export
setGeneric("get_desc", function(self, de = FALSE, limit_lengthout = TRUE) {
  standardGeneric("get_desc")
})

setMethod("get_desc", "BSPID", function(self, de = FALSE, limit_lengthout = TRUE) {
  sep <- as.numeric(strsplit(self@bspid, "-", fixed = TRUE)[[1]])

  cds = self@bspid_codes

  if (length(sep) == 1) {
    desc <- cds$tl_desc[cds$class1 == sep[1]][1]
    desc = desc[!is.na(desc)]
  }

  if (length(sep) == 2) {
    idx_base <- cds$class1 == sep[1]
    idx_12 <- idx_base & cds$class2 == sep[2]

    desc <- c(
      cds$tl_desc[idx_12],
      cds$dtl_desc[idx_12]
    )

    desc = desc[!is.na(desc)]

  }

  if (length(sep) == 3) {
    idx_base <- cds$class1 == sep[1]
    idx_12 <- idx_base & cds$class2 == sep[2]
    desc <- c(
      cds$tl_desc[idx_12],
      cds$dtl_desc[idx_12]
    )
    idx_13 <- idx_base & cds$class3 == sep[3]
    desc <- c(desc, cds$dtl_desc[idx_13])
    desc = desc[!is.na(desc)]

    if (limit_lengthout) {
      desc[2] <- paste(desc[2], desc[3], sep = " - ")
      desc <- desc[-3]
    }
  }

  return(desc)
})

# setGeneric(plot_target_age, "get_data", function(self) {
#   standardGeneric("get_data")
# })
#
# setMethod("get_data", "BSPID", function(self) {
#   sub_data = lapply(self@subs, get_data)
#   appended = do.call(rbind, c(list(self@data), sub_data))
#   return(appended)
# })

#' @export
setGeneric("get_pairings", function(self) {
  standardGeneric("get_pairings")
})

setMethod("get_pairings", "BSPID", function(self) {

  pairings = unlist(
    lapply(1:length(self@subs), function(b) {
      utils::combn(self@subs, b, simplify = FALSE)
    }),
    recursive = FALSE
  )

  pairings = lapply(pairings, sort)

  return(Filter(function(x) length(x) > 1, pairings))
})

#' @export
setGeneric("trend_over_combi", function(self, v) {
  standardGeneric("trend_over_combi")
})

setMethod("trend_over_combi", "BSPID", function(self, v = FALSE) {

  pairings <- get_pairings(self)
  split_data <- split(self@data, self@data$bsp_id)

  regs_df <- purrr::map2_dfr(pairings, seq_along(pairings), function(p, i) {

    if (v) {
      print(round(i / length(pairings), 2))
    }

    d <- dplyr::bind_rows(split_data[p])

    if (nrow(d) < 2) {
      return(tibble::tibble(
        intercept = NA_real_,
        steigung = NA_real_,
        signifikanz = NA_real_,
        nobs = 0,
        combi = paste(sort(p), collapse = ", "),
        ncombi = length(p)
      ))
    }

    X <- cbind(1, d$alter)
    y <- d$bewert_d

    fit <- stats::lm.fit(X, y)
    coefs <- fit$coefficients

    tibble::tibble(
      intercept   = coefs[1],
      steigung    = coefs[2],
      signifikanz = NA_real_,
      nobs        = length(y),
      combi       = paste(sort(p), collapse = ", "),
      ncombi      = length(p)
    )
  })

  return(regs_df)

})

#' @export
setGeneric("set_data", function(self, data) {
  standardGeneric("set_data")
})

setMethod("set_data", "BSPID", function(self, data) {
  self@data <- data[startsWith(as.character(data$bsp_id), self@bspid),] |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character) & !dplyr::all_of("id"), as.factor))
  return(self)
})

#' @export
setGeneric("delete_data", function(self) {
  standardGeneric("delete_data")
})

setMethod("delete_data", "BSPID", function(self) {
  self@data <- data.frame()
  return(self)
})

#' @export
setGeneric("get_data", function(self) {
  standardGeneric("get_data")
})

setMethod("get_data", "BSPID", function(self = FALSE) {
  self@data
})

#' @export
setGeneric("plot_target_age", function(self, bygroup = FALSE, interactive = FALSE) {
  standardGeneric("plot_target_age")
})

setMethod("plot_target_age", "BSPID", function(self, bygroup = FALSE, interactive = FALSE) {

  df = get_data(self) # |>
  # dplyr::filter(id %in% sample(unique(id), 10000))

  desc = get_desc(self)

  if (length(desc) == 1) {
    title = paste(self@bspid, "|" , desc)
    subtitle = NULL
  } else if (length(desc) > 1) {
    title = paste(self@bspid, "|" , desc[1])
    subtitle = desc[2:length(desc)]
  }

  if (bygroup) {

    group_df <- tibble::enframe(self@groups, name = "group", value = self@bspid_col) |>
      tidyr::unnest(self@bspid_col)

    df_long <- dplyr::left_join(
      df,
      group_df,
      by = self@bspid_col,
      relationship = "many-to-many") |>
      dplyr::filter(!is.na(group))

    df_long[[self@target_col]] <- factor(
      df_long[[self@target_col]],
      levels = rev(sort(unique(df_long[[self@target_col]])))
    )

    df_counts <- df_long |>
      dplyr::count(group, .data[[self@target_col]])


    p = ggplot2::ggplot(
      df_long,
      ggplot2::aes(
        x = .data[[self@age_col]],
        y = .data[[self@target_col]]
      )
    ) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[self@bspid_col]]), alpha = 0.05, position = ggplot2::position_jitter(height = 0.1)) +
      ggplot2::geom_violin(fill = "transparent") +
      ggplot2::stat_summary(
        fun = median,
        geom = "point",
        size = 2,
        color = "black"
      ) +
      ggplot2::stat_summary(
        fun.data = function(x) {
          data.frame(
            y = median(x),
            ymin = quantile(x, 0.25),
            ymax = quantile(x, 0.75)
          )
        },
        geom = "errorbar",
        width = 0.2,
        color = "black"
      ) +
      ggplot2::geom_text(
        data = df_counts,
        ggplot2::aes(
          y = .data[[self@target_col]],
          x = Inf,
          label = paste0("n = ", n)
        ),
        inherit.aes = FALSE,
        hjust = 1.2,
        vjust = -0.6,
        color = "darkgray"
      ) +
      ggplot2::facet_wrap(
        ~ group,
        ncol = 1,
        labeller = ggplot2::as_labeller(function(x) {
          paste0(self@bspid, "-group: '", x, "'")
        })
      ) +
      ggplot2::theme_minimal() +
      ggplot2::guides(
        color = ggplot2::guide_legend(override.aes = list(alpha = 1))
      ) +
      ggplot2::labs(
        x = self@age_name,
        y = self@target_name,
        title = title,
        subtitle = subtitle,
        color = "BSP-ID"
      )

  } else {
    p = ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = .data[[self@age_col]],
        y = .data[[self@target_col]],
        group = .data[[self@id_col]],
        color = .data[[self@id_col]],
        text = id
      )) +
      ggplot2::geom_line() +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
  }

  if (interactive) {
    p = plotly::ggplotly(p, tooltip = "text")
  }

  return(p)
})

#' @export
setGeneric("binary_classification", function(self, model, target = NA, test_run = FALSE, ...) {
  standardGeneric("binary_classification")
})

setMethod("binary_classification", "BSPID", function(self, model, target = NA, test_run = FALSE, ...) {

  if (is.na(target)) {
    target = self@target_col
  }

  self@models[["binary_classification"]] = model(
    data = self@data,
    id = self@id_col,
    id_vars = self@id_vars,
    target_vars = self@target_vars,
    target = target,
    test_run = test_run,
    time_ax = self@age_col,
    ...
  )

  return(self)
})

#' @export
setGeneric("train_groups", function(self, model, target = NA, test_run = FALSE, path = NULL, ...) {
  standardGeneric("train_groups")
})

setMethod("train_groups", "BSPID", function(self, model, target = NA, test_run = FALSE, path = NULL, ...) {

  if (is.na(target)) {
    target = self@target_col
  }

  self@models[names(self@groups)] = lapply(seq_along(self@groups), function(i) {

    m = model(
      identity = setNames(self@groups[i], names(self@groups)[i]),
      data = self@data[self@data[[self@bspid_col]] %in% self@groups[[i]],],
      id = self@id_col,
      id_vars = self@id_vars,
      target_vars = self@target_vars,
      target = target,
      test_run = test_run,
      time_ax = self@age_col,
      ...
    )

    if (!is.null(path)) saveRDS(m, file.path(path, paste0(names(self@groups)[i], ".rds")))

    m
  })

  return(self)
})

#' @export
setGeneric("predict", function(self, model, ...) {
  standardGeneric("predict")
})

setMethod("predict", "BSPID", function(self, model) {

  if (!is.character(model)) stop("model must be a character vector of model name(s)")

  # Classification path: single model with a best_threshold
  if (length(model) == 1 &&
      !is.null(self@models[[model]]@prediction$best_threshold)) {

    m     = self@models[[model]]
    baked = suppressWarnings(recipes::bake(m@trained_model$recipe, new_data = self@data))

    return(
      self@data |>
        dplyr::mutate(
          pred_prob = stats::predict(m@trained_model$fit, data = baked)$predictions[, "1"]
        ) |>
        dplyr::group_by(id) |>
        dplyr::mutate(
          pred = as.numeric(any(pred_prob >= m@prediction$best_threshold))
        ) |>
        dplyr::ungroup()
    )
  }

  # Regression path: route each id to group model(s) via init_bsp_id (stable)
  # All rows of an id are predicted by the group if its init_bsp_id is a member
  group_map = tibble::enframe(self@groups, name = "group", value = self@bspid_col) |>
    tidyr::unnest(self@bspid_col) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(self@bspid_col), as.character))

  id_group_map = self@data |>
    dplyr::distinct(id, init_bsp_id) |>
    dplyr::mutate(init_bsp_id = as.character(init_bsp_id)) |>
    dplyr::left_join(group_map, by = setNames(self@bspid_col, "init_bsp_id")) |>
    dplyr::filter(!is.na(group)) |>
    dplyr::distinct(id, group)

  self@data |>
    dplyr::left_join(id_group_map, by = "id") |>
    dplyr::filter(group %in% model) |>
    dplyr::group_split(group) |>
    purrr::map_dfr(function(chunk) {
      grp   = unique(chunk$group)
      m     = self@models[[grp]]
      baked = suppressWarnings(recipes::bake(m@trained_model$recipe, new_data = chunk |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))))
      quant_matrix = stats::predict(
        m@trained_model$fit,
        data      = baked,
        type      = "quantiles",
        quantiles = m@trained_model$quantiles
      )$predictions
      colnames(quant_matrix) = paste0("q", m@trained_model$quantiles)
      dplyr::bind_cols(chunk, as.data.frame(quant_matrix))
    })
})

