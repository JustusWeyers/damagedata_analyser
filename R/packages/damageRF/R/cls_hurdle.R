# Class RF definition
methods::setClass(
  "Hurdle",
  slots = c(
    split_year           = "numeric",
    data                 = "data.frame",
    train_data           = "data.frame",
    test_sets            = "list",
    classification_model = "ANY",
    regression_model     = "ANY",
    tima_ax              = "character",
    target_col           = "character",
    cls_res              = "ANY",
    reg_res              = "ANY",
    arreg_res            = "ANY",
    dynamic_ids          = "character"
  ),
  prototype = list(
    split_year           = 2020,
    data                 = data.frame(),
    train_data           = data.frame(),
    test_sets            = list(),
    classification_model = list(),
    regression_model     = list(),
    tima_ax              = "inspection_yr",
    target_col           = "rtg_d",
    cls_res              = NULL,
    reg_res              = NULL,
    arreg_res            = NULL,
    dynamic_ids          = NA_character_
  )
)

setMethod("initialize", "Hurdle", function(
    .Object, split_year = 2020, data,
    tima_ax = "inspection_yr", target_col = "rtg_d", ...
) {

  .Object@split_year  <- split_year
  .Object@data        <- data
  .Object@tima_ax     <- tima_ax
  .Object@target_col  <- target_col

  .Object@test_sets <- list(
    holdout = get_holdout_testdata(.Object),
    tempsplit = get_tempsplit_testdata(.Object)
  )

  .Object@train_data <- get_train_data(.Object)

  methods::callNextMethod(.Object, ...)

  return(.Object)
})

#' @export
setGeneric("get_holdout_testdata", function(self) {
  standardGeneric("get_holdout_testdata")
})

setMethod("get_holdout_testdata", "Hurdle", function(self) {

  global_train_data = self@data |>
    dplyr::filter(inspection_yr < self@split_year)

  held_out_candidates = global_train_data |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      span   = max(inspection_yr) - min(inspection_yr),
      n_detr = sum(diff(rtg_d) > 0),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      detr_cat = dplyr::case_when(
        n_detr == 0  ~ "constant",
        n_detr == 1  ~ "one",
        n_detr >= 2  ~ "two_plus"
      )
    )

  three_plus_ids = held_out_candidates |>
    dplyr::filter(detr_cat == "two_plus") |>
    dplyr::arrange(dplyr::desc(span)) |>
    dplyr::slice_head(n = 6) |>
    dplyr::pull(id)

  constant_ids = held_out_candidates |>
    dplyr::filter(detr_cat == "constant") |>
    dplyr::arrange(dplyr::desc(span)) |>
    dplyr::slice_head(n = 4) |>
    dplyr::pull(id)

  one_ids = held_out_candidates |>
    dplyr::filter(detr_cat == "one") |>
    dplyr::arrange(dplyr::desc(span)) |>
    dplyr::slice_head(n = max(0L, 16L - length(three_plus_ids) - length(constant_ids))) |>
    dplyr::pull(id)

  held_out_ids = c(constant_ids, one_ids, rev(three_plus_ids))

  held_out_base_ids = sapply(strsplit(held_out_ids, "_"), `[`, 1)

  global_test_data_long = self@data |>
    dplyr::filter(sapply(strsplit(id, "_"), `[`, 1) %in% held_out_base_ids)

  global_train_data = global_train_data |>
    dplyr::filter(!sapply(strsplit(id, "_"), `[`, 1) %in% held_out_base_ids)

  id_order_ext = unlist(lapply(held_out_base_ids, function(b) {
    sort(unique(global_test_data_long$id[
      sapply(strsplit(global_test_data_long$id, "_"), `[`, 1) == b
    ]))
  }))

  truth = global_test_data_long[, self@target_col]

  global_test_data_long = global_test_data_long |>
    dplyr::group_by(id) |>
    dplyr::arrange(.data[[self@tima_ax]], .by_group = TRUE) |>
    dplyr::mutate(
      !!self@target_col := dplyr::if_else(
        dplyr::row_number() == 1L,
        .data[[self@target_col]],
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  return(list(set = global_test_data_long, train = global_train_data, ids_ext = id_order_ext, truth = truth))
})

#' @export
setGeneric("get_tempsplit_testdata", function(self) {
  standardGeneric("get_tempsplit_testdata")
})

setMethod("get_tempsplit_testdata", "Hurdle", function(self) {

  data  = self@data
  truth = data |> dplyr::select(id, dplyr::all_of(c(self@tima_ax, self@target_col)))

  pre  = data |> dplyr::filter(.data[[self@tima_ax]] <  self@split_year)
  post = data |> dplyr::filter(.data[[self@tima_ax]] >= self@split_year)

  time_cols  = c("id", self@tima_ax, "age")

  last_state = pre |>
    dplyr::group_by(id) |>
    dplyr::slice_max(.data[[self@tima_ax]], n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of(setdiff(time_cols, "id")), -dplyr::all_of(self@target_col))

  post_filled = post |>
    dplyr::select(dplyr::all_of(time_cols)) |>
    dplyr::left_join(last_state, by = "id") |>
    dplyr::mutate(!!self@target_col := NA_real_)

  list(
    set   = dplyr::bind_rows(pre, post_filled),
    truth = truth
  )
})

#' @export
setGeneric("get_train_data", function(self) {standardGeneric("get_train_data")})

setMethod("get_train_data", "Hurdle", function(self) {

  holdout_ids = unique(self@test_sets[["holdout"]][["set"]]$id)

  data = self@data |>
    dplyr::filter(
      inspection_yr < self@split_year,
      !id %in% holdout_ids
    )

  return(data)
})


setMethod("predict", "Hurdle", function(self, model, ...) {
  testset = model

  test_data = self@test_sets[[testset]]$set

  bspid_cls     = set_data(self@classification_model, test_data)
  self@cls_res  = bspid_cls |> predict("binary_classification")

  self@dynamic_ids = self@cls_res |>
    dplyr::filter(pred == 1) |>
    dplyr::distinct(id) |>
    dplyr::pull(id)

  reg_data = if (testset == "tempsplit") {
    test_data |> dplyr::filter(.data[[self@tima_ax]] >= self@split_year)
  } else {
    test_data
  }

  bspid_reg    = set_data(self@regression_model, reg_data)
  self@reg_res = lapply(bspid_reg@models, function(m) {
    bspid_reg |> predict(names(m@identity))
  })

  self@arreg_res = ar(bspid_reg, dynamic_ids = self@dynamic_ids)

  return(self)
})

#' @export
setGeneric("plotter", function(self) standardGeneric("plotter"))

setMethod("plotter", "Hurdle", function(self) {



})

#' @export
setGeneric("plot_results", function(self, testset, mode = "reg", pred_col = "q0.5", ribbon_lo = "q0.05", ribbon_hi = "q0.95", n = 16L) {
  standardGeneric("plot_results")
})

setMethod("plot_results", "Hurdle", function(self, testset, mode = "reg", pred_col = "q0.5", ribbon_lo = "q0.05", ribbon_hi = "q0.95", n = 16L) {

  ts = self@test_sets[[testset]]

  truth = if ("id" %in% colnames(ts$truth)) {
    ts$truth
  } else {
    dplyr::tibble(id = ts$set$id, inspection_yr = ts$set[[self@tima_ax]], rtg_d = dplyr::pull(ts$truth))
  }

  obs = ts$set |>
    dplyr::select(id, inspection_yr, init_bsp_id) |>
    dplyr::left_join(truth, by = c("id", "inspection_yr"))

  pred = if (mode == "ar") self@arreg_res else self@reg_res

  data = dplyr::bind_rows(obs, dplyr::bind_rows(pred))

  ids_ext = ts$ids_ext

  ids = if (!is.null(ids_ext)) {
    intersect(ids_ext, unique(data$id))
  } else if (testset == "tempsplit") {
    all_ids    = unique(data$id[!is.na(data$id) & data$inspection_yr >= self@split_year])
    n_change   = round(n * 0.5)
    n_rest     = n - n_change
    change_ids = obs |>
      dplyr::filter(inspection_yr >= self@split_year - 3, !is.na(rtg_d)) |>
      dplyr::group_by(id) |>
      dplyr::filter(dplyr::n_distinct(rtg_d) > 1) |>
      dplyr::pull(id) |>
      unique() |>
      intersect(all_ids)
    rest_ids   = setdiff(all_ids, change_ids)
    c(
      sample(change_ids, min(n_change, length(change_ids))),
      sample(rest_ids,   min(n_rest,   length(rest_ids)))
    )
  } else {
    all_ids = unique(data$id[!is.na(data$id) & data$inspection_yr >= self@split_year])
    sample(all_ids, min(n, length(all_ids)))
  }

  .pw_core(data, ids, pred_col, ribbon_lo, ribbon_hi, split_year = self@split_year)
})
