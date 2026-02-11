#' @export

bspid_to_color = function(bspid) {
  parts = strsplit(bspid, "-", fixed = TRUE)[[1]]
  parts_num = suppressWarnings(as.numeric(parts))
  if (length(parts_num) < 3) parts_num = c(parts_num, rep(1, 3 - length(parts_num)))
  main = parts_num[1]
  sub1 = parts_num[2]
  sub2 = parts_num[3]
  h = ((main * 137.508) %% 360) / 360
  s = 0.4 + 0.6 * ((sub1 - 1) / max(1, 9))
  v = 0.5 + 0.5 * ((sub2 - 1) / max(1, 9))
  hex = grDevices::hsv(h = h, s = min(s**2, 1), v = v)

  return(hex)
}
