count_plt = function(pts, n, title) {
  old_mar = par("mar")
  par(mar = c(3, 4, 4, 2) + 0.1)
  plot(
    pts, xlab = "", ylab = "N [-]", axes = FALSE,
    pch = c(rep(20, n), rep(4, length(pts)-n))
  )
  text(names(pts)[1:n], x = 1:n, y = pts[1:n], adj = c(-0.3, 0.2), cex = 0.8)
  axis(2)
  box()
  title(title, adj = 0)
  mtext("BSP-ID", side = 1, line = 1.0)
  par(mar = old_mar)
}