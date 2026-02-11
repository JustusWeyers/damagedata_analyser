#' @export

dependency_report = function() {

  browser_opt = getOption("browser")
  options(browser = "false")
  report = pkgnet::CreatePackageReport("damageRF")
  options(browser = browser_opt)

  # Nodes
  nodes = report$DependencyReporter$nodes

  edges = report$DependencyReporter$edges

  # Graph
  g = igraph::graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

  # Tree
  xy = igraph::layout_as_tree(g)

  # Some jitter

  rescale = function(x, new_min=0, new_max=1) {
    (x - min(x)) / (max(x) - min(x)) * (new_max - new_min) + new_min
  }

  xy[,1] = xy[,1] + rnorm(nrow(xy)) * 0.1
  xy[,2] = xy[,2] + rnorm(nrow(xy)) * 0.2

  xy[,1] = rescale(xy[,1], 0, 9)
  xy[,2] = rescale(xy[,2], 0, 7)

  # Colorcoding
  type_colors = c(
    "report_package" = "cadetblue",
    "regular_dependency" = "cadetblue2",
    "base_dependency" = "grey",
    "suggested" = "orange"
  )

  # Fetch color vector
  vertex_colors = unname(type_colors[nodes$package_type])
  # Make NAs transparent
  vertex_colors[is.na(vertex_colors)] = "transparent"

  # Paste names and version
  pkgnames = sapply(igraph::V(g)$name, function(n) {
    paste0(n, "\n", utils::packageVersion(n))
  })

  return(list(
    report = report,
    tree = list(
      graph = g,
      coords = xy,
      labels = unname(pkgnames),
      colors = vertex_colors)
  ))

}

