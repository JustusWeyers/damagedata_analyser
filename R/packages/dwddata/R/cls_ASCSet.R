#' @include cls_RasterSet.R
#' @exportClass ASCSet

methods::setClass(
  "ASCSet",
  contains = "RasterSet",
  slots = list(
    filetype = "character"
  ),
  prototype = list(
    filetype = ".asc"
  )
)

methods::setMethod(
  "initialize",
  "ASCSet",
  function(.Object,
           name = NA_character_,
           path = NA_character_,
           years = NA,
           filetype = ".asc",
           ...) {

    if (is.na(path)) stop("No path provided for ASCSet")
    p = normalizePath(path, mustWork = FALSE)
    if (!dir.exists(p)) stop("Directory does not exist: ", p)

    files = normalizePath(list.files(p, full.names = TRUE))
    files = files[endsWith(files, filetype)]

    .Object@name <- name
    .Object@path <- p
    .Object@files <- files
    .Object@filetype <- filetype
    .Object@years <- years

    methods::callNextMethod(
      .Object, name = name, path = p,years = years, files = files, ...
    )
  }
)

#' @exportMethod geo_stats
setMethod("geo_stats", "ASCSet", function(self, points) {

  pts = points |>
    terra::project(self@crs)

  asc_stats = do.call(c, lapply(seq_along(self@files), function(i) {
    r = terra::rast(self@files[i])
    terra::crs(r) <- self@crs
    ext = as.matrix(terra::extract(r, pts)[,-1])
    dimnames(ext) <- list(ID = seq_len(nrow(ext)), Metric = self@name)
    l = setNames(list(ext), as.character(self@years[i]))
    return(l)
  }))

  return(asc_stats)

})
