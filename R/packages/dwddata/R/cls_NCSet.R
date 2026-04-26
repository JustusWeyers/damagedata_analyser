#' @include cls_RasterSet.R
#' @exportClass NCSet

methods::setClass(
  "NCSet",
  contains = "RasterSet",
  slots = list(
    filetype = "character"
  ),
  prototype = list(
    filetype = ".nc"
  )
)

methods::setMethod(
  "initialize",
  "NCSet",
  function(.Object,
           name = NA_character_,
           path = NA_character_,
           years = NA,
           filetype = ".nc",
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

setMethod("geo_stats", "NCSet", function(self, points) {

  # Thaw-frost-changes ^= number of sign changes
  thaw_frost_changes <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 2) return(NA_integer_)
    sum(diff(sign(x)) != 0)/2
  }

  custom_stats <- function(x) {
    x <- x[!is.na(x)]
    c(
      meanT = mean(x),
      maxT  = max(x),
      minT = min(x),
      diffT = max(x)-min(x),
      TFC   = thaw_frost_changes(x)
    )
  }

  pts = points |>
    terra::project(self@crs)

  nc_stats = do.call(c, lapply(self@files, function(fn) {
    r = terra::rast(fn)
    ext = terra::extract(r, pts)
    res = t(apply(ext[, -1], 1, custom_stats))
    dimnames(res) <- list(ID = seq_len(nrow(res)), Metric = colnames(res))
    l = setNames(list(res), format(terra::time(r)[1], "%Y"))
    return(l)
  }))

  return(nc_stats)

})
