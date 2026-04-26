#' @exportClass RasterSet

methods::setClass(
  "RasterSet",
  slots = list(
    name = "character",
    path = "character",
    files = "character",
    years = "numeric",
    crs = "character"
  ),
  prototype = list(
    name = NA_character_,
    path = NA_character_,
    files = character(),
    years = numeric(),
    crs = character()
  )
)

methods::setMethod(
  "initialize",
  "RasterSet",
  function(.Object,
           name = NA_character_,
           path = NA_character_,
           years = NA,
           ...) {

    if (is.na(path)) stop("No path provided for RasterSet")
    p = normalizePath(path, mustWork = FALSE)
    if (!dir.exists(p)) stop("Directory does not exist: ", p)

    files = list.files(p)

    .Object@name <- name
    .Object@path <- p
    .Object@files <- files
    .Object@years <- years

    methods::callNextMethod(.Object, ...)
  }
)

methods::setGeneric("geo_stats", function(self, points) {standardGeneric("geo_stats")})

