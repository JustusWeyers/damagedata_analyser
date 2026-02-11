.onLoad <- function(libname, pkgname) {
  if (!methods::isClass("ggplot")) methods::setOldClass(c("gg", "ggplot"))
  if (!methods::isClass("rand_forest")) methods::setOldClass("rand_forest")
  if (!methods::isClass("ranger")) methods::setOldClass("ranger")
}
