#' @export

get_desc = function(bspid, mainclass = bspid_mainclass, df = bspid_classes) {

  x = as.character(as.numeric(strsplit(bspid, split = "-", fixed = TRUE)[[1]]))

  d1 = mainclass$description[mainclass$class1 == x[1]]

  if (length(x) == 2) {
    d2 = df$description[df$class1 == x[1] & df$class2 == x[2]]
    return(c(d1, d2))
  } else if (length(x) == 3) {
    d2 = paste(
      df$description[df$class1 == x[1] & is.na(df$class2) & df$class3 == x[3]]
    )
    return(c(d1, d2))
  } else {
    return("-")
  }
}
