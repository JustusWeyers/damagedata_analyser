transition_dynamicity = function(df) {
  mat = as.matrix(df)
  total = sum(mat)
  diagonal = sum(diag(mat))
  off_diag = total - diagonal
  off_diag / total
}