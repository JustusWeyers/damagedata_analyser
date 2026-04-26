print_mat = function(transition_counts_freq, bspid) {
  red_mat = transition_counts_freq[[bspid]]
  colnames(red_mat) = gsub(paste0(bspid, "-"), "", colnames(red_mat))
  colnames(red_mat) = sub("-$", "", colnames(red_mat))
  rownames(red_mat) = sub("-$", "", rownames(red_mat))
  
  cat(paste0("<h5>BSP-ID ", bspid, "</h5>"), "\n")
  cat("<pre style='font-size:10px'>")
  print(red_mat)
  cat("</pre>\n")
  cat(paste0("<p>Anzahl Übergänge N = ", sum(transition_counts_freq[[bspid]]), "</p>\n"))
}
