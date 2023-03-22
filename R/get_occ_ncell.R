# compute the number of grid cells in which occurrence would have reported
# based on the number of grids and the number of cases reported
# For the n grid cells and r cases, the average number of unique grid cells that
# would have reported the occurrence is n*(1 - ((n-1)/n)^r)
get_occ_ncell <- function(ncase, ncell){
  ncell*(1 - ((ncell-1)/ncell)^ncase)
}
