# find the best boosted regression fit based on the cross-validation error
find_best_fit <- function(fits){
  cverrors <- lapply(fits, function(x) x$cv.values[length(x$cv.values)])
  which.min(unlist(cverrors))
}
