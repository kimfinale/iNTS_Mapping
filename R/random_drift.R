#' Clean and standardize country names according to VIMC report templates
#'
#' The \code{random_drift()} is used to make a random drift of the locations
#' of the occurrence points
#' This is designed to mitigate the fact that many occurrence locations are
#' the location of healthcare facilities
#' population for both sexes and incidence rate
#' @param xy A data.frame of location with long and Y (latitude)
#' @export
#' @examples
#' ## random_drift()
#' at a given point, select a point satisfying the distance between
#' the two points are exponentially distributed with the mean_dist
#' dist in terms of a degree, of which one degree is ~111 km on the equator

random_drift <- function(xy, mean_dist = 0.1) {
  n <- nrow(xy)
  r <- rexp(n, 1 / mean_dist) # sample a radius
  t <- 2 * pi * runif(n) # distribute a random sample from a circle with radius r
  newx <- xy$long + r * cos(t)
  newy <- xy$lat + r * sin(t)
  df <- data.frame(long = newx, lat = newy)
  return (df)
}
