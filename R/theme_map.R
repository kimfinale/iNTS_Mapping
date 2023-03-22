#' Clean and standardize country names according to VIMC report templates
#'
#' The \code{theme_map()} is used to make a random drift of the locations
#' of the occurrence points
#' This is designed to mitigate the fact that many occurrence locations are
#' the location of healthcare facilities
#' population for both sexes and incidence rate
#' @param xy A data.frame of location with long and Y (latitude)
#' @export
#' @examples
#' at a given point, select a point satisfying the distance between
#' the two points are exponentially distributed with the mean_dist
#' dist in terms of a degree, of which one degree is ~111 km on the equator

## custom theme for plotting a map
theme_map <- function(legend_position = c(0.2, 0.3)) {
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(), # bg of the panel
    legend.background = element_blank(), # get rid of legend bg
    legend.box.background = element_blank(),
    panel.spacing = unit( c(0,0,0,0), "null" ),
    plot.margin = unit( c(0,0,0,0), "null" ),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(legend_position[1],legend_position[2]))
}
