#' Plot a polygon using ggplot2
#'
#' The \code{polygon_plot()} is used to plot a raster image read by the raster
#' package
#' @param raster A vector of country names in character
#' @export
#' @examples
polygon_plot <- function(polygon = NULL,
                        title = "", xlab = "", ylab = "",
                        title_size = 16, legend_title = "",
                        legend_position = c(0.2, 0.3), 
                        fill_color = "gray95",
                        line_color = "white") {
  library(viridis)
  library(raster)
  library(plyr)
  if(!"id" %in% names(polygon)) {
    polygon$id <- 1:nrow(polygon)
  }
  # grid$fitted_pois <- mod3$summary.fitted.values$mean * grid$cellarea
  poly_pts <- broom::tidy(polygon, region = "id")
  poly_df <- plyr::join(poly_pts, polygon@data, by = "id")
  library(ggplot2)
  p <- ggplot(poly_df) +
    geom_polygon(aes(long, lat, group = group), 
                 fill = fill_color, color = line_color) +
    scale_color_discrete(legend_title) +
    labs(title = title, x = xlab, y = ylab) +
    theme(panel.background = element_blank(), # bg of the panel
          plot.background = element_blank(), # bg of the plot
          legend.background = element_blank(), # get rid of legend bg
          legend.box.background = element_blank(),
          panel.spacing = unit(c(0,0,0,0), "null"),
          plot.margin = unit(c(0,0,0,0), "null"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = legend_position,
          plot.title = element_text(hjust = 0.5, size = title_size))

  return (p)
}
