#' Plot a raster image using ggplot2
#'
#' The \code{raster_plot()} is used to plot a raster image read by the raster
#' package
#' @param raster A vector of country names in character
#' @export
#' @examples

raster_plot <- function(raster = NULL, title = "", xlab = "", ylab = "",
                        legend_title = "val", title_size = 16,
                        legend_position = c(0.25, 0.3)) {
  library(viridis)
  library(raster)
  library(tidyverse)
  # legend_title <- "Probability \nof occurrence"
  rst_points <- raster::rasterToPoints(raster)
  rst_points_df <- tbl_df(rst_points)
  colnames(rst_points_df) <- c("lon", "lat", "val")
  mybreaks <- c(10, 100, 1000, 10000)
  p <- ggplot(data = rst_points_df, aes(x = lon, y = lat, fill = (val))) +
    geom_raster() +
    # scale_fill_viridis(legend_title, trans = "log",
    #                    breaks = mybreaks, labels = mybreaks) +
    scale_fill_viridis(legend_title) +
    labs(title = title, x = xlab, y = ylab) +
    expand_limits(x = rst_points_df$lon, y = rst_points_df$lat) +
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
