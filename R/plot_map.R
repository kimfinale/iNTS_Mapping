#############################################################################
## probocc_plot
## Holds default settings for plotting incidence rates
# IR_plot <- function(raster=NULL, shape=NULL, afss=NULL){
plot_map <- function(raster=NULL, shape=NULL, shape2=NULL,
                     color_ramp="YlOrBr", rev=FALSE, level=9,
                     title=NULL,
                     legend_title=NULL,
                     theme=theme_map(),
                     limits=c(0,1),
                     log10=FALSE){
  # source("util/ggplot2_theme.R")
  library(raster)
  library(ggplot2)
  library(RColorBrewer)
  library(scico)
  if (is.null(shape)){
    shape <- readRDS("data/africa_sub_Sahara_adm0_shp.rds")
  }
  if (is.null(shape2)){
    shape2 <- readRDS("data/africa_sub_Sahara_adm1_shp.rds")
  }
  mypal <- brewer.pal(level, color_ramp)
  if (rev) mypal <- rev(mypal)

  rpts <- as.data.frame(raster::rasterToPoints(raster))
  colnames(rpts) <- c("lon", "lat", "val")
  # identify a way to plot the incidence rate on a log scale
  p <- ggplot(rpts)

  if (log10) {
    p <- p +
      geom_raster(aes(lon, lat, fill=(val+1)))+
      scale_fill_gradientn(colors = mypal, legend_title,
                           limits=c(limits[1]+1,limits[2]+1),
                           trans='log10')
  }
  else {
    p <- p +
      geom_raster(aes(lon, lat, fill=val))+
        scale_fill_gradientn(colors = mypal, legend_title,
                             limits=c(limits[1],limits[2]))
  }
  p <- p + geom_polygon(data = shape, aes(long, lat, group = group),
                 fill = NA, inherit.aes = FALSE) +
    geom_path(data = shape, aes(long, lat, group = group),
              color = "black", linewidth = 0.8, inherit.aes = FALSE) +
    geom_polygon(data = shape2, aes(long, lat, group = group),
                 fill = NA, inherit.aes = FALSE) +
    geom_path(data = shape2, aes(long, lat, group = group),
              color = "black", linewidth = 0.4, inherit.aes = FALSE) +
    coord_equal() +
    theme +
    labs(title=title) +
    theme(legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          plot.title = element_text(size=22))

  return (p)
}
