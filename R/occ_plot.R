#############################################################################
## occ_plot
occ_plot <- function(raster=NULL, shape0=NULL, shape1=NULL,
                     color_ramp="YlOrBr", rev=FALSE, level=9,
                     title=NULL,
                     plot_yesno=TRUE,
                     shape0_lwd=0.9,
                     shape1_lwd=0.4,
                     ext=c(28,40,-24,-8),
                     add_zoom=FALSE,
                     theme=theme_map()){
  library(raster)
  library(ggplot2)
  library(RColorBrewer)

  if (is.null(shape0)){
    shape0 <- readRDS("data/africa_sub_Sahara_adm0_shp.rds")
  }
  if (is.null(shape1)){
    shape1 <- readRDS("data/africa_sub_Sahara_adm1_shp.rds")
  }
  mypal <- brewer.pal(level, color_ramp)
  if (rev) mypal <- rev(mypal)

  occrst_mask <- mask(raster, shape0) # without this, sea becomes grey..
  rptsdf <- as.data.frame(rasterToPoints(occrst_mask))
  colnames(rptsdf) <- c("lon","lat","num_occ")
  rptsdf$num_occ <- ifelse(rptsdf$num_occ == 0, NA, rptsdf$num_occ)
  rptsdf$occabs <- ifelse(rptsdf$num_occ == 0, "No", "Yes")
  rptsdf$occabs <- as.factor(rptsdf$occabs)

  e = extent(ext) # extent to zoom in QECH, Blantyre, Malawi
  rcrop <- crop(raster, e, snap="out")
  rcrop <- crop(rcrop, shape0, snap="out")
  rcrop <- mask(rcrop, shape0) # without this, sea becomes grey..
  rpts_crop <- as.data.frame(rasterToPoints(rcrop))
  colnames(rpts_crop) <- c("lon","lat","num_occ")


  if (!plot_yesno){
    plt <- ggplot(rptsdf) +
      geom_polygon(data=shape0, aes(long, lat, group = group),
                   fill="grey95", inherit.aes = FALSE)+
      geom_raster(aes(lon, lat, fill=num_occ), alpha=0.8) +
      scale_fill_gradientn(trans = "log10",
                           limits = c(1,1e4),
                           breaks = c(1,1e1,1e2,1e3,1e4),
                           colors = mypal,
                           "Number of\niNTS occurrence")+
      geom_path(data=shape0, aes(long, lat, group=group),
                color="black", linewidth=shape0_lwd, inherit.aes=FALSE) +
      geom_path(data=shape1, aes(long, lat, group=group),
                color="black", linewidth=shape1_lwd, inherit.aes=FALSE) +
      coord_equal() +
      theme_map() +
      theme(legend.title = element_text(size=12),
            legend.text = element_text(size=12))
  }
  else {
    plt <- ggplot(rptsdf) +
      geom_polygon(data=shape0, aes(long, lat, group = group),
                   fill=NA, inherit.aes=FALSE)+
      geom_raster(aes(lon, lat, fill=occabs), alpha=0.8) +
      scale_fill_manual(values=c("Yes"="brown","No"="white"),
                        labels=c("Yes"="Yes","No"="No"),
                        "iNTS occurrence")
    if (add_zoom) {
      plt = plt +
        geom_raster(data=rpts_crop, aes(lon, lat), fill="steelblue",
                    alpha=0.4, inherit.aes=FALSE)
    }
    plt = plt +
      geom_path(data=shape0, aes(long, lat, group=group),
                color="black", linewidth=shape0_lwd, inherit.aes=FALSE) +
      geom_path(data=shape1, aes(long, lat, group=group),
                color="black", linewidth=shape1_lwd, inherit.aes=FALSE) +
      coord_equal() +
      theme_map() +
      theme(legend.title = element_text(size=12))
  }

  return(plt)
}

