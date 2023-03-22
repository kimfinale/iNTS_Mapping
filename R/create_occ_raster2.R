# create a raster of occurrence
create_occ_raster2 <- function(dat, raster, shapelist=NULL){
  library(raster)
  rstocc <- raster
  rstocc[] <- 0
  shpcnt = 1
  for(i in 1:nrow(dat)) {
    area <- dat$catchment_area[i]
    place <- dat$place_lowest_level[i]
    ncase <- as.numeric(dat$num_case[i])
    cat("i =", i, "\n")
    if(!is.na(area) & ncase > 0){
      lonlat <- data.frame(dat$longitude[i], dat$latitude[i])
      occ = pick_random_point(x=lonlat, n=ncase, d=area)
      cell = raster::extract(rstocc, occ, small=T, cellnumber=T)
      ids = as.data.frame(cell)$cells
      ids = ids[!is.na(ids)]
      # rstocc[ids] <- rstocc[ids] + 1
      for(id in ids){
        rstocc[id] <- rstocc[id] + 1
      }
     }

    if(is.na(area) & !is.na(place) & ncase > 0){
      # get shape file
      if (is.null(shapelist)){
        shp <- get_shape(str=place, country=dat$country[i], fuzzy=F)
      } else{
        shp <- shapelist[[shpcnt]]
      }
      shpcnt = shpcnt + 1
      cell <- raster::extract(rstocc, shp$shape, cellnumber=T, df=T)
      if(nrow(cell) > 2){ # if more than one cell exist
        ids <- unique(cell$cell)
        idsocc <- sample(ids, get_occ_ncell(ncase=ncase, ncell=length(ids)))
        rstocc[idsocc] <- rstocc[idsocc] + 1
      }
    }
  }
  return(rstocc)
}
