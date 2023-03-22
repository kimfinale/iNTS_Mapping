# create a raster of occurrence
create_occ_raster <- function(dat, raster){
  library(raster)
  rstocc <- raster
  rstocc[] <- 0
  for(i in 1:nrow(dat)) {
    longlat <- cbind(dat$long[i], dat$lat[i])
    cell = raster::extract(rstocc, longlat, small=T, cellnumber=T)
    ids = as.data.frame(cell)$cells
    ids = ids[!is.na(ids)]
    for(id in ids){
      rstocc[id] <- rstocc[id] + 1
    }
  }

  return(rstocc)
}
