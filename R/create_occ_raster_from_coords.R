# create a raster of occurrence where the value of each cell represents
# the number of occurrence
create_occ_raster_from_coords <- function(longlat, raster){
  library(raster)
  raster[] <- 0 # value will record the number of hits (occurrences)
  longlat <- longlat[,c("long","lat")]
  grids = raster::extract(raster, longlat, small=T, cellnumber=T)
  ids = as.data.frame(grids)$cells
  ids = ids[!is.na(ids)]
  for(id in ids){
    raster[id] <- raster[id] + 1
  }
  return(raster)
}
