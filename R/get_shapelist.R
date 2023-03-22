# create a list of shapefiles used to generate occurrence data
# this is because identifying shapefiles is costly
get_shapelist = function(dat, fuzzy=FALSE){
  shapelist = list()
  shpcnt = 1
  for (i in 1:nrow(d)) {
    area <- dat$catchment_area[i]
    place <- dat$place_lowest_level[i]
    ncase <- as.numeric(dat$num_case[i])
    cat("i =", i, "\n")
    if(is.na(area) & !is.na(place) & ncase > 0){
      cat("shpcnt =", shpcnt, "\n")
      # get_shape is a costly function
      shp <- get_shape(str=place, country=dat$country[i], fuzzy=fuzzy)
      shapelist[[shpcnt]] = shp
      shpcnt = shpcnt + 1
      # cell <- raster::extract(rstocc, shp$shape, cellnumber=T, df=T)
    }
  }
  return(shapelist)
}
