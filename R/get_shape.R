get_shape <- function(str, country, fuzzy=FALSE){
  library(countrycode)
  iso3c <- countryname(country, destination='iso3c')
  fls <- list.files(path = "data/country_shapefile/",
             pattern = paste0("^gadm41_", iso3c, ".*[1-4].shp$"), full.names = T)

  shp_return <- NULL
  n <- nfile <- length(fls)
  # search the list backwards (smaller admin levels first)
  shp <- raster::shapefile(fls[n])
  str <- strsplit(str, ",")[[1]][1] # use the smallest area (that comes first)
  if(fuzzy){
    match <- agrepl(str, eval(parse(text=paste0("shp$NAME_", n))))
    while(sum(match) == 0 & n > 0){
      n <- n - 1
      match <- agrepl(str, eval(parse(text=paste0("shp$NAME_", n))))
    }
  }
  else{
    match <- grepl(str, eval(parse(text=paste0("shp$NAME_", n))))
    while(sum(match) == 0 & n > 1){
      n <- n - 1
      match <- grepl(str, eval(parse(text=paste0("shp$NAME_", n))))
    }
  }
  if (sum(match) >= 1) {
    shp_return <- shp[match, ]
  }
  return(list(shape=shp_return, name=str))
}




get_shape_old <- function(str, shapefile, fuzzy=FALSE){
  shp <- NULL
  if(fuzzy){
    match <- agrepl(str, shapefile$NAME_2)
    if(sum(match) == 0){
      match <- agrepl(str, shapefile$NAME_1)
    }
  }
  else{
    match <- grepl(str, shapefile$NAME_2)
    if(sum(match) == 0){
      match <- grepl(str, shapefile$NAME_1)
    }
  }
  if (sum(match) >= 1) {
    shp <- shapefile[match, ]
  }
  return(shp)
}


