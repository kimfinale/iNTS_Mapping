
##############################################################################
# tidy raster images
# adjust the resolution, crop and mask based on the reference shapefile or
# raster
tidy_raster <- function(rst, target_res_km = NULL,
                        func=c("mean","sum"),
                        extent=NULL, ref=NULL){


  deg_20km <- 0.1666667 # approx. 20 km in degrees
  if (is.null(target_res_km)) {
    if (!is.null(ref)) {
      target_res_deg <- round(res(ref)[1], digits=7)
    }
  } else{
    target_res_deg <- round(target_res_km * deg_20km / 20, digits=7)
  }
  rst_res_deg <- round(res(rst)[1], digits=7)

  if (!is.null(target_res_km)) { # aggregate or disaggregate
    if (target_res_deg > rst_res_deg) {
      rst <-
        raster::aggregate(rst, fact = round(target_res_deg / rst_res_deg),
                          fun = eval(parse(text=func)))
    }
    else if(target_res_deg < rst_res_deg){
      rst <-
        raster::disaggregate(rst, fact = round(rst_res_deg / target_res_deg))
    }
  }
  if(!is.null(extent)){
    rst <- raster::crop(rst, extent(extent), snap="out")
    rst <- raster::mask(rst, mask = extent)
  }
  if (!is.null(ref)) {
    rst <- raster::resample(rst, ref)
  }
  return(rst)
}

