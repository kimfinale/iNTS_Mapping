##########################################################################
## get_covariates
## get covariate based on location
get_covariates <- function(data,
                           covariates,
                           lon_name = "lon",
                           lat_name = "lat",
                           year_begin_name = "STUDY_DATE_BEGIN",
                           year_end_name = "STUDY_DATE_END"){


  distw <- covariates[["distance_to_water_20km_af_2010_20220720"]]
  elev <- covariates[["elevation_20km_africa"]]
  travel <- covariates[["travel_time_cities_20km_af_2017_20220802"]]

  ## create a dataframe that will hold covariates and locations
  occ <- data.frame(X = as.numeric(data[, lon_name, drop=T]),
                    Y = as.numeric(data[, lat_name, drop=T]),
                    grid = as.integer(data[, c("grid"), drop=T]))
  nr <- nrow(occ)

  occ$annual_mean_temp <- rep(NA, nr)
  occ$annual_rainfall <- rep(NA, nr)
  occ$distance_to_water <- rep(NA, nr)
  occ$elevation <- rep(NA, nr)
  occ$improved_sanitation <- rep(NA, nr)
  occ$improved_water <- rep(NA, nr)
  occ$prev_HIV <- rep(NA, nr)
  occ$prev_stunting <- rep(NA, nr)
  occ$travel_time_to_cities <- rep(NA, nr)

  occ$open_defecation <- rep(NA, nr)
  occ$piped_sanitation <- rep(NA, nr)
  occ$piped_water <- rep(NA, nr)
  occ$ppp <- rep(NA, nr)
  occ$surface_water <- rep(NA, nr)
  occ$underweight <- rep(NA, nr)
  occ$wasting <- rep(NA, nr)

  for(i in 1:nr) {
    pt <- occ[i, c("X", "Y")]
    grid <- occ[i, c("grid")]
    # for some reason, raster::extract does not recognize these as numeric

    occ$travel_time_to_cities[i] <- get_raster_value(travel, pt, grid)
    occ$elevation[i] <- get_raster_value(elev, pt, grid)
    occ$distance_to_water[i] <- get_raster_value(distw, pt, grid)

    hiv_list <- list()
    stunt_list <- list()
    temp_list <- list()
    rain_list <- list()
    sanitation_list <- list()
    water_list <- list()

    open_defecation_list <- list()
    piped_sanitation_list <- list()
    piped_water_list <- list()
    ppp_list <- list()
    surface_water_list <- list()
    underweight_list <- list()
    wasting_list <- list()


    cnt <- 1

    varnames <- names(data)

    if (year_begin_name %in% varnames && year_end_name %in% varnames) {
      ya <- data[i, year_begin_name, drop=T]
      yb <- data[i, year_end_name, drop=T]
      yayb <- tidy_year(ya, yb)
      ya <- yayb[1]
      yb <- yayb[2]

      yrs <- seq(ya, yb)
      for (yr in yrs){
        hiv <- covariates[[paste0("prev_HIV_adults_20km_af_", yr, "_20220802")]]
        stunt <- covariates[[paste0("prev_stunting_20km_af_", yr, "_20220802")]]
        temp <- covariates[[paste0("annual_mean_temperature_20km_af_", yr, "_20220802")]]
        rain <- covariates[[paste0("annual_rainfall_20km_af_", yr, "_20220802")]]
        sanitation <- covariates[[paste0("improved_sanitation_20km_af_", yr, "_20220802")]]
        water <- covariates[[paste0("improved_water_20km_af_", yr, "_20220802")]]

        open_defecation <- covariates[[paste0("open_defecation_20km_af_", yr, "_20220802")]]
        piped_sanitation <- covariates[[paste0("piped_sanitation_20km_af_", yr, "_20220802")]]
        piped_water <- covariates[[paste0("piped_water_20km_af_", yr, "_20220802")]]
        ppp <- covariates[[paste0("ppp_20km_af_", yr, "_20220929")]]
        surface_water <- covariates[[paste0("surface_water_20km_af_", yr, "_20220802")]]
        underweight <- covariates[[paste0("underweight_20km_af_", yr, "_20220802")]]
        wasting <- covariates[[paste0("wasting_20km_af_", yr, "_20220802")]]

        hiv_list[[ cnt ]] <- get_raster_value(hiv, pt, grid)
        stunt_list[[ cnt ]] <- get_raster_value(stunt, pt, grid)
        temp_list[[ cnt ]] <- get_raster_value(temp, pt, grid)
        rain_list[[ cnt ]] <- get_raster_value(rain, pt, grid)
        sanitation_list[[ cnt ]] <- get_raster_value(sanitation, pt, grid)
        water_list[[ cnt ]] <- get_raster_value(water, pt, grid)

        open_defecation_list[[ cnt ]] <- get_raster_value(open_defecation, pt, grid)
        piped_sanitation_list[[ cnt ]] <- get_raster_value(piped_sanitation, pt, grid)
        piped_water_list[[ cnt ]] <- get_raster_value(piped_water, pt, grid)
        ppp_list[[ cnt ]] <- get_raster_value(ppp, pt, grid)
        surface_water_list[[ cnt ]] <- get_raster_value(surface_water, pt, grid)
        underweight_list[[ cnt ]] <- get_raster_value(underweight, pt, grid)
        wasting_list[[ cnt ]] <- get_raster_value(wasting, pt, grid)


        cnt <- cnt + 1
      }
      occ$prev_HIV[i] <- mean(unlist(hiv_list), na.rm = TRUE)
      occ$prev_stunting[i] <- mean(unlist(stunt_list), na.rm = TRUE)
      occ$annual_mean_temp[i] <- mean(unlist(temp_list), na.rm = TRUE)
      occ$annual_rainfall[i] <- mean(unlist(rain_list), na.rm = TRUE)
      occ$improved_sanitation[i] <- mean(unlist(sanitation_list), na.rm = TRUE)
      occ$improved_water[i] <- mean(unlist(water_list), na.rm = TRUE)

      occ$open_defecation[i] <- mean(unlist(open_defecation_list), na.rm = TRUE)
      occ$piped_sanitation[i] <- mean(unlist(piped_sanitation_list), na.rm = TRUE)
      occ$piped_water[i] <- mean(unlist(piped_water_list), na.rm = TRUE)
      occ$ppp[i] <- mean(unlist(ppp_list), na.rm = TRUE)
      occ$surface_water[i] <- mean(unlist(surface_water_list), na.rm = TRUE)
      occ$underweight[i] <- mean(unlist(underweight_list), na.rm = TRUE)
      occ$wasting[i] <- mean(unlist(wasting_list), na.rm = TRUE)

    }
    else {
      # when there is no information on the year, we take the mid point
      yr <- 2010
      hiv <- covariates[[paste0("prev_HIV_adults_20km_af_", yr, "_20220802")]]
      stunt <- covariates[[paste0("prev_stunting_20km_af_", yr, "_20220802")]]
      temp <- covariates[[paste0("annual_mean_temperature_20km_af_", yr, "_20220802")]]
      rain <- covariates[[paste0("annual_rainfall_20km_af_", yr, "_20220802")]]
      sanitation <- covariates[[paste0("improved_sanitation_20km_af_", yr, "_20220802")]]
      water <- covariates[[paste0("improved_water_20km_af_", yr, "_20220802")]]

      open_defecation <- covariates[[paste0("open_defecation_20km_af_", yr, "_20220802")]]
      piped_sanitation <- covariates[[paste0("piped_sanitation_20km_af_", yr, "_20220802")]]
      piped_water <- covariates[[paste0("piped_water_20km_af_", yr, "_20220802")]]
      ppp <- covariates[[paste0("ppp_20km_af_", yr, "_20220929")]]
      surface_water <- covariates[[paste0("surface_water_20km_af_", yr, "_20220802")]]
      underweight <- covariates[[paste0("underweight_20km_af_", yr, "_20220802")]]
      wasting <- covariates[[paste0("wasting_20km_af_", yr, "_20220802")]]


      occ$prev_HIV[i] <- raster::extract(hiv, pt, method = "simple")
      occ$prev_stunting[i] <- raster::extract(stunt, pt, method = "simple")
      occ$annual_mean_temp[i] <- raster::extract(temp, pt, method = "simple")
      occ$annual_rainfall[i] <- raster::extract(rain, pt, method = "simple")
      occ$improved_sanitation[i] <- raster::extract(sanitation, pt, method = "simple")
      occ$improved_water[i] <- raster::extract(water, pt, method = "simple")

      occ$open_defecation[i] <- raster::extract(open_defecation, pt, method = "simple")
      occ$piped_sanitation[i] <- raster::extract(piped_sanitation, pt, method = "simple")
      occ$piped_water[i] <- raster::extract(piped_water, pt, method = "simple")
      occ$ppp[i] <- raster::extract(ppp, pt, method = "simple")
      occ$surface_water[i] <- raster::extract(surface_water, pt, method = "simple")
      occ$underweight[i] <- raster::extract(underweight, pt, method = "simple")
      occ$wasting[i] <- raster::extract(wasting, pt, method = "simple")
    }
  }

  # leave only covariates
  occ <- subset(occ, select = -c(X, Y, grid))
  return(occ)
}
