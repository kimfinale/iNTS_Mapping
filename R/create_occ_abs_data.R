# go through data year by year such that we match the covariates
# by year and location
create_occ_abs_data = function(occdata, seed=42, years=2000:2020, refraster=NULL){
  library(dplyr)
# absence selected based on time to healthcare facilities
  if (is.null(refraster)){
    refraster = raster("data/covars/motorized_travel_time_to_healthcare_20km_af_20230314.tif")
  }
  refraster[] = rev_minmax(refraster[])

  occabs_datlist = list()
  set.seed(seed)

  for (i in seq_along(years)) {
    yr = years[i]
    covbrick = raster::brick(paste0("data/covars/covars_brick_", yr, "_20230315.grd"))
    cat("year =", yr, "\n")
    if(yr == 2000) {
      occdata %>% dplyr::filter(year <= yr) %>% dplyr::select(long,lat) -> occ_lonlat
    } else {
      occdata %>% dplyr::filter(year == yr) %>% dplyr::select(long,lat) -> occ_lonlat
    }
    if (nrow(occ_lonlat) > 0) {
      cells = raster::extract(covbrick, occ_lonlat, cellnumbers=T)
      uniquecells = unique(cells[,"cells"])
      matchids = match(uniquecells, cells[,"cells"])
      unique_occ_lonlat = occ_lonlat[matchids, ]
      covars_occ = covbrick[uniquecells]

      abs_lonlat = dismo::randomPoints(refraster,
                                       n=nrow(covars_occ)*2,
                                       p=occ_lonlat, prob=TRUE)

      abs_lonlat = as.data.frame(abs_lonlat)
      names(abs_lonlat) = names(occ_lonlat)

      cells_abs <- raster::extract(covbrick, abs_lonlat, cellnumbers=T)
      uniquecells_abs = unique(cells_abs[,"cells"])
      covars_abs <- covbrick[uniquecells_abs]
      matchids_abs = match(uniquecells_abs, cells_abs[,"cells"])
      unique_abs_lonlat = abs_lonlat[matchids_abs, ]
      # 1 and 0 representing presence and absence, respectively
      yesno <- data.frame(yesno = c(rep(1, nrow(covars_occ)), rep(0, nrow(covars_abs))))
      occ_longlat_cov = cbind(unique_occ_lonlat, covars_occ)
      abs_longlat_cov = cbind(unique_abs_lonlat, covars_abs)
      occ_abs_dat = cbind(yesno, rbind(occ_longlat_cov, abs_longlat_cov))

      occ_abs_dat = occ_abs_dat[complete.cases(occ_abs_dat),]

      occabs_datlist[[i]] = occ_abs_dat
    }
  }

  #clean names before rbind
  nms = gsub("_af_([0-9]+)", "", names(occabs_datlist[[1]]))
  for (i in 1:length(occabs_datlist)) {
    names(occabs_datlist[[i]]) = nms
  }

  data_brt = do.call('rbind', occabs_datlist)

  return(data_brt)
}
