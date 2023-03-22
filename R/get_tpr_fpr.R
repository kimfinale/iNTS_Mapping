get_tpr_fpr = function(dat, pred, yr=2000) {

  # dat = readRDS(paste0("outputs/data_brt_20230316T184332.rds"))
  dat = dat
  occ_longlat = dat[dat$yesno == 1, c("long","lat")]
  occ_longlat = dat[dat$yesno == 0, c("long","lat")]
  pres <- raster::extract(pred, occ_longlat)
  abs <- raster::extract(pred, abs_lonlat)
  ev <- evaluate(p=pres, a=abs)
# plot(e, 'ROC')
# e2 <- evaluate(p=occ_longlat,
#                a=abs_lonlat,
#                model=fit_brt,
#                x=covariates)
# plot(e2, "ROC")
  tpr <- ev@confusion[,"tp"]/(ev@confusion[,"tp"] + ev@confusion[,"fn"])
  fpr <- ev@confusion[,"fp"]/(ev@confusion[,"fp"] + ev@confusion[,"tn"])
  return(data.frame(tpr = tpr, fpr = fpr))

}

