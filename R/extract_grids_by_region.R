# extract grids of the raster based on the
# defined region (subnational, country, or subregion [e.g., Eastern Africa])
extract_grids_by_region <-
  function(rst = NULL,
           shape = NULL,
           region = c("country","subnational","subregion")) {
  library(raster)
  if (is.null(shape)) {
    shape <- readRDS("data/africa_sub_Sahara_adm1_shp.rds")
  }
  afregions <- read.csv("data/africa_subregion_country_names.csv")

  names(rst) <- "value" # change the name for writing convenience

  areas <- unique(shape$NAME_0)
  if (region == "subnational") {
    areas <- unique(shape$NAME_1)
  }
  else if (region == "subregion") {
    areas <- unique(afregions$Subregion)
  }

  lst <- vector("list", length=length(areas))
  names(lst) <- areas

  for (i in 1:length(areas)) {
    ar = areas[i]
    if (region == "country"){
      poly <- shape[shape$NAME_0 == ar, ] # SpatialPolygon
    }
    else if (region == "subnational"){
      poly <- shape[shape$NAME_1 == ar, ] # SpatialPolygon
    }
    else { # Africa subregion
      # cntries <- afregions[Subregion == ar, Country]
      cntries <- afregions[afregions$Subregion == ar, ]$Country
      poly <- shape[shape$NAME_0 %in% cntries, ] # SpatialPolygon
    }
    lst[[i]] <- raster::extract(rst, poly, df=TRUE, cellnumbers=TRUE)
  }

  # change the column names for easier interpretation
  return(lst)
}
