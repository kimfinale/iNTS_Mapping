# computes the mean or sum by aggregating the raster image based on the
# defined region (subnational, country, or subregion [e.g., Eastern Africa])
aggregate_by_region <- function(rst = NULL,
                               shape = NULL,
                               region = c("country","subnational","subregion"),
                               func = c("mean","sum"),
                               afregions=NULL) {
  library(raster)
  # library(data.table)
  # if (is.null(ppp)) {
  #   ppp <- readRDS("data/covariates/prediction/ppp_20km_af_2017_20221208.rds")
  # }
  if (is.null(shape)) {
    shape <- readRDS("data/africa_sub_Sahara_adm1_shp.rds")
  }
  if (is.null(afregions)) {
    afregions <- read.csv("data/africa_subregion_country_names.csv")
  }
  # names(ppp) <- "value" # change the name for writing convenience
  names(rst) <- "value" # change the name for writing convenience
  # results will be returned in raster format
  # Create a vector with the same length.
  # This is for presumed efficiency but not tested
  rstvec <- rep(NA, length(rst[])) # initialize the vector with NA

  areas <- unique(shape$NAME_0)
  if (region == "subnational") {
    areas <- unique(shape$NAME_1)
  }
  else if (region == "subregion") {
    areas <- unique(afregions$Subregion)
  }

  df <- data.frame(matrix(NA, nrow=length(areas), ncol=2))
  names(df) <- c("area","value")
  df$area <- areas

  for (ar in areas) {
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
    rst_poly <- raster::extract(rst, poly, df=TRUE, cellnumbers=TRUE)
    # summary statistics
    if (func == "mean") {
      value_area <- mean(rst_poly$value, na.rm=T)
    } else if(func == "sum") {
      value_area <- sum(rst_poly$value, na.rm=T)
    }

    rstvec[unlist(rst_poly$cell)] <- value_area

    df[df$area == ar,]$value <- value_area
  }
  rst[] <- rstvec
  # change the column names for easier interpretation
  return(list(raster=rst, data=df))
}
