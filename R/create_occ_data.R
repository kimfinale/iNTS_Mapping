# create a sample occurrence data set
# this has random component (eg, determining a location of occurrence given
# occurrence in a certain hospital)
create_occ_data <- function(dat, dist1=20, dist2=20, dist3=100,
                            shapelist=NULL, fuzzy=F){
  shpcnt = 1
  totcase = sum(dat$num_case)
  out = data.frame(matrix(NA, nrow=totcase, ncol=3))
  names(out) = c("year","long","lat")
  rowid = 1
  for (i in 1:nrow(dat)) {
    year = dat$year[i]
    area = dat$catchment_area[i]
    place = dat$place_lowest_level[i]
    ncase = as.integer(dat$num_case[i])
    htype = dat$hospital_type[i]
    out[rowid:(rowid+ncase-1),"year"] = year
    cat("i =", i, "\n")

    if (!is.na(area) & ncase > 0){
      avgdist = dist3
      if (htype == "Primary or secondary") avgdist = dist1

      longlat = cbind(dat$longitude[i], dat$latitude[i])
      occ = pick_random_point(x=longlat, n=ncase, d=avgdist)
      out[rowid:(rowid+ncase-1),c("long","lat")] = occ
      rowid = rowid + ncase
    }

    if (is.na(area) & !is.na(place) & ncase > 0) {
      if (is.null(shapelist)){  # get shape file
        shp = get_shape(str=place, country=dat$country[i], fuzzy=fuzzy)
      } else{
        shp = shapelist[[shpcnt]]
      }
      shpcnt = shpcnt + 1
      polygon = sf::st_as_sf(shp$shape)
      points = sf::st_sample(polygon, size=ncase)
      coords = sf::st_coordinates(points)
      out[rowid:(rowid+ncase-1), c("long","lat")] =
        coords[sample.int(nrow(coords),ncase,replace=T), c("X","Y")]
      rowid = rowid + ncase
    }
  }
  return(out)
}
