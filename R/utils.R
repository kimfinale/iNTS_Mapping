tstamp <- function(year=TRUE, month=TRUE, day=TRUE,
                   hour=FALSE, minute=FALSE, second=FALSE) {
  stamp1 <- c()
  stamp2 <- c()
  if (year & !month & !day) {
    stamp <- format(Sys.time(), "%Y")
  } else if (year & month & !day) {
    stamp1 <- format(Sys.time(), "%Y%m")
  } else if (year & month & day) {
    stamp1 <- format(Sys.time(), "%Y%m%d")
  } else if (!year & month & day) {
    stamp1 <- format(Sys.time(), "%m%d")
  } else if (year & !month & day) {
    stamp1 <- format(Sys.time(), "%Y%d")
  } else if (!year & month & !day) {
    stamp1 <- format(Sys.time(), "%m")
  } else if (!year & !month & day) {
    stamp1 <- format(Sys.time(), "%d")
  } else{ stamp1 <- "You'd better select parameters well."}

  if (hour & !minute & !second) {
    stamp2 <- format(Sys.time(), "%H")
  } else if (hour & minute & !second) {
    stamp2 <- format(Sys.time(), "%H%M")
  } else if (hour & minute & second) {
    stamp2 <- format(Sys.time(), "%H%M%S")
  } else if (!hour & minute & !second) {
    stamp2 <- format(Sys.time(), "%M")
  } else if (!hour & !minute & second) {
    stamp2 <- format(Sys.time(), "%S")
  } else if (!hour & minute & second) {
    stamp2 <- format(Sys.time(), "%M%S")
  } else{}

  if (!is.null(stamp2)) {
    stamp1 <- paste0(stamp1, "T", stamp2)
  }
  return (stamp1)
}


#' Clean and standardize country names according to VIMC report templates
#'
#' The \code{clean_country_names()} is used to clean and standardize country names according to VIMC report templates
#' population for both sexes and incidence rate
#' @param country A vector of country names in character
#' @export
#' @examples
#' country <- clean_country_names(country = "DRC")
#' # Congo, the Democratic Republic of the
clean_country_names <- function(country){
  for (i in 1:length(country)) {
    if (country[i] %in% c("DR Congo", "Democratic Republic of the Congo", "DRC", "Congo, Dem. Rep.", "Congo, DR",  "Congo, the Democratic Republic of the")){
      country[i] <- "Congo, Democratic Republic of the"
    }
    if (country[i] %in% c("Congo, Rep.", "Republic of the Congo", "Congo", "Republic of Congo")){
      country[i] <- "Congo, Republic of the"
    }
    if (country[i] %in% c("São Tomé and Príncipe", "SÃ£o TomÃ© and PrÃ­ncipe")){
      country[i] <- "Sao Tome e Principe"
    }
    if (country[i] %in% c("Iran", "Iran, Islamic Rep.", "Iran (Islamic Republic of)")){
      country[i] <- "Iran, Islamic Republic of"
    }
    if (country[i] %in% c("Dem. People's Republic of Korea","North Korea", "Korea:North", "Korea, DPR", "DPRK", "Democratic People's Republic of Korea", "Korea DPR")){
      country[i] <- "Korea, Democratic People's Republic of"
    }
    if (country[i] %in% c("South Korea", "Korea:South", "Korea, Rep.")){
      country[i] <- "Korea, Republic of"
    }
    if (country[i] %in% c("Sudan: South")){
      country[i] <- "South Sudan"
    }
    if (country[i] %in% c("Sudan: North")){
      country[i] <- "Sudan"
    }
    if (country[i] %in% c("Venezuela", "Venezuela, RB", "Venezuela (Bolivarian Republic of)")){
      country[i] <- "Venezuela, Bolivarian Republic of"
    }
    if (country[i] %in% c("Tanzania", "United Republic of Tanzania")){
      country[i] <- "Tanzania, United Republic of"
    }
    if (country[i] %in% c("Syria")){
      country[i] <- "Syrian Arab Republic"
    }
    if (country[i] %in% c("Moldova")){
      country[i] <- "Moldova, Republic of"
    }
    if (country[i] %in% c("CAR")){
      country[i] <- "Central African Republic"
    }
    if (country[i] %in% c("Lao", "Laos", "Lao PDR")){
      country[i] <- "Lao People's Democratic Republic"
    }
    if (country[i] %in% c("US", "USA", "United States")){
      country[i] <- "United States of America"
    }
    if (country[i] %in% c("C?te d'Ivoire", "CÃ´te d'Ivoire", "Côte d'Ivoire",
                          "Ivory Coast", "Côte D'Ivoir")){
      country[i] <- "Cote d'Ivoire"
    }
    if (country[i] %in% c("Bolivia")){
      country[i] <- "Bolivia, Plurinational State of"
    }
    if (country[i] %in% c("Cape Verde")){
      country[i] <- "Cabo Verde"
    }
    if (country[i] %in% c("Micronesia")){
      country[i] <- "Micronesia, Federated States of"
    }
    if (country[i] %in% c("Sao Tome e Principe")){
      country[i] <- "Sao Tome and Principe"
    }
    if (country[i] %in% c("Vietnam")){
      country[i] <- "Viet Nam"
    }
    if (country[i] %in% c("Libya")){
      country[i] <- "Libyan Arab Jamahiriya"
    }
    if (country[i] %in% c("Saint Vincent & the Grenadines", "St. Vincent and the Grenadines")){
      country[i] <- "Saint Vincent and the Grenadines"
    }
    if (country[i] %in% c("Taiwan")) {
      country[i] <- "Taiwan, Province of China"
    }
    if (country[i] %in% c("Trinidad & Tobago")) {
      country[i] <- "Trinidad and Tobago"
    }
    if (country[i] %in% c("R?union", "Réunion")) {
      country[i] <- "Reunion"
    }
    if (country[i] %in% c("Russia")) {
      country[i] <- "Russian Federation"
    }
    if (country[i] %in% c("Eswatini")) {
      country[i] <- "Swaziland"
    }
  }
  return (country)
}

country_abbr <- function(x){
  x[x == "Central African Republic"] <- "CAR"
  x[x == "Congo, Republic of the"] <- "Congo"
  x[x == "Congo, Democratic Republic of the"] <- "DR Congo"
  x[x == "Tanzania, United Republic of"] <- "Tanzania"

  return(x)
}

# pick a random point that is r meters away from the reference point x
pick_random_point <- function(x=NULL, n=1, r=NULL, d=NULL, theta=NULL){
  # km_per_degree = 110 #
  # if(is.null(theta)){
  #   theta <- runif(1, 0, 2*pi) # random direction
  # }
  # if(is.null(x)){
  #   stop("reference point x must be provided")
  # }
  # if(is.null(r)){
  #   stop("distance r must be provided")
  # }
  # return(x + r * c(cos(theta), sin(theta))/km_per_degree)

  if (is.null(d) & is.null(r)){
    stop("Both d and r can be null")
  }
  else if (is.null(d) & !is.null(r)){
    dsample = rexp(n, rate=r) # distance is randomly selected based on the exponential decay
  }
  else if (!is.null(d) & is.null(r)){
    dsample = rexp(n, rate=1/d) # distance (km) is randomly selected based on the exponential decay
  }
  if (is.null(theta)){
    theta = runif(n, 0, 360) # bearing in degrees
  }
  geosphere::destPoint(p=x, b=theta, d=dsample*1000) # works in terms of meters
}

format_mean_95CI <- function(x, digits) {
  pred <- format(round(x[[1]], digits=digits), big.mark=",", trim=TRUE)
  lb <- format(round(x[[2]], digits=digits), big.mark=",", trim=TRUE)
  ub <- format(round(x[[3]], digits=digits), big.mark=",", trim=TRUE)

  paste0(pred, " (95% CI: ", lb , " to ", ub, ")")
}

rev_minmax_norm = function(x){
  mx = max(x, na.rm=T)
  mn = min(x, na.rm=T)
  (mx-x)/(mx-mn)
}
rev_minmax = function(x){
  mx = max(x, na.rm=T)
  mn = min(x, na.rm=T)
  (mx-x+mn)
}
