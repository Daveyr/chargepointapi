# National Chargepoint Registry API library
# Using http://chargepoints.dft.gov.uk/api/help

#' Fetches the number of charge points  within a search radius
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param radius search radius
#' @param units unit of radius (default is miles)

ncr_count <- function(lat, long, radius = 1, units = "mi"){
  url <- "http://chargepoints.dft.gov.uk/api/retrieve/registry/format/json"
  url <- paste(url, "lat", lat, "long", long, "dist", radius, "units", units,
               sep = "/")
  # out <- jsonlite::stream_in(url(url))
  out <- flatten(jsonlite::fromJSON(url))

  nrow(out$ChargeDevice)#[out$ChargeDevice$ChargeDeviceStatus == "In service",])
}

#' Fetches the number of charge points  within a search radius
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#' @param radius search radius
#' @param units unit of radius (default is miles)

ncr_nearest <- function(lat, long){
  conditionunmet <- T
  radius <- 1
  url <- "http://chargepoints.dft.gov.uk/api/retrieve/registry/format/json"
  while(conditionunmet){
    fullurl <- paste(url, "lat", lat, "long", long, "dist", radius,
                 sep = "/")
    out <- jsonlite::fromJSON(fullurl)$ChargeDevice
    if(nrow(out) > 0){
     conditionunmet <- F
     geo <- st_as_sf(out$ChargeDeviceLocation, coords = c("Longitude", "Latitude"))
    } else {
      radius <- radius + 1
    }
  }
}


lat <- 51.545581
long <- -0.077301

x <- ncr_count(lat,long, radius = 0.5, units = "km")

xj <- jsonlite::stream_in(url(x))
xjf <- jsonlite::fromJSON(url(x))
