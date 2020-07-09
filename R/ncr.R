# National Chargepoint Registry API library
# Using http://chargepoints.dft.gov.uk/api/help

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("
 ____  _____   ______  _______
|_   \\|_   _|.' ___  ||_   __ \\
  |   \\ | | / .'   \\_|  | |__) |
  | |\\ \\| | | |         |  __ /
 _| |_\\   |_\\ `.___.'\\ _| |  \\ \\_
|_____|\\____|`.____ .'|____| |___|
Access to the National Chargepoint Registry API
 Run vignette(\"intro-to-chargepointAPI\") to learn more")
}

#' Fetches the number of charge points  within a search radius
#'
#' @param lat latitude in decimal degrees
#' @param long longitude in decimal degrees
#' @param radius search radius
#' @param units unit of radius (default is miles, the alternative is "km")
#'
#' @examples
#' ncr_count(51.545581, -0.077301)
#' ncr_count(51.545581, -0.077301, radius = 2.5, units = "km")

ncr_count <- function(lat, long, radius = 1, units = "mi"){
  url <- "http://chargepoints.dft.gov.uk/api/retrieve/registry/format/json"
  url <- paste(url, "lat", lat, "long", long, "dist", radius, "units", units,
               sep = "/")
  # out <- jsonlite::stream_in(url(url))
  out <- jsonlite::fromJSON(url)$ChargeDevice

  nrow(out)#[out$ChargeDeviceStatus == "In service",])
}

#' Fetches the number of charge points  within a search radius
#'
#' @param lat latitude in decimal degrees
#' @param long longitude in decimal degrees
#'
#' @examples
#' ncr_nearest(51.545581, -0.077301)

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
     centroid <- st_as_sf(data.frame(lat = lat, long = long),
                          coords = c("long", "lat"))
     geo <- sf::st_as_sf(out$ChargeDeviceLocation,
                         coords = c("Longitude", "Latitude"))
     nearest <- subset(out,
                       st_distance(centroid, geo) ==
                         min(st_distance(centroid, geo))
                       )
     nearest <- jsonlite::flatten(nearest)[, c("ChargeDeviceId", "ChargeDeviceName",
                             "ChargeDeviceLocation")]
    } else {
      radius <- radius + 1
    }
  }
}


lat <- 51.545581
long <- -0.077301

x <- ncr_count(lat,long, radius = 0.5, units = "km")
y <- ncr_nearest(51.545581, -0.077301)
