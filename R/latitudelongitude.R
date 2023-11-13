#' Retrieve Latitude and Longitude for a Given Location
#'
#' This function takes a location string, converts spaces to '+', and retrieves
#' latitude and longitude coordinates for that location using the Nominatim
#' API from OpenStreetMap.
#'
#' @param loca_tion A character string representing the location for which
#'   latitude and longitude are to be retrieved.
#' @return A list containing the latitude and longitude coordinates of the
#'   given location.
#' @examples
#' \dontrun{
#'   result <- lat_long("New York City")
#'   print(result)
#' }

#' @import httr
#' @import jsonlite
#' @export

lat_long <- function(loca_tion) {
  loca_tion <- gsub(" ", "+", loca_tion) # replacing 'space' with '+' to get the location
  url_lnk <- "https://nominatim.openstreetmap.org/search?q="
  get_coords <- paste0(url_lnk, loca_tion, "&format=geojson")
  
  response <- httr::GET(get_coords) %>%
    httr::content("text") %>%
    jsonlite::fromJSON()
  
  lat <- response$features$geometry$coordinates[[1]][2]
  lon <- response$features$geometry$coordinates[[1]][1]
  
  list("latitude" = lat, "longitude" = lon)
}
