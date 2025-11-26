# Geographic constants ----

#' Get study area bounding box
#'
#' Returns the bounding box coordinates for the Arctic study area
#'
#' @return An sf bbox object with study area extent
#'
#' @importFrom sf st_bbox
#'
#' @export
get_study_area_bbox <- function() {
  sf::st_bbox(c(xmin = -100, xmax = 90, ymin = 30, ymax = 90))
}

#' Get Northern Hemisphere bounding box
#'
#' Returns the bounding box coordinates for the entire Northern Hemisphere
#' plus one degree south of the equator
#'
#' @return An sf bbox object with Northern Hemisphere extent
#'
#' @importFrom sf st_bbox
#'
#' @export
northern_hemisphere_bbox <- function() {
  sf::st_bbox(c(xmin = -180, xmax = 180, ymin = 0, ymax = 90))
}


# Geographic feature creation ----

#' Create Arctic Circle line
#'
#' Generates an sf linestring representing the Arctic Circle at 66.5°N
#'
#' @param lon_range Numeric vector of length 2, longitude range (default: c(-70, 90))
#' @param lat Numeric, latitude of Arctic Circle (default: 66.5)
#' @param resolution Numeric, spacing between points along line (default: 1 degree)
#'
#' @return An sf LINESTRING object
#'
#' @importFrom sf st_as_sf st_cast
#' @importFrom dplyr summarise

#'
#' @export
create_arctic_circle <- function(
  lon_range = c(-70, 90),
  lat = 66.5,
  resolution = 1
) {
  # Generate points along Arctic Circle
  arctic_coords <- data.frame(
    lon = seq(lon_range[1], lon_range[2], by = resolution),
    lat = lat
  )

  arctic_circle <- arctic_coords |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    summarise(do_union = FALSE) |>
    st_cast("LINESTRING")

  return(arctic_circle)
}
