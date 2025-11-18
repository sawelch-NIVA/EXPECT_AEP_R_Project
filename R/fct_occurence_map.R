#' Create map with literature data points
#'
#' Plots literature data points on the WGS84 study area map. Filters out
#' records with missing coordinates and converts to sf object.
#'
#' @param wgs84_map A ggplot object containing the base WGS84 map
#' @param literature_data A tibble containing literature data with LATITUDE
#'   and LONGITUDE columns
#' @param point_color Character string. Color for data points. Default is "red"
#'
#' @return A ggplot object with data points added to the base map
#'
#' @examples
#' \dontrun{
#' map_literature_data_wgs84(
#'   wgs84_map = wgs84_map,
#'   literature_data = literature_clean
#' )
#' }
#'
#' @export
map_literature_data_wgs84 <- function(
  wgs84_map,
  literature_data,
  point_color = "red"
) {
  # Convert to sf object, removing records with missing coordinates
  # TODO: Obviously this is arse and needs to be better
  data_sf <- literature_data |>
    dplyr::filter(!is.na(LATITUDE)) |>
    dplyr::filter(!is.na(LONGITUDE)) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("WGS84"))

  # Add points to map with study area bbox constraints
  wgs84_map +
    geom_sf(
      data = data_sf,
      fill = point_color
    ) +
    coord_sf(
      xlim = c(
        get_study_area_bbox()[[1]],
        get_study_area_bbox()[[3]]
      ),
      ylim = c(
        get_study_area_bbox()[[2]],
        get_study_area_bbox()[[4]]
      )
    )
}
