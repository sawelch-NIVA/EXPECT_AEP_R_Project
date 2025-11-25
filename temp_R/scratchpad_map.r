if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

## Test Maps ----
# Prepare all data for WGS84 map
wgs84_geo <- prepare_geography_wgs84()

wgs84_map <- create_study_area_map_wgs84(
  ocean_sf = wgs84_geo$marine_polys,
  country_sf = wgs84_geo$countries,
  arctic_circle_sf = wgs84_geo$arctic_circle,
  graticule_sf = wgs84_geo$graticule
)

wgs84_map

# Prepare all data for polar map
polar_geo <- prepare_geography_polar()

polar_map <- create_study_area_map_polar(
  ocean_sf = polar_geo$marine_polys,
  country_sf = polar_geo$countries,
  arctic_circle_sf = polar_geo$arctic_circle,
  graticule_sf = polar_geo$graticule
)

# map with data points

data <- load_literature_parquet()

data_sf <- data |>
  dplyr::filter(!is.na(LATITUDE)) |>
  dplyr::filter(!is.na(LONGITUDE)) |> # we shouldn't have anything without these values... but we do. oops.
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("WGS84"))

wgs84_map +
  geom_sf(
    data = data_sf,
    fill = "red"
  ) + # I guess we need to constrain again?
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


library(leaflet)
library(sf)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = wgs84_geo$marine_polys |> filter(highlight_name),
    fillColor = ~ ifelse(
      highlight_name == TRUE,
      marine_colors[["default"]],
      marine_colors[["highlight"]]
    ),
    fillOpacity = ~ ifelse(
      highlight_name == TRUE,
      0.5,
      0
    ),
    color = "black",
    weight = 1,
    popup = ~name,
    group = "Oceans"
  ) %>%
  addPolygons(
    data = wgs84_geo$countries |> filter(highlight_name),
    fillColor = ~ ifelse(
      highlight_name == TRUE,
      country_colours[["default"]],
      country_colours[["highlight"]]
    ),
    fillOpacity = ~ ifelse(
      highlight_name == TRUE,
      0.5,
      0
    ),
    weight = 1,
    color = "black",
    popup = ~name,
    group = "Countries"
  ) %>%
  addCircleMarkers(
    data = data_sf,
    popup = ~REFERENCE_ID,
    group = "Samples",
    clusterOptions = markerClusterOptions()
  ) |>
  addLabelOnlyMarkers(data = data_sf, label = ~SITE_CODE) |>
  addLayersControl(
    overlayGroups = c("Oceans", "Countries", "Samples"),
    options = layersControlOptions(collapsed = FALSE)
  )
