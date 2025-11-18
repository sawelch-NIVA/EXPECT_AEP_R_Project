data <- load_literature_parquet()

data_sf <- data |>
  dplyr::filter(!is.na(LATITUDE)) |>
  dplyr::filter(!is.na(LONGITUDE)) |> # we shouldn't have anything without these values... but we do. oops.
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs("WGS84"))

wgs84_map +
  geom_sf(
    data = data_sf,
    color = "red"
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
