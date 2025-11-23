# Stuff for testing and half-complete code we can't put in /R or it'll get sourced

# Load required libraries ----
load_all_requirements <- function() {
  sapply(
    X = c(
      "STOPeData",
      "sf",
      "arrow",
      "tidyverse",
      "sfhelper",
      "rnaturalearth",
      "rnaturalearthdata",
      "mapproj",
      "ggspatial",
      "shadowtext",
      "ggrepel",
      "rlang",
      "data.table",
      "dtplyr",
      "leaflet",
      "janitor",
      "shiny",
      "readxl",
      "purrr",
      "qs2",
      "units",
      "targets",
      "glue",
      "ggraph",
      "tidygraph",
      "esquisse"
    ),
    FUN = library,
    character.only = TRUE
  )

  `%notin%` <- purrr:::negate(`%in%`)
  devtools::load_all()

  requirements_loaded <<- TRUE
}

load_all_requirements()

roxygen2::roxygenise()

# # Run the pipeline with these two lines
{
  source("_targets.R")
  tar_make()
}

# get constitutent tables
biota_data <- tar_read(name = biota_data) |>
  dplyr::filter(
    SAMPLE_ID ==
      "GreenlandSeals1985_GS1985-Copper-BiotaAquatic-1985-01-01-Rhooded kidney"
  )

measurements_data <- tar_read(name = measurements_data)
sites_data <- tar_read(name = sites_data)

# get the parquet data for testing
main_table <- arrow::read_parquet("data/clean/literature_data.parquet")


# References data types bug

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

# Messy Dates

dates <- main_table |>
  group_by(SAMPLING_DATE) |>
  reframe(count = n()) |>
  arrange(desc(count))
dates

# I guess we don't have a lot of dates in here. Still, looks fine... to me.
main_table |> standardise_IDate_all() |> select(contains("DATE"))
