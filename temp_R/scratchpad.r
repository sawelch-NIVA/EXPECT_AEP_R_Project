# Stuff for testing and half-complete code we can't put in /R or it'll get sourced

# Load required libraries ----
library(STOPeData)
library(sf)
library(arrow)
library(tidyverse)
library(sfhelper)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapproj)
library(ggspatial)
library(shadowtext)
library(ggrepel)
library(rlang)
library(data.table)
library(dtplyr)
library(leaflet)
library(janitor)
library(shiny)
library(readxl)
library(purrr)
library(qs2)
library(units)
library(targets)
library(glue)
library(rlang)

`%notin%` <- negate(`%in%`)
devtools::load_all()

roxygen2::roxygenise()

# TODO: Run the pipeline with these two lines
source("_targets.R")
tar_make()

measurements_data <- tar_read("measurements_data")
sites_data <- tar_read("sites_data")
reference_data <- tar_read("reference_data")
campaign_data <- tar_read("campaign_data")
parameters_data <- tar_read("parameters_data")
methods_data <- tar_read("methods_data")

result <- measurements_data |>
  left_join(sites_data, by = "SITE_CODE") |>
  left_join(reference_data, by = "REFERENCE_ID") |>
  left_join(parameters_data, by = "PARAMETER_NAME")

#
test_refs <- fread_all_module_files(
  module = "Samples",
  format_initialiser = initialise_samples_tibble
)

# References data types bug
filepath = get_literature_csv_list(module = "Reference")[[1, 1]]
module_name = "Reference"


biiiig_table <- arrow::read_parquet("data/clean/literature_data.parquet")

ref_refs <- reference_data |> pull(REFERENCE_ID)
meas_refs <- measurements_data |> pull(REFERENCE_ID)

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

### Units ---

test_units <- biiiig_table |>
  select(MEASURED_UNIT) |>
  distinct()


# ok, we can't do mixed units in one table
test_units |>
  mutate(
    MEASURED_VALUE_UNIT = set_units(MEASURED_VALUE, !!unit_map[[MEASURED_UNIT]])
  )

# what do our units actually look like?
biiiig_table |> select(MEASURED_UNIT) |> distinct()

# and post-standardisation
test <- standardise_measured_units(biiiig_table) |>
  slice_sample(n = 10) |>
  select(
    SITE_CODE,
    MEASURED_VALUE,
    MEASURED_UNIT,
    MEASURED_VALUE_STANDARD,
    MEASURED_UNIT_STANDARD
  )

# from a sample of 10 it looks fine, although we have lost some precision in places

biiiig_table |>
  group_by(MEASURED_UNIT) |>
  reframe(MEASURED_UNIT, count = count())

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

dates <- biiiig_table |>
  group_by(SAMPLING_DATE) |>
  reframe(count = n()) |>
  arrange(desc(count))
dates

# I guess we don't have a lot of dates in here. Still, looks fine... to me.
biiiig_table |> standardise_IDate_all() |> select(contains("DATE"))
