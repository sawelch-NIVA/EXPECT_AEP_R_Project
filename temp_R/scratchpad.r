# Load required libraries ----
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
library(STOPeData)
library(qs2)

`%notin%` <- negate(`%in%`)

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

# something fucked here
# fixme
#
test_refs <- fread_all_module_files(
  module = "Reference",
  format_initialiser = initialise_references_tibble
)

# References data types bug
filepath = get_literature_csv_tibble(module = "Reference")[[1, 1]]
module_name = "Reference"


biiiig_table <- arrow::read_parquet("data/clean/literature_data.parquet")

ref_refs <- reference_data |> pull(REFERENCE_ID)
meas_refs <- measurements_data |> pull(REFERENCE_ID)

## Test Maps ----
# Prepare all data for WGS84 map
wgs84_geo <- prepare_geography_wgs84()

wgs84_map <- create_study_area_map_wgs84(
  bbox = northern_hemisphere_bbox(),
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
