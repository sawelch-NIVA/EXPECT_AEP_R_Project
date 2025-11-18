# Shapefile reading and processing ----

#' Read and process ocean shapefiles
#'
#' Reads raw ocean shapefile, crops to northern, and simplifies geometry.
#' Caches processed shapefile to avoid reprocessing.
#'
#' @param raw_path Character string, path to raw ocean shapefile
#' @param output_path Character string, path to save processed shapefile
#' @param bbox sf bbox object, bounding box to crop to (default: study area bbox)
#' @param force_reprocess Logical, whether to force reprocessing even if cached file exists
#'
#' @return An sf object with processed ocean polygons
#'
#' @importFrom sf read_sf st_crop st_simplify write_sf sf_use_s2

#'
#' @export
read_and_process_oceans <- function(
  raw_path = "data/raw/shapefiles/World_Seas_IHO_v3.shp",
  output_path = "data/clean/study_area_shapefile.shp",
  bbox = northern_hemisphere_bbox(),
  force_reprocess = FALSE
) {
  # Disable spherical geometry for cropping
  sf::sf_use_s2(FALSE)

  # Check if the raw source file exists
  stopifnot(
    "The named file doesn't exist at the path. World_Seas_IHO_v3.shp is ~150 MB, so it's not in the github repo. 
    Get it here: https://www.marineregions.org/downloads.php" = file.exists(
      raw_path
    )
  )

  # Check if processed file exists
  if (file.exists(output_path) && !force_reprocess) {
    message(sprintf("Reading cached shapefile from: %s", output_path))
    return(sf::read_sf(output_path))
  }

  # Process from raw data
  message(sprintf("Processing ocean shapefile from: %s", raw_path))

  oceans_raw <- sf::read_sf(raw_path)

  study_area <- oceans_raw |>
    sf::st_crop(bbox) |>
    sf::st_simplify()

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save processed shapefile
  sf::write_sf(study_area, output_path)
  message(sprintf("Processed shapefile saved to: %s", output_path))

  return(study_area)
}

#' Read and process country shapefiles
#'
#' Reads country boundaries from Natural Earth, crops to study area,
#' and flags countries within the study region
#'
#' @param scale Numeric, Natural Earth scale (10, 50, or 110)
#' @param bbox sf bbox object, bounding box to crop to (default: study area bbox)
#' @param study_countries Character vector, names of countries in study area
#'
#' @return An sf object with country polygons and study area flag
#'
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr mutate select case_match
#' @importFrom sf st_as_sf st_crop
#'
#' @export
read_and_process_countries <- function(
  scale = 10,
  bbox = northern_hemisphere_bbox(),
  study_countries = c("Norway", "Greenland", "Iceland")
) {
  message("Reading country boundaries from Natural Earth")

  countries <- rnaturalearth::ne_countries(scale = scale) |>
    mutate(
      in_study = case_match(
        name,
        study_countries ~ TRUE,
        .default = FALSE
      )
    ) |>
    select(
      sovereignt,
      in_study,
      admin,
      geounit,
      subunit,
      name,
      name_long,
      abbrev,
      postal,
      note_brk,
      fips_10,
      iso_a2,
      iso_a3,
      woe_note,
      geometry
    ) |>
    st_as_sf() |>
    sf::st_crop(bbox)

  return(countries)
}

#' Get Natural Earth data with caching
#'
#' Downloads or loads cached Natural Earth data. If the shapefile exists locally,
#' loads it; otherwise downloads and caches it.
#'
#' @param scale Numeric, map scale (10, 50, or 110)
#' @param type Character string, feature type (e.g., "countries", "ocean")
#' @param category Character string, feature category (e.g., "cultural", "physical")
#' @param destdir Character string, destination directory for cached files
#'
#' @return An sf object with Natural Earth data
#'
#' @importFrom rnaturalearth ne_load ne_download
#'
#' @export
ne_get <- function(scale, type, category, destdir = "data/raw/shapefiles") {
  filepath <- file.path(destdir, paste0("ne_", scale, "m_", type, ".shp"))

  if (file.exists(filepath)) {
    rnaturalearth::ne_load(scale, type, category, destdir)
  } else {
    rnaturalearth::ne_download(scale, type, category, destdir)
  }
}
