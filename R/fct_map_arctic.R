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
  sf::st_bbox(c(xmin = -70, xmax = 90, ymin = 55, ymax = 90))
}

# Shapefile reading and processing ----

#' Read and process ocean shapefiles
#'
#' Reads raw ocean shapefile, crops to study area, and simplifies geometry.
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
#' @importFrom magrittr |>
#'
#' @export
read_and_process_oceans <- function(
  raw_path = "data/raw/shapefiles/World_Seas_IHO_v3.shp",
  output_path = "data/clean/study_area_shapefile.shp",
  bbox = get_study_area_bbox(),
  force_reprocess = FALSE
) {
  # Disable spherical geometry for cropping
  sf::sf_use_s2(FALSE)

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
#' @importFrom magrittr |>
#'
#' @export
read_and_process_countries <- function(
  scale = 10,
  bbox = get_study_area_bbox(),
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
#' @importFrom magrittr |>
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

# Data annotation and styling ----

#' Annotate ocean polygons for mapping
#'
#' Adds display names, visibility flags, and color mappings to ocean polygons
#'
#' @param ocean_sf An sf object with ocean polygons (must have NAME column)
#' @param named_oceans Character vector, names of oceans to highlight
#'
#' @return An sf object with annotation columns added
#'
#' @importFrom dplyr mutate case_match case_when
#' @importFrom magrittr |>
#'
#' @export
annotate_oceans_for_display <- function(
  ocean_sf,
  named_oceans = c(
    "Norwegian Sea",
    "Greenland Sea",
    "Barents Sea",
    "Arctic Ocean"
  )
) {
  ocean_sf |>
    mutate(
      # Standardize names
      NAME = case_match(NAME, "Barentsz Sea" ~ "Barents Sea", .default = NAME),

      # Flag which names to show
      show_name = NAME %in% named_oceans,

      # Assign colors to named oceans
      ocean_color = case_when(
        NAME == "Norwegian Sea" ~ "#4A90E2", # Medium blue
        NAME == "Greenland Sea" ~ "#7BB3F0", # Light blue
        NAME == "Barents Sea" ~ "#2E5C8A", # Dark blue
        NAME == "Arctic Ocean" ~ "#5BA3D0", # Blue-cyan
        TRUE ~ "#E8E8E8" # Light gray for others
      )
    )
}

# Map creation ----

#' Create Arctic study area base map
#'
#' Generates a ggplot map showing the Arctic study area with oceans,
#' countries, and the Arctic Circle
#'
#' @param ocean_sf An sf object with annotated ocean polygons
#' @param country_sf An sf object with country polygons
#' @param arctic_circle_sf An sf object with Arctic Circle line
#' @param background_color Character string, hex color for background (default: "#f8f9fa")
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot geom_sf geom_sf_text aes scale_fill_identity guides theme element_rect
#' @importFrom magrittr |>
#'
#' @export
create_study_area_map <- function(
  ocean_sf,
  country_sf,
  arctic_circle_sf,
  background_color = "#f8f9fa"
) {
  ggplot() +
    # Ocean polygons with custom colors
    geom_sf(
      data = ocean_sf,
      aes(fill = ocean_color),
      color = "white",
      size = 0.2
    ) +
    # Ocean labels
    geom_sf_text(
      data = ocean_sf,
      mapping = aes(label = NAME, color = show_name),
      size = 3.5,
      fontface = "bold"
    ) +
    scale_fill_identity() +
    guides(fill = "none") +
    # Country polygons
    geom_sf(
      data = country_sf,
      mapping = aes(fill = ifelse(in_study, "lightgreen", "grey"))
    ) +
    # Country abbreviations
    geom_sf_text(
      data = country_sf,
      aes(label = abbrev)
    ) +
    # Arctic Circle line
    geom_sf(
      data = arctic_circle_sf,
      color = "#333333",
      linetype = "dashed",
      size = 0.8
    ) +
    theme(
      panel.background = element_rect(fill = background_color, color = NA),
      plot.background = element_rect(fill = background_color, color = NA),
      legend.position = "none"
    )
}

# Usage example (not exported) ----
#
# # Load and process geographic data
# study_area <- read_and_process_oceans()
# study_countries <- read_and_process_countries()
# arctic_circle <- create_arctic_circle()
#
# # Annotate for display
# study_area_annotated <- annotate_oceans_for_display(study_area)
#
# # Create map
# study_map <- create_study_area_map(
#   ocean_sf = study_area_annotated,
#   country_sf = study_countries,
#   arctic_circle_sf = arctic_circle
# )
