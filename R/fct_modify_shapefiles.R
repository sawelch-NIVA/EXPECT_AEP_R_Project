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
        NAME == "Norwegian Sea" ~ ocean_colors[1],
        NAME == "Greenland Sea" ~ ocean_colors[2],
        NAME == "Barents Sea" ~ ocean_colors[3],
        NAME == "Arctic Ocean" ~ ocean_colors[4],
        TRUE ~ ocean_colors[5]
      )
    )
}

# Ocean polygon processing ----

#' Process marine geography polygons
#'
#' Reads Natural Earth marine geography data, splits the Atlantic Ocean
#' into North and South, and adds annotation columns for display
#'
#' @param scale Numeric, Natural Earth scale (10, 50, or 110)
#' @param destdir Character string, destination directory for cached files
#' @param highlight_oceans Character vector, names of oceans to highlight
#' @param major_oceans Character vector, names of major ocean bodies
#'
#' @return An sf object with processed marine geography polygons in WGS84
#'
#' @importFrom dplyr select rename group_by reframe filter mutate bind_rows case_when n
#' @importFrom sf st_as_sf
#' @importFrom stringr str_replace_all
#'
#' @export
process_marine_geography_wgs84 <- function(
  scale = 10,
  destdir = "data/raw/shapefiles/",
  highlight_oceans = c(
    "Norwegian Sea",
    "North Atlantic Ocean",
    "Greenland Sea",
    "Barents Sea",
    "Arctic Ocean",
    "Vestfjorden",
    "Storfjorden",
    "Denmark Strait"
  ),
  major_oceans = c(
    "Norwegian Sea",
    "North Atlantic Ocean",
    "Greenland Sea",
    "Barents Sea",
    "Arctic Ocean",
    "Kara Sea",
    "North Sea",
    "Davis Strait",
    "Baffin Bay",
    "Lincoln Sea"
  )
) {
  # Read marine geography data
  marine_polys <- ne_get(
    scale = scale,
    type = "geography_marine_polys",
    category = "physical",
    destdir = destdir
  ) |>
    select(name_en, geometry) |>
    rename(name = name_en)

  # Split Atlantic Ocean into North and South
  atlantic <- marine_polys |>
    group_by(name) |>
    reframe(n = row_number(), geometry) |>
    filter(name == "Atlantic Ocean", n == 1) |>
    st_as_sf()

  atlantic_split <- st_line_split(
    atlantic,
    c(0, 180),
    direction = "horizontal"
  ) |>
    mutate(name = c("North Atlantic Ocean", "South Atlantic Ocean")) |>
    select(-n)

  # Replace original Atlantic with split versions
  marine_polys <- marine_polys |>
    filter(name != "Atlantic Ocean") |>
    bind_rows(atlantic_split)

  # Add annotation columns, split any two-word seas onto two lines
  marine_polys_annotated <- marine_polys |>
    mutate(
      highlight_name = name %in% highlight_oceans,
      major_body = name %in% major_oceans,
      name = str_replace_all(name, "(Sea|Ocean|Gulf|Bay|Strait)", "\n\\1")
    ) |>
    st_as_sf() |>
    group_by(name) |>
    mutate(
      count = n(),
      ocean_color = case_when(
        highlight_name ~ marine_colors["highlight"],
        TRUE ~ marine_colors["default"]
      )
    )

  return(marine_polys_annotated)
}

#' Transform marine geography to polar projection
#'
#' Transforms marine geography polygons to North Polar Stereographic projection
#' and stitches up the Arctic Ocean polygon across the polar seam
#'
#' @param marine_polys_wgs84 An sf object with marine geography in WGS84
#' @param crs Character string or CRS object, target projection (default: EPSG:3575)
#' @param add_line_breaks Logical, whether to add line breaks in ocean names (default: TRUE)
#'
#' @return An sf object with marine geography in polar projection
#'
#' @importFrom sf st_transform st_convex_hull
#' @importFrom dplyr filter mutate bind_rows
#' @importFrom stringr str_replace_all

#'
#' @export
transform_marine_to_polar <- function(
  marine_polys_wgs84,
  crs = "EPSG:3575",
  add_line_breaks = TRUE
) {
  # Transform to polar projection
  marine_polys_polar <- marine_polys_wgs84 |>
    st_transform(crs)

  # Fix Arctic Ocean polar seam issue
  arctic <- marine_polys_polar |>
    filter(name == "Arctic Ocean") |>
    mutate(geometry = st_convex_hull(geometry))

  marine_polys_polar <- marine_polys_polar |>
    filter(name != "Arctic Ocean") |>
    bind_rows(arctic)

  # Add line breaks to ocean names if requested
  if (add_line_breaks) {
    marine_polys_polar <- marine_polys_polar |>
      mutate(
        name = str_replace_all(name, "(Sea|Ocean|Gulf|Bay|Strait)", "\n\\1")
      )
  }

  return(marine_polys_polar)
}

# Country polygon processing ----

#' Process country polygons with special handling for Norway
#'
#' Reads Natural Earth country data and processes Norway separately to
#' distinguish continental Norway, Jan Mayen, and Svalbard
#'
#' @param scale Numeric, Natural Earth scale (10, 50, or 110)
#' @param highlight_countries Character vector, names of countries to highlight
#' @param exclude_countries Character vector, countries to exclude from general dataset
#'
#' @return An sf object with processed country polygons in WGS84
#'
#' @importFrom rnaturalearth ne_countries ne_states
#' @importFrom dplyr mutate select filter bind_rows case_match reframe row_number
#' @importFrom sf st_as_sf st_crop st_union

#'
#' @export
process_countries_wgs84 <- function(
  scale = 10,
  highlight_countries = c("Norway", "Greenland", "Iceland"),
  exclude_countries = c("Norway", "Antarctica")
) {
  # Get general country polygons (excluding special cases)
  countries_general <- rnaturalearth::ne_countries(scale = scale) |>
    mutate(
      highlight_name = case_match(
        name,
        highlight_countries ~ TRUE,
        .default = FALSE
      )
    ) |>
    select(
      name,
      geometry,
      highlight_name
    ) |>
    filter(name %notin% exclude_countries)

  # Process Norway components separately
  norway_mainland <- ne_states(geounit = "norway") |>
    select(name, geometry) |>
    st_crop(c(xmin = 0, xmax = 40, ymin = 57, ymax = 90)) |>
    reframe(name = "Norway", geometry = st_union(geometry))

  norway_janmayen <- ne_states(geounit = "norway") |>
    filter(name == "Nordland") |>
    st_crop(c(xmin = -10, xmax = 5, ymin = 70, ymax = 90)) |>
    select(name, geometry) |>
    mutate(name = "Jan \nMayen")

  norway_svalbard <- ne_states(geounit = "svalbard") |>
    select(name, geometry) |>
    mutate(name = "Svalbard & \nBear Island")

  # Combine Norway components
  norway_combined <- bind_rows(
    norway_mainland,
    norway_janmayen,
    norway_svalbard
  ) |>
    mutate(highlight_name = TRUE) |>
    st_as_sf()

  # Combine all countries
  countries_all <- bind_rows(
    norway_combined,
    countries_general
  ) |>
    st_as_sf()

  return(countries_all)
}

#' Transform country polygons to polar projection
#'
#' Transforms country polygons to North Polar Stereographic projection
#'
#' @param countries_wgs84 An sf object with country polygons in WGS84
#' @param crs Character string or CRS object, target projection (default: EPSG:3575)
#'
#' @return An sf object with country polygons in polar projection
#'
#' @importFrom sf st_transform

#'
#' @export
transform_countries_to_polar <- function(
  countries_wgs84,
  crs = "EPSG:3575"
) {
  countries_wgs84 |>
    st_transform(crs)
}

# Geometry utilities ----

#' Split sf geometry along a line
#'
#' Splits an sf geometry object along a horizontal or vertical line by
#' creating bounding boxes and cropping
#'
#' @param geometry An sf geometry object to split
#' @param line Numeric vector of length 2, coordinates of the split line
#' @param direction Character string, either "horizontal" or "vertical"
#'
#' @return An sf object with the split geometry
#'
#' @importFrom sf st_bbox st_crs st_crop
#'
#' @export
st_line_split <- function(
  geometry,
  line = c(0, 180),
  direction = "horizontal"
) {
  # Get the bounding box of the input geometry
  bbox <- st_bbox(geometry)
  geometry_crs <- st_crs(geometry)

  if (direction == "horizontal") {
    # Split along horizontal line (e.g., latitude)
    lat_split <- line[1]
    # Create bounding boxes for north and south parts
    bbox_north <- st_bbox(
      c(
        bbox$xmin,
        ymin = lat_split,
        bbox$xmax,
        bbox$ymax
      ),
      crs = geometry_crs
    )
    bbox_south <- st_bbox(
      c(
        bbox$xmin,
        bbox$ymin,
        bbox$xmax,
        ymax = lat_split
      ),
      crs = geometry_crs
    )
  } else if (direction == "vertical") {
    # Split along vertical line (e.g., longitude)
    lon_split <- line[1]
    # Create bounding boxes for east and west parts
    bbox_east <- st_bbox(
      c(
        xmin = lon_split,
        bbox$ymin,
        bbox$xmax,
        bbox$ymax
      ),
      crs = geometry_crs
    )
    bbox_west <- st_bbox(
      c(
        bbox$xmin,
        bbox$ymin,
        xmax = lon_split,
        bbox$ymax
      ),
      crs = geometry_crs
    )
    bbox_north <- bbox_east
    bbox_south <- bbox_west
  }

  # Convert to bbox objects and crop
  part1 <- tryCatch(
    st_crop(geometry, bbox_north),
    error = function(e) NULL
  )
  part2 <- tryCatch(
    st_crop(geometry, bbox_south),
    error = function(e) NULL
  )

  # Combine results
  parts <- list()
  if (!is.null(part1) && nrow(part1) > 0) {
    parts <- append(parts, list(part1))
  }
  if (!is.null(part2) && nrow(part2) > 0) {
    parts <- append(parts, list(part2))
  }

  if (length(parts) > 0) {
    return(do.call(rbind, parts))
  } else {
    return(geometry) # Return original if splitting failed
  }
}

# Graticule creation ----

#' Create graticule lines for map
#'
#' Generates latitude and longitude grid lines for map display
#'
#' @param lon_seq Numeric vector, longitude values for vertical lines
#' @param lat_seq Numeric vector, latitude values for horizontal lines
#' @param crs CRS object or string, coordinate reference system
#'
#' @return An sf object with graticule lines
#'
#' @importFrom sf st_graticule st_crs
#'
#' @export
create_graticule <- function(
  lon_seq = seq(-180, 180, 30),
  lat_seq = seq(60, 90, 10),
  crs = 4326
) {
  st_graticule(
    lon = lon_seq,
    lat = lat_seq,
    crs = st_crs(crs)
  )
}

# Complete workflow functions ----

#' Prepare all geography data for WGS84 map
#'
#' Convenience function that processes all geography data needed for
#' a WGS84-projected map
#'
#' @param scale Numeric, Natural Earth scale (10, 50, or 110)
#' @param destdir Character string, destination directory for cached files
#'
#' @return A named list with marine_polys, countries, arctic_circle, and graticule
#'
#' @export
prepare_geography_wgs84 <- function(
  scale = 10,
  destdir = "data/raw/shapefiles/"
) {
  list(
    marine_polys = process_marine_geography_wgs84(
      scale = scale,
      destdir = destdir
    ),
    countries = process_countries_wgs84(scale = scale),
    arctic_circle = create_arctic_circle(),
    graticule = create_graticule()
  )
}

#' Prepare all geography data for polar projection map
#'
#' Convenience function that processes all geography data needed for
#' a polar-projected map
#'
#' @param scale Numeric, Natural Earth scale (10, 50, or 110)
#' @param destdir Character string, destination directory for cached files
#' @param crs Character string or CRS object, target projection (default: EPSG:3575)
#'
#' @return A named list with marine_polys, countries, arctic_circle, and graticule
#'
#' @export
prepare_geography_polar <- function(
  scale = 10,
  destdir = "data/raw/shapefiles/",
  crs = "EPSG:3575"
) {
  # First prepare WGS84 versions
  wgs84_data <- prepare_geography_wgs84(scale = scale, destdir = destdir)

  # Transform to polar projection
  list(
    marine_polys = transform_marine_to_polar(
      wgs84_data$marine_polys,
      crs = crs
    ),
    countries = transform_countries_to_polar(wgs84_data$countries, crs = crs),
    arctic_circle = wgs84_data$arctic_circle,
    graticule = wgs84_data$graticule
  )
}
