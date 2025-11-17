# Arctic Ocean Study Area Map ----

# Functions and Settings ----

# Disable s2 processing for simpler geometry operations
# sf::sf_use_s2(FALSE)

# Simple Natural Earth data getter ----
ne_get <- function(scale, type, category, destdir = "data/raw/shapefiles") {
  filepath <- file.path(destdir, paste0("ne_", scale, "m_", type, ".shp"))

  if (file.exists(filepath)) {
    rnaturalearth::ne_load(scale, type, category, destdir)
  } else {
    rnaturalearth::ne_download(scale, type, category, destdir)
  }
}

# Shadow sf text?
geom_sf_shadowtext <- function(
  mapping = aes(),
  data = NULL,
  stat = "sf_coordinates",
  position = "identity",
  ...,
  parse = FALSE,
  nudge_x = 0,
  nudge_y = 0,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun.geometry = NULL
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "Both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied.",
        i = "Only use one approach to alter the position."
      ))
    }
    position <- position_nudge(nudge_x, nudge_y)
  }
  layer_sf(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomShadowText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}

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


# Set up shapefiles ----
study_area_bbox <- st_bbox(
  c(xmin = -180, xmax = 180, ymin = 40, ymax = 90),
  crs = "EPSG:3575"
)

## Ocean shapefiles ----
ne_10m_geography_marine_polys <- ne_get(
  scale = 10,
  type = "geography_marine_polys",
  category = "physical",
  destdir = "data/raw/shapefiles/"
) |>
  select(name_en, geometry) |>
  rename(name = name_en)

### Bisect the Atlantic
atlantic <- ne_10m_geography_marine_polys |>
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

# re-add
ne_10m_geography_marine_polys <- ne_10m_geography_marine_polys |>
  filter(name != "Atlantic Ocean") |>
  rbind(atlantic_split)

## Country shapefiles ----
ne_10m_country_polys <- rnaturalearth::ne_countries(scale = 10) |>
  mutate(
    highlight_name = case_match(
      name,
      c("Norway", "Greenland", "Iceland") ~ TRUE,
      .default = FALSE
    )
  ) |>
  select(
    name,
    geometry,
    highlight_name
  ) |>
  # sf::st_crop(study_area_bbox) |>
  filter(name %notin% c("Norway", "Antarctica"))

# get continental Norway, Jan Mayen, and Svalbard separately
ne_10m_norway_poly <- ne_states(geounit = "norway") |>
  select(name, geometry) |>
  st_crop(c(xmin = 0, xmax = 40, ymin = 57, ymax = 90)) |> # crop to mainland
  reframe(name = "Norway", geometry = st_union(geometry))

ne_10m_janmayen_poly <- ne_states(geounit = "norway") |>
  filter(name == "Nordland") |>
  st_crop(c(xmin = 5, xmax = -10, ymin = 70, ymax = 90)) |> # crop to jan mayen
  select(name, geometry) |>
  mutate(name = "Jan \nMayen")

ne_10m_svalbard_poly <- ne_states(geounit = "svalbard") |>
  select(name, geometry) |>
  mutate(name = "Svalbard & \nBear Island")

ne_10m_norway_combined <- rbind(
  ne_10m_norway_poly,
  ne_10m_janmayen_poly,
  ne_10m_svalbard_poly
) |>
  mutate(highlight_name = TRUE) |>
  st_as_sf()

ne_10m_countries_combined_wgs84 <- rbind(
  ne_10m_norway_combined,
  ne_10m_country_polys
) |>
  # mutate(name = str_replace_all(name, "(Jan|Bear)\\s", "\\1\n")) |>
  st_as_sf()

ne_10m_countries_combined_npolar <- ne_10m_countries_combined_wgs84 |>
  st_transform(
    "EPSG:3575"
  )

## Ocean annotations and styling ----
ne_10m_geography_marine_polys_wgs84 <- ne_10m_geography_marine_polys |>
  mutate(
    # Determine which ocean names to show
    highlight_name = name %in%
      c(
        "Norwegian Sea",
        "North Atlantic Ocean",
        "Greenland Sea",
        "Barents Sea",
        "Arctic Ocean",
        "Vestfjorden",
        "Storfjorden",
        "Denmark Strait"
      ),
    major_body = name %in%
      c(
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
  ) |>
  st_as_sf() |>
  group_by(name) |>
  mutate(
    count = n(), # many oceans show up twice, perhaps because they're non-contiguous?
    # Create colour mapping for named oceans
    ocean_color = case_when(
      highlight_name ~ "#9dd9fe",
      TRUE ~ "#6e98b2" # Dark blue for others
    )
  )


### Ocean polar projection ----
ne_10m_geography_marine_polys_npolar <- ne_10m_geography_marine_polys_wgs84 |>
  st_transform(
    "EPSG:3575"
  )

# Stitch up the polar seam in the Arctic ocean then add it back to the sf
arctic <- ne_10m_geography_marine_polys_npolar %>%
  filter(name == "Arctic Ocean") %>%
  mutate(geometry = st_convex_hull(geometry)) # big wide North pole line in A. Ocean

ne_10m_geography_marine_polys_npolar <- ne_10m_geography_marine_polys_npolar %>%
  filter(name != "Arctic Ocean") %>%
  bind_rows(arctic) |>
  mutate(
    name = str_replace_all(name, "(Sea|Ocean|Gulf|Bay|Strait)", "\n\\1")
  )


# Create Arctic Circle reference line ----
arctic_circle_coords <- data.frame(
  lon = seq(-180, 180, by = 1),
  lat = 66.5
)

arctic_circle_sf <- arctic_circle_coords |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

# Create graticule lines
graticule <- st_graticule(
  lon = seq(-180, 180, 30), # longitude lines every 30 degrees
  lat = seq(60, 90, 10), # latitude lines every 10 degrees (adjust range as needed)
  crs = st_crs(ne_10m_geography_marine_polys)
)

# Create polar map ----
polar_map <- {
  ggplot() +
    # Base ocean areas
    geom_sf(
      data = ne_10m_geography_marine_polys_npolar,
      aes(fill = ocean_color),
      color = "black",
      linewidth = 0.1
    ) +
    # Country polygons with proper fill aesthetic
    geom_sf(
      data = ne_10m_countries_combined_npolar,
      aes(
        fill = ifelse(highlight_name, "#5c887b", "#e3d7bf")
      ),
      color = "black",
      linewidth = 0.2
    ) +

    # Graticules
    geom_sf(
      data = graticule,
      size = 1,
      color = "gray40",
      alpha = 0.5
    ) +
    # Graticule labels don't work well
    # geom_sf_text(
    #   data = graticule,
    #   aes(label = degree_label),
    #   size = 2.5,
    #   color = "gray40"
    # ) +

    # Arctic Circle reference line
    geom_sf(
      data = arctic_circle_sf,
      color = "darkred",
      linetype = "dashed",
      linewidth = 0.5
    ) +

    # Ocean labels
    geom_sf_shadowtext(
      data = ne_10m_geography_marine_polys_npolar,
      aes(
        label = if_else(highlight_name, name, NA_character_),
        fontface = if_else(highlight_name, "bold.italic", "italic"),
        size = if_else(major_body, "12px", "10px"),
        color = "#EEE",
        alpha = if_else(major_body | highlight_name, 1, 0)
      )
    ) +

    # Country names
    geom_sf_shadowtext(
      data = ne_10m_countries_combined_npolar,
      aes(
        label = if_else(highlight_name, name, NA_character_),
        fontface = if_else(highlight_name, "bold", "plain"),
        alpha = if_else(highlight_name, 1, 0)
      ),
      size = 3,
      color = "#2C3E50",
      bg.color = "white",
      stat = "sf_coordinates",
      inherit.aes = TRUE
    ) +

    # Styling
    scale_fill_identity() +
    scale_color_identity() +
    # guides(fill = "none") +
    # theme_void() +
    theme(
      panel.background = element_rect(fill = "#6e98b2", color = NA),
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    # polar stereographic projection
    coord_sf(
      crs = st_crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0"),
      xlim = c(-9000000, 9000000), # Slightly wider
      ylim = c(-5000000, 3000000), # Extended south
      expand = FALSE
    )
}

# Display the map
print(polar_map) |> suppressWarnings()

ggsave(
  filename = "images/aep_polar_map.svg",
  plot = polar_map,
  device = "svg",
  width = 10,
  height = 6
)

# Create rectangular map ----
rectangular_map <- ggplot() +
  # Base ocean areas
  geom_sf(
    data = ne_10m_geography_marine_polys_wgs84,
    aes(fill = ocean_color),
    color = "black",
    linewidth = 0.1
  ) +
  # Country polygons with proper fill aesthetic
  geom_sf(
    data = ne_10m_countries_combined_wgs84,
    aes(
      fill = ifelse(highlight_name, "#5c887b", "#e3d7bf")
    ),
    color = "black",
    linewidth = 0.2
  ) +

  # Graticules
  geom_sf(
    data = graticule,
    size = 1,
    color = "gray40",
    alpha = 0.5
  ) +
  # Graticule labels don't work well
  # geom_sf_text(
  #   data = graticule,
  #   aes(label = degree_label),
  #   size = 2.5,
  #   color = "gray40"
  # ) +

  # Arctic Circle reference line
  geom_sf(
    data = arctic_circle_sf,
    color = "darkred",
    linetype = "dashed",
    linewidth = 0.5
  ) +

  # Ocean labels
  geom_sf_shadowtext(
    data = ne_10m_geography_marine_polys_wgs84,
    aes(
      label = if_else(highlight_name, name, NA_character_),
      fontface = if_else(highlight_name, "bold.italic", "italic"),
      size = if_else(major_body, "10px", "8px"),
      color = "#EEE",
      alpha = if_else(major_body | highlight_name, 1, 0)
    )
  ) +

  # Country names
  geom_sf_shadowtext(
    data = ne_10m_countries_combined_wgs84,
    aes(
      label = if_else(highlight_name, name, NA_character_),
      fontface = if_else(highlight_name, "bold", "plain"),
      alpha = if_else(highlight_name, 1, 0)
    ),
    size = 3,
    color = "#2C3E50",
    bg.color = "white",
    stat = "sf_coordinates",
    inherit.aes = TRUE
  ) +

  # Styling
  scale_fill_identity() +
  scale_color_identity() +
  # guides(fill = "none") +
  # theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

rectangular_map |> suppressWarnings()

ggsave(
  filename = "images/area_map.png",
  plot = study_map,
  device = "png",
  dpi = 300,
  width = 30,
  height = 30,
  units = "cm"
)
