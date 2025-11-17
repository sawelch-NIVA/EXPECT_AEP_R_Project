study_area_bbox <- sf::st_bbox(c(xmin = -70, xmax = 90, ymin = 55, ymax = 90))
oceans_shapefile <- sf::read_sf("data/raw/shapefiles/World_Seas_IHO_v3.shp")

sf::sf_use_s2(FALSE)

# Ocean shapefiles

if (!file.exists("data/clean/study_area_shapefile.shp")) {
  oceans_shapefile <- sf::read_sf("data/raw/shapefiles/World_Seas_IHO_v3.shp")

  study_area <- oceans_shapefile |>
    sf::st_crop(study_area_bbox) |>
    sf::st_simplify()

  sf::write_sf(study_area, "data/clean/study_area_shapefile.shp")
} else {
  study_area <- sf::read_sf("data/clean/study_area_shapefile.shp")
}

# Continent shapefiles (low res)
study_countries <- rnaturalearth::ne_countries(scale = 10) |>
  mutate(
    in_study = case_match(
      name,
      c("Norway", "Greenland", "Iceland") ~ TRUE,
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
  sf::st_crop(study_area_bbox)

# study_oceans_110 <- ne_download(scale = 10, type = "ocean_scale_rank", category = "physical")

ggplot() +
  geom_sf(
    data = study_countries,
    aes(fill = ifelse(in_study, "lightgreen", "grey"))
  ) +
  geom_sf_text(data = study_countries, aes(label = abbrev))

# Need to convert shapefile from lat/long to azimuthal equidistant for simplification
# None of this works properly, lmao.
# study_area_simple <- oceans_shapefile |>
#   st_simplify(preserveTopology = TRUE, dTolerance = 0.1) |>
#   st_transform_repair(crs = "+proj=ortho +lat_0=90 +lon_0=0")
#
# ggplot(data = study_area_simple) +
#   geom_sf(fill = "#4A90E2", size = 0.2) +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "white", color = "black"),
#     plot.margin = margin(0, 0, 0, 0)
#   )

# Map styling and data preparation ----
study_area_annotated <- study_area |>
  mutate(
    NAME = case_match(NAME, "Barentsz Sea" ~ "Barents Sea", .default = NAME),
    show_name = case_when(
      NAME %in%
        c("Norwegian Sea", "Greenland Sea", "Barents Sea", "Arctic Ocean") ~
        TRUE,
      TRUE ~ FALSE
    ),
    # Create colour mapping for named oceans
    ocean_color = case_when(
      NAME == "Norwegian Sea" ~ "#4A90E2", # Medium blue
      NAME == "Greenland Sea" ~ "#7BB3F0", # Light blue
      NAME == "Barents Sea" ~ "#2E5C8A", # Dark blue
      NAME == "Arctic Ocean" ~ "#5BA3D0", # Blue-cyan
      TRUE ~ "#E8E8E8" # Light gray for others
    )
  )

# Create Arctic Circle line as sf object ----
# Generate points along Arctic Circle (66.5°N)
arctic_circle_coords <- data.frame(
  lon = seq(-70, 90, by = 1),
  lat = 66.5
)

arctic_circle_sf <- arctic_circle_coords |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

# Create the map ----
ggplot() +
  geom_sf(
    data = study_area_annotated,
    aes(fill = ocean_color),
    color = "white",
    size = 0.2
  ) +
  geom_sf_text(
    data = study_area_annotated,
    mapping = aes(label = NAME, color = show_name),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_identity() + # Use the actual color values
  guides(fill = "none") + # Remove legend
  # theme_void() +
  theme(
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    legend.position = "none"
  ) +
  geom_sf(
    data = study_countries,
    mapping = aes(fill = ifelse(in_study, "lightgreen", "grey"))
  ) +
  geom_sf_text(data = study_countries, aes(label = abbrev)) +
  geom_sf(
    data = arctic_circle_sf,
    color = "#333333",
    linetype = "dashed",
    size = 0.8
  )
