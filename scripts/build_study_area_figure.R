# Builds the three-panel study area figure for index.qmd (@fig-study-area):
#   (a) large panel: study area map + sample site density
#   (b) small panel: points of interest
#   (c) small panel: PRTR copper release coverage, by fylke
#
# Not wired into the pipeline (targets). PRTR integration is still deferred
# per PLAN.md section 10 (no facility coordinates, fylke-level only), and POI
# data is a new hand-maintained list that hasn't been reviewed yet, so this
# stays a script until both settle. Re-run by hand with:
#   Rscript scripts/build_study_area_figure.R

suppressMessages({
  library(targets)
  library(ggplot2)
  library(sf)
  library(dplyr)
  library(shadowtext)
  library(patchwork)
  pkgload::load_all(".", quiet = TRUE)
})

sf::sf_use_s2(FALSE)

study_bbox <- st_bbox(c(xmin = -35, xmax = 55, ymin = 58, ymax = 82), crs = st_crs(4326))

# Panel (a): base map + sample density ----------------------------------

wgs84_geo <- tar_read(wgs84_geography)
literature_merged_data <- tar_read(load_literature_pqt)

wgs84_geo$graticule <- create_graticule(
  lon_seq = seq(-30, 50, 10),
  lat_seq = seq(60, 80, 5)
)

site_points <- literature_merged_data |>
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) |>
  distinct(SITE_CODE, LATITUDE, LONGITUDE) |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

panel_density <- ggplot() +
  geom_sf(
    data = wgs84_geo$marine_polys,
    aes(fill = I(ocean_color)),
    color = "white",
    linewidth = 0.15
  ) +
  geom_sf(
    data = wgs84_geo$countries,
    aes(
      fill = I(ifelse(highlight_name, country_colours["highlight"], country_colours["default"]))
    ),
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(data = wgs84_geo$graticule, linewidth = 0.15, color = "white", alpha = 0.45) +
  geom_sf(
    data = wgs84_geo$arctic_circle,
    color = "#b23b3b",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  ggnewscale::new_scale_fill() +
  stat_bin_hex(
    data = site_points,
    aes(x = LONGITUDE, y = LATITUDE, fill = after_stat(count)),
    bins = 45,
    linewidth = 0,
    alpha = 0.55,
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colors = c("#ffe9a3", "#fdae6b", "#e6550d", "#7f0000"),
    trans = "log10",
    guide = "none"
  ) +
  geom_sf_shadowtext(
    data = wgs84_geo$marine_polys,
    aes(
      label = ifelse(highlight_name, name, NA_character_),
      fontface = ifelse(highlight_name, "bold.italic", "italic"),
      alpha = ifelse(major_body | highlight_name, 1, 0)
    ),
    color = "#f4faff",
    bg.color = "#1c3d57",
    bg.r = 0.18,
    size = 3.0,
    lineheight = 0.85
  ) +
  geom_sf_shadowtext(
    data = wgs84_geo$countries |> filter(highlight_name),
    aes(label = name),
    color = "#20342c",
    bg.color = "white",
    bg.r = 0.15,
    fontface = "bold",
    size = 3.2,
    lineheight = 0.85
  ) +
  coord_sf(
    xlim = c(study_bbox["xmin"], study_bbox["xmax"]),
    ylim = c(study_bbox["ymin"], study_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#eaf4fb", color = NA),
    plot.background = element_rect(fill = "#eaf4fb", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 11, hjust = 0.01, margin = margin(b = 2))
  ) +
  labs(title = "Study area & sampling density") |>
  suppressWarnings()

# Panel (b): points of interest ------------------------------------------

poi_path <- "data/clean/points_of_interest.csv"
if (!file.exists(poi_path)) {
  stop(
    "Missing ", poi_path,
    ". Scaffold it first (name, lat, lon, poi_type, notes) before running this script."
  )
}

poi <- readr::read_csv(poi_path, show_col_types = FALSE) |>
  filter(!is.na(lat), !is.na(lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

poi_pal <- setNames(
  palette.colors(n = max(3, length(unique(poi$poi_type))), palette = "Tableau"),
  sort(unique(poi$poi_type))
)

panel_poi <- ggplot() +
  geom_sf(data = wgs84_geo$marine_polys, fill = "#eaf4fb", color = "white", linewidth = 0.1) +
  geom_sf(
    data = wgs84_geo$countries,
    fill = country_colours["default"],
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(data = poi, aes(color = poi_type), size = 2.2) +
  scale_color_manual(values = poi_pal, name = NULL) +
  coord_sf(
    xlim = c(study_bbox["xmin"], study_bbox["xmax"]),
    ylim = c(study_bbox["ymin"], study_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#eaf4fb", color = NA),
    plot.background = element_rect(fill = "#eaf4fb", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"),
    plot.title = element_text(face = "bold", size = 10, hjust = 0.01, margin = margin(b = 2))
  ) +
  labs(title = "Points of interest") |>
  suppressWarnings()

# Panel (c): PRTR copper release coverage, by fylke -----------------------

prtr_long <- read_prtr_long()
prtr_by_fylke <- summarise_prtr_releases(prtr_long, by = "fylke")

norway_fylker <- rnaturalearth::ne_states(geounit = "norway") |>
  select(fylke = name, geometry) |>
  st_as_sf()

# Names differ slightly between Natural Earth and the PRTR extract (accents,
# "og" spellings); normalise both sides the same way before joining.
normalise_fylke <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all("[æøå]", "") |>
    stringr::str_remove_all("[^a-z]")
}

norway_fylker <- norway_fylker |>
  mutate(fylke_key = normalise_fylke(fylke))

prtr_by_fylke <- prtr_by_fylke |>
  mutate(fylke_key = normalise_fylke(fylke))

fylke_totals <- norway_fylker |>
  left_join(
    prtr_by_fylke |> select(fylke_key, total_kg_yr),
    by = "fylke_key"
  )

panel_prtr <- ggplot() +
  geom_sf(data = wgs84_geo$marine_polys, fill = "#eaf4fb", color = "white", linewidth = 0.1) +
  geom_sf(data = fylke_totals, aes(fill = total_kg_yr), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    name = "Cu released\n(kg/yr)",
    option = "magma",
    direction = -1,
    na.value = "grey85",
    trans = "log10",
    labels = scales::comma
  ) +
  coord_sf(
    xlim = c(study_bbox["xmin"], study_bbox["xmax"]),
    ylim = c(study_bbox["ymin"], study_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#eaf4fb", color = NA),
    plot.background = element_rect(fill = "#eaf4fb", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "bottom",
    legend.text = element_text(size = 6, angle = 45, hjust = 1),
    legend.title = element_text(size = 7),
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.5, "cm"),
    plot.title = element_text(face = "bold", size = 10, hjust = 0.01, margin = margin(b = 2))
  ) +
  labs(title = "PRTR copper releases (by fylke)") |>
  suppressWarnings()

# Compose -------------------------------------------------------------------

layout <- panel_density | (panel_poi / panel_prtr)
layout <- layout + plot_layout(widths = c(2, 1))

ggsave(
  ".images-raw/map-study-area.png",
  layout,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
) |> suppressWarnings()

cat("saved .images-raw/map-study-area.png\n")
