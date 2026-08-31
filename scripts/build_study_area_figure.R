# Builds the three-panel study area figure for index.qmd (@fig-study-area):
#   a) large panel: study area map + sample site density
#   b) small panel: points of interest
#   c) small panel: PRTR copper release coverage, by fylke
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
  library(ggrepel)
  pkgload::load_all(".", quiet = TRUE)
})

sf::sf_use_s2(FALSE)

study_bbox <- get_study_area_bbox() # full study area: North Atlantic cutoff to the Arctic
norway_bbox <- st_bbox(c(xmin = 4, xmax = 31.5, ymin = 57.5, ymax = 71.5), crs = st_crs(4326))

# Panel a): base map + sample density ----------------------------------

wgs84_geo <- tar_read(wgs84_geography)
literature_merged_data <- tar_read(load_literature_pqt)

wgs84_geo$graticule <- create_graticule(
  lon_seq = seq(-90, 90, 20),
  lat_seq = seq(30, 90, 10)
)

# Full loop, not just the Canada-to-Norway arc get_study_area_bbox() spans.
arctic_circle_full <- create_arctic_circle(lon_range = c(-180, 180))

site_points <- literature_merged_data |>
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(MEASURED_N),
    # 6 Vannmiljø rows (Vannmiljø_HAV-114981/83/84/85/94/96, VannmiljøCopper2010-2025)
    # carry LATITUDE ~= 0.0005, LONGITUDE ~= 4.51 -- off West Africa, clearly a
    # coordinate bug upstream, not a real site. Flagged separately; excluded
    # here so they don't paint a bogus low-latitude stripe across the map.
    LATITUDE >= study_bbox["ymin"],
    # CHINARE2008 (Chinese Arctic Research Expedition) sites are real, out in
    # the Bering Strait/Chukchi Sea around lon -178 to -160 -- genuinely
    # Arctic, just outside this Nordic-focused bbox. stat_bin_hex() sizes its
    # grid off the full data x-range regardless of coord_sf's clip, so a
    # handful of far-west points were stretching every hex cell into a smear.
    LONGITUDE >= study_bbox["xmin"], LONGITUDE <= study_bbox["xmax"]
  ) |>
  summarise(
    total_n = sum(MEASURED_N, na.rm = TRUE),
    .by = c(SITE_CODE, LATITUDE, LONGITUDE)
  ) |>
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
    data = arctic_circle_full,
    color = "#b23b3b",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  ggnewscale::new_scale_fill() +
  stat_bin_hex(
    data = site_points,
    aes(x = LONGITUDE, y = LATITUDE, weight = total_n, fill = after_stat(count)),
    bins = 45,
    linewidth = 0,
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    name = "Samples (sum n)",
    trans = "log10",
    labels = scales::comma
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
    lineheight = 0.85,
    show.legend = FALSE
  ) +
  scale_alpha_identity() +
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
  # ratio = 2: at this latitude range a degree of longitude covers roughly
  # half the ground distance of a degree of latitude, so this is what makes
  # the hexbin cells (computed in plain lon/lat units) draw as regular
  # hexagons instead of stretched ones. Same trick as triage_plot_spatial()
  # in R/fct_group_triage.R.
  # Equirectangular, true-scale at 60N (roughly the centre of the study area):
  # this is what actually makes the hexbin cells (computed in plain lon/lat
  # units) draw as regular hexagons, since at 60N a degree of longitude
  # covers half the ground distance of a degree of latitude (cos(60) = 0.5).
  # Same effect as the ratio = 2 used in triage_plot_spatial() (R/fct_group_triage.R),
  # generalised here via crs instead of coord_fixed because geom_sf() requires coord_sf().
  coord_sf(
    crs = st_crs("+proj=eqc +lat_ts=60"),
    xlim = c(study_bbox["xmin"], study_bbox["xmax"]),
    ylim = c(study_bbox["ymin"], study_bbox["ymax"]),
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "bottom",
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 11, hjust = 0.01, margin = margin(b = 2))
  ) +
  labs(title = "a) Study area & sampling density") |>
  suppressWarnings()

# Panel b): points of interest --------------------------------------------

poi_path <- "data/clean/points_of_interest.csv"
if (!file.exists(poi_path)) {
  stop(
    "Missing ", poi_path,
    ". Scaffold it first (name, lat, lon, poi_type, notes) before running this script."
  )
}

poi <- readr::read_csv(poi_path, show_col_types = FALSE) |>
  filter(!is.na(lat), !is.na(lon), name %in% c("Sorfjorden", "Repparfjorden")) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

poi_raw_bbox <- st_bbox(poi)
poi_bbox <- st_bbox(c(
  xmin = poi_raw_bbox["xmin"] - 3,
  xmax = poi_raw_bbox["xmax"] + 3,
  ymin = poi_raw_bbox["ymin"] - 1.5,
  ymax = poi_raw_bbox["ymax"] + 1.5
), crs = st_crs(4326))

panel_poi <- ggplot() +
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
  geom_sf(data = poi, color = "#b23b3b", size = 2.5) +
  geom_text_repel(
    data = poi,
    aes(x = lon, y = lat, label = name),
    size = 3.2,
    fontface = "bold",
    color = "#20342c",
    bg.color = "white",
    bg.r = 0.15,
    min.segment.length = 0,
    seed = 1
  ) +
  coord_sf(
    xlim = c(poi_bbox["xmin"], poi_bbox["xmax"]),
    ylim = c(poi_bbox["ymin"], poi_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 10, hjust = 0.01, margin = margin(b = 2))
  ) +
  labs(title = "b) Points of interest") |>
  suppressWarnings()

# Panel c): PRTR copper release coverage, by fylke -------------------------

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
  ggnewscale::new_scale_fill() +
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
    xlim = c(norway_bbox["xmin"], norway_bbox["xmax"]),
    ylim = c(norway_bbox["ymin"], norway_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "bottom",
    legend.text = element_text(size = 6, angle = 45, hjust = 1),
    legend.title = element_text(size = 7),
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.5, "cm"),
    plot.title = element_text(face = "bold", size = 10, hjust = 0.01, margin = margin(b = 2))
  ) +
  labs(title = "c) PRTR copper releases (by fylke)") |>
  suppressWarnings()

# Compose -------------------------------------------------------------------

layout <- panel_density | (panel_poi / panel_prtr)
layout <- layout + plot_layout(widths = c(2, 1))

ggsave(
  "images/dev/map-study-area.png",
  layout,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
) |> suppressWarnings()

cat("saved images/dev/map-study-area.png\n")
