# Comparison variant of panel a) from build_study_area_figure.R: 2D kernel
# density contours instead of hexbin, one filled and one line-only, side by
# side for Sam to pick between. Not wired into anything; scratch comparison.
#   Rscript scripts/build_study_area_density_variant.R

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

study_bbox <- get_study_area_bbox()

wgs84_geo <- tar_read(wgs84_geography)
literature_merged_data <- tar_read(load_literature_pqt)

wgs84_geo$graticule <- create_graticule(
  lon_seq = seq(-90, 90, 20),
  lat_seq = seq(30, 90, 10)
)
arctic_circle_full <- create_arctic_circle(lon_range = c(-180, 180))

site_points <- literature_merged_data |>
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(MEASURED_N),
    # See build_study_area_figure.R for why these two filters exist: a
    # handful of West-Africa-coordinate rows (data bug) and genuinely real
    # but out-of-bbox CHINARE2008 Bering Strait sites.
    LATITUDE >= study_bbox["ymin"],
    LONGITUDE >= study_bbox["xmin"], LONGITUDE <= study_bbox["xmax"]
  ) |>
  summarise(
    total_n = sum(MEASURED_N, na.rm = TRUE),
    .by = c(SITE_CODE, LATITUDE, LONGITUDE)
  )

# stat_density_2d()/geom_density_2d() have no weight aesthetic (MASS::kde2d()
# has no weights argument either), so the only way to get a frequency-weighted
# KDE out of them is to fake it: repeat each site's row total_n times, so a
# site with 500 samples counts 500x as much in the density estimate as one
# with 1. ~24.8k distinct sites, sum(total_n) ~= 95.8k rows after expansion --
# kde2d() bins onto a grid (default 25x25) before estimating, so the actual
# density computation doesn't scale with row count; the expansion costs
# nothing beyond memory for ~96k rows.
#
# Bandwidth is computed on the UNEXPANDED coordinates and passed explicitly.
# MASS::bandwidth.nrd() scales as n^(-1/5), so leaving it on defaults after a
# ~4x row expansion would shrink the bandwidth and make the expanded version
# look artificially tighter than a straight visual "more weight = more
# spread-out mass" comparison intends.
site_points_weighted <- site_points |>
  tidyr::uncount(total_n)

density_h <- c(
  MASS::bandwidth.nrd(site_points$LONGITUDE),
  MASS::bandwidth.nrd(site_points$LATITUDE)
)

base_layers <- function() {
  list(
    geom_sf(
      data = wgs84_geo$marine_polys,
      aes(fill = I(ocean_color)),
      color = "white",
      linewidth = 0.15
    ),
    geom_sf(
      data = wgs84_geo$countries,
      aes(
        fill = I(ifelse(highlight_name, country_colours["highlight"], country_colours["default"]))
      ),
      color = "white",
      linewidth = 0.25
    ),
    geom_sf(data = wgs84_geo$graticule, linewidth = 0.15, color = "white", alpha = 0.45),
    geom_sf(
      data = arctic_circle_full,
      color = "#b23b3b",
      linetype = "dashed",
      linewidth = 0.5
    )
  )
}

label_layers <- function() {
  list(
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
    ),
    scale_alpha_identity(),
    geom_sf_shadowtext(
      data = wgs84_geo$countries |> filter(highlight_name),
      aes(label = name),
      color = "#20342c",
      bg.color = "white",
      bg.r = 0.15,
      fontface = "bold",
      size = 3.2,
      lineheight = 0.85
    )
  )
}

coord_panel <- coord_sf(
  crs = st_crs("+proj=eqc +lat_ts=60"),
  xlim = c(study_bbox["xmin"], study_bbox["xmax"]),
  ylim = c(study_bbox["ymin"], study_bbox["ymax"]),
  default_crs = st_crs(4326),
  expand = FALSE
)

panel_theme <- theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "bottom",
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.text = element_text(size = 6),
    plot.title = element_text(face = "bold", size = 11, hjust = 0.01, margin = margin(b = 2))
  )

# Filled contours, unweighted (site density only) --------------------------

panel_filled <- ggplot() +
  base_layers() +
  ggnewscale::new_scale_fill() +
  stat_density_2d_filled(
    data = site_points,
    aes(x = LONGITUDE, y = LATITUDE, fill = after_stat(level)),
    h = density_h,
    alpha = 0.75,
    contour_var = "ndensity",
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(name = "Relative density", guide = guide_legend(nrow = 1)) +
  label_layers() +
  coord_panel +
  panel_theme +
  labs(title = "Filled, unweighted (site density)")

# Filled contours, faked weight-by-repetition -------------------------------

panel_filled_weighted <- ggplot() +
  base_layers() +
  ggnewscale::new_scale_fill() +
  stat_density_2d_filled(
    data = site_points_weighted,
    aes(x = LONGITUDE, y = LATITUDE, fill = after_stat(level)),
    h = density_h,
    alpha = 0.75,
    contour_var = "ndensity",
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(name = "Relative density", guide = guide_legend(nrow = 1)) +
  label_layers() +
  coord_panel +
  panel_theme +
  labs(title = "Filled, weighted by sum(MEASURED_N) via row repetition")

# Line-only contours, faked weight-by-repetition ----------------------------

panel_lines_weighted <- ggplot() +
  base_layers() +
  geom_density_2d(
    data = site_points_weighted,
    aes(x = LONGITUDE, y = LATITUDE, color = after_stat(level)),
    h = density_h,
    linewidth = 0.5,
    contour_var = "ndensity",
    inherit.aes = FALSE
  ) +
  scale_color_viridis_c(name = "Relative density", guide = guide_colorbar(barwidth = unit(3, "cm"), barheight = unit(0.25, "cm"))) +
  label_layers() +
  coord_panel +
  panel_theme +
  labs(title = "Contour lines, weighted by sum(MEASURED_N) via row repetition")

layout <- (panel_filled / panel_filled_weighted / panel_lines_weighted) |> suppressWarnings()

ggsave(
  "images/dev/map-study-area-density-variant.png",
  layout,
  width = 10,
  height = 17,
  dpi = 250,
  bg = "white"
) |> suppressWarnings()

cat("saved images/dev/map-study-area-density-variant.png\n")
