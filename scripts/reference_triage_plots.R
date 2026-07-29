# Reference implementation for the triage plots (PLAN.md P1.1).
#
# Salvaged 2026-07-29 from docs/NBXX-Distributions-Aquatic-Sediment.qmd before
# that notebook and its 13 siblings were deleted along with the outlier tar_map
# factory (PLAN.md P0.2). This file is NOT rendered and NOT part of the
# pipeline. It exists so the plot specs survive to be generalised into
# R/fct_group_triage.R as one function per plot.
#
# Design rules these follow (see CLAUDE.md 4.4):
#   - summarising geoms only (bin2d, summary_hex, density); never geom_point
#     at group level
#   - each plot stands alone; no patchwork until submission prep (Phase 5)
#   - written to PNG by a format = "file" target, never returned as a ggplot
#
# The original notebook composed these with patchwork and then gave up:
#   "TODO: Stop trying to use patchwork here and just do an HTML graph gallery"
#   pw <- (p1 | p2) / (p3 | p5) / (p4 | plot_spacer())
# That conclusion is now the standing rule.

library(tidyverse)
library(targets)

# ---- Group data -------------------------------------------------------
# In the original this was tar_read(outlier_compartment_Aquatic_Aquatic_Sediment).
# Under the new architecture the equivalent input is a group subset of
# literature_analysis_ready (PLAN.md P1.0), which already drops NA / <= 0
# measured values globally.
#
# DATA FINDING, now handled upstream by P1.0 rather than inline here:
#   24 rows from Vm_2010_2025 campaigns have MEASURED_VALUE == 0 with no
#   LOD/LOQ or censoring flag. True zero Cu in marine sediment is implausible;
#   these are almost certainly "not detected" entries stored as 0 by the
#   Vannmiljø source.

grp <- tar_read(literature_analysis_ready) |>
  filter(
    ENVIRON_COMPARTMENT == "Aquatic",
    ENVIRON_COMPARTMENT_SUB == "Aquatic Sediment"
  ) |>
  mutate(SAMPLING_YEAR = factor(year(SAMPLING_DATE)))

unit_label <- paste0("Measured value (", unique(grp$MEASURED_UNIT_STANDARD)[1], ")")

# ---- a) Overall density -----------------------------------------------

p1 <- grp |>
  ggplot(aes(x = MEASURED_VALUE_STANDARD, colour = MEASURED_UNIT_STANDARD)) +
  geom_density() +
  geom_rug(alpha = 0.1) +
  scale_x_log10() +
  labs(x = unit_label, y = "Density", title = "a) Overall Density") +
  coord_cartesian(clip = "off") +
  theme(
    axis.text.y = element_text(hjust = 0, margin = margin(r = -2, unit = "lines"))
  )

# ---- b) Concentration by date -----------------------------------------
# geom_bin2d rather than geom_point: this is the aesthetic choice that made
# attempt one unusable at n = 40,000.

p2 <- grp |>
  ggplot(aes(x = SAMPLING_DATE, y = MEASURED_VALUE_STANDARD)) +
  geom_bin2d(bins = 125) +
  scale_fill_viridis_b(option = "plasma") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  scale_y_log10() +
  labs(x = "Sampling date", y = unit_label, title = "b) Concentration by Date")

# ---- c) Distribution by campaign --------------------------------------

p3 <- grp |>
  mutate(
    # Was a case_when() with no .default, which turned all 28 non-Vannmiljø
    # campaign names into NA. Now handled by prettify_campaign_name() in
    # R/fct_group_triage.R. The n >= 10 facet filter that used to sit here is
    # also gone: these panels are about coverage, not statistical validity.
    campaign_name_pretty = prettify_campaign_name(CAMPAIGN_NAME_SHORT),
    campaign_name_pretty = fct_reorder(
      campaign_name_pretty,
      MEASURED_VALUE_STANDARD,
      median,
      .na_rm = TRUE
    ) |>
      fct_relabel(str_wrap, width = 15)
  ) |>
  ggplot(aes(x = MEASURED_VALUE_STANDARD, y = campaign_name_pretty)) +
  geom_bin2d(bins = 40) +
  scale_x_log10() +
  scale_fill_viridis_b(name = "Count") +
  coord_cartesian(clip = "off") +
  labs(
    x = unit_label,
    title = "c) Distribution by Campaign"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      hjust = 0,
      size = rel(0.6),
      margin = margin(r = -4, unit = "lines")
    )
  )

# ---- d) Distribution by site type -------------------------------------

p5 <- grp |>
  filter(!is.na(SITE_GEOGRAPHIC_FEATURE), !is.na(SITE_GEOGRAPHIC_FEATURE_SUB)) |>
  mutate(
    SITE_GEOGRAPHIC_FEATURE_SUB = fct_reorder(
      SITE_GEOGRAPHIC_FEATURE_SUB,
      MEASURED_VALUE_STANDARD,
      median,
      .na_rm = TRUE
    ) |>
      fct_relabel(str_wrap, width = 15)
  ) |>
  ggplot(aes(x = MEASURED_VALUE_STANDARD, y = SITE_GEOGRAPHIC_FEATURE_SUB)) +
  geom_bin2d(bins = 40) +
  scale_x_log10() +
  scale_fill_viridis_b(name = "Count") +
  scale_y_discrete(position = "right") +
  facet_wrap(
    ~SITE_GEOGRAPHIC_FEATURE,
    scales = "free_y",
    ncol = 1,
    space = "free_y",
    labeller = label_wrap_gen(width = 50)
  ) +
  labs(x = unit_label, y = NULL, title = "d) Distribution by Site Type") +
  theme(axis.text.y = element_text(size = rel(0.6)))

# ---- e) Spatial distribution ------------------------------------------

world_map <- map_data("world")

spatial <- grp |>
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |>
  dplyr::mutate(
    x = sf::st_coordinates(geometry)[, 1],
    y = sf::st_coordinates(geometry)[, 2]
  )

p4 <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    colour = "white"
  ) +
  stat_summary_hex(
    data = spatial,
    aes(x = x, y = y, z = MEASURED_VALUE_STANDARD),
    fun = "median",
    bins = 60,
    alpha = 0.75
  ) +
  scale_fill_viridis_b(
    name = unit_label,
    trans = "log10",
    n.breaks = 6,
    option = "rocket"
  ) +
  coord_fixed(
    ratio = 2,
    xlim = c(get_study_area_bbox()[[1]], get_study_area_bbox()[[3]]),
    ylim = c(50, get_study_area_bbox()[[4]])
  ) +
  labs(x = "", y = "", title = "e) Spatial Distribution", subtitle = "median concentration") +
  theme(legend.position = "right")

# ---- Original interpretation notes -------------------------------------
#
# - Aquatic sediment might be expected to vary considerably: sediment is a
#   known sink for copper pollution, and it isn't subject to active regulation
#   as in e.g. biota tissues.
# - It's a commonly sampled compartment/medium, so we can expect a lot of data
#   points and need to adjust our plots accordingly.
