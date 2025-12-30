# TODO: Add roxgyen for all theme stuff. and use consistently.
# Map Color palettes & Themes ----

#' Ocean color palette
#'
#' Unnamed vector of colors for specific ocean bodies.
#' Order matches: Norwegian Sea, Greenland Sea, Barents Sea, Arctic Ocean, default
#'
#' @export
ocean_colors <- c(
  "#4A90E2", # Norwegian Sea - Medium blue
  "#7BB3F0", # Greenland Sea - Light blue
  "#2E5C8A", # Barents Sea - Dark blue
  "#5BA3D0", # Arctic Ocean - Blue-cyan
  "#E8E8E8" # Default - Light gray
)

#' Marine geography color palette
#'
#' Colors for highlighted vs non-highlighted marine features
#'
#' @export
marine_colors <- c(
  highlight = "#9dd9fe",
  default = "#6e98b2"
)

# Color palettes ----

#' Country fill color palette
#'
#' Colors for highlighted vs non-highlighted countries
#'
#' @export
country_colours <- c(
  highlight = "#5c887b",
  default = "#e3d7bf"
)

#' Label color palette
#'
#' Colors for different types of labels
#'
#' @export
label_colors <- c(
  ocean = "#EEE",
  country = "#2C3E50"
)

#' Background color palette
#'
#' Default background colors for different projection types
#'
#' @export
background_colors <- c(
  wgs84 = "#f8f9fa",
  polar = "#6e98b2"
)

#' Arctic Circle line color
#'
#' @export
arctic_circle_color <- "darkred"

#' Graticule color
#'
#' @export
graticule_color <- "gray40"

# ggplot2 themes ----

#' Arctic map theme
#'
#' Custom theme for Arctic maps with minimal styling
#'
#' @param background_color Character string, hex color for panel background
#' @param ... Additional arguments passed to theme()
#'
#' @return A ggplot2 theme object
#'
#' @importFrom ggplot2 theme element_rect element_blank margin unit
#'
#' @export
theme_arctic_map <- function(
  background_color = background_colors["wgs84"],
  ...
) {
  theme(
    panel.background = element_rect(fill = background_color, color = NA),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    ...
  )
}

# Single source of truth for theming. Will adjust as needed.
theme_aep_plots <- function() {
  theme_minimal()
}


# Define threshold colors based on the report ----
threshold_colours <- c(
  "Background (I)" = "#0070C0", # Blue
  "Good (II)" = "#00B050", # Green
  "Moderate (III)" = "#FFC000", # Yellow
  "Poor (IV)" = "#FF6600", # Orange
  "Very Poor (V)" = "#FF0000" # Red
)

compartment_colours <- c(
  # Aquatic ----
  "Freshwater" = "#4A90E2", # clear blue
  "Marine/Salt Water" = "#1E5A8E", # deep ocean blue
  "Brackish/Transitional Water" = "#6BA5C8", # mid-blue (between fresh/marine)
  "Groundwater" = "#2C5F7B", # darker, subdued blue
  "Wastewater" = "#8B7355", # murky brown
  "Liquid Growth Medium" = "#9FD8CB", # pale greenish-blue
  "Rainwater" = "#B3D9FF", # light sky blue
  "Stormwater" = "#607D8B", # grey-blue
  "Leachate" = "#6B5344", # dark muddy brown
  "Aquatic Sediment" = "#EBCF1B", # your existing yellow
  "Porewater" = "#7FA0B8", # muted blue-grey
  "Sludge" = "#67AB33", # your existing green

  # Atmospheric ----
  "Indoor Air" = "#E8F4F8", # very pale blue-grey
  "Outdoor Air" = "#87CEEB", # sky blue

  # Terrestrial ----
  "Terrestrial Biological Residue" = "#8B6F47", # organic brown
  "Soil H Horizon (Peat)" = "#3E2723", # very dark brown
  "Soil O Horizon (Organic)" = "#5D4037", # dark organic brown
  "Soil A Horizon (Topsoil)" = "#795548", # medium brown
  "Soil E Horizon (Mineral)" = "#A1887F", # light greyish-brown
  "Soil S Horizon (Mineral)" = "#BCAAA4", # pale brown-grey
  "Soil C Horizon (Parent Material)" = "#D7CCC8", # very pale brown
  "Soil R Horizon (Bedrock)" = "#9E9E9E", # grey

  # Biota ----
  "Biota, Terrestrial" = "#558B2F", # forest green
  "Biota, Aquatic" = "#00695C", # teal
  "Biota, Atmospheric" = "#B39DDB", # light purple (birds/insects)
  "Biota, Other" = "#9C27B0" # distinct purple
)

# Create sub-compartment letter codes ----
subcomp_codes <- c(
  # Aquatic
  "Freshwater" = "F",
  "Marine/Salt Water" = "M",
  "Brackish/Transitional Water" = "B",
  "Groundwater" = "G",
  "Wastewater" = "WW",
  "Liquid Growth Medium" = "LGM",
  "Rainwater" = "R",
  "Stormwater" = "SW",
  "Leachate" = "L",
  "Aquatic Sediment" = "AS",
  "Porewater" = "P",
  "Sludge" = "Sl",
  # Atmospheric
  "Indoor Air" = "IA",
  "Outdoor Air" = "OA",
  # Terrestrial
  "Terrestrial Biological Residue" = "TBR",
  "Soil H Horizon (Peat)" = "H",
  "Soil O Horizon (Organic)" = "O",
  "Soil A Horizon (Topsoil)" = "A",
  "Soil E Horizon (Mineral)" = "E",
  "Soil S Horizon (Mineral)" = "S",
  "Soil C Horizon (Parent Material)" = "C",
  "Soil R Horizon (Bedrock)" = "R",
  # Biota
  "Biota, Terrestrial" = "BT",
  "Biota, Aquatic" = "BA",
  "Biota, Atmospheric" = "BAm",
  "Biota, Other" = "BO"
)
