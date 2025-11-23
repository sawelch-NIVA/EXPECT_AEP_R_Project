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
