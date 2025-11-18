# ggplot2 extensions ----

#' Add shadow text to sf plots
#'
#' Custom geom for adding text with shadow/outline to sf plots, useful for
#' improving label visibility over complex backgrounds
#'
#' @param mapping Set of aesthetic mappings created by aes()
#' @param data The data to be displayed
#' @param stat The statistical transformation to use
#' @param position Position adjustment
#' @param ... Other arguments passed to layer
#' @param parse If TRUE, labels will be parsed as expressions
#' @param nudge_x Horizontal adjustment to nudge labels by
#' @param nudge_y Vertical adjustment to nudge labels by
#' @param check_overlap If TRUE, overlapping labels will be removed
#' @param na.rm If FALSE (default), removes missing values with warning
#' @param show.legend Logical indicating whether this layer should be included in legends
#' @param inherit.aes If FALSE, overrides default aesthetics
#' @param fun.geometry Function for transforming geometry (sf-specific)
#'
#' @return A ggplot2 layer
#'
#' @importFrom ggplot2 aes layer_sf position_nudge
#' @importFrom rlang list2
#' @importFrom cli cli_abort
#'
#' @export
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


# Map creation ----

#' Create Arctic study area base map (WGS84 projection)
#'
#' Generates a ggplot map showing the Arctic study area with oceans,
#' countries, and the Arctic Circle using WGS84 (EPSG:4326) coordinate system
#'
#' @param ocean_sf An sf object with annotated ocean polygons
#' @param country_sf An sf object with country polygons
#' @param arctic_circle_sf An sf object with Arctic Circle line
#' @param graticule_sf An sf object with graticule lines (optional)
#' @param background_color Character string, hex color for background
#' @param bbox A bounding box used to crop the displayed map. Defaults to none.
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot geom_sf geom_sf_text aes scale_fill_identity scale_color_identity guides theme element_rect element_blank margin
#' @importFrom dplyr if_else
#'
#' @export
create_study_area_map_wgs84 <- function(
  ocean_sf,
  country_sf,
  arctic_circle_sf,
  graticule_sf = NULL,
  background_color = background_colors["wgs84"],
  bbox = NULL
) {
  # if (!is.null(bbox)) {
  #   ocean_sf |> st_crop(ocean_sf, bbox)
  #   country_sf |> st_crop(country_sf, bbox)
  #   arctic_circle_sf |> st_crop(arctic_circle_sf, bbox)
  #   if (!is.null(graticule_sf)) {
  #     graticule_sf |> st_crop(graticule_sf, bbox)
  #   }
  # }

  map <- ggplot() +
    # Ocean polygons with custom colors
    geom_sf(
      data = ocean_sf,
      aes(fill = ocean_color),
      color = "black",
      linewidth = 0.1
    ) +
    # Country polygons
    geom_sf(
      data = country_sf,
      aes(
        fill = ifelse(
          highlight_name,
          country_colors["highlight"],
          country_colors["default"]
        )
      ),
      color = "black",
      linewidth = 0.2
    )

  # Add graticules if provided
  if (!is.null(graticule_sf)) {
    map <- map +
      geom_sf(
        data = graticule_sf,
        size = 0.5,
        color = graticule_color,
        alpha = 0.5
      )
  }

  map <- map +
    # Arctic Circle line
    geom_sf(
      data = arctic_circle_sf,
      color = arctic_circle_color,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    # Ocean labels
    geom_sf_shadowtext(
      data = ocean_sf,
      aes(
        label = dplyr::if_else(highlight_name, name, NA_character_),
        fontface = dplyr::if_else(highlight_name, "bold.italic", "italic"),
        # size = dplyr::if_else(major_body, "12px", "10px"),
        color = label_colors["ocean"],
        alpha = dplyr::if_else(major_body | highlight_name, 1, 0)
      )
    ) +
    # Country names
    geom_sf_shadowtext(
      data = country_sf,
      aes(
        label = dplyr::if_else(highlight_name, name, NA_character_),
        fontface = dplyr::if_else(highlight_name, "bold", "plain"),
        alpha = dplyr::if_else(highlight_name, 1, 0)
      ),
      # size = 3,
      color = label_colors["country"],
      bg.color = "white",
      stat = "sf_coordinates",
      inherit.aes = TRUE
    ) +
    # Styling
    scale_fill_identity() +
    scale_color_identity() +
    theme_arctic_map(background_color = background_color)
  if (!is.null(bbox)) {
    map <- map +
      coord_sf(
        xlim = c(
          northern_hemisphere_bbox()[[1]],
          northern_hemisphere_bbox()[[3]]
        ),
        ylim = c(
          northern_hemisphere_bbox()[[2]],
          northern_hemisphere_bbox()[[4]]
        )
      )
  }

  return(map)
}

#' Create Arctic study area base map (North Polar Stereographic projection)
#'
#' Generates a ggplot map showing the Arctic study area with oceans,
#' countries, and the Arctic Circle using North Polar Stereographic projection
#' (EPSG:3575 or custom stereographic)
#'
#' @param ocean_sf An sf object with annotated ocean polygons (should be in polar projection)
#' @param country_sf An sf object with country polygons (should be in polar projection)
#' @param arctic_circle_sf An sf object with Arctic Circle line
#' @param graticule_sf An sf object with graticule lines (optional)
#' @param background_color Character string, hex color for background
#' @param crs Character string or CRS object, coordinate reference system for projection
#'   (default: "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0")
#' @param xlim Numeric vector of length 2, x-axis limits in projected coordinates
#' @param ylim Numeric vector of length 2, y-axis limits in projected coordinates
#'
#' @return A ggplot object
#'
#' @importFrom ggplot2 ggplot geom_sf geom_sf_text aes scale_fill_identity scale_color_identity guides theme element_rect element_blank margin coord_sf
#' @importFrom sf st_crs
#' @importFrom dplyr if_else
#'
#' @export
create_study_area_map_polar <- function(
  ocean_sf,
  country_sf,
  arctic_circle_sf,
  graticule_sf = NULL,
  background_color = background_colors["polar"],
  crs = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=0",
  xlim = c(-9000000, 9000000),
  ylim = c(-5000000, 3000000)
) {
  map <- ggplot() +
    # Ocean polygons with custom colors
    geom_sf(
      data = ocean_sf,
      aes(fill = ocean_color),
      color = "black",
      linewidth = 0.1
    ) +
    # Country polygons
    geom_sf(
      data = country_sf,
      aes(
        fill = ifelse(
          highlight_name,
          country_colors["highlight"],
          country_colors["default"]
        )
      ),
      color = "black",
      linewidth = 0.2
    )

  # Add graticules if provided
  if (!is.null(graticule_sf)) {
    map <- map +
      geom_sf(
        data = graticule_sf,
        size = 0.5,
        color = graticule_color,
        alpha = 0.5
      )
  }

  map <- map +
    # Arctic Circle line
    geom_sf(
      data = arctic_circle_sf,
      color = arctic_circle_color,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    # Ocean labels
    geom_sf_shadowtext(
      data = ocean_sf,
      aes(
        label = dplyr::if_else(highlight_name, name, NA_character_),
        fontface = dplyr::if_else(highlight_name, "bold.italic", "italic"),
        size = dplyr::if_else(major_body, "12px", "10px"),
        color = label_colors["ocean"],
        alpha = dplyr::if_else(major_body | highlight_name, 1, 0)
      )
    ) +
    # Country names
    geom_sf_shadowtext(
      data = country_sf,
      aes(
        label = dplyr::if_else(highlight_name, name, NA_character_),
        fontface = dplyr::if_else(highlight_name, "bold", "plain"),
        alpha = dplyr::if_else(highlight_name, 1, 0)
      ),
      size = 3,
      color = label_colors["country"],
      bg.color = "white",
      stat = "sf_coordinates",
      inherit.aes = TRUE
    ) +
    # Styling
    scale_fill_identity() +
    scale_color_identity() +
    theme_arctic_map(background_color = background_color)
  # Apply polar stereographic projection
  coord_sf(
    crs = st_crs(crs),
    xlim = xlim,
    ylim = ylim,
    expand = FALSE
  )

  return(map)
}
