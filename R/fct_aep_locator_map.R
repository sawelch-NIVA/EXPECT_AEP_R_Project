# Geographic locator figure for a spatially-scoped AEP (2026-08-27).
#
# Two panels, side by side:
#
#   a) Location   -- the AEP's bounding box drawn in red over the whole study
#                    area, so a reader can see where in Norway it is.
#   b) Study area -- a zoom to the bounding box plus a 20% margin, on a fresh
#                    high-resolution Natural Earth coastline, with the sampling
#                    sites that fall inside the box.
#
# Panel a reuses the project's study-area geography and its highlight colours.
# Panel b does NOT: at fjord scale the pipeline's simplified `wgs84_geography`
# polygons do not register cleanly, so panel b pulls NE 10m country polygons
# fresh and fills them a single muted grey (Sam, 2026-08-27). No ocean/country
# highlight palette, no shadowtext labels there.
#
# Not wired into the pipeline. scripts/build_geographic_aep_figure.R drives it.

#' Compartment Colours for the Locator's Site Points
#'
#' @return A named character vector keyed by `ENVIRON_COMPARTMENT`.
#' @export
locator_compartment_colours <- function() {
  c(
    Aquatic = "#1f78b4",
    Biota = "#e31a1c",
    Terrestrial = "#33a02c"
  )
}

#' A Two-Panel Geographic Locator for One AEP
#'
#' @param aep_id Which AEP. Must have a bounding box in `manifest`.
#' @param manifest The `aep_manifest` table (from [read_aep_manifest()]).
#' @param wgs84_geo The `wgs84_geography` target: a list with `marine_polys` and
#'   `countries`. Used for panel a only.
#' @param sites Sampling locations. An `sf` of points, or a data frame with
#'   `LONGITUDE` / `LATITUDE` columns; an `ENVIRON_COMPARTMENT` column, if
#'   present, colours the points.
#' @param land_hires Optional pre-fetched high-resolution land `sf` for panel b.
#'   `NULL` fetches NE 10m Norway via [rnaturalearth::ne_countries()].
#' @param poi Optional one-row data frame with `name`, `lat`, `lon` for a
#'   labelled marker in panel b.
#' @param pad Fraction of the bounding box width/height to add as margin on
#'   every side of panel b. `0.20` per Sam's spec.
#' @return A `patchwork` of two `ggplot`s, `panel_a | panel_b`.
#' @export
aep_locator_figure <- function(
  aep_id,
  manifest,
  wgs84_geo,
  sites,
  land_hires = NULL,
  poi = NULL,
  pad = 0.20
) {
  row <- manifest[manifest$aep_id == aep_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    cli::cli_abort(
      "aep_locator_figure(): no single manifest row for {.val {aep_id}}."
    )
  }
  if (anyNA(c(row$lon_min, row$lat_min, row$lon_max, row$lat_max))) {
    cli::cli_abort(
      "aep_locator_figure(): {.val {aep_id}} has no bounding box; \\
       there is nothing to locate."
    )
  }

  red <- "#b23b3b"
  bbox <- sf::st_bbox(
    c(
      xmin = row$lon_min, ymin = row$lat_min,
      xmax = row$lon_max, ymax = row$lat_max
    ),
    crs = sf::st_crs(4326)
  )
  bbox_sfc <- sf::st_as_sfc(bbox)

  # --- Panel a) Location -------------------------------------------------

  norway_view <- c(xmin = 4, xmax = 31.5, ymin = 57.5, ymax = 71.5)
  panel_a <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = wgs84_geo$marine_polys,
      ggplot2::aes(fill = I(.data$ocean_color)),
      colour = "white", linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data = wgs84_geo$countries,
      ggplot2::aes(fill = I(ifelse(
        .data$highlight_name,
        country_colours["highlight"],
        country_colours["default"]
      ))),
      colour = "white", linewidth = 0.25
    ) +
    ggplot2::geom_sf(data = bbox_sfc, fill = NA, colour = red, linewidth = 0.9) +
    ggplot2::coord_sf(
      xlim = c(norway_view["xmin"], norway_view["xmax"]),
      ylim = c(norway_view["ymin"], norway_view["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2),
      plot.title = ggplot2::element_text(
        face = "bold", size = 10, hjust = 0.01,
        margin = ggplot2::margin(b = 2)
      )
    ) +
    ggplot2::labs(title = "a) Location")

  # --- Panel b) Study area (zoom) --------------------------------------

  pad_lon <- (row$lon_max - row$lon_min) * pad
  pad_lat <- (row$lat_max - row$lat_min) * pad
  view <- c(
    xmin = as.numeric(row$lon_min - pad_lon),
    ymin = as.numeric(row$lat_min - pad_lat),
    xmax = as.numeric(row$lon_max + pad_lon),
    ymax = as.numeric(row$lat_max + pad_lat)
  )
  centre_lat <- mean(c(row$lat_min, row$lat_max))
  eqc <- sf::st_crs(paste0("+proj=eqc +lat_ts=", centre_lat, " +datum=WGS84"))

  land <- land_hires
  if (is.null(land)) {
    # Mainland Norway only: both POIs are on it, and a crop to the fjord
    # removes Svalbard / Jan Mayen anyway.
    land <- rnaturalearth::ne_countries(
      scale = 10, country = "Norway", returnclass = "sf"
    )
  }
  view_sfc <- sf::st_as_sfc(sf::st_bbox(view, crs = sf::st_crs(4326)))
  land_crop <- suppressWarnings(sf::st_crop(land, view_sfc))

  # Sites: accept an sf or a plain lon/lat frame, then keep only those inside
  # the STRICT box (not the padded view).
  if (!inherits(sites, "sf")) {
    sites <- sites |>
      dplyr::filter(!is.na(.data$LONGITUDE), !is.na(.data$LATITUDE)) |>
      sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = sf::st_crs(4326))
  }
  sites_in <- suppressWarnings(sf::st_filter(sites, bbox_sfc))

  has_compartment <- "ENVIRON_COMPARTMENT" %in% names(sites_in)

  panel_b <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = land_crop, fill = "grey86", colour = "grey70", linewidth = 0.2
    )

  if (nrow(sites_in) > 0) {
    panel_b <- panel_b +
      if (has_compartment) {
        ggplot2::geom_sf(
          data = sites_in,
          mapping = ggplot2::aes(colour = .data$ENVIRON_COMPARTMENT),
          size = 1.5, alpha = 0.85
        )
      } else {
        ggplot2::geom_sf(
          data = sites_in, colour = "#1f78b4", size = 1.5, alpha = 0.85
        )
      }
  }

  panel_b <- panel_b +
    ggplot2::geom_sf(data = bbox_sfc, fill = NA, colour = red, linewidth = 0.9)

  if (!is.null(poi) && nrow(poi) > 0) {
    poi_sf <- sf::st_as_sf(poi, coords = c("lon", "lat"), crs = sf::st_crs(4326))
    panel_b <- panel_b +
      ggplot2::geom_sf(
        data = poi_sf, shape = 21, fill = red, colour = "white",
        size = 2.6, stroke = 0.5
      ) +
      ggplot2::geom_sf_text(
        data = poi_sf, ggplot2::aes(label = .data$name),
        colour = "grey15", fontface = "bold", size = 3, vjust = -1
      )
  }

  if (has_compartment && nrow(sites_in) > 0) {
    panel_b <- panel_b +
      ggplot2::scale_colour_manual(
        values = locator_compartment_colours(),
        name = NULL, na.value = "grey50", drop = TRUE
      )
  }

  panel_b <- panel_b +
    ggplot2::coord_sf(
      crs = eqc,
      default_crs = sf::st_crs(4326),
      xlim = c(view["xmin"], view["xmax"]),
      ylim = c(view["ymin"], view["ymax"]),
      expand = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "#eaf2f7", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2),
      legend.position = "bottom",
      legend.key.height = ggplot2::unit(0.25, "cm"),
      legend.text = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(
        face = "bold", size = 10, hjust = 0.01,
        margin = ggplot2::margin(b = 2)
      )
    ) +
    ggplot2::labs(title = "b) Study area")

  patchwork::wrap_plots(panel_a, panel_b, ncol = 2) |>
    suppressWarnings()
}
