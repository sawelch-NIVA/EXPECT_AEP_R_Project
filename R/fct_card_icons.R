# Small corner icons on node cards (added 2026-09-03).
#
# Marks whether an AEP node's data is specific to the AEP's bounding box (a map
# pin) or drawn from a wider region via geo_scope = "arctic" (a globe). Icons are
# Bootstrap Icons (MIT), kept as SVG in inst/extdata/icons/ and baked to PNG by
# scripts/bake_card_icons.R. The renderer loads the PNG: rsvg's in-memory raster
# segfaults on Sam's machine, and a pre-baked PNG keeps this path deterministic
# and free of a native call. png::readPNG is the only dependency here.

#' Icon File for a Node's `geo_scope`
#'
#' @param geo_scope One value, from the `geo_scope` column of an AEP membership
#'   file (see [aep_geo_scope_levels()]): `"arctic"`, `"local"`, `NA`, or the
#'   column absent entirely (`NULL`).
#' @return An absolute path to a baked PNG, or `NULL`. `NULL` means "draw no
#'   icon": either the node carries no `geo_scope` column at all (the national,
#'   non-AEP [write_node_cards()] path), or the value is unrecognised. A blank
#'   cell (`NA`) on an AEP node is treated as `"local"`, since that is the
#'   default an AEP scope applies.
#' @export
geo_scope_icon_path <- function(geo_scope) {
  if (is.null(geo_scope) || length(geo_scope) == 0L) {
    return(NULL)
  }
  value <- if (is.na(geo_scope[1])) "local" else as.character(geo_scope[1])
  file <- switch(
    value,
    arctic = "geo-regional.png",
    local = "geo-local.png",
    NULL
  )
  if (is.null(file)) {
    return(NULL)
  }
  path <- system.file("extdata", "icons", file, package = "STOPAEP")
  if (nzchar(path)) path else NULL
}

#' Icon File for a Node's `trend`
#'
#' The small grey glyph drawn after the headline figure on a node card, marking
#' which way that figure is judged to be moving over the study period.
#'
#' @param trend One value from the `trend` column of `aep_nodes.csv`: one of
#'   [node_trend_levels()], `NA`, or the column absent (`NULL`).
#' @return An absolute path to a baked PNG, or `NULL`. `NULL` means "draw no
#'   glyph": a blank cell (not yet assessed), an unrecognised value, or no
#'   `trend` column at all. `"unknown"` is *not* `NULL`; it draws a question
#'   mark, the assessed-but-undecided case.
#' @export
trend_icon_path <- function(trend) {
  if (is.null(trend) || length(trend) == 0L || is.na(trend[1])) {
    return(NULL)
  }
  value <- trimws(as.character(trend[1]))
  if (!nzchar(value) || !value %in% node_trend_levels()) {
    return(NULL)
  }
  path <- system.file(
    "extdata", "icons", paste0("trend-", value, ".png"),
    package = "STOPAEP"
  )
  if (nzchar(path)) path else NULL
}

#' A Baked PNG Trend Glyph, Anchored to the End of the Headline Row
#'
#' Mirror of [card_icon_grob()], but pinned to the right edge of the card at the
#' headline figure's own height rather than to a physical corner. Placed by
#' [node_card_header()] via `annotation_custom()` with the anchor point given in
#' the header panel's data coordinates, so the glyph tracks the headline text
#' vertically even if that panel's y-scale is retuned.
#'
#' Sam 2026-09-04: put it at the end of the quantity line, do not measure the
#' text width, keep the cards the same height. So this does not shift the
#' headline or grow the panel; a headline long enough to reach the glyph is
#' handled by keeping the wording short, not by code here.
#'
#' @param path A baked PNG, from [trend_icon_path()]. `NULL` returns `NULL`.
#' @param dpi The card's save resolution.
#' @param px On-card glyph side, in pixels at `dpi`.
#' @return A [grid::rasterGrob], or `NULL`.
#' @export
trend_badge_grob <- function(path, dpi = 300, px = 34) {
  if (is.null(path)) {
    return(NULL)
  }
  raster <- png::readPNG(path, native = FALSE)
  grid::rasterGrob(
    raster,
    width = grid::unit(px / dpi, "inches"),
    height = grid::unit(px / dpi, "inches"),
    hjust = 0.5,
    vjust = 0.5,
    interpolate = TRUE
  )
}

#' A Baked PNG Icon as a Corner-Anchored Grob
#'
#' Pinned to the card's physical top-right corner, the mirror of the node-id
#' grob in [node_card()]'s top-left. Sizes and offset are in inches derived from
#' the save resolution, so the icon stays a fixed pixel size on the card
#' regardless of the card's data range (same reasoning as `corner_offset`).
#'
#' @param path A PNG file, from [geo_scope_icon_path()].
#' @param dpi The card's save resolution.
#' @param px On-card icon side, in pixels at `dpi`.
#' @param offset_px Inset from the corner, in pixels at `dpi`.
#' @return A [grid::rasterGrob], or `NULL` if `path` is `NULL`.
#' @export
card_icon_grob <- function(path, dpi = 150, px = 48, offset_px = 6) {
  if (is.null(path)) {
    return(NULL)
  }
  raster <- png::readPNG(path, native = FALSE)
  grid::rasterGrob(
    raster,
    x = grid::unit(1, "npc") - grid::unit(offset_px / dpi, "inches"),
    y = grid::unit(1, "npc") - grid::unit(offset_px / dpi, "inches"),
    width = grid::unit(px / dpi, "inches"),
    height = grid::unit(px / dpi, "inches"),
    hjust = 1,
    vjust = 1,
    interpolate = TRUE
  )
}
