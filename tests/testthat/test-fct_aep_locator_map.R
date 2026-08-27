# Geographic locator figure for a spatially-scoped AEP (2026-08-27).
#
# Structural tests only (CLAUDE.md 2.3.1): the figure assembles, both panels
# draw, and the degenerate cases (no sites, no bounding box) behave. Panel b is
# handed a fake `land_hires` so nothing fetches from Natural Earth.

locator_manifest <- function(...) {
  base <- tibble::tibble(
    aep_id = c("A001", "A002"),
    label = c("National", "Testfjorden"),
    scope_note = NA_character_,
    lat_min = c(NA, 59.8),
    lat_max = c(NA, 60.3),
    lon_min = c(NA, 6.3),
    lon_max = c(NA, 6.8),
    date_min = as.Date(c(NA, "1900-01-01")),
    date_max = as.Date(c(NA, "2100-01-01")),
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

fake_square <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)
  )))
}

locator_geo <- function() {
  list(
    marine_polys = sf::st_sf(
      ocean_color = "#6e98b2",
      geometry = sf::st_sfc(fake_square(0, 55, 35, 75), crs = 4326)
    ),
    countries = sf::st_sf(
      highlight_name = TRUE,
      geometry = sf::st_sfc(fake_square(4, 58, 12, 62), crs = 4326)
    )
  )
}

locator_land <- function() {
  sf::st_sf(
    admin = "Norway",
    geometry = sf::st_sfc(fake_square(5.5, 59.4, 7.2, 60.6), crs = 4326)
  )
}

locator_sites <- function() {
  tibble::tibble(
    SITE_CODE = paste0("S", 1:4),
    LONGITUDE = c(6.5, 6.6, 6.55, 20.0),   # last one is outside the box
    LATITUDE = c(60.0, 60.1, 59.95, 65.0),
    ENVIRON_COMPARTMENT = c("Aquatic", "Biota", "Aquatic", "Aquatic")
  )
}

test_that("the figure is a two-panel patchwork that draws", {
  fig <- aep_locator_figure(
    "A002", locator_manifest(), locator_geo(), locator_sites(),
    land_hires = locator_land()
  )
  expect_s3_class(fig, "patchwork")
  expect_length(fig, 2)
  expect_no_error(ggplot2::ggplot_build(fig[[1]]))
  expect_no_error(ggplot2::ggplot_build(fig[[2]]))
})

test_that("only sites inside the strict bounding box are drawn on panel b", {
  fig <- aep_locator_figure(
    "A002", locator_manifest(), locator_geo(), locator_sites(),
    land_hires = locator_land()
  )
  b <- ggplot2::ggplot_build(fig[[2]])
  # The site layer is the geom_sf whose data carries ENVIRON_COMPARTMENT via
  # the colour aesthetic; three of the four sites are inside the box.
  pt_layer <- Filter(function(d) "colour" %in% names(d) && nrow(d) %in% c(3L),
                     b$data)
  expect_length(pt_layer, 1)
  expect_equal(nrow(pt_layer[[1]]), 3)
})

test_that("zero sites in the box is not an error", {
  far <- locator_sites()
  far$LONGITUDE <- 25
  far$LATITUDE <- 68
  expect_no_error(
    fig <- aep_locator_figure(
      "A002", locator_manifest(), locator_geo(), far,
      land_hires = locator_land()
    )
  )
  expect_no_error(ggplot2::ggplot_build(fig[[2]]))
})

test_that("an AEP with no bounding box is refused", {
  expect_error(
    aep_locator_figure(
      "A001", locator_manifest(), locator_geo(), locator_sites(),
      land_hires = locator_land()
    ),
    "no bounding box"
  )
})

test_that("an unknown aep_id is refused", {
  expect_error(
    aep_locator_figure(
      "A999", locator_manifest(), locator_geo(), locator_sites(),
      land_hires = locator_land()
    ),
    "no single manifest row"
  )
})

test_that("the 20% margin widens the panel b window beyond the strict box", {
  # pad = 0.20 on a 0.5-degree box adds 0.1 degrees each side.
  fig <- aep_locator_figure(
    "A002", locator_manifest(), locator_geo(), locator_sites(),
    land_hires = locator_land(), pad = 0.20
  )
  b <- ggplot2::ggplot_build(fig[[2]])
  # coord_sf stores the requested lon/lat window before projection.
  rng <- b$layout$coord$limits
  expect_equal(unname(rng$x), c(6.3 - 0.1, 6.8 + 0.1))
  expect_equal(unname(rng$y), c(59.8 - 0.1, 60.3 + 0.1))
})
