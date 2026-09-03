# Corner geo_scope icons on node cards (R/fct_card_icons.R, added 2026-09-03).
# A pin marks a node whose data is specific to its AEP's bounding box; a globe
# marks one drawn from a wider region (geo_scope = "arctic").

test_that("geo_scope_icon_path maps values to the right baked icon", {
  expect_match(geo_scope_icon_path("arctic"), "geo-regional\\.png$")
  expect_match(geo_scope_icon_path("local"), "geo-local\\.png$")
  # A blank cell on an AEP membership row is the default the scope applies,
  # i.e. local, so it still earns the pin rather than nothing.
  expect_match(geo_scope_icon_path(NA_character_), "geo-local\\.png$")
})

test_that("geo_scope_icon_path returns NULL when there is nothing to mark", {
  expect_null(geo_scope_icon_path(NULL)) # national card: no geo_scope column
  expect_null(geo_scope_icon_path(character(0)))
  expect_null(geo_scope_icon_path("continental")) # not a known level
})

test_that("the baked PNGs the paths resolve to exist", {
  expect_true(file.exists(geo_scope_icon_path("arctic")))
  expect_true(file.exists(geo_scope_icon_path("local")))
})

test_that("card_icon_grob rasterises to a top-right anchored grob, or NULL", {
  expect_null(card_icon_grob(NULL))
  g <- card_icon_grob(
    geo_scope_icon_path("local"),
    dpi = 300, px = 48, offset_px = 6
  )
  expect_s3_class(g, "rastergrob")
  # Pinned to the physical top-right corner: right- and top-justified, and both
  # coordinates are 1npc minus a fixed inch inset rather than a data value.
  expect_equal(as.numeric(g$hjust), 1)
  expect_equal(as.numeric(g$vjust), 1)
  expect_equal(as.numeric(g$width), 48 / 300)
})

test_that("aep_scope_nodes carries geo_scope onto every scoped node", {
  skip_if_not(file.exists(here::here("data/clean/aep/aep_manifest.csv")))
  nodes <- read_aep_nodes()
  manifest <- read_aep_manifest()
  membership <- read_aep_membership(nodes = nodes, manifest = manifest)
  scoped <- aep_scope_nodes(nodes, membership, manifest, "A002")
  expect_true("geo_scope" %in% names(scoped))
  # A002 puts the two river nodes on geo_scope = "arctic"; the rest inherit NA.
  arctic <- scoped$node_id[scoped$geo_scope %in% "arctic"]
  expect_setequal(
    arctic,
    c("N027-river-water-column", "N028-river-benthic-sed")
  )
})
