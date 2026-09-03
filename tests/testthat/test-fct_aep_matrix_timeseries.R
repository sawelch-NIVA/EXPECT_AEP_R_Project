# Per-AEP matrix time series (R/fct_aep_matrix_timeseries.R, added 2026-09-03).

test_that("the M-608 palette is the four copper classes, matching the map figure", {
  cols <- copper_m608_class_colours()
  expect_named(cols, c("Background", "Good", "Poor", "Very Poor"))
  # copper skips M-608 Class III, so no Moderate here
  expect_false("Moderate" %in% names(cols))
  # same hexes as threshold_class_colours()[c("I","II","IV","V")]
  expect_equal(
    unname(cols),
    unname(threshold_class_colours()[c("I", "II", "IV", "V")])
  )
})

test_that("the PROREF status colours stay off the M-608 palette", {
  ps <- proref_status_colours()
  expect_named(ps, c("at or below PROREF", "above PROREF"))
  expect_length(intersect(ps, copper_m608_class_colours()), 0)
})

test_that("aep_ts_tag_compartment maps species/tissue/matrix to a compartment", {
  d <- tibble::tibble(
    SAMPLE_SPECIES = c("Gadus morhua", "Mytilus edulis", NA, NA, "Gadus morhua"),
    SAMPLE_TISSUE = c("Liver", "Total soft tissues", NA, NA, "Muscle"),
    ENVIRON_COMPARTMENT_SUB = c(
      "Biota, Aquatic", "Biota, Aquatic", "Marine/Salt Water",
      "Aquatic Sediment", "Biota, Aquatic"
    ),
    MEASURED_UNIT_STANDARD = c(
      "mg/kg (wet)", "mg/kg (wet)", "mg/L", "mg/kg (dry)", "mg/kg (wet)"
    )
  )
  expect_equal(
    aep_ts_tag_compartment(d),
    c("Cod liver", "Blue mussel", "Coastal water", "Sediment", NA)
  )
})

test_that("the plot builds and writes against the real store", {
  store <- here::here("_targets")
  skip_if_not(dir.exists(store))
  rd <- function(x) targets::tar_read_raw(x, store = store)
  data <- rd("literature_analysis_ready")
  thr <- rd("copper_toxicity_thresholds")
  gids <- rd("group_ids")
  mani <- rd("aep_manifest")

  p <- aep_matrix_timeseries_plot("A001", data, thr, gids, mani)
  expect_s3_class(p, "ggplot")

  out <- withr::local_tempdir()
  path <- write_aep_matrix_timeseries(
    "A001", data, thr, gids, mani,
    dir = out, width = 8, height = 6, dpi = 72
  )
  expect_true(file.exists(path))
  expect_match(basename(path), "^fig07-aep1-timeseries\\.png$")
})
