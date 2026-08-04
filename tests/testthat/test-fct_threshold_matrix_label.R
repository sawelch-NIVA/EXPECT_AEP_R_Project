# Tests for the secondary-axis title naming both the source and the matrix the
# threshold was set for.

test_that("external thresholds name sub-compartment and fraction", {
  thr <- tibble::tibble(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Freshwater",
    THRESHOLD_FRACTION = "Dissolved",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    REFERENCE_ID = "M-608|2016"
  )

  expect_equal(threshold_matrix_label(thr), "Freshwater, dissolved")
  expect_equal(
    threshold_source_title(thr),
    "M-608|2016 (Freshwater, dissolved)"
  )
})

test_that("biota thresholds name species and tissue instead", {
  thr <- tibble::tibble(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    THRESHOLD_FRACTION = "Total",
    SAMPLE_SPECIES = "Mytilus edulis",
    SAMPLE_TISSUE = "Total soft tissues",
    REFERENCE_ID = "PROREF"
  )

  expect_equal(
    threshold_matrix_label(thr),
    "Mytilus edulis, total soft tissues"
  )
})

test_that("a missing tissue does not leave a dangling comma", {
  thr <- tibble::tibble(
    ENVIRON_COMPARTMENT = "Biota",
    SAMPLE_SPECIES = "Mytilus spp.",
    SAMPLE_TISSUE = NA_character_,
    REFERENCE_ID = "BAC"
  )

  expect_equal(threshold_matrix_label(thr), "Mytilus spp.")
})

test_that("distinct source and matrix pairs are joined, duplicates collapsed", {
  thr <- tibble::tibble(
    ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Biota"),
    ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Freshwater", "Biota, Aquatic"),
    THRESHOLD_FRACTION = c("Dissolved", "Dissolved", "Total"),
    SAMPLE_SPECIES = c(NA, NA, "Gadus morhua"),
    SAMPLE_TISSUE = c(NA, NA, "Liver"),
    REFERENCE_ID = c("M-608|2016", "M-608|2016", "PROREF")
  )

  # Three rows, two distinct pairs: the repeated classification boundary must
  # not be named twice.
  expect_equal(
    threshold_source_title(thr),
    "M-608|2016 (Freshwater, dissolved) / PROREF (Gadus morhua, liver)"
  )
})

test_that("missing columns degrade rather than error", {
  # empty_threshold_match() carries neither ENVIRON_COMPARTMENT_SUB nor
  # THRESHOLD_FRACTION, and callers may hand over a subset.
  bare <- tibble::tibble(
    ENVIRON_COMPARTMENT = "Aquatic",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    REFERENCE_ID = "M-608|2016"
  )

  expect_equal(threshold_matrix_label(bare), NA_character_)
  # With no matrix to name, fall back to the bare reference rather than
  # printing "M-608|2016 (NA)".
  expect_equal(threshold_source_title(bare), "M-608|2016")
})

test_that("an empty threshold table yields no title", {
  expect_equal(threshold_matrix_label(empty_threshold_match()), character(0))
  expect_equal(threshold_source_title(empty_threshold_match()), "")
})
