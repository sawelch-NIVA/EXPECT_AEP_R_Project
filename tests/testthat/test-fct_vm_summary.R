# Descriptive Vannmiljø summary for the manuscript (R/fct_vm_summary.R).
#
# The functions never touch the target store, so these run on a ~20-row
# synthetic frame. Degenerate cases matter here because the real data is
# heterogeneous: an empty source slice, all-NA MEASURED_N, biota with no
# species, a single compartment.

vm_fixture <- function(...) {
  base <- tibble::tibble(
    DATA_SOURCE = "Vannmiljø",
    MEASURED_N = 1L,
    MEASURED_FLAG = "",
    SITE_CODE = c(
      "S1", "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",
      "S10", "S11", "S12", "S13", "S14"
    ),
    CAMPAIGN_NAME = rep(c("Cx", "Cy", "Cz"), length.out = 15),
    SAMPLING_DATE = as.Date("2012-01-01") + 0:14 * 30,
    ENVIRON_COMPARTMENT = c(
      rep("Aquatic", 10), rep("Biota", 5)
    ),
    ENVIRON_COMPARTMENT_SUB = c(
      rep("Freshwater", 6), rep("Aquatic Sediment", 4),
      rep("Biota, Aquatic", 5)
    ),
    SAMPLE_SPECIES = c(
      rep(NA_character_, 10),
      "Gadus morhua", "Gadus morhua", "Mytilus edulis", "Mytilus edulis", NA
    )
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

test_that("summarise_vm_dataset filters to the source and counts the basics", {
  d <- dplyr::bind_rows(
    vm_fixture(),
    vm_fixture(DATA_SOURCE = "Primary", SITE_CODE = paste0("P", 1:15))
  )
  s <- summarise_vm_dataset(d)

  expect_named(s, c("scale", "composition", "totals"))
  get_metric <- function(m) s$scale$value[s$scale$metric == m]
  expect_equal(get_metric("Rows"), "15")
  expect_equal(get_metric("Measurements"), "15")
  expect_equal(get_metric("Sampling sites"), "14")
  expect_equal(get_metric("Monitoring campaigns"), "3")
  expect_match(get_metric("Sampling period"), "^2012-01-01 to 2013-")

  expect_equal(s$totals$measurements, 15)
  expect_equal(s$totals$rows, 15L)
  expect_equal(s$totals$sites, 14L) # distinct, not the sum of composition$sites
})

test_that("summarise_vm_dataset uses sum(MEASURED_N), not row count", {
  s <- summarise_vm_dataset(vm_fixture(MEASURED_N = 4L))
  expect_equal(s$scale$value[s$scale$metric == "Measurements"], "60")
  expect_equal(s$scale$value[s$scale$metric == "Rows"], "15")
})

test_that("censored count and percent are of measurements", {
  d <- vm_fixture(
    MEASURED_N = 2L,
    MEASURED_FLAG = c(rep("< LOQ", 3), rep("", 12))
  )
  s <- summarise_vm_dataset(d)
  # 3 flagged rows * 2 = 6 censored of 30 measurements
  expect_equal(
    s$scale$value[s$scale$metric == "Censored (< LOD / < LOQ)"],
    "6 (20.0%)"
  )
})

test_that("composition splits Water / Sediment / Biota and orders by size", {
  s <- summarise_vm_dataset(vm_fixture())
  co <- s$composition
  expect_equal(as.character(co$matrix), c("Water", "Sediment", "Biota"))
  expect_equal(co$subcompartment, c("Freshwater", "Aquatic Sediment", "Biota, Aquatic"))
  expect_equal(co$measurements, c(6L, 4L, 5L))
  expect_equal(co$n_species, c(NA_integer_, NA_integer_, 2L))
})

test_that("summarise_vm_dataset survives an empty source slice", {
  d <- vm_fixture(DATA_SOURCE = "Primary")
  s <- summarise_vm_dataset(d) # no Vannmiljø rows
  expect_equal(s$scale$value[s$scale$metric == "Rows"], "0")
  expect_equal(s$scale$value[s$scale$metric == "Measurements"], "0")
  expect_true(is.na(s$scale$value[s$scale$metric == "Sampling period"]))
  expect_equal(nrow(s$composition), 0L)
})

test_that("summarise_vm_dataset tolerates all-NA MEASURED_N", {
  s <- summarise_vm_dataset(vm_fixture(MEASURED_N = NA_integer_))
  expect_equal(s$scale$value[s$scale$metric == "Measurements"], "0")
  expect_equal(s$composition$measurements, c(0L, 0L, 0L))
})

test_that("vm_matrix_class classifies the three matrices", {
  expect_equal(
    vm_matrix_class(
      c("Aquatic", "Aquatic", "Biota"),
      c("Freshwater", "Aquatic Sediment", "Biota, Aquatic")
    ),
    c("Water", "Sediment", "Biota")
  )
})

test_that("vm_cleaning_funnel computes removed as the step-to-step drop", {
  f <- vm_cleaning_funnel(c(
    raw = 100L, compartments = 90L, sites = 90L, dates = 60L,
    compartment_conflicts = 55L, geographic_conflicts = 50L, analysis = 50L
  ))
  expect_equal(f$rows, c(100L, 90L, 90L, 60L, 55L, 50L, 50L))
  expect_equal(f$removed, c(NA, -10L, 0L, -30L, -5L, -5L, 0L))
  expect_equal(f$step[1], "Raw export")
  expect_equal(f$step[4], "Outside 2010-2025 removed")
})

test_that("vm_cleaning_funnel falls back to raw names for unmapped steps", {
  f <- vm_cleaning_funnel(c(raw = 10L, mystery = 8L))
  expect_equal(f$step, c("Raw export", "mystery"))
})
