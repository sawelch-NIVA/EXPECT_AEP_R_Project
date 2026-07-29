# Tests ----

fake_summary <- function(...) {
  base <- data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Fish",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Fjord",
    MEASURED_UNIT_STANDARD = "mg/kg (dry)",
    n = 500L,
    n_sources = 3L,
    date_min = as.Date("2001-01-01"),
    date_max = as.Date("2019-01-01"),
    sd = 2,
    mean = 10,
    n_double_outliers = 1L,
    median = 9,
    unit = "mg/kg (dry)",
    dip_p = 0.01,
    multimodal = TRUE
  )
  modifyList(base, list(...)) |> as.data.frame()
}

test_that("build_sample_groups_table produces the expected columns", {
  result <- build_sample_groups_table(fake_summary())

  expect_named(
    result,
    c(
      "group", "location", "dates", "n", "mean_sd", "median",
      "n_outliers", "dip_p_label", ".is_multimodal", ".is_outlier"
    )
  )
})

test_that("build_sample_groups_table labels biota by taxonomy", {
  result <- build_sample_groups_table(fake_summary())

  expect_match(result$group, "Gadus morhua")
  expect_match(result$group, "Liver")
})

test_that("build_sample_groups_table labels non-biota by compartment", {
  result <- build_sample_groups_table(fake_summary(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Freshwater"
  ))

  expect_match(result$group, "Aquatic")
  expect_match(result$group, "Freshwater")
  expect_false(grepl("Gadus", result$group))
})

test_that("build_sample_groups_table folds dates into a year range", {
  expect_equal(build_sample_groups_table(fake_summary())$dates, "2001–2019")
})

test_that("untested groups are not flagged as multimodal", {
  # multimodal is NA where n fell below dip_test_safe()'s min_n. NA must not
  # leak into the highlight set, or flextable colours arbitrary rows.
  result <- build_sample_groups_table(fake_summary(multimodal = NA))

  expect_false(result$.is_multimodal)
  expect_equal(result$dip_p_label, "")
})

test_that("multimodal groups carry a formatted p-value", {
  result <- build_sample_groups_table(fake_summary(dip_p = 0.0123))

  expect_true(result$.is_multimodal)
  expect_equal(result$dip_p_label, "0.01")
})

test_that("the outlier flag trips only above the 5% threshold", {
  under <- build_sample_groups_table(fake_summary(n = 1000L, n_double_outliers = 10L))
  over <- build_sample_groups_table(fake_summary(n = 100L, n_double_outliers = 10L))

  expect_false(under$.is_outlier)
  expect_true(over$.is_outlier)
})

test_that("build_sample_groups_table sorts by group then location", {
  data <- rbind(
    fake_summary(SAMPLE_SPECIES = "Zoarces viviparus"),
    fake_summary(SAMPLE_SPECIES = "Anarhichas lupus")
  )

  result <- build_sample_groups_table(data)

  expect_equal(result$group, sort(result$group))
})

test_that("sample_groups_flextable builds, and survives filtering", {
  data <- rbind(
    fake_summary(SAMPLE_SPECIES = "Anarhichas lupus", n = 50L),
    fake_summary(SAMPLE_SPECIES = "Zoarces viviparus", n = 5000L)
  )
  tbl <- build_sample_groups_table(data)

  expect_s3_class(sample_groups_flextable(tbl), "flextable")
  # index.qmd filters before formatting; highlight indices must be recomputed
  # from the filtered table, not inherited from the full one.
  expect_s3_class(
    sample_groups_flextable(tbl[tbl$n >= 100, , drop = FALSE]),
    "flextable"
  )
})

test_that("sample_groups_flextable copes with no rows flagged", {
  tbl <- build_sample_groups_table(fake_summary(
    multimodal = FALSE,
    n_double_outliers = 0L
  ))

  expect_s3_class(sample_groups_flextable(tbl), "flextable")
})
