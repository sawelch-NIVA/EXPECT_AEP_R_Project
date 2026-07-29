# Tests ----

# Minimal fixture carrying every column the triage plots touch.
fake_group_data <- function(n = 50, unit = "mg/kg (dry)", coords = TRUE) {
  data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Fish",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = rep(c("Fjord", "Open coast"), length.out = n),
    MEASURED_UNIT_STANDARD = unit,
    MEASURED_VALUE_STANDARD = seq(1, 100, length.out = n),
    SAMPLING_DATE = seq(as.Date("2000-01-01"), by = "month", length.out = n),
    CAMPAIGN_NAME_SHORT = rep(c("Camp A", "Camp B"), length.out = n),
    LONGITUDE = if (coords) seq(5, 30, length.out = n) else NA_real_,
    LATITUDE = if (coords) seq(60, 80, length.out = n) else NA_real_
  )
}

test_that("triage_use_points switches at the threshold", {
  expect_true(triage_use_points(1:29))
  expect_false(triage_use_points(1:30))
  expect_false(triage_use_points(1:31))
  # data frames are measured by rows, not columns
  expect_true(triage_use_points(data.frame(a = 1:5)))
  expect_false(triage_use_points(data.frame(a = 1:40)))
})

test_that("triage_use_points honours a custom threshold", {
  expect_false(triage_use_points(1:10, threshold = 5))
  expect_true(triage_use_points(1:10, threshold = 50))
})

test_that("filter_to_group matches groups defined by NA", {
  # Non-biota groups have NA taxonomy. A plain `== ` filter drops these
  # silently, which would leave whole compartments unplottable.
  data <- data.frame(
    ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Biota"),
    ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Freshwater", "Fish"),
    SPECIES_GROUP = c(NA, NA, "Fish"),
    SAMPLE_SPECIES = c(NA, NA, "Gadus morhua"),
    SAMPLE_TISSUE = c(NA, NA, "Liver"),
    SITE_GEOGRAPHIC_FEATURE = c("River", "River", "Coastal, fjord"),
    SITE_GEOGRAPHIC_FEATURE_SUB = c("Stream", "Stream", "Fjord"),
    MEASURED_UNIT_STANDARD = c("mg/L", "mg/L", "mg/kg (dry)"),
    value = 1:3
  )
  grp <- data[1, , drop = FALSE]

  result <- filter_to_group(data, grp)

  expect_equal(nrow(result), 2)
  expect_equal(result$value, 1:2)
})

test_that("filter_to_group excludes groups differing only by unit", {
  # Constant site sub-feature, so unit is the only thing separating the two
  # blocks (the default fixture alternates site types).
  dry <- fake_group_data(n = 3, unit = "mg/kg (dry)")
  wet <- fake_group_data(n = 4, unit = "mg/kg (wet)")
  dry$SITE_GEOGRAPHIC_FEATURE_SUB <- "Fjord"
  wet$SITE_GEOGRAPHIC_FEATURE_SUB <- "Fjord"
  data <- rbind(dry, wet)
  grp <- data[data$MEASURED_UNIT_STANDARD == "mg/kg (wet)", ][1, , drop = FALSE]

  expect_equal(nrow(filter_to_group(data, grp)), 4)
})

test_that("triage_group_label distinguishes biota from other compartments", {
  biota <- fake_group_data(n = 1)[1, , drop = FALSE]
  expect_match(triage_group_label(biota), "Gadus morhua")
  expect_match(triage_group_label(biota), "Liver")

  water <- biota
  water$ENVIRON_COMPARTMENT <- "Aquatic"
  water$ENVIRON_COMPARTMENT_SUB <- "Freshwater"
  expect_match(triage_group_label(water), "Freshwater")
  expect_false(grepl("Gadus", triage_group_label(water)))
})

test_that("triage_group_label fills missing taxonomy rather than returning NA", {
  grp <- fake_group_data(n = 1)[1, , drop = FALSE]
  grp$SAMPLE_SPECIES <- NA
  grp$SAMPLE_TISSUE <- NA

  label <- triage_group_label(grp)

  expect_false(is.na(label))
  expect_match(label, "spp\\.")
})

test_that("all five triage plots build on a normal group", {
  d <- fake_group_data(n = 50)
  lbl <- "test group"

  expect_s3_class(triage_plot_density(d, lbl), "ggplot")
  expect_no_error(ggplot2::ggplot_build(triage_plot_density(d, lbl)))
  expect_no_error(ggplot2::ggplot_build(triage_plot_by_date(d, lbl)))
  expect_no_error(ggplot2::ggplot_build(
    triage_plot_by_category(d, "CAMPAIGN_NAME_SHORT", "c", lbl)
  ))
  expect_no_error(ggplot2::ggplot_build(
    triage_plot_by_category(d, "SITE_GEOGRAPHIC_FEATURE_SUB", "d", lbl)
  ))
  expect_no_error(ggplot2::ggplot_build(triage_plot_spatial(d, lbl)))
})

test_that("triage plots build on a tiny group via the points branch", {
  d <- fake_group_data(n = 5)

  expect_no_error(ggplot2::ggplot_build(triage_plot_density(d)))
  expect_no_error(ggplot2::ggplot_build(triage_plot_by_date(d)))
  expect_no_error(ggplot2::ggplot_build(triage_plot_spatial(d)))
})

test_that("triage_plot_spatial degrades gracefully without coordinates", {
  d <- fake_group_data(n = 50, coords = FALSE)

  p <- triage_plot_spatial(d)

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("triage_plot_by_category degrades when no category clears min_facet_n", {
  d <- fake_group_data(n = 50)

  p <- triage_plot_by_category(d, "CAMPAIGN_NAME_SHORT", "c", min_facet_n = 1e6)

  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("triage_unit_label reports the group unit", {
  expect_equal(
    triage_unit_label(fake_group_data(n = 2, unit = "mg/L")),
    "Measured value (mg/L)"
  )
})

test_that("triage_unit_label stays generic when a subset spans units", {
  mixed <- rbind(
    fake_group_data(n = 2, unit = "mg/kg (dry)"),
    fake_group_data(n = 2, unit = "mg/kg (wet)")
  )

  expect_equal(triage_unit_label(mixed), "Measured value")
})

test_that("filter_to_group can ignore the unit column", {
  data <- rbind(
    fake_group_data(n = 3, unit = "mg/kg (dry)"),
    fake_group_data(n = 4, unit = "mg/kg (wet)")
  )
  data$SITE_GEOGRAPHIC_FEATURE_SUB <- "Fjord"
  grp <- data[1, , drop = FALSE]

  # Plot (a) needs both units so dry vs wet can be compared
  expect_equal(nrow(filter_to_group(data, grp)), 3)
  expect_equal(
    nrow(filter_to_group(data, grp, exclude_cols = "MEASURED_UNIT_STANDARD")),
    7
  )
})

test_that("triage_plot_density keeps both units distinguishable", {
  mixed <- rbind(
    fake_group_data(n = 40, unit = "mg/kg (dry)"),
    fake_group_data(n = 40, unit = "mg/kg (wet)")
  )

  built <- ggplot2::ggplot_build(triage_plot_density(mixed))

  # Two density curves, one per unit
  expect_equal(dplyr::n_distinct(built$data[[1]]$colour), 2)
})

test_that("prettify_campaign_name strips the Vannmiljo prefix", {
  expect_equal(
    prettify_campaign_name("Vm_2010_2025 (Polluted Seabed)"),
    "Polluted Seabed"
  )
  expect_equal(
    prettify_campaign_name("Vm_2010_2025 (Urban Fjord Contaminants)"),
    "Urban Fjord Contaminants"
  )
})

test_that("prettify_campaign_name leaves non-Vannmiljo names alone", {
  # The case_when() in NBXX-Outliers.qmd has no .default and turns these into
  # NA. 28 of 72 campaigns are non-Vannmiljo, so that matters.
  expect_equal(prettify_campaign_name("NorSeal1988"), "NorSeal1988")
  expect_equal(prettify_campaign_name("GlommaCu1990-95"), "GlommaCu1990-95")
})

test_that("prettify_campaign_name is vectorised and never returns NA", {
  x <- c("Vm_2010_2025 (Polluted Seabed)", "NorSeal1988", "ARKIXb1993FramGrnld")

  result <- prettify_campaign_name(x)

  expect_length(result, 3)
  expect_false(any(is.na(result)))
})

test_that("triage_plot_by_category applies label_fn to categories", {
  d <- fake_group_data(n = 40)
  d$CAMPAIGN_NAME_SHORT <- rep(
    c("Vm_2010_2025 (Polluted Seabed)", "NorSeal1988"),
    length.out = 40
  )

  p <- triage_plot_by_category(
    d,
    "CAMPAIGN_NAME_SHORT",
    "c",
    label_fn = prettify_campaign_name
  )
  labels <- levels(ggplot2::ggplot_build(p)$plot$data$.facet)

  expect_true(any(grepl("Polluted", labels)))
  expect_false(any(grepl("Vm_2010_2025", labels)))
  expect_true(any(grepl("NorSeal1988", labels)))
})

test_that("write_triage_plots_for_group writes one PNG per view", {
  d <- fake_group_data(n = 40)
  grp <- d[1, , drop = FALSE]
  grp$group_slug <- "test_group"
  dir <- withr::local_tempdir()

  paths <- write_triage_plots_for_group(d, grp, dir = dir)

  expect_length(paths, 5)
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 0))
  # Letter prefixes so a file browser sorts them into reading order
  expect_setequal(
    basename(paths),
    paste0(
      "test_group_",
      c("a_density", "b_date", "c_campaign", "d_site_type", "e_spatial"),
      ".png"
    )
  )
  expect_equal(basename(paths)[1], "test_group_a_density.png")
})
