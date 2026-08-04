# Tests for the categorical-panel overlay: per-category outlier flags, the
# right-margin count labels, and the layers themselves.
#
# Synthetic fixtures throughout, per CLAUDE.md: these must run in seconds and
# must not break when the target store is rebuilt.

# One category that is clean, one with a clear high outlier, one too small to
# test. Values are chosen so the flagged rows are unambiguous under both
# criteria rather than borderline.
fake_panel <- function() {
  tibble::tibble(
    .facet = factor(c(
      rep("clean", 20),
      rep("spiked", 20),
      rep("tiny", 4)
    )),
    MEASURED_VALUE_STANDARD = c(
      seq(1, 2, length.out = 20),
      c(seq(1, 2, length.out = 19), 1e6),
      c(1, 2, 3, 4)
    )
  )
}

test_that("triage_flag_by_category() flags within a category, not across", {
  out <- triage_flag_by_category(fake_panel(), min_n = 10)

  expect_true(all(c(".outlier", ".tested") %in% names(out)))
  # The spike is enormous relative to its own category and would also be
  # extreme pooled; what matters is that "clean" is untouched by it.
  expect_equal(sum(out$.outlier[out$.facet == "clean"]), 0)
  expect_equal(sum(out$.outlier[out$.facet == "spiked"]), 1)
  expect_true(out$.outlier[out$MEASURED_VALUE_STANDARD == 1e6])
})

test_that("categories below min_n are untested rather than unflagged", {
  out <- triage_flag_by_category(fake_panel(), min_n = 10)

  expect_false(any(out$.tested[out$.facet == "tiny"]))
  expect_true(all(out$.tested[out$.facet == "clean"]))
  # Untested must read as FALSE, never NA: an NA here would propagate into the
  # tick layer's subset and into sum() in the labels.
  expect_false(any(is.na(out$.outlier)))
  expect_equal(sum(out$.outlier[out$.facet == "tiny"]), 0)
})

test_that("a category flagged at one min_n can be untested at another", {
  low <- triage_flag_by_category(fake_panel(), min_n = 3)
  high <- triage_flag_by_category(fake_panel(), min_n = 25)

  expect_true(all(low$.tested[low$.facet == "tiny"]))
  expect_false(any(high$.tested))
  expect_equal(sum(high$.outlier), 0)
})

test_that("triage_flag_by_category() survives an empty panel", {
  empty <- fake_panel()[0, ]
  out <- triage_flag_by_category(empty)

  expect_equal(nrow(out), 0)
  expect_true(all(c(".outlier", ".tested") %in% names(out)))
})

test_that("count labels read n (k), and name the threshold when untested", {
  labs <- triage_flag_by_category(fake_panel(), min_n = 10) |>
    triage_category_labels(min_n = 10)

  expect_equal(
    labs$label[labs$.facet == "clean"],
    "20 (0)"
  )
  expect_equal(
    labs$label[labs$.facet == "spiked"],
    "20 (1)"
  )
  # Not "4 (0)", which would claim a test that did not run.
  expect_equal(
    labs$label[labs$.facet == "tiny"],
    "4 (n < 10)"
  )
})

test_that("the untested label interpolates min_n rather than hardcoding it", {
  labs <- triage_flag_by_category(fake_panel(), min_n = 25) |>
    triage_category_labels(min_n = 25)

  expect_true(all(grepl("n < 25", labs$label)))
  expect_false(any(grepl("n < 10", labs$label)))
})

test_that("count labels use a thousands separator without padding", {
  big <- tibble::tibble(
    .facet = factor(rep("big", 1500)),
    MEASURED_VALUE_STANDARD = seq(1, 2, length.out = 1500)
  )
  labs <- triage_flag_by_category(big) |> triage_category_labels()

  expect_equal(labs$label, "1,500 (0)")
})

test_that("the overlay returns layers, and suppresses ticks on request", {
  d <- triage_flag_by_category(fake_panel(), min_n = 10)

  with_ticks <- triage_category_overlay(d, limits = c(0.1, 1e7))
  without <- triage_category_overlay(d, limits = c(0.1, 1e7), ticks = FALSE)

  expect_true(all(vapply(with_ticks, ggplot2::is_layer, logical(1))))
  # Two box layers plus two tick layers plus two text layers; dropping the
  # ticks removes exactly two.
  expect_equal(length(with_ticks) - length(without), 2)
})

test_that("the overlay draws no tick layer when nothing is flagged", {
  clean <- tibble::tibble(
    .facet = factor(rep("clean", 20)),
    MEASURED_VALUE_STANDARD = seq(1, 2, length.out = 20)
  ) |>
    triage_flag_by_category()

  expect_equal(
    length(triage_category_overlay(clean, limits = c(0.5, 5))),
    length(triage_category_overlay(clean, limits = c(0.5, 5), ticks = FALSE))
  )
})

test_that("the overlay places labels without limits", {
  d <- triage_flag_by_category(fake_panel(), min_n = 10)

  # NULL limits must fall back to the data's own maximum rather than erroring or
  # placing the text at Inf.
  expect_no_error(triage_category_overlay(d, limits = NULL))
})

test_that("the overlay is empty for an empty panel", {
  empty <- triage_flag_by_category(fake_panel()[0, ])
  expect_equal(triage_category_overlay(empty), list())
})

test_that("boxplot layers suppress their own outlier points", {
  d <- triage_flag_by_category(fake_panel(), min_n = 10)
  layers <- triage_category_overlay(d, limits = c(0.1, 1e7))
  boxes <- Filter(
    function(l) inherits(l$geom, "GeomBoxplot"),
    layers
  )

  expect_length(boxes, 2)
  # One mark per row is exactly what CLAUDE.md 4.4 forbids at group level.
  #
  # Read from both places on purpose. ggplot2 4.0 rewrote the boxplot geom and
  # `outlier.shape` now lands in geom_params$outlier_gp$shape; older versions
  # kept it in aes_params. Checking only the current slot would pass silently on
  # a machine with the other version installed, and renv is switched off here so
  # the installed version is whatever the user library holds.
  shape_of <- function(l) {
    l$aes_params$outlier.shape %||% l$geom_params$outlier_gp$shape
  }
  for (b in boxes) {
    expect_true(is.na(shape_of(b)))
  }
})

test_that("the panel draws, and the threshold lines are the last layer", {
  d <- fake_panel() |>
    dplyr::mutate(
      MEASURED_UNIT_STANDARD = "mg/L",
      CAMPAIGN_NAME_SHORT = as.character(.facet)
    )
  thr <- tibble::tibble(
    THRESHOLD_VALUE_STANDARD = 3,
    THRESHOLD_CLASS = "Good (II)",
    THRESHOLD_TYPE = "Classification boundary",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    REFERENCE_ID = "M-608|2016",
    threshold_label = "Good (II)"
  )

  p <- triage_plot_by_category(
    d,
    "CAMPAIGN_NAME_SHORT",
    "test panel",
    limits = c(0.1, 1e7)
  )
  p <- p + triage_threshold_layers(thr, orientation = "vertical")

  expect_s3_class(p, "ggplot")
  # A ggplot object constructs fine and only fails when drawn, so force it.
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("overlay = FALSE gives back the bare heatmap", {
  d <- fake_panel() |>
    dplyr::mutate(
      MEASURED_UNIT_STANDARD = "mg/L",
      CAMPAIGN_NAME_SHORT = as.character(.facet)
    )

  with_overlay <- triage_plot_by_category(
    d, "CAMPAIGN_NAME_SHORT", "t", limits = c(0.1, 1e7)
  )
  without <- triage_plot_by_category(
    d, "CAMPAIGN_NAME_SHORT", "t", limits = c(0.1, 1e7), overlay = FALSE
  )

  expect_gt(length(with_overlay$layers), length(without$layers))
  expect_no_error(ggplot2::ggplot_build(without))
})
