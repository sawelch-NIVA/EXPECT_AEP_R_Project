# Colour-scale breaks for the spatial panel (2026-08-05).
#
# Sam asked for one bin per order of magnitude, plus the classification
# boundaries marked. The thresholds cannot be drawn as lines on this panel: its
# axes are longitude and latitude and concentration is the fill, so a threshold
# has no position in the panel and can only appear on the legend.

sed_thresholds <- function() {
  thresholds_for_group(
    generate_copper_thresholds(),
    tibble::tibble(
      ENVIRON_COMPARTMENT = "Aquatic",
      ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
      SPECIES_GROUP = NA_character_,
      SAMPLE_SPECIES = NA_character_,
      SAMPLE_TISSUE = NA_character_,
      MEASURED_UNIT_STANDARD = "mg/kg (dry)"
    )
  )
}

test_that("there is one break per order of magnitude", {
  # n.breaks = 6 across the real Aquatic limits gave a bin every two orders,
  # which cannot separate a contaminated fjord from a clean one.
  b <- spatial_colour_breaks(c(1e-3, 1e5), thresholds = NULL)
  expect_equal(b$breaks, 10^(-3:5))
  expect_equal(b$labels, formatC(10^(-3:5), format = "e", digits = 0))
})

test_that("thresholds are inserted as extra breaks and named", {
  b <- spatial_colour_breaks(c(1e-1, 1e5), sed_thresholds())

  expect_true(20 %in% b$breaks)
  expect_true(84 %in% b$breaks)
  expect_true(147 %in% b$breaks)
  expect_true(any(grepl("(II)", b$labels, fixed = TRUE)))
  expect_true(any(grepl("(IV)", b$labels, fixed = TRUE)))
  expect_true(any(grepl("(V)", b$labels, fixed = TRUE)))
})

test_that("breaks come back sorted, with labels still attached to them", {
  # Thresholds are appended before sorting, so a labels vector reordered
  # independently of the breaks would mislabel every band.
  b <- spatial_colour_breaks(c(1e-1, 1e5), sed_thresholds())
  expect_false(is.unsorted(b$breaks))
  expect_equal(length(b$breaks), length(b$labels))
  expect_match(b$labels[which(b$breaks == 147)], "\\(V\\)")
  expect_match(b$labels[which(b$breaks == 20)], "\\(II\\)")
})

test_that("a threshold near a decade absorbs it rather than drawing a hairline", {
  # 84 and 147 sit either side of 100. Without absorption, 84 and 100 would be
  # 0.08 orders apart and draw as an unreadable sliver.
  b <- spatial_colour_breaks(c(1e-1, 1e5), sed_thresholds(), tol = 0.15)
  expect_false(100 %in% b$breaks)
  expect_true(84 %in% b$breaks)
  # The absorbing label carries the threshold's own value, so the scale still
  # reads correctly at that edge.
  expect_match(b$labels[which(b$breaks == 84)], "^8\\.4e\\+01")
})

test_that("threshold labels keep a mantissa, decades do not", {
  # Rounding 1.56e-02 to "2e-02" would misstate the value the class starts at.
  b <- spatial_colour_breaks(c(1e-1, 1e5), sed_thresholds())
  expect_match(b$labels[which(b$breaks == 147)], "^1\\.5e\\+02")
  expect_equal(b$labels[which(b$breaks == 1e3)], "1e+03")
})

test_that("thresholds outside the limits are not drawn", {
  b <- spatial_colour_breaks(c(1e3, 1e6), sed_thresholds())
  expect_false(any(grepl("(II)", b$labels, fixed = TRUE)))
  expect_false(20 %in% b$breaks)
})

test_that("unusable limits return NULL so the caller can fall back", {
  expect_null(spatial_colour_breaks(NULL))
  expect_null(spatial_colour_breaks(c(0, 100)))
  expect_null(spatial_colour_breaks(c(-5, 100)))
  expect_null(spatial_colour_breaks(c(NA, 100)))
  expect_null(spatial_colour_breaks(c(100, 100)))
})

test_that("no thresholds is a normal outcome, not an error", {
  b <- spatial_colour_breaks(c(1e-2, 1e2), empty_threshold_match())
  expect_equal(b$breaks, 10^(-2:2))
  b2 <- spatial_colour_breaks(c(1e-2, 1e2), NULL)
  expect_equal(b2$breaks, 10^(-2:2))
})

test_that("the spatial panel builds and carries the binned scale", {
  data <- data.frame(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
    SPECIES_GROUP = NA_character_,
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    MEASURED_UNIT_STANDARD = "mg/kg (dry)",
    MEASURED_VALUE_STANDARD = 10^seq(-1, 4, length.out = 80),
    LONGITUDE = seq(5, 30, length.out = 80),
    LATITUDE = seq(58, 71, length.out = 80)
  )
  grp <- data[1, c(
    "ENVIRON_COMPARTMENT", "ENVIRON_COMPARTMENT_SUB", "SPECIES_GROUP",
    "SAMPLE_SPECIES", "SAMPLE_TISSUE", "MEASURED_UNIT_STANDARD"
  )]

  p <- triage_plot_spatial(
    data,
    label = "test",
    limits = c(1e-1, 1e5),
    thresholds = generate_copper_thresholds(),
    grp = grp
  )
  expect_s3_class(p, "ggplot")
  # ggplot_build() does not exercise the device, but it does apply the scales,
  # which is where a malformed breaks/labels pair would error.
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})

test_that("a group with no coordinates gets a labelled blank, not an error", {
  data <- data.frame(
    MEASURED_UNIT_STANDARD = "mg/L",
    MEASURED_VALUE_STANDARD = c(1, 2, 3),
    LONGITUDE = NA_real_,
    LATITUDE = NA_real_
  )
  p <- triage_plot_spatial(data)
  expect_s3_class(p, "ggplot")
})

# ---- Per-bin strokes (2026-08-05) ---------------------------------------

test_that("there is one stroke entry per break, not per band", {
  # MEASURED, not reasoned. k breaks bound k + 1 bands, but a binned scale drawn
  # through guide_legend() emits exactly k keys, and ggplot2 rejects an
  # override.aes of the wrong length outright.
  thr <- sed_thresholds()
  b <- spatial_colour_breaks(c(1e-1, 1e5), thr)
  s <- spatial_bin_strokes(b$breaks, thr)

  expect_length(s$colour, length(b$breaks))
  expect_length(s$linetype, length(b$breaks))
  expect_length(s$linewidth, length(b$breaks))
})

test_that("strokes land on the threshold keys and nowhere else", {
  thr <- sed_thresholds()
  b <- spatial_colour_breaks(c(1e-1, 1e5), thr)
  s <- spatial_bin_strokes(b$breaks, thr)

  stroked <- which(!is.na(s$colour))
  expect_equal(sort(b$breaks[stroked]), c(20, 84, 147))
  # And the labels on those keys are the ones naming a class.
  expect_true(all(grepl("\\((II|IV|V)\\)", b$labels[stroked])))
})

test_that("stroke styling matches the threshold lines exactly", {
  # The whole point: a reader who learned "solid red = entering Very Poor" from
  # panel (c) must read the same thing off this key.
  thr <- sed_thresholds()
  b <- spatial_colour_breaks(c(1e-1, 1e5), thr)
  s <- spatial_bin_strokes(b$breaks, thr)

  i <- which(b$breaks == 147)
  expect_equal(s$colour[i], unname(threshold_class_colours()["V"]))
  expect_equal(s$linetype[i], unname(threshold_class_linetypes()["V"]))

  j <- which(b$breaks == 20)
  expect_equal(s$colour[j], unname(threshold_class_colours()["II"]))
  expect_equal(s$linetype[j], unname(threshold_class_linetypes()["II"]))
})

test_that("keys with no threshold draw no border at all", {
  thr <- sed_thresholds()
  b <- spatial_colour_breaks(c(1e-1, 1e5), thr)
  s <- spatial_bin_strokes(b$breaks, thr)
  plain <- which(is.na(s$colour))

  expect_gt(length(plain), 0)
  expect_true(all(s$linetype[plain] == "blank"))
  expect_true(all(s$linewidth[plain] == 0))
})

test_that("no thresholds gives an all-blank stroke set of the right length", {
  b <- spatial_colour_breaks(c(1e-2, 1e2), NULL)
  s <- spatial_bin_strokes(b$breaks, NULL)
  expect_length(s$colour, length(b$breaks))
  expect_true(all(is.na(s$colour)))
})

test_that("the guide reverses the strokes to match its reversed keys", {
  # REGRESSION. override.aes is applied in DRAWING order, and reverse = TRUE has
  # already flipped that, so an unreversed vector puts every stroke the same
  # distance from the wrong end of the key. Observed on the sediment panel: the
  # three strokes landed on 1e-02, 1e-03 and 1e-04 instead of 20, 84 and 147.
  thr <- sed_thresholds()
  b <- spatial_colour_breaks(c(1e-1, 1e5), thr)
  s <- spatial_bin_strokes(b$breaks, thr)
  g <- spatial_colour_guide(s)

  expect_true(g$params$reverse)
  expect_equal(g$params$override.aes$colour, rev(s$colour))
  expect_equal(g$params$override.aes$linetype, rev(s$linetype))
})
