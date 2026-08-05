# Tests for the 2026-08-05 outlier-scale change (both criteria on log10) and for
# the measurement-weighted count labels that went in alongside it.
#
# Synthetic fixtures only: these must run in seconds and must not break when the
# target store is rebuilt.

# ---- Both criteria on the log10 scale -----------------------------------

test_that("the RMZ criterion is two-sided on lognormal data", {
  # THE POINT OF THE WHOLE CHANGE. On the raw scale, MAD is set by the bulk near
  # the median, so a value two orders BELOW the median cannot reach 3.5 MADs
  # while one two orders above sails past it. Sam read that as a missing abs();
  # it was the scale.
  set.seed(20260805)
  x <- c(10^rnorm(200, mean = 1, sd = 0.3), 1e-3, 1e5)

  flags <- flag_outliers(x)
  low <- flags$outlier_RMZ[201]
  high <- flags$outlier_RMZ[202]

  expect_true(low)
  expect_true(high)
})

test_that("RMZ is computed on log10, not on the raw values", {
  # Asserted directly against the definition rather than through a symptom, so a
  # future edit that silently reverts the scale fails here rather than showing up
  # as a plot nobody re-reads.
  x <- c(1, 10, 100, 1000, 10000, 1e6, 1e-6, 5, 50, 500)
  flags <- flag_outliers(x)
  lv <- log10(x)
  expect_equal(flags$RMZ, (lv - median(lv)) / mad(lv))
})

test_that("Tukey fences stay on log10 and remain two-sided", {
  x <- c(rep(c(8, 9, 10, 11, 12), 4), 1e-4, 1e6)
  flags <- flag_outliers(x)
  expect_true(flags$outlier_IQR[21])
  expect_true(flags$outlier_IQR[22])
  expect_false(any(flags$outlier_IQR[1:20]))
})

test_that("a zero MAD makes the RMZ abstain rather than flag everything", {
  # mad() is 0 when more than half the values are identical, which makes every
  # score Inf or NaN, and abs(Inf) > 3.5 is TRUE. Left unguarded that flags every
  # value that is not exactly the median. More likely on log10 than on raw, since
  # rounding to a reporting precision collapses more ties.
  x <- c(rep(10, 15), 11, 12, 9, 1000, 8)
  flags <- flag_outliers(x)

  expect_true(all(is.na(flags$RMZ)))
  expect_true(all(is.na(flags$outlier_RMZ)))
  # The fences still decide, so the criterion abstains rather than blocking.
  expect_true(flags$outlier_IQR[19])
  # And an NA RMZ must not become "both", which is what the flag counts use.
  expect_false(any(flags$dot_fill %in% "both"))
  expect_true(any(flags$dot_fill %in% "IQR"))
})

test_that("below min_n nothing is computed and nothing is claimed", {
  x <- c(1, 5, 900)
  flags <- flag_outliers(x, min_n = 10)
  expect_true(all(is.na(flags$outlier_RMZ)))
  expect_true(all(is.na(flags$outlier_IQR)))
  expect_true(all(flags$dot_fill == "not tested"))
})

test_that("flag_outliers returns one row per input value", {
  # It is spliced into a mutate() in summarise_literature_data, so a length
  # mismatch would error there rather than anywhere near here.
  for (n in c(1, 9, 10, 37)) {
    expect_equal(nrow(flag_outliers(seq_len(n) * 1.0)), n)
  }
})

# ---- Measurement-weighted count labels ----------------------------------

weighted_category_data <- function() {
  # Two categories. The first has few rows carrying many measurements each, the
  # second the reverse, so a row count and a measurement count disagree in
  # opposite directions and neither can pass by accident.
  #
  # The bulk values are SPREAD rather than repeated. An earlier version of this
  # fixture used eleven identical values, which makes mad(log10(x)) exactly zero,
  # trips the abstention guard in flag_outliers(), and leaves nothing
  # double-flagged: the weighting test then failed for a reason that had nothing
  # to do with weighting.
  bulk <- c(8, 9, 10, 11, 12, 9, 10, 11, 10, 9, 11)
  data.frame(
    .facet = factor(c(rep("aggregated", 12), rep("per-sample", 12))),
    MEASURED_VALUE_STANDARD = c(
      c(bulk, 100000),
      c(bulk * 2, 0.0002)
    ),
    MEASURED_N = c(rep(50L, 12), rep(1L, 12))
  )
}

local_category_data <- function(n = 60) {
  # A self-contained equivalent of the fixture in
  # test-fct_group_triage_headings.R. Defined locally so this file passes under
  # `filter=`, which sources only the matching file.
  data.frame(
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Aquatic Sediment",
    SPECIES_GROUP = NA_character_,
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Other",
    MEASURED_UNIT_STANDARD = "mg/kg (dry)",
    MEASURED_VALUE_STANDARD = seq(1, 100, length.out = n),
    MEASURED_N = 1L,
    SAMPLING_DATE = seq(as.Date("2000-01-01"), by = "month", length.out = n),
    CAMPAIGN_NAME_SHORT = rep(c("Camp A", "Camp B", "Camp C"), length.out = n),
    LONGITUDE = seq(5, 30, length.out = n),
    LATITUDE = seq(60, 80, length.out = n)
  )
}

test_that("count labels report measurements, not rows", {
  # Sam's rule, 2026-08-05: a sample size is always sum(MEASURED_N); anything
  # counting rows says "n rows". The group headings above these panels already
  # reported measurements, so row-counting labels disagreed with the heading by a
  # factor of five on the fish overview with nothing explaining why.
  d <- triage_flag_by_category(weighted_category_data())
  lab <- triage_category_labels(d)

  agg <- lab[lab$.facet == "aggregated", ]
  expect_equal(agg$n, 600)
  expect_equal(agg$n_rows, 12L)
  expect_match(agg$label, "^600 ")
})

test_that("the outlier count is weighted the same way as the sample size", {
  # Numerator and denominator must be the same currency or the fraction means
  # nothing. Same reasoning as PLAN.md P1.5 for n_double_outliers.
  d <- triage_flag_by_category(weighted_category_data())
  lab <- triage_category_labels(d)
  agg <- lab[lab$.facet == "aggregated", ]

  flagged_rows <- sum(d$.outlier[d$.facet == "aggregated"])
  expect_gt(flagged_rows, 0)
  expect_equal(agg$k, flagged_rows * 50)
})

test_that("an untested category says so rather than reporting zero outliers", {
  d <- weighted_category_data()
  d <- d[c(1:3, 13:24), ]
  d <- triage_flag_by_category(d)
  lab <- triage_category_labels(d)
  small <- lab[lab$.facet == "aggregated", ]

  expect_false(small$tested)
  expect_match(small$label, "\\(n < 10\\)")
  # Gated on ROWS, since that is what the statistics are computed over: three
  # rows carrying 150 measurements is still three numbers.
  expect_equal(small$n_rows, 3L)
  expect_equal(small$n, 150)
})

test_that("labels fall back to row counts when MEASURED_N is absent", {
  # Visibly wrong beats blank: without the guard every label would be NA.
  d <- weighted_category_data()
  d$MEASURED_N <- NULL
  lab <- triage_category_labels(triage_flag_by_category(d))
  expect_equal(lab$n, c(12L, 12L))
  expect_false(any(is.na(lab$label)))
})

# ---- Label placement ----------------------------------------------------

test_that("the label anchor stays inside the scale limits", {
  # REGRESSION. Placing the anchor beyond limits[2] put it out of bounds, and a
  # continuous scale with explicit limits censors out-of-bounds values to NA, so
  # every count label vanished with only a "removed N rows" warning to show for
  # it. Expansion adds drawing room without widening the limits.
  d <- weighted_category_data()
  d$MEASURED_VALUE_STANDARD <- c(rep(10, 12), rep(20, 12))
  limits <- c(0.001, 1000)

  x_at <- triage_label_x(d, limits)
  expect_lte(x_at, limits[2])
  expect_gte(x_at, limits[1])
})

test_that("the label anchor falls back to the data maximum without limits", {
  d <- weighted_category_data()
  expect_equal(
    triage_label_x(d, NULL),
    max(d$MEASURED_VALUE_STANDARD)
  )
})

test_that("a categorical panel draws its labels rather than censoring them", {
  # The end-to-end version of the regression above: build the plot and count the
  # text grobs that survive. ggplot_build() applies the scale, which is where the
  # censoring happened.
  data <- local_category_data()
  p <- triage_plot_by_category(
    data,
    "CAMPAIGN_NAME_SHORT",
    "c) test",
    limits = c(0.5, 500)
  )
  built <- ggplot2::ggplot_build(p)

  text_layers <- Filter(
    function(d) "label" %in% names(d),
    built$data
  )
  expect_gt(length(text_layers), 0)
  drawn <- unlist(lapply(text_layers, function(d) d$label))
  # Three campaigns, plus the "n (n outliers)" header.
  expect_true("n (n outliers)" %in% drawn)
  expect_equal(sum(!is.na(drawn)), length(drawn))
  expect_gte(length(drawn), 4)
})

test_that("the header is omitted where there are no categories", {
  d <- weighted_category_data()
  d$.facet <- factor(d$.facet, levels = character(0))
  expect_null(triage_category_header(d, 10))
  expect_null(triage_category_header(weighted_category_data(), NA_real_))
})

test_that("the reserved margin carries no axis label, and no literal NA", {
  # REGRESSION. Suppressing the margin labels with NA_character_ drew the string
  # "NA" at 1e+06, because the break is a real value inside the expanded range
  # and only its label was unwanted. ggplot2 drops a break whose VALUE is NA; it
  # renders a label that is NA.
  data <- local_category_data()
  p <- triage_plot_by_category(
    data, "CAMPAIGN_NAME_SHORT", "c) test", limits = c(0.5, 500)
  )
  params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  labels <- params$x$get_labels()
  labels <- labels[!is.na(labels)]

  expect_false("NA" %in% labels)
  # Nothing labelled above the upper limit.
  drawn <- labels[nzchar(labels)]
  expect_true(all(as.numeric(drawn) <= 500))
})
