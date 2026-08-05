# Detecting unit errors that arrive already made (2026-08-05).
#
# These are errors in data this project does not control, so no care in our own
# conversion code can prevent them. The Vannmiljo Urban Fjord rows are the
# worked example: a submitter took ug/g values and multiplied them by 1000 to
# "convert" them, when ug/g already IS mg/kg.

anomaly_data <- function() {
  # One sampling group, four campaigns. Three agree; one is a thousandfold high.
  base <- tibble::tibble(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Muscle",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)"
  )
  purrr::list_rbind(purrr::map(
    c("Good A", "Good B", "Good C", "Bad"),
    function(camp) {
      mult <- if (camp == "Bad") 1000 else 1
      dplyr::bind_cols(
        base,
        tibble::tibble(
          CAMPAIGN_NAME_SHORT = camp,
          MEASURED_VALUE_STANDARD = c(0.15, 0.2, 0.25, 0.3, 0.35) * mult,
          MEASUREMENT_COMMENT = if (camp == "Bad") {
            "Verdier oppgitt i µg/g (w.w.) og multiplisert med 1000."
          } else {
            "ICP-MS"
          }
        )
      )
    }
  ))
}

# ---- The self-documenting detector --------------------------------------

test_that("a comment describing unit arithmetic is caught", {
  # The certain detector: not an inference, the row says what was done to it.
  hits <- scan_comment_unit_flags(anomaly_data())
  expect_equal(nrow(hits), 1)
  expect_equal(hits$n_rows, 5L)
  expect_match(hits$comment, "multiplisert med 1000")
})

test_that("ordinary method comments are not caught", {
  d <- anomaly_data()
  d$MEASUREMENT_COMMENT <- "ICP-MS"
  expect_equal(nrow(scan_comment_unit_flags(d)), 0)
})

test_that("English phrasings are caught too", {
  d <- anomaly_data()
  d$MEASUREMENT_COMMENT <- "Values converted from dry weight"
  expect_gt(nrow(scan_comment_unit_flags(d)), 0)
})

test_that("a missing comment column is not an error", {
  d <- anomaly_data()
  d$MEASUREMENT_COMMENT <- NULL
  expect_equal(nrow(scan_comment_unit_flags(d)), 0)
})

# ---- The statistical detector -------------------------------------------

test_that("a thousandfold campaign is found, and the good ones are not", {
  off <- scan_group_scale_offsets(anomaly_data())
  flagged <- off[off$check_units, ]
  expect_equal(nrow(flagged), 1)
  expect_equal(flagged$CAMPAIGN_NAME_SHORT, "Bad")
  expect_equal(round(flagged$log10_ratio), 3)
})

test_that("the reference survives the faulty campaign being the largest", {
  # REGRESSION, and the reason the reference is a median over campaigns rather
  # than a pooled mean over rows. On the real data the 18 bad Gadus morhua rows
  # outweighed the correct ones, and a pooled reference flagged the four CORRECT
  # campaigns as two orders low while the faulty one slipped under the
  # threshold. Exactly inverted.
  d <- anomaly_data()
  bad <- d[d$CAMPAIGN_NAME_SHORT == "Bad", ]
  # Make the faulty campaign dominate on row count.
  d <- dplyr::bind_rows(d, bad[rep(seq_len(nrow(bad)), 8), ])

  off <- scan_group_scale_offsets(d)
  flagged <- off[off$check_units, ]
  expect_true("Bad" %in% flagged$CAMPAIGN_NAME_SHORT)
  expect_false(any(c("Good A", "Good B", "Good C") %in% flagged$CAMPAIGN_NAME_SHORT))
})

test_that("a partly-affected campaign is still flagged", {
  # THE CASE A CLEAN-DECADE TEST MISSES. Urban Fjord is 3 correct rows and 15
  # wrong, so its geometric mean sits at 10^3.56, nowhere near a whole power of
  # ten. Flagging on magnitude rather than on decade-ness is what catches it.
  # One row of five left correct, roughly the real proportion (3 of 18). Two of
  # five dilutes the geometric mean below the two-order threshold, which is a
  # fair warning that this detector weakens as the contamination approaches half
  # the campaign.
  d <- anomaly_data()
  d$MEASURED_VALUE_STANDARD[d$CAMPAIGN_NAME_SHORT == "Bad"][1] <- 0.15

  off <- scan_group_scale_offsets(d)
  flagged <- off[off$check_units, ]
  expect_true("Bad" %in% flagged$CAMPAIGN_NAME_SHORT)
  # And it is genuinely not a clean decade, which is the point.
  expect_gt(flagged$near_decade[flagged$CAMPAIGN_NAME_SHORT == "Bad"], 0.25)
})

test_that("a genuinely contaminated site is reported but not flagged", {
  # A mine draining into a river really is an order or so above background, and
  # must not read the same as a unit error.
  d <- anomaly_data()
  d$MEASURED_VALUE_STANDARD[d$CAMPAIGN_NAME_SHORT == "Bad"] <-
    d$MEASURED_VALUE_STANDARD[d$CAMPAIGN_NAME_SHORT == "Bad"] / 1000 * 30

  off <- scan_group_scale_offsets(d)
  expect_equal(nrow(off[off$check_units, ]), 0)
})

test_that("campaigns below min_rows are not compared", {
  d <- anomaly_data()
  d <- d[!(d$CAMPAIGN_NAME_SHORT == "Bad" & seq_len(nrow(d)) %% 2 == 0), ]
  off <- scan_group_scale_offsets(d, min_rows = 10)
  expect_equal(nrow(off), 0)
})

test_that("a group with only one campaign yields nothing", {
  d <- anomaly_data()
  d <- d[d$CAMPAIGN_NAME_SHORT == "Good A", ]
  expect_equal(nrow(scan_group_scale_offsets(d)), 0)
})

test_that("non-positive and missing values do not break the log", {
  d <- anomaly_data()
  d$MEASURED_VALUE_STANDARD[1] <- 0
  d$MEASURED_VALUE_STANDARD[2] <- NA_real_
  expect_no_error(scan_group_scale_offsets(d))
})

# ---- Reporting ----------------------------------------------------------

test_that("the pipeline warns when anything is found", {
  d <- anomaly_data()
  expect_warning(
    report_unit_anomalies(
      scan_comment_unit_flags(d), scan_group_scale_offsets(d)
    ),
    "SOURCE data"
  )
})

test_that("a clean dataset produces no warning", {
  d <- anomaly_data()
  d <- d[d$CAMPAIGN_NAME_SHORT != "Bad", ]
  d$MEASUREMENT_COMMENT <- "ICP-MS"
  expect_no_warning(
    report_unit_anomalies(
      scan_comment_unit_flags(d), scan_group_scale_offsets(d)
    )
  )
})
