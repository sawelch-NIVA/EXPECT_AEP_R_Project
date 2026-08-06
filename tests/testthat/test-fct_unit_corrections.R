# Tests for R/fct_unit_corrections.R.
#
# Synthetic fixtures only. This layer overwrites measured values, so the
# degenerate cases matter more here than anywhere else in the project: almost
# every test below asserts that something is REFUSED.

corr_data <- function() {
  tibble::tibble(
    row_id = c("R1", "R2", "R3", "R4"),
    MEASURED_VALUE = c(1000, 2000, 3, 4),
    MEASURED_VALUE_STANDARD = c(1000, 2000, 3, 4),
    LOD_VALUE_STANDARD = c(10, 20, 0.03, 0.04),
    LOQ_VALUE_STANDARD = c(30, 60, 0.09, 0.12),
    MEASURED_N = c(1L, 2L, 3L, 4L),
    MEASUREMENT_COMMENT = c(
      "multiplisert med 1000", "multiplisert med 1000", NA, NA
    ),
    CAMPAIGN_NAME_SHORT = c("UrbanFjord", "UrbanFjord", "Other", "Other"),
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Muscle",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)"
  )
}

corr_row <- function(...) {
  base <- list(
    correction_id = "C001",
    group_id = NA_character_,
    campaign_name_short = NA_character_,
    comment_match = "multiplisert med 1000",
    value_min = NA_real_,
    value_max = NA_real_,
    factor = 0.001,
    row_ids = NA_character_,
    reason = "Submitter states ug/g multiplied by 1000; ug/g is already mg/kg.",
    evidence = "MEASUREMENT_COMMENT on every affected row.",
    date_added = "2026-08-06"
  )
  args <- list(...)
  base[names(args)] <- args
  tibble::as_tibble(base)
}

# apply_unit_corrections: the happy path ------------------------------------

test_that("a matching correction scales the value and both limits", {
  out <- apply_unit_corrections(corr_data(), corr_row())
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
  expect_equal(out$LOD_VALUE_STANDARD, c(0.01, 0.02, 0.03, 0.04))
  expect_equal(out$LOQ_VALUE_STANDARD, c(0.03, 0.06, 0.09, 0.12))
})

test_that("MEASURED_VALUE is left as the audit trail", {
  out <- apply_unit_corrections(corr_data(), corr_row())
  expect_equal(out$MEASURED_VALUE, c(1000, 2000, 3, 4))
})

test_that("provenance columns record which correction touched which row", {
  out <- apply_unit_corrections(corr_data(), corr_row())
  expect_equal(out$unit_correction_id, c("C001", "C001", NA, NA))
  expect_equal(out$unit_correction_factor, c(0.001, 0.001, NA, NA))
})

test_that("no corrections still yields the provenance columns", {
  out <- apply_unit_corrections(corr_data(), empty_unit_corrections())
  expect_true(all(is.na(out$unit_correction_id)))
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1000, 2000, 3, 4))
})

test_that("absent limit columns are simply not scaled", {
  d <- corr_data()[, setdiff(names(corr_data()), "LOD_VALUE_STANDARD")]
  out <- apply_unit_corrections(d, corr_row())
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

# Selectors -----------------------------------------------------------------

test_that("comment_match is a fixed substring, not a regex", {
  d <- corr_data()
  d$MEASUREMENT_COMMENT <- c("a (b) c", "a (b) c", NA, NA)
  out <- apply_unit_corrections(d, corr_row(comment_match = "a (b) c"))
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

test_that("a plain `ug` in the CSV matches a micro sign in the comment", {
  # CLAUDE.md 4.4.-2. The real Urban Fjord comment carries U+00B5; nobody should
  # have to type that into a spreadsheet to select rows for overwriting.
  d <- corr_data()
  d$MEASUREMENT_COMMENT <- c(
    "Verdier oppgitt i µg/g (w.w.) og multiplisert med 1000.",
    "Verdier oppgitt i μg/g (w.w.) og multiplisert med 1000.",
    NA, NA
  )
  out <- apply_unit_corrections(d, corr_row(comment_match = "ug/g (w.w.)"))
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

test_that("value bounds select the bad cluster within a mixed campaign", {
  # The case that rules out a blanket campaign-level factor.
  out <- apply_unit_corrections(
    corr_data(),
    corr_row(comment_match = NA, campaign_name_short = "UrbanFjord", value_min = 500)
  )
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

test_that("selectors combine with AND", {
  out <- apply_unit_corrections(
    corr_data(),
    corr_row(campaign_name_short = "UrbanFjord", value_min = 1500)
  )
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1000, 2, 3, 4))
})

test_that("a group_id selector needs the ledger", {
  expect_error(
    apply_unit_corrections(corr_data(), corr_row(group_id = "G047")),
    "no group ledger"
  )
})

test_that("an unknown group_id is refused", {
  ids <- tibble::tibble(SAMPLE_TISSUE = "Muscle", group_id = "G047")
  expect_error(
    apply_unit_corrections(corr_data(), corr_row(group_id = "G999"), ids = ids),
    "unknown group"
  )
})

test_that("a group_id selector restricts to that group's rows", {
  ids <- tibble::tibble(SAMPLE_TISSUE = "Muscle", group_id = "G047")
  out <- apply_unit_corrections(
    corr_data(), corr_row(group_id = "G047"), ids = ids
  )
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

# Refusals ------------------------------------------------------------------

test_that("a correction matching no rows aborts", {
  expect_error(
    apply_unit_corrections(corr_data(), corr_row(comment_match = "nothing here")),
    "matches no rows"
  )
})

test_that("recorded row_ids that no longer match abort, naming the drift", {
  expect_error(
    apply_unit_corrections(corr_data(), corr_row(row_ids = "R1")),
    "no longer matches"
  )
  expect_error(
    apply_unit_corrections(corr_data(), corr_row(row_ids = "R1")),
    "R2"
  )
})

test_that("recorded row_ids that still match are accepted", {
  out <- apply_unit_corrections(corr_data(), corr_row(row_ids = "R1;R2"))
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

test_that("row id order in the cell does not matter", {
  out <- apply_unit_corrections(corr_data(), corr_row(row_ids = "R2; R1"))
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1, 2, 3, 4))
})

test_that("a row matched by two corrections aborts", {
  two <- dplyr::bind_rows(
    corr_row(correction_id = "C001"),
    corr_row(correction_id = "C002", comment_match = NA, value_min = 500)
  )
  expect_error(apply_unit_corrections(corr_data(), two), "overlaps")
})

test_that("two corrections touching disjoint rows are fine", {
  two <- dplyr::bind_rows(
    corr_row(correction_id = "C001", comment_match = NA, value_min = 1500),
    corr_row(correction_id = "C002", comment_match = NA, value_max = 5, factor = 10)
  )
  out <- apply_unit_corrections(corr_data(), two)
  expect_equal(out$MEASURED_VALUE_STANDARD, c(1000, 2, 30, 40))
})

test_that("value selectors see the original values, not corrected ones", {
  # Order-independence. If matching happened as each correction was applied,
  # C002's value_max would catch R2 after C001 had scaled it down into range,
  # and the answer would depend on the row order of the CSV.
  two <- dplyr::bind_rows(
    corr_row(correction_id = "C001", comment_match = NA, value_min = 1500),
    corr_row(correction_id = "C002", comment_match = NA, value_max = 5, factor = 10)
  )
  forward <- apply_unit_corrections(corr_data(), two)
  reversed <- apply_unit_corrections(corr_data(), two[c(2, 1), ])
  expect_equal(forward$MEASURED_VALUE_STANDARD, reversed$MEASURED_VALUE_STANDARD)
  expect_equal(forward$MEASURED_VALUE_STANDARD, c(1000, 2, 30, 40))
})

test_that("data with no row_id aborts", {
  d <- corr_data()[, setdiff(names(corr_data()), "row_id")]
  expect_error(apply_unit_corrections(d, corr_row()), "row_id")
})

# validate_unit_corrections -------------------------------------------------

test_that("an empty table validates", {
  expect_silent(validate_unit_corrections(empty_unit_corrections()))
})

test_that("duplicate correction_id is refused", {
  two <- dplyr::bind_rows(corr_row(), corr_row())
  expect_error(validate_unit_corrections(two), "duplicate")
})

test_that("a non-positive or non-finite factor is refused", {
  expect_error(validate_unit_corrections(corr_row(factor = 0)), "finite and positive")
  expect_error(validate_unit_corrections(corr_row(factor = -1)), "finite and positive")
  expect_error(validate_unit_corrections(corr_row(factor = NA_real_)), "finite and positive")
})

test_that("a correction with no reason or no evidence is refused", {
  expect_error(validate_unit_corrections(corr_row(reason = NA)), "required")
  expect_error(validate_unit_corrections(corr_row(evidence = NA)), "required")
})

test_that("a correction with no selector at all is refused", {
  expect_error(
    validate_unit_corrections(corr_row(comment_match = NA)),
    "no selector"
  )
})

test_that("an inverted value range is refused", {
  expect_error(
    validate_unit_corrections(corr_row(value_min = 10, value_max = 1)),
    "below"
  )
})

test_that("a missing correction_id is refused", {
  expect_error(
    validate_unit_corrections(corr_row(correction_id = NA)),
    "correction_id"
  )
})

# read_unit_corrections -----------------------------------------------------

test_that("a missing file gives an empty table rather than an error", {
  out <- read_unit_corrections(file.path(tempdir(), "nope.csv"))
  expect_equal(nrow(out), 0L)
  expect_true("correction_id" %in% names(out))
})

test_that("a header-only file gives an empty table", {
  p <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(empty_unit_corrections(), p)
  expect_equal(nrow(read_unit_corrections(p)), 0L)
})

test_that("an extra comma is caught by field count, not by a confusing downstream error", {
  # The real failure, 2026-08-06. readr truncates the row to the header width
  # silently, records nothing in problems(), and every column after the extra
  # comma shifts one place.
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(names(empty_unit_corrections()), collapse = ","),
      '"C001",,"UrbanFjord","ug/g",,,,0.001,,"why","what","2026-08-06"'
    ),
    p
  )
  expect_error(read_unit_corrections(p), "wrong number of fields")
  expect_error(read_unit_corrections(p), "line 2")
})

test_that("a row with too few fields is refused too", {
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(names(empty_unit_corrections()), collapse = ","),
      '"C001",,"UrbanFjord","ug/g",,0.001,,"why","what","2026-08-06"'
    ),
    p
  )
  expect_error(read_unit_corrections(p), "wrong number of fields")
})

test_that("quoted fields containing commas do not trip the field count", {
  p <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    corr_row(reason = "Comment states ug/g, multiplied by 1000, wrongly."),
    p
  )
  expect_equal(nrow(read_unit_corrections(p)), 1L)
})

test_that("leading spaces before quoted fields are tolerated", {
  # How a hand-edited row usually arrives.
  p <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(names(empty_unit_corrections()), collapse = ","),
      '"C001", "", "UrbanFjord", "ug/g", , ,0.001, , "why", "what", "2026-08-06"'
    ),
    p
  )
  out <- read_unit_corrections(p)
  expect_equal(out$factor[1], 0.001)
  expect_equal(out$campaign_name_short[1], "UrbanFjord")
  expect_true(is.na(out$group_id[1]))
})

test_that("a file missing required columns is refused", {
  p <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(correction_id = "C001"), p)
  expect_error(read_unit_corrections(p), "missing required column")
})

test_that("a valid file round-trips and blank cells become NA", {
  p <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(corr_row(campaign_name_short = ""), p)
  out <- read_unit_corrections(p)
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$campaign_name_short[1]))
  expect_equal(out$factor[1], 0.001)
})

# split_row_ids -------------------------------------------------------------

test_that("split_row_ids handles NA, blank, single and spaced lists", {
  expect_equal(split_row_ids(NA_character_), character(0))
  expect_equal(split_row_ids(""), character(0))
  expect_equal(split_row_ids("R1"), "R1")
  expect_equal(split_row_ids("R1; R2 ;R3"), c("R1", "R2", "R3"))
  expect_equal(split_row_ids(character(0)), character(0))
})

# report_unit_corrections ---------------------------------------------------

test_that("nothing corrected gives an empty report and no message", {
  out <- apply_unit_corrections(corr_data(), empty_unit_corrections())
  rep <- report_unit_corrections(out)
  expect_equal(nrow(rep), 0L)
  expect_silent(report_unit_correction_status(rep))
})

test_that("the report counts measurements and rows separately", {
  # CLAUDE.md 4.4.-1: n is sum(MEASURED_N), rows are rows, and they differ.
  out <- apply_unit_corrections(corr_data(), corr_row())
  rep <- report_unit_corrections(out)
  expect_equal(sum(rep$n_rows_corrected), 2L)
  expect_equal(sum(rep$n_corrected), 3L)
})

test_that("the report names the correction and factor", {
  rep <- report_unit_corrections(apply_unit_corrections(corr_data(), corr_row()))
  expect_equal(unique(rep$unit_correction_id), "C001")
  expect_equal(unique(rep$unit_correction_factor), 0.001)
})

test_that("applied corrections are announced on every build", {
  rep <- report_unit_corrections(apply_unit_corrections(corr_data(), corr_row()))
  expect_message(report_unit_correction_status(rep), "overridden")
})

# write_unit_corrections_template -------------------------------------------

test_that("the template writes headers and refuses to overwrite", {
  p <- withr::local_tempfile(fileext = ".csv")
  write_unit_corrections_template(p)
  expect_equal(names(readr::read_csv(p, show_col_types = FALSE)),
               names(empty_unit_corrections()))
  expect_error(write_unit_corrections_template(p), "refusing to overwrite")
})
