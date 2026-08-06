# Tests for R/fct_row_ids.R.
#
# Synthetic fixtures only, per CLAUDE.md section 5: these must run in seconds
# and must not break when the target store is rebuilt.

make_rows <- function(
  sample_id = c("A", "B", "C"),
  subsample = c("1", "1", "1"),
  measured_type = "Concentration",
  ...
) {
  tibble::tibble(
    SAMPLE_ID = sample_id,
    SUBSAMPLE = subsample,
    MEASURED_TYPE = measured_type,
    MEASURED_VALUE = seq_along(sample_id),
    ...
  )
}

# add_row_ids --------------------------------------------------------------

test_that("unique SAMPLE_IDs pass through unchanged", {
  out <- add_row_ids(make_rows())
  expect_equal(out$row_id, c("A", "B", "C"))
})

test_that("row_id is the first column and the input is otherwise preserved", {
  input <- make_rows()
  out <- add_row_ids(input)
  expect_equal(names(out)[1], "row_id")
  expect_equal(out[names(input)], input)
})

test_that("colliding SAMPLE_IDs are disambiguated by SUBSAMPLE", {
  out <- add_row_ids(make_rows(
    sample_id = c("A", "A", "B"),
    subsample = c("cod", "crab", "1")
  ))
  expect_equal(out$row_id, c("A|cod", "A|crab", "B"))
  expect_false(anyDuplicated(out$row_id) > 0)
})

test_that("rows that were already unique keep the bare SAMPLE_ID", {
  # The point of conditional disambiguation: B is untouched by A's collision.
  out <- add_row_ids(make_rows(
    sample_id = c("A", "A", "B"),
    subsample = c("cod", "crab", "cod")
  ))
  expect_equal(out$row_id[3], "B")
})

test_that("MEASURED_TYPE is used only when SUBSAMPLE leaves a tie", {
  out <- add_row_ids(make_rows(
    sample_id = c("A", "A"),
    subsample = c("1", "1"),
    measured_type = c("Concentration", "Load")
  ))
  expect_equal(out$row_id, c("A|1|Concentration", "A|1|Load"))
})

test_that("the id is a pure function of content, not of row order", {
  # The property the whole scheme rests on: shuffling the input, or inserting a
  # row before it, must not change any existing row's id.
  base <- make_rows(sample_id = c("A", "A", "B"), subsample = c("x", "y", "1"))
  shuffled <- base[c(3, 1, 2), ]
  inserted <- dplyr::bind_rows(
    make_rows(sample_id = "Z", subsample = "1"),
    base
  )

  ref <- add_row_ids(base)
  expect_equal(add_row_ids(shuffled)$row_id, ref$row_id[c(3, 1, 2)])
  expect_equal(add_row_ids(inserted)$row_id[-1], ref$row_id)
})

test_that("a positional counter is never used as a fallback", {
  # Identical on every disambiguator: must abort, not invent "A_1" / "A_2".
  dat <- make_rows(sample_id = c("A", "A"), subsample = c("1", "1"))
  expect_error(add_row_ids(dat), "ambiguous")
})

test_that("missing SAMPLE_ID aborts", {
  dat <- make_rows(sample_id = c("A", NA, "C"))
  expect_error(add_row_ids(dat), "missing")
})

test_that("absent id column aborts", {
  expect_error(
    add_row_ids(tibble::tibble(x = 1)),
    "no .*SAMPLE_ID.* column|SAMPLE_ID"
  )
})

test_that("absent disambiguator columns degrade to an abort, not a crash", {
  dat <- tibble::tibble(SAMPLE_ID = c("A", "A"))
  expect_error(add_row_ids(dat), "ambiguous")
})

test_that("a single disambiguator column is enough on its own", {
  dat <- tibble::tibble(
    SAMPLE_ID = c("A", "A"),
    SUBSAMPLE = c("cod", "crab")
  )
  expect_equal(add_row_ids(dat)$row_id, c("A|cod", "A|crab"))
})

test_that("an empty table returns an empty row_id column", {
  dat <- make_rows()[0, ]
  out <- add_row_ids(dat)
  expect_equal(nrow(out), 0L)
  expect_true("row_id" %in% names(out))
})

test_that("NA in a disambiguator still yields a distinct id", {
  # The halibut case: SUBSAMPLE present, species/tissue NA. The row must still
  # get an id rather than colliding away.
  dat <- tibble::tibble(
    SAMPLE_ID = c("A", "A"),
    SUBSAMPLE = c("cod", NA_character_)
  )
  out <- add_row_ids(dat)
  expect_equal(anyDuplicated(out$row_id), 0L)
})

test_that("the column name is configurable", {
  out <- add_row_ids(make_rows(), col = "measurement_id")
  expect_true("measurement_id" %in% names(out))
})

# report_row_id_collisions -------------------------------------------------

test_that("no collisions gives a zero-row report", {
  out <- report_row_id_collisions(add_row_ids(make_rows()))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("row_id", "sample_id", "n_sharing") %in% names(out)))
})

test_that("collisions are reported one row per affected measurement", {
  dat <- add_row_ids(make_rows(
    sample_id = c("A", "A", "A", "B"),
    subsample = c("x", "y", "z", "1")
  ))
  out <- report_row_id_collisions(dat)
  expect_equal(nrow(out), 3L)
  expect_true(all(out$n_sharing == 3L))
  expect_false("B" %in% out$sample_id)
})

test_that("the report survives an absent source_file_measurements column", {
  dat <- add_row_ids(make_rows(
    sample_id = c("A", "A"),
    subsample = c("x", "y")
  ))
  out <- report_row_id_collisions(dat)
  expect_equal(nrow(out), 2L)
  expect_true(all(is.na(out$source_file)))
})

test_that("source_file is carried through as a basename when present", {
  dat <- add_row_ids(make_rows(
    sample_id = c("A", "A"),
    subsample = c("x", "y"),
    source_file_measurements = c("a/b/c.csv", "a/b/c.csv")
  ))
  out <- report_row_id_collisions(dat)
  expect_equal(unique(out$source_file), "c.csv")
})

test_that("the report degrades on a table with no row_id", {
  expect_equal(nrow(report_row_id_collisions(make_rows())), 0L)
})

test_that("report_row_id_status warns only when there are collisions", {
  expect_silent(report_row_id_status(empty_row_id_collisions()))
  dat <- add_row_ids(make_rows(
    sample_id = c("A", "A"),
    subsample = c("x", "y")
  ))
  expect_warning(
    report_row_id_status(report_row_id_collisions(dat)),
    "disambiguated"
  )
})
