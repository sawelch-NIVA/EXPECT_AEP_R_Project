# Tests ----
#
# pick_common_name() only. The fetchers hit live APIs, so they are exercised by a
# real run rather than here; this file covers the picking and the cache/join
# logic, which is where the bugs were.

test_that("parenthesised names are dropped", {
  # The original pattern "[()\\[\\]]" is malformed: in a POSIX bracket expression
  # the `]` closes the class early, so it only matched a paren FOLLOWED BY a
  # literal `]`. "(Common) Atlantic mackerel shark" went straight through.
  expect_equal(
    pick_common_name(c("(common) Atlantic mackerel shark", "American porbeagle")),
    "American porbeagle"
  )
  expect_equal(
    pick_common_name(c("name [sic]", "clean name")),
    "Clean name"
  )
})

test_that("the first name wins, not the shortest", {
  # A shortest-name rule returned "Popweed" for Fucus vesiculosus rather than
  # bladder wrack, and "Blue dog" for Lamna nasus, which is a porbeagle shark.
  # Shortness has no relationship to how well known a name is.
  expect_equal(
    pick_common_name(c("bladder wrack", "popweed", "rockweed")),
    "Bladder wrack"
  )
})

test_that("only the first letter is capitalised", {
  # str_to_sentence() lowercases the rest, turning "Atlantic" into "atlantic" and
  # "European" into "european".
  expect_equal(
    pick_common_name("Common Northern European opossum shrimp"),
    "Common Northern European opossum shrimp"
  )
  expect_equal(pick_common_name("bladder wrack"), "Bladder wrack")
})

test_that("everything parenthesised falls back rather than returning nothing", {
  # Better a slightly ugly name than an empty cell.
  expect_equal(pick_common_name("(only) name"), "(only) name")
})

test_that("empty and all-NA input give NA, not an error", {
  # Most copepods and amphipods have no English vernacular at all; WoRMS answers
  # 204 No Content. That is the expected path, not an exceptional one.
  expect_true(is.na(pick_common_name(character(0))))
  expect_true(is.na(pick_common_name(c(NA_character_, NA_character_))))
  expect_true(is.na(pick_common_name(c("", NA_character_))))
})

# ---- Cache and join behaviour ------------------------------------------

test_that("get_common_names does not query NA species", {
  # unique() on the input column includes NA, which was being sent to the API and
  # cached as a row that could never match, so it was re-queried every run.
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      scientific_name = "Gadus morhua",
      common_name = "Atlantic cod",
      db = "worms",
      date_retrieved = Sys.Date()
    ),
    cache
  )
  data <- data.frame(SAMPLE_SPECIES = c("Gadus morhua", NA, "Gadus morhua"))

  # dbs = character(0) means no fetcher runs, so this cannot touch the network.
  out <- get_common_names(
    data,
    cache_path = cache,
    dbs = character(0),
    verbose = FALSE
  )
  expect_equal(nrow(out), 3)
  expect_equal(out$SPECIES_COMMON_NAME, c("Atlantic cod", NA, "Atlantic cod"))
  # Nothing appended for the NA species.
  expect_equal(nrow(readr::read_csv(cache, show_col_types = FALSE)), 1)
})

test_that("a duplicated cache cannot inflate the row count", {
  # A left join against a cache with two rows per species multiplies the
  # measurement data. This used to be unguarded.
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      scientific_name = c("Gadus morhua", "Gadus morhua"),
      common_name = c("Atlantic cod", "Cod"),
      db = c("worms", "ncbi"),
      date_retrieved = Sys.Date()
    ),
    cache
  )
  data <- data.frame(SAMPLE_SPECIES = rep("Gadus morhua", 5))

  out <- get_common_names(
    data,
    cache_path = cache,
    dbs = character(0),
    verbose = FALSE
  )
  expect_equal(nrow(out), 5)
  # The earlier database in `dbs` order wins; here neither is in dbs, so the
  # cache order decides, but the count must not change either way.
  expect_equal(length(unique(out$SPECIES_COMMON_NAME)), 1)
})

test_that("database preference order decides which name is used", {
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      scientific_name = c("Gadus morhua", "Gadus morhua"),
      common_name = c("From ncbi", "From worms"),
      db = c("ncbi", "worms"),
      date_retrieved = Sys.Date()
    ),
    cache
  )
  data <- data.frame(SAMPLE_SPECIES = "Gadus morhua")

  worms_first <- get_common_names(
    data, cache_path = cache, dbs = c("worms", "ncbi"), verbose = FALSE
  )
  expect_equal(worms_first$SPECIES_COMMON_NAME, "From worms")

  ncbi_first <- get_common_names(
    data, cache_path = cache, dbs = c("ncbi", "worms"), verbose = FALSE
  )
  expect_equal(ncbi_first$SPECIES_COMMON_NAME, "From ncbi")
})

test_that("an existing output column is replaced, not duplicated", {
  # Re-running on already-named data used to silently overwrite, and a second
  # join would otherwise produce .x/.y suffixed columns.
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      scientific_name = "Gadus morhua",
      common_name = "Atlantic cod",
      db = "worms",
      date_retrieved = Sys.Date()
    ),
    cache
  )
  data <- data.frame(
    SAMPLE_SPECIES = "Gadus morhua",
    SPECIES_COMMON_NAME = "stale value"
  )
  out <- get_common_names(
    data, cache_path = cache, dbs = character(0), verbose = FALSE
  )
  expect_equal(out$SPECIES_COMMON_NAME, "Atlantic cod")
  expect_false(any(grepl("\\.x$|\\.y$", names(out))))
})

test_that("a malformed cache fails loudly", {
  # The cache is hand-editable, so a dropped column should not surface as a
  # confusing dplyr error deeper in.
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(scientific_name = "Gadus morhua"), cache)
  expect_error(
    get_common_names(
      data.frame(SAMPLE_SPECIES = "Gadus morhua"),
      cache_path = cache,
      dbs = character(0),
      verbose = FALSE
    ),
    "missing column"
  )
})
