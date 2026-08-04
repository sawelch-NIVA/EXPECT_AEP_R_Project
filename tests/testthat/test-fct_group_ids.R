# Tests ----
#
# The property under test throughout is STABILITY. These IDs end up in hand-written
# notes, so an ID that changes when the data change silently re-points every
# existing reference.

id_summary <- function(n = c(900, 60, 30, 10), species = LETTERS[1:4]) {
  data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = species,
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Not reported",
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    n = n
  )
}

test_that("IDs are zero-padded and prefixed", {
  # Prefixed so they cannot be mistaken for n or rank and so they are greppable;
  # zero-padded so they sort lexically.
  expect_equal(format_group_id(1), "G001")
  expect_equal(format_group_id(245), "G245")
  expect_equal(sort(format_group_id(c(2, 10, 1))), c("G001", "G002", "G010"))
})

test_that("an absent ledger reads as empty rather than erroring", {
  ids <- read_group_ids(tempfile(fileext = ".csv"))
  expect_equal(nrow(ids), 0)
  expect_true(all(c(triage_group_cols(), "group_id") %in% names(ids)))
})

test_that("allocation is by n descending on a fresh ledger", {
  path <- tempfile(fileext = ".csv")
  ledger <- allocate_group_ids(id_summary(), path, verbose = FALSE)
  expect_equal(ledger$group_id, c("G001", "G002", "G003", "G004"))
  expect_equal(ledger$SAMPLE_SPECIES, c("A", "B", "C", "D"))
})

test_that("an ID never changes when n changes", {
  # THE property. Anything rank-derived would fail here: reversing the sizes
  # reverses the ranks, and a rank-derived G001 would move from A to D.
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)

  reversed <- id_summary(n = c(10, 30, 60, 900))
  again <- allocate_group_ids(reversed, path, verbose = FALSE)

  expect_equal(again$group_id[again$SAMPLE_SPECIES == "A"], "G001")
  expect_equal(again$group_id[again$SAMPLE_SPECIES == "D"], "G004")
  expect_equal(nrow(again), 4)
})

test_that("new groups take the next free ID", {
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)
  wider <- id_summary(n = c(900, 60, 30, 10, 5000), species = c(LETTERS[1:4], "E"))
  again <- allocate_group_ids(wider, path, verbose = FALSE)

  # E is now the largest group, but allocation order is irrelevant to an existing
  # ledger: it takes the next number, not the first.
  expect_equal(again$group_id[again$SAMPLE_SPECIES == "E"], "G005")
  expect_equal(again$group_id[again$SAMPLE_SPECIES == "A"], "G001")
})

test_that("a retired ID is never reused", {
  # Reuse is how a note written in March ends up pointing at a different group in
  # September. The next ID is one past the highest EVER issued.
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)

  # D disappears (a species rename, say), then a new group arrives.
  shrunk <- id_summary(n = c(900, 60, 30), species = LETTERS[1:3])
  allocate_group_ids(shrunk, path, verbose = FALSE)
  revived <- id_summary(n = c(900, 60, 30, 5), species = c(LETTERS[1:3], "Z"))
  final <- allocate_group_ids(revived, path, verbose = FALSE)

  expect_equal(final$group_id[final$SAMPLE_SPECIES == "Z"], "G005")
  # D's row is kept, so G004 stays retired rather than being handed to Z.
  expect_true("G004" %in% final$group_id)
  expect_equal(final$group_id[final$SAMPLE_SPECIES == "D"], "G004")
})

test_that("allocation is idempotent", {
  path <- tempfile(fileext = ".csv")
  first <- allocate_group_ids(id_summary(), path, verbose = FALSE)
  second <- allocate_group_ids(id_summary(), path, verbose = FALSE)
  expect_equal(first$group_id, second$group_id)
  expect_equal(nrow(second), 4)
})

test_that("a duplicated ledger key is caught rather than multiplying the data", {
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)
  ledger <- readr::read_csv(path, show_col_types = FALSE)
  doubled <- rbind(ledger, ledger[1, ])
  doubled$group_id[nrow(doubled)] <- "G999"

  expect_error(
    attach_group_ids(id_summary(), doubled),
    "changed the row count"
  )
})

test_that("a duplicated group_id in the ledger is rejected on read", {
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)
  ledger <- readr::read_csv(path, show_col_types = FALSE)
  ledger$group_id[2] <- ledger$group_id[1]
  readr::write_csv(ledger, path, na = "")
  expect_error(read_group_ids(path), "duplicate group_id")
})

test_that("attaching warns about groups with no ID", {
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)
  ledger <- read_group_ids(path)
  wider <- id_summary(n = c(900, 60, 30, 10, 5), species = c(LETTERS[1:4], "E"))
  expect_warning(attach_group_ids(wider, ledger), "no ID yet")
})

test_that("a ledger missing a column fails loudly", {
  path <- tempfile(fileext = ".csv")
  allocate_group_ids(id_summary(), path, verbose = FALSE)
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::select(-"group_id") |>
    readr::write_csv(path, na = "")
  expect_error(read_group_ids(path), "missing column")
})

# ---- The real ledger ---------------------------------------------------

test_that("the committed ledger pins its IDs to specific groups", {
  # Deliberately hard-coded. If a future refactor "helpfully" regenerates the
  # ledger, this fails rather than silently re-pointing every note Sam has
  # written. Update it only when the underlying group genuinely changes.
  skip_if_not(file.exists(here_rel("data/clean/group_ids.csv")))
  ids <- read_group_ids(here_rel("data/clean/group_ids.csv"))

  g001 <- ids[ids$group_id == "G001", ]
  expect_equal(g001$ENVIRON_COMPARTMENT_SUB, "Freshwater")
  expect_equal(g001$MEASURED_UNIT_STANDARD, "mg/L")

  g005 <- ids[ids$group_id == "G005", ]
  expect_equal(g005$SAMPLE_SPECIES, "Mytilus edulis")
  expect_equal(g005$SAMPLE_TISSUE, "Total soft tissues")

  g006 <- ids[ids$group_id == "G006", ]
  expect_equal(g006$SAMPLE_SPECIES, "Gadus morhua")
  expect_equal(g006$SAMPLE_TISSUE, "Liver")

  expect_equal(anyDuplicated(ids$group_id), 0L)
})
