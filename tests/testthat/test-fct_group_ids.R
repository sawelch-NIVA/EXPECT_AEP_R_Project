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

# ---- format_composite_group_id() ---------------------------------------

composite_codes_fixture <- function() {
  list(
    compartment = data.frame(
      ENVIRON_COMPARTMENT = c("Aquatic", "Terrestrial", "Biota"),
      code = c("W", "E", "B")
    ),
    subcompartment = data.frame(
      ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Marine/Salt Water"),
      code = c("fw", "mw")
    ),
    species_group = data.frame(
      SPECIES_GROUP = c("Fish", "Molluscs"),
      code = c("f", "l")
    ),
    geography = data.frame(
      SITE_GEOGRAPHIC_FEATURE = c("Coastal, fjord", "Ocean, sea, territorial waters"),
      code = c("C", "O")
    ),
    geography_sub = data.frame(
      SITE_GEOGRAPHIC_FEATURE_SUB = c("Water column, pelagic zone", "Water benthos"),
      code = c("wc", "wb")
    ),
    tissue = data.frame(
      SAMPLE_TISSUE = c("Liver", "Muscle"),
      code = c("Liv", "Mus")
    ),
    unit = data.frame(
      MEASURED_UNIT_STANDARD = c("mg/kg (wet)", "mg/kg (dry)", "mg/L"),
      code = c("Mw", "Md", "C")
    )
  )
}

composite_data_fixture <- function(...) {
  fixture <- data.frame(
    group_id = "G014",
    ENVIRON_COMPARTMENT = "Aquatic",
    ENVIRON_COMPARTMENT_SUB = "Freshwater",
    SPECIES_GROUP = NA_character_,
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Water column, pelagic zone",
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    MEASURED_UNIT_STANDARD = "mg/kg (wet)",
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (col in names(overrides)) fixture[[col]] <- overrides[[col]]
  fixture
}

format_fixture <- function(data, species_overrides = NULL) {
  codes <- composite_codes_fixture()
  format_composite_group_id(
    data, codes$compartment, codes$subcompartment, codes$species_group,
    codes$geography, codes$geography_sub, codes$tissue, codes$unit,
    species_overrides
  )
}

test_that("blocks have no internal hyphen and a lowercase child code", {
  # Sam 2026-08-07: "take out the hyphen between B and L and make the second
  # letter small so that the hierarchy is represented" -- capitalisation
  # alone marks parent vs. child within a block; hyphens only separate blocks.
  out <- format_fixture(composite_data_fixture())
  expect_equal(out, "G014-Wfw-Cwc-Mw")
})

test_that("a biota group's compartment block uses a 1-letter species-group code", {
  data <- composite_data_fixture(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic",
    SPECIES_GROUP = "Molluscs"
  )
  out <- format_fixture(data)
  expect_equal(out, "G014-Bl-Cwc-Mw")
})

test_that("an unmapped value warns once and falls back to the bare group_id", {
  data <- composite_data_fixture(ENVIRON_COMPARTMENT_SUB = "Porewater")
  expect_warning(out <- format_fixture(data), "no compartment/geography code")
  expect_equal(out, "G014")
})

test_that("the species/tissue segment is omitted for a group with no species", {
  # Sam 2026-08-07: "this is an optional block, we don't need to include it
  # in stuff without a species" -- no warning either, unlike the
  # compartment/geography gap, since a missing species isn't a lookup gap.
  out <- format_fixture(composite_data_fixture())
  expect_equal(out, "G014-Wfw-Cwc-Mw")
})

test_that("the species/tissue segment appears, abbreviated, when species is known", {
  data <- composite_data_fixture(
    SAMPLE_SPECIES = "Gadus morhua", SAMPLE_TISSUE = "Liver"
  )
  out <- format_fixture(data)
  expect_equal(out, "G014-Wfw-Cwc-G.mor-Liv-Mw")
})

test_that("a single-word species name takes its own first 4 letters", {
  data <- composite_data_fixture(
    SAMPLE_SPECIES = "Chironomidae", SAMPLE_TISSUE = "Whole body"
  )
  codes <- composite_codes_fixture()
  codes$tissue <- rbind(codes$tissue, data.frame(SAMPLE_TISSUE = "Whole body", code = "Wbd"))
  out <- format_composite_group_id(
    data, codes$compartment, codes$subcompartment, codes$species_group,
    codes$geography, codes$geography_sub, codes$tissue, codes$unit
  )
  expect_equal(out, "G014-Wfw-Cwc-Chir-Wbd-Mw")
})

test_that("a species code override wins over the derived code", {
  data <- composite_data_fixture(
    SAMPLE_SPECIES = "Odobenus rosmarus divergens", SAMPLE_TISSUE = "Liver"
  )
  overrides <- data.frame(
    SAMPLE_SPECIES = "Odobenus rosmarus divergens", code = "O.rmd"
  )
  out <- format_fixture(data, overrides)
  expect_equal(out, "G014-Wfw-Cwc-O.rmd-Liv-Mw")
})

test_that("a species with no tissue code yet warns and drops just that segment", {
  data <- composite_data_fixture(
    SAMPLE_SPECIES = "Gadus morhua", SAMPLE_TISSUE = "Otolith"
  )
  expect_warning(out <- format_fixture(data), "no tissue code")
  expect_equal(out, "G014-Wfw-Cwc-Mw")
})

test_that("an unmapped unit falls back to X without warning", {
  data <- composite_data_fixture(MEASURED_UNIT_STANDARD = "ng/g")
  expect_no_warning(out <- format_fixture(data))
  expect_equal(out, "G014-Wfw-Cwc-X")
})

test_that("format_composite_group_id is vectorised across mixed rows", {
  data <- rbind(
    composite_data_fixture(),
    composite_data_fixture(
      group_id = "G020", ENVIRON_COMPARTMENT = "Biota",
      ENVIRON_COMPARTMENT_SUB = "Biota, Aquatic", SPECIES_GROUP = "Fish",
      SITE_GEOGRAPHIC_FEATURE = "Ocean, sea, territorial waters",
      SITE_GEOGRAPHIC_FEATURE_SUB = "Water benthos",
      SAMPLE_SPECIES = "Gadus morhua", SAMPLE_TISSUE = "Muscle",
      MEASURED_UNIT_STANDARD = "mg/L"
    )
  )
  out <- format_fixture(data)
  expect_equal(out, c("G014-Wfw-Cwc-Mw", "G020-Bf-Owb-G.mor-Mus-C"))
})

test_that("format_species_code abbreviates and applies overrides", {
  expect_equal(format_species_code("Gadus morhua"), "G.mor")
  expect_equal(format_species_code("Cancer pagurus"), "C.pag")
  expect_equal(format_species_code("Chironomidae"), "Chir")
  expect_equal(format_species_code(NA_character_), NA_character_)
  expect_equal(format_species_code(""), NA_character_)

  overrides <- data.frame(SAMPLE_SPECIES = "Odobenus rosmarus divergens", code = "O.rmd")
  expect_equal(
    format_species_code("Odobenus rosmarus divergens", overrides), "O.rmd"
  )
  # An un-overridden species in the same call still gets the derived code.
  expect_equal(
    format_species_code(c("Odobenus rosmarus divergens", "Gadus morhua"), overrides),
    c("O.rmd", "G.mor")
  )
})

test_that("the real species list has no unresolved collisions after overrides", {
  skip_if_not(file.exists(here_rel("data/clean/decisions/group_ids.csv")))
  species <- unique(read_group_ids(here_rel("data/clean/decisions/group_ids.csv"))$SAMPLE_SPECIES)
  species <- species[!is.na(species) & nzchar(species)]
  codes <- format_species_code(species)
  # The two genuine-duplicate pairs flagged in misc-todo.md item 14 are left
  # deliberately un-overridden and still collide -- that collision is the
  # point, a visible flag that they might be the same species under two
  # names rather than something to paper over with a distinguishing code.
  # Everything else should be unique or explicitly overridden.
  dupes <- codes[duplicated(codes) | duplicated(codes, fromLast = TRUE)]
  expect_setequal(unique(dupes), c("E.ham", "P.gro"))
})

test_that("the real lookup CSVs cover the ledger except known genuine data gaps", {
  # G127 has no SPECIES_GROUP/SAMPLE_SPECIES/SAMPLE_TISSUE; G087, G094, G131
  # have no SITE_GEOGRAPHIC_FEATURE. All four are unclassified samples, not a
  # lookup that has fallen behind the data. Pinning the exact set here means a
  # REAL coverage gap (a new vocabulary string with no code) shows up as a
  # changed set rather than being lost in the noise.
  skip_if_not(file.exists(here_rel("data/clean/decisions/group_ids.csv")))
  ids <- read_group_ids(here_rel("data/clean/decisions/group_ids.csv"))
  expect_warning(out <- format_composite_group_id(ids), "4 group\\(s\\)")
  expect_equal(
    sort(ids$group_id[out == ids$group_id]),
    c("G087", "G094", "G127", "G131")
  )
  # Every ID is either the bare group_id (a compartment/geography gap) or
  # starts with "<compartment block>-<geography block>", each block one
  # capital letter followed by lowercase; the rest is an optional
  # species/tissue segment and a mandatory trailing unit code.
  expect_true(all(grepl("^G\\d{3}(-[A-Z][a-z]+-[A-Z][a-z]+.*)?$", out)))
})

# ---- The real ledger ---------------------------------------------------

test_that("the committed ledger pins its IDs to specific groups", {
  # Deliberately hard-coded. If a future refactor "helpfully" regenerates the
  # ledger, this fails rather than silently re-pointing every note Sam has
  # written. Update it only when the underlying group genuinely changes.
  skip_if_not(file.exists(here_rel("data/clean/decisions/group_ids.csv")))
  ids <- read_group_ids(here_rel("data/clean/decisions/group_ids.csv"))

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
