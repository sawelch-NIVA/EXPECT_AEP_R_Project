# Tests for the per-group data slice that lets crew workers receive one group's
# rows instead of the whole table. See targets.qmd section 6.
#
# Synthetic fixtures only, deliberately: these must run in seconds and must not
# break when the target store is rebuilt.

# One group of interest, plus rows that probe each boundary of the slice.
fake_triage_data <- function() {
  tibble::tibble(
    ENVIRON_COMPARTMENT = c(
      "Biota", "Biota", "Biota", "Biota", "Biota", "Aquatic", "Biota"
    ),
    ENVIRON_COMPARTMENT_SUB = c(
      "Marine", "Marine", "Marine", "Marine", "Marine", "Sediment", "Marine"
    ),
    SPECIES_GROUP = c("Fish", "Fish", "Fish", "Fish", "Fish", NA, NA),
    SAMPLE_SPECIES = c(
      "Gadus morhua", "Gadus morhua", "Gadus morhua",
      "Clupea harengus", "Gadus morhua", NA, NA
    ),
    SAMPLE_TISSUE = c(
      "Liver", "Liver", "Liver", "Liver", "Muscle", NA, NA
    ),
    SITE_GEOGRAPHIC_FEATURE = c(
      "Coastal", "Coastal", "Open", "Coastal", "Coastal", "Coastal", "Coastal"
    ),
    SITE_GEOGRAPHIC_FEATURE_SUB = c(
      "Fjord", "Fjord", "Ocean", "Fjord", "Fjord", "Fjord", "Fjord"
    ),
    MEASURED_UNIT_STANDARD = c(
      "mg_kg_wet", "mg_kg_dry", "mg_kg_wet", "mg_kg_wet",
      "mg_kg_wet", "mg_L", "mg_kg_wet"
    ),
    MEASURED_VALUE_STANDARD = c(1, 2, 3, 4, 5, 6, 7)
  )
}

# Rows 1-3 belong to this group's slice: exact match, unit-relaxed, geo-relaxed.
# Row 4 differs by species, row 5 by tissue, rows 6-7 are the NA cases.
fake_triage_group <- function() {
  tibble::tibble(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Marine",
    SPECIES_GROUP = "Fish",
    SAMPLE_SPECIES = "Gadus morhua",
    SAMPLE_TISSUE = "Liver",
    SITE_GEOGRAPHIC_FEATURE = "Coastal",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Fjord",
    MEASURED_UNIT_STANDARD = "mg_kg_wet",
    group_slug = "biota_marine_fish_gadus_morhua_liver"
  )
}

test_that("relaxed columns are exactly what the panels actually relax", {
  # Guards the failure mode called out in triage_relaxed_cols(): a panel starts
  # relaxing a new column, the slice is not widened to match, and that panel
  # silently loses rows.
  #
  # Walks the plotting function's AST for filter_to_group() calls and evaluates
  # their exclude_cols argument, rather than grepping deparsed source, so it
  # cannot pass vacuously on a formatting change.
  found <- character(0)
  walk <- function(x) {
    if (!is.call(x)) {
      return(invisible(NULL))
    }
    if (identical(x[[1]], quote(filter_to_group))) {
      m <- match.call(filter_to_group, x, expand.dots = FALSE)
      if (!is.null(m$exclude_cols)) {
        found <<- c(found, eval(m$exclude_cols))
      }
    }
    for (i in seq_along(x)) {
      el <- x[[i]]
      # Empty symbols appear for blank subscripts, e.g. data[keep, , drop = ...].
      if (is.symbol(el) && !nzchar(as.character(el))) next
      walk(el)
    }
    invisible(NULL)
  }
  walk(body(write_triage_plots_for_group))

  # The function must actually relax something, or the walk found nothing and
  # the rest of this test would be meaningless.
  expect_gt(length(found), 0)
  expect_setequal(unique(found), triage_relaxed_cols())
})

test_that("slice keeps rows the relaxed panels need", {
  d <- fake_triage_data()
  grp <- fake_triage_group()
  s <- triage_group_slice(d, grp)

  # Rows 1 (strict), 2 (other unit, panel a) and 3 (other geography, panel d).
  expect_equal(nrow(s), 3)
  expect_equal(sort(s$MEASURED_VALUE_STANDARD), c(1, 2, 3))
})

test_that("slice drops rows no panel can need", {
  d <- fake_triage_data()
  grp <- fake_triage_group()
  s <- triage_group_slice(d, grp)

  expect_false(4 %in% s$MEASURED_VALUE_STANDARD) # different species
  expect_false(5 %in% s$MEASURED_VALUE_STANDARD) # different tissue
  expect_false(6 %in% s$MEASURED_VALUE_STANDARD) # different compartment
})

test_that("slicing is transparent to every subset the panels derive", {
  # THE invariant that makes this refactor safe: filtering the slice gives the
  # same rows as filtering the full table, for the strict group and for both
  # relaxations. If this holds, the PNGs cannot change.
  d <- fake_triage_data()
  grp <- fake_triage_group()
  s <- triage_group_slice(d, grp)

  expect_identical(filter_to_group(s, grp), filter_to_group(d, grp))

  expect_identical(
    filter_to_group(s, grp, exclude_cols = "MEASURED_UNIT_STANDARD"),
    filter_to_group(d, grp, exclude_cols = "MEASURED_UNIT_STANDARD")
  )

  expect_identical(
    filter_to_group(
      s, grp,
      exclude_cols = c("SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB")
    ),
    filter_to_group(
      d, grp,
      exclude_cols = c("SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB")
    )
  )
})

test_that("NA grouping values match NA rows, not everything", {
  # NA group values are common in the non-biota compartments, and a plain ==
  # filter would drop them. Rows 6 and 7 both carry NA species/tissue.
  d <- fake_triage_data()
  grp <- tibble::tibble(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = "Marine",
    SPECIES_GROUP = NA_character_,
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    SITE_GEOGRAPHIC_FEATURE = "Coastal",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Fjord",
    MEASURED_UNIT_STANDARD = "mg_kg_wet"
  )
  s <- triage_group_slice(d, grp)

  # Only row 7: row 6 is Aquatic/Sediment, so it fails the compartment match.
  expect_equal(nrow(s), 1)
  expect_equal(s$MEASURED_VALUE_STANDARD, 7)
})

test_that("a group matching nothing gives an empty slice, not an error", {
  d <- fake_triage_data()
  grp <- fake_triage_group()
  grp$SAMPLE_SPECIES <- "Salmo salar"

  s <- triage_group_slice(d, grp)
  expect_equal(nrow(s), 0)
  expect_identical(names(s), names(d))
})

test_that("split_triage_data pairs each group with its own slice", {
  # The ordering trap: elements carry their own grp, so nothing downstream has
  # to assume the groups table and the branch order agree.
  d <- fake_triage_data()
  g1 <- fake_triage_group()
  g2 <- fake_triage_group()
  g2$SAMPLE_SPECIES <- "Clupea harengus"
  g2$group_slug <- "biota_marine_fish_clupea_harengus_liver"
  groups <- rbind(g1, g2)

  out <- split_triage_data(d, groups)

  expect_length(out, 2)
  expect_named(out[[1]], c("grp", "data"))

  # Each element's slice must actually be that element's group.
  for (el in out) {
    expect_identical(
      el$data,
      triage_group_slice(d, el$grp)
    )
    expect_equal(nrow(el$grp), 1)
  }

  expect_equal(out[[1]]$grp$SAMPLE_SPECIES, "Gadus morhua")
  expect_equal(out[[2]]$grp$SAMPLE_SPECIES, "Clupea harengus")
  expect_equal(nrow(out[[2]]$data), 1) # only row 4
})

test_that("split_triage_data handles a single-group table", {
  d <- fake_triage_data()
  out <- split_triage_data(d, fake_triage_group())

  expect_length(out, 1)
  expect_equal(nrow(out[[1]]$data), 3)
})
