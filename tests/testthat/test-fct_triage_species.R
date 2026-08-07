# Tests ----
#
# The species-group tier. The property that matters most is that tissue stays
# visible: it moves the measured value further than species does (Fish, mg/kg
# wet: median 5.20 in liver against 0.228 in muscle), so a panel that pooled
# tissues would present tissue variation as species variation.

species_rows <- function(
  n = 60,
  sub = "Biota, Aquatic",
  species_group = "Fish",
  unit = "mg/kg (wet)",
  species = c("Gadus morhua", "Salmo trutta"),
  tissue = c("Liver", "Muscle")
) {
  set.seed(42)
  data.frame(
    ENVIRON_COMPARTMENT = "Biota",
    ENVIRON_COMPARTMENT_SUB = sub,
    SPECIES_GROUP = species_group,
    SAMPLE_SPECIES = rep_len(species, n),
    SAMPLE_TISSUE = rep_len(tissue, n),
    SITE_GEOGRAPHIC_FEATURE = "Coastal, fjord",
    SITE_GEOGRAPHIC_FEATURE_SUB = "Water column, pelagic zone",
    MEASURED_UNIT_STANDARD = unit,
    MEASURED_VALUE_STANDARD = 10^stats::runif(n, -2, 2),
    MEASURED_N = 2L,
    SAMPLING_DATE = as.Date("2015-01-01") + seq_len(n),
    CAMPAIGN_NAME_SHORT = "NorSeal1988",
    stringsAsFactors = FALSE
  )
}

# ---- Band labels --------------------------------------------------------

test_that("bands combine species and tissue", {
  expect_equal(
    species_tissue_label("Gadus morhua", "Liver"),
    "Gadus morhua (Liver)"
  )
})

test_that("missing tissue gives the bare species, not '(NA)'", {
  expect_equal(species_tissue_label("Gadus morhua", NA), "Gadus morhua")
  expect_equal(species_tissue_label("Gadus morhua", ""), "Gadus morhua")
})

test_that("missing species is named rather than dropped", {
  # triage_plot_by_category() filters out NA categories, so an NA species would
  # vanish from the panel without saying so.
  expect_equal(species_tissue_label(NA, "Liver"), "Unknown species (Liver)")
  expect_equal(species_tissue_label(NA, NA), "Unknown species")
})

test_that("band labels are vectorised elementwise", {
  expect_equal(
    species_tissue_label(c("A", "B"), c("Liver", NA)),
    c("A (Liver)", "B")
  )
})

# ---- Group id prefixes --------------------------------------------------

species_ids <- function(d = species_rows(), prefix = "G") {
  key <- triage_group_cols()
  ids <- unique(d[, key])
  ids$group_id <- sprintf("%s%03d", prefix, seq_len(nrow(ids)))
  ids
}

test_that("a band gains the id of the group beneath it", {
  d <- add_species_tissue_col(species_rows())
  out <- add_group_ids_to_bands(d, species_ids())
  expect_setequal(
    unique(out$.species_tissue),
    c("G001 Gadus morhua (Liver)", "G002 Salmo trutta (Muscle)")
  )
})

test_that("a band spanning several groups lists all of them, sorted", {
  # Mytilus edulis (Total soft tissues) in mg/kg (wet) really does cover four
  # groups, split only by geography. The ids are what a lump decision needs.
  d <- species_rows()
  d$SITE_GEOGRAPHIC_FEATURE <- rep_len(c("Coastal, fjord", "Lake"), nrow(d))
  d$SAMPLE_SPECIES <- "Mytilus edulis"
  d$SAMPLE_TISSUE <- "Total soft tissues"
  ids <- species_ids(d)
  # Reversed, so a result in ledger order rather than sorted order would fail.
  ids$group_id <- rev(ids$group_id)

  out <- add_group_ids_to_bands(add_species_tissue_col(d), ids)
  expect_equal(
    unique(out$.species_tissue),
    "G001, G002 Mytilus edulis (Total soft tissues)"
  )
})

test_that("the prefix does not merge or split bands", {
  d <- add_species_tissue_col(species_rows())
  out <- add_group_ids_to_bands(d, species_ids())
  expect_equal(
    dplyr::n_distinct(out$.species_tissue),
    dplyr::n_distinct(d$.species_tissue)
  )
})

test_that("a band absent from the ledger keeps its bare label", {
  # Better a missing prefix than a mangled label such as "NA Gadus morhua".
  d <- add_species_tissue_col(species_rows())
  ids <- species_ids()[1, ]
  out <- add_group_ids_to_bands(d, ids)
  expect_setequal(
    unique(out$.species_tissue),
    c("G001 Gadus morhua (Liver)", "Salmo trutta (Muscle)")
  )
})

test_that("no ledger leaves the labels alone", {
  d <- add_species_tissue_col(species_rows())
  expect_equal(add_group_ids_to_bands(d, NULL), d)
  expect_equal(add_group_ids_to_bands(d, species_ids()[0, ]), d)
})

test_that("an empty node is handled without erroring", {
  d <- add_species_tissue_col(species_rows()[0, ])
  expect_equal(nrow(add_group_ids_to_bands(d, species_ids())), 0)
})

test_that("a duplicated ledger key is an error, not a silently grown table", {
  d <- add_species_tissue_col(species_rows())
  ids <- species_ids()
  ids <- rbind(ids, ids[1, ])
  ids$group_id[nrow(ids)] <- "G999"
  expect_error(add_group_ids_to_bands(d, ids), "duplicate group keys")
})

test_that("NA group-key values still match the ledger", {
  # left_join() matches NA to NA, and SAMPLE_TISSUE is legitimately missing for
  # whole-organism rows. Matching on 'not NA' would leave those bands unlabelled.
  d <- species_rows(species = c("A", "B"), tissue = NA)
  ids <- species_ids(d)
  out <- add_group_ids_to_bands(add_species_tissue_col(d), ids)
  expect_false(any(startsWith(out$.species_tissue, "A")))
  expect_true(all(grepl("^G[0-9]{3} ", out$.species_tissue)))
})

# ---- Node selection -----------------------------------------------------

test_that("a species group with two bands qualifies", {
  nodes <- triage_species_nodes(species_rows())
  expect_equal(nrow(nodes), 1)
  expect_equal(nodes$SPECIES_GROUP, "Fish")
  # rep_len pairs species with tissue, so two bands, not four.
  expect_equal(nodes$n_bands, 2)
  expect_equal(nodes$n_species, 2)
  expect_equal(nodes$n_tissues, 2)
})

test_that("a single band does not qualify", {
  # Biota, Terrestrial / Mammals / mg/kg (dry) in the real data: 317
  # measurements, all Ursus maritimus liver. A one-band panel answers nothing.
  d <- species_rows(species = "Ursus maritimus", tissue = "Liver")
  expect_equal(nrow(triage_species_nodes(d)), 0)
})

test_that("the same species in two tissues is two bands", {
  # The whole point of the tissue decision: this must not collapse to one band.
  d <- species_rows(species = "Gadus morhua", tissue = c("Liver", "Muscle"))
  nodes <- triage_species_nodes(d)
  expect_equal(nodes$n_bands, 2)
  expect_equal(nodes$n_species, 1)
})

test_that("nodes below min_n are dropped", {
  expect_equal(nrow(triage_species_nodes(species_rows(n = 10))), 0)
})

test_that("min_n counts measurements, not rows", {
  # 60 rows at MEASURED_N = 2 is 120 measurements, over a bar of 100 that a row
  # count would fail.
  nodes <- triage_species_nodes(species_rows(n = 60), min_n = 100)
  expect_equal(nodes$n, 120)
  expect_equal(nodes$n_rows, 60)
})

test_that("the unit is part of the node", {
  d <- rbind(species_rows(), species_rows(unit = "mg/kg (dry)"))
  nodes <- triage_species_nodes(d)
  expect_equal(nrow(nodes), 2)
  expect_setequal(
    nodes$MEASURED_UNIT_STANDARD,
    c("mg/kg (wet)", "mg/kg (dry)")
  )
})

test_that("rows with no species group are excluded", {
  d <- species_rows()
  d$SPECIES_GROUP <- NA_character_
  expect_equal(nrow(triage_species_nodes(d)), 0)

  d$SPECIES_GROUP <- ""
  expect_equal(nrow(triage_species_nodes(d)), 0)
})

test_that("empty input returns an empty node table rather than erroring", {
  nodes <- triage_species_nodes(species_rows()[0, ])
  expect_equal(nrow(nodes), 0)
  expect_true(all(c("n_bands", "node_slug") %in% names(nodes)))
})

test_that("nodes carry every group-key column, with a real species group", {
  # Unlike the sub-compartment tier, SPECIES_GROUP is populated here, so
  # thresholds_for_group() can match biota thresholds at this level.
  nodes <- triage_species_nodes(species_rows())
  expect_true(all(triage_group_cols() %in% names(nodes)))
  expect_equal(nodes$SPECIES_GROUP[1], "Fish")
  expect_true(is.na(nodes$SAMPLE_SPECIES[1]))
})

# ---- Reachability -------------------------------------------------------

test_that("a species group with no displayed group beneath it is dropped", {
  # Invertebrates in the real data: 211 measurements as a node, but no single
  # group reaching 100, so the notebook never opens a heading for it and the
  # panel would be written and never referenced.
  d <- species_rows()
  groups <- d[1, triage_group_cols()]
  groups$SPECIES_GROUP <- "Molluscs"
  expect_equal(nrow(triage_species_nodes(d, groups = groups)), 0)
})

test_that("a species group with a displayed group beneath it survives", {
  d <- species_rows()
  groups <- d[1, triage_group_cols()]
  expect_equal(nrow(triage_species_nodes(d, groups = groups)), 1)
})

test_that("groups = NULL skips the reachability check", {
  expect_equal(nrow(triage_species_nodes(species_rows(), groups = NULL)), 1)
})

test_that("filter_reachable_nodes rejects a groups table missing a key column", {
  nodes <- triage_species_nodes(species_rows())
  expect_error(
    filter_reachable_nodes(nodes, data.frame(x = 1), "SPECIES_GROUP"),
    "missing column"
  )
})

# ---- Drawing ------------------------------------------------------------

test_that("the by-species panel builds and writes", {
  d <- species_rows()
  nodes <- triage_species_nodes(d)
  dir <- withr::local_tempdir()

  path <- write_species_overview_for_node(d, nodes[1, ], dir = dir)

  expect_length(path, 1)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 0)
  expect_match(path, "_a_species\\.png$")
})

test_that("the panel draws id-prefixed bands on the y axis", {
  # ggplot_build() rather than inspecting the object: a discrete scale's labels
  # are only resolved when the plot is built.
  d <- species_rows()
  nodes <- triage_species_nodes(d)
  dir <- withr::local_tempdir()

  path <- write_species_overview_for_node(
    d, nodes[1, ],
    dir = dir, ids = species_ids()
  )
  expect_true(file.exists(path))

  p <- triage_plot_by_category(
    add_group_ids_to_bands(
      add_species_tissue_col(filter_to_species_node(d, nodes[1, ])),
      species_ids()
    ),
    ".species_tissue", "t",
    wrap_width = 24
  )
  labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  expect_true(all(grepl("^G[0-9]{3}", labs)))
})

test_that("the panel's categories are species/tissue bands", {
  d <- species_rows()
  nodes <- triage_species_nodes(d)
  node_data <- add_species_tissue_col(filter_to_species_node(d, nodes[1, ]))
  expect_setequal(
    unique(node_data$.species_tissue),
    c("Gadus morhua (Liver)", "Salmo trutta (Muscle)")
  )
})

test_that("many bands truncate with a note rather than growing without bound", {
  d <- species_rows(n = 120, species = paste("Species", 1:40))
  nodes <- triage_species_nodes(d)
  node_data <- add_species_tissue_col(filter_to_species_node(d, nodes[1, ]))
  out <- truncate_categories(node_data, ".species_tissue", max_categories = 25)
  expect_equal(length(unique(out$data$.species_tissue)), 25)
  expect_match(out$note, "25 largest of")
})
