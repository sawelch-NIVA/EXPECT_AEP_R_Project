# The multi-AEP layer (PLAN.md P5.3, added 2026-08-06).
#
# The property everything rests on: an AEP is a VIEW over one pool of nodes, so a
# node is scored once and reused. The tests that matter are therefore about
# scoping (does the view narrow correctly?) rather than about drawing.

manifest_fixture <- function(...) {
  base <- tibble::tibble(
    aep_id = c("A001", "A002"),
    label = c("National", "Repparfjorden"),
    scope_note = NA_character_,
    lat_min = c(NA, 70),
    lat_max = c(NA, 71),
    lon_min = c(NA, 23),
    lon_max = c(NA, 25),
    date_min = as.Date(c(NA, "2000-01-01")),
    date_max = as.Date(c(NA, "2020-12-31")),
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

membership_fixture <- function(...) {
  base <- tibble::tibble(
    aep_id = c("A001", "A001", "A001", "A002", "A002"),
    node_id = c("N001", "N002", "N003", "N001", "N002"),
    x = NA_real_,
    y = NA_real_,
    evidence_score = NA_real_,
    evidence_justification = NA_character_,
    quantification_score = NA_real_,
    quantification_justification = NA_character_,
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

manifest_nodes <- function() {
  tibble::tibble(
    node_id = c("N001", "N002", "N003"),
    label = c("Source", "Water", "Mussels"),
    level = c("source", "medium", "organism"),
    node_type = "empirical",
    x = c(0, 1, 2),
    y = c(0, 0, 0),
    lat_min = c(NA, 60, NA),
    lat_max = c(NA, NA, 80),
    essentiality_score = 3,
    plausibility_score = 3,
    evidence_score = 3,
    evidence_justification = "national",
    quantification_score = 3,
    quantification_justification = "national",
    date_min = as.Date(c(NA, "2010-01-01", NA)),
    date_max = as.Date(c(NA, NA, "2015-12-31"))
  )
}

# ---- Scoping the node pool ----------------------------------------------

test_that("an AEP sees only its member nodes", {
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )
  expect_equal(scoped$node_id, c("N001", "N002"))
})

test_that("scope bounds intersect with a node's own, never replace them", {
  # THE PROPERTY THAT KEEPS THIS HONEST. A node restriction says what the node
  # is; an AEP scope says what the diagram is about. Both must hold, so an AEP
  # can narrow a node but must never widen it past its own stated limit.
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )

  # N002 declares lat_min 60; the AEP declares 70. The tighter wins.
  n002 <- scoped[scoped$node_id == "N002", ]
  expect_equal(n002$lat_min, 70)
  # N002 declares no lat_max; it inherits the AEP's.
  expect_equal(n002$lat_max, 71)
  # N002 declares date_min 2010, later than the AEP's 2000. The node's wins.
  expect_equal(n002$date_min, as.Date("2010-01-01"))
})

test_that("a node with no bounds of its own takes the AEP's whole scope", {
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )
  n001 <- scoped[scoped$node_id == "N001", ]
  expect_equal(c(n001$lat_min, n001$lat_max), c(70, 71))
  expect_equal(c(n001$lon_min, n001$lon_max), c(23, 25))
})

test_that("date bounds survive scoping as Dates", {
  # resolve_node_data() ABORTS on a numeric date bound, because a bare year
  # compared against a Date reads as days since 1970 and silently empties the
  # node. ifelse() strips the class and reintroduced exactly that, so this is a
  # regression test for a bug that happened rather than one that might.
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )
  expect_s3_class(scoped$date_min, "Date")
  expect_s3_class(scoped$date_max, "Date")
})

test_that("an unrestricted AEP leaves every node bound untouched", {
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A001"
  )
  original <- manifest_nodes()
  expect_equal(scoped$lat_min, original$lat_min)
  expect_equal(scoped$date_max, original$date_max)
  expect_true(all(is.na(scoped$lon_min)))
})

test_that("layout comes from membership, falling back to the node", {
  m <- membership_fixture()
  m$x[m$aep_id == "A002" & m$node_id == "N001"] <- 5
  m$y[m$aep_id == "A002" & m$node_id == "N001"] <- 6

  scoped <- aep_scope_nodes(manifest_nodes(), m, manifest_fixture(), "A002")
  expect_equal(scoped$x[scoped$node_id == "N001"], 5)
  expect_equal(scoped$y[scoped$node_id == "N001"], 6)
  # N002 leaves both blank, so it keeps the node's own coordinates.
  expect_equal(scoped$x[scoped$node_id == "N002"], 1)
})

test_that("an AEP with no members scopes to zero rows, not an error", {
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture()[0, ], manifest_fixture(), "A002"
  )
  expect_equal(nrow(scoped), 0)
  # Still the same shape, or resolve_node_data() sees a different table.
  expect_true(all(c("lon_min", "lon_max") %in% names(scoped)))
})

test_that("an unknown aep_id is refused rather than silently empty", {
  expect_error(
    aep_scope_nodes(
      manifest_nodes(), membership_fixture(), manifest_fixture(), "A999"
    ),
    "No manifest row"
  )
})

# ---- Scoping edges and boxes --------------------------------------------

test_that("an edge is kept only where both endpoints are in the AEP", {
  # No aep_id on the edges file: which diagrams an edge belongs on follows from
  # which diagrams its nodes are on, so there is one place to change it.
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )
  edges <- tibble::tibble(
    edge_id = c("E001", "E002"),
    from = c("N001", "N002"),
    to = c("N002", "N003"),
    status = "putative"
  )
  expect_equal(aep_scope_edges(edges, scoped)$edge_id, "E001")
})

test_that("a grouping box is intersected, and dropped below two nodes", {
  # A box around one node is not a grouping, it is a second border on a card.
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )
  groups <- tibble::tibble(
    group_key = c("both", "one"),
    label = c("Both", "One"),
    node_ids = c("N001;N002;N003", "N003"),
    notes = NA_character_,
    members = list(c("N001", "N002", "N003"), "N003")
  )
  out <- aep_scope_groups(groups, scoped)
  expect_equal(out$group_key, "both")
  expect_equal(out$members[[1]], c("N001", "N002"))
})

test_that("scoping groups tolerates an absent or empty group table", {
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A002"
  )
  expect_null(aep_scope_groups(NULL, scoped))
  empty <- dplyr::mutate(empty_aep_node_groups(), members = list())
  expect_equal(nrow(aep_scope_groups(empty, scoped)), 0)
})

# ---- Reading the files ---------------------------------------------------

write_csv_fixture <- function(x) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  readr::write_csv(x, path)
  path
}

test_that("a duplicate aep_id is refused", {
  path <- write_csv_fixture(manifest_fixture(aep_id = c("A001", "A001")))
  expect_error(read_aep_manifest(path), "Duplicate aep_id")
})

test_that("an inverted bound is refused rather than emptying the AEP", {
  path <- write_csv_fixture(manifest_fixture(lat_min = c(NA, 71), lat_max = c(NA, 70)))
  expect_error(read_aep_manifest(path), "lat_min above lat_max")

  path <- write_csv_fixture(manifest_fixture(lon_min = c(NA, 25), lon_max = c(NA, 23)))
  expect_error(read_aep_manifest(path), "lon_min above lon_max")
})

test_that("a bare year in the manifest parses to the ends of its interval", {
  m <- manifest_fixture()
  m$date_min <- c(NA, "2000")
  m$date_max <- c(NA, "2020")
  path <- write_csv_fixture(m)
  out <- read_aep_manifest(path)
  expect_equal(out$date_min[2], as.Date("2000-01-01"))
  expect_equal(out$date_max[2], as.Date("2020-12-31"))
})

test_that("membership naming an unknown node or AEP is refused", {
  path <- write_csv_fixture(membership_fixture(node_id = c(
    "N001", "N002", "N003", "N001", "NOPE"
  )))
  expect_error(
    read_aep_membership(path, nodes = manifest_nodes()),
    "unknown node_id"
  )

  path <- write_csv_fixture(membership_fixture(aep_id = c(
    "A001", "A001", "A001", "A002", "A999"
  )))
  expect_error(
    read_aep_membership(path, manifest = manifest_fixture()),
    "unknown aep_id"
  )
})

test_that("a duplicate membership row is refused", {
  path <- write_csv_fixture(membership_fixture(
    aep_id = c("A001", "A001", "A001", "A002", "A002"),
    node_id = c("N001", "N001", "N003", "N001", "N002")
  ))
  expect_error(read_aep_membership(path), "Duplicate membership")
})

test_that("an AEP with no members warns rather than silently drawing nothing", {
  path <- write_csv_fixture(membership_fixture(
    aep_id = rep("A001", 5),
    node_id = c("N001", "N002", "N003", "N004", "N005")
  ))
  expect_warning(
    read_aep_membership(path, manifest = manifest_fixture()),
    "no member nodes"
  )
})

test_that("x, y and notes default in when the file omits them", {
  # The membership file is hand-edited and a column may simply not be typed yet.
  path <- write_csv_fixture(membership_fixture()[, c("aep_id", "node_id")])
  out <- read_aep_membership(path)
  expect_true(all(c("x", "y", "notes") %in% names(out)))
  expect_true(all(is.na(out$x)))
})

test_that("the real manifest and membership files read and scope", {
  # A smoke run against the files actually in the repo, per CLAUDE.md: a
  # testthat pass on fixtures does not prove the hand-edited CSVs are valid.
  manifest <- read_aep_manifest()
  nodes <- read_aep_nodes()
  membership <- read_aep_membership(nodes = nodes, manifest = manifest)

  scoped <- aep_scoped_nodes(nodes, membership, manifest)
  expect_named(scoped, manifest$aep_id)
  expect_true(all(vapply(scoped, nrow, integer(1)) > 0))

  # A002 is the spatially restricted one and must genuinely be smaller.
  expect_lt(nrow(scoped$A002), nrow(scoped$A001))
  expect_equal(scoped$A002$lon_min[1], 23)
})

# ---- The EPEQ split ------------------------------------------------------
# Sam 2026-08-06: "the evidence in repparfjorden for such and such is clearly
# much weaker than the overall national AEP". Correct, and it is specifically
# EVIDENCE. Essentiality and plausibility are claims about the world and do not
# move with a bounding box; evidence and quantification are claims about the
# dataset, and the scope is what changes the dataset.

test_that("an AEP overrides evidence and quantification but not the others", {
  m <- membership_fixture()
  sel <- m$aep_id == "A002" & m$node_id == "N001"
  m$evidence_score[sel] <- 1
  m$evidence_justification[sel] <- "806 measurements from one reference"
  m$quantification_score[sel] <- 2
  m$quantification_justification[sel] <- "no pre-1970s baseline"

  scoped <- aep_scope_nodes(manifest_nodes(), m, manifest_fixture(), "A002")
  n001 <- scoped[scoped$node_id == "N001", ]

  expect_equal(n001$evidence_score, 1)
  expect_equal(n001$quantification_score, 2)
  expect_equal(n001$evidence_justification, "806 measurements from one reference")
  # THE POINT OF THE SPLIT: the two world-claims are untouched.
  expect_equal(n001$essentiality_score, 3)
  expect_equal(n001$plausibility_score, 3)
})

test_that("a blank membership score inherits from the node", {
  # A001 restricts nothing and must need no entries at all.
  scoped <- aep_scope_nodes(
    manifest_nodes(), membership_fixture(), manifest_fixture(), "A001"
  )
  expect_true(all(scoped$evidence_score == 3))
  expect_true(all(scoped$evidence_justification == "national"))
})

test_that("an out-of-range membership score is refused", {
  path <- write_csv_fixture(membership_fixture(evidence_score = c(1, 2, 3, 4, NA)))
  expect_error(read_aep_membership(path), "out-of-range evidence_score")
})

test_that("a membership score with no justification warns", {
  # Same guard as validate_aep_edges(): a score asserts a judgement without
  # saying on what.
  path <- write_csv_fixture(membership_fixture(
    evidence_score = c(NA, NA, NA, 1, NA)
  ))
  expect_warning(read_aep_membership(path), "evidence_justification")
})

test_that("membership scoring columns default in when the file omits them", {
  path <- write_csv_fixture(membership_fixture()[, c("aep_id", "node_id")])
  out <- read_aep_membership(path)
  expect_true(all(aep_scoped_epeq_cols() %in% names(out)))
  expect_true(all(is.na(out$evidence_score)))
})

test_that("the split covers exactly the data-dependent half of EPEQ", {
  # If a criterion is ever added or renamed, these must not drift apart.
  expect_true(all(aep_scoped_epeq_cols() %in% epeq_cols()))
  expect_equal(length(aep_scoped_epeq_cols()), 4)
})

# ---- Inset squeeze compensation (2026-08-08) -----------------------------

test_that("no inset means no compensation at all", {
  sq <- aep_diagram_squeeze(
    draw_inset = FALSE, width = 12, height = 8,
    image_size = 0.19, inset_width = 0.25
  )
  expect_equal(sq$image_size, 0.19)
  expect_equal(sq$device_aspect, 12 / 8)
})

test_that("an inset inflates image_size and shrinks the effective width by the same factor", {
  # REGRESSION. Sam 2026-08-08, comparing A001 (no bounding box) against A002
  # (Repparfjorden, boxed): "AEPs 1 and 2 use different size rectangles. Why?"
  # A002 alone got `+ inset + plot_layout(widths = c(1, inset_width))`, which
  # squeezes its diagram panel to 1/(1+inset_width) of the canvas -- so
  # ggimage::geom_image() (a fraction of PANEL width, whatever that panel
  # turns out to be) drew smaller cards purely because that AEP happened to
  # carry a bounding box. The fix inflates image_size by exactly the factor
  # the panel will later be squeezed by, so the two cancel out.
  sq <- aep_diagram_squeeze(
    draw_inset = TRUE, width = 12, height = 8,
    image_size = 0.19, inset_width = 0.25
  )
  expect_equal(sq$image_size, 0.19 * 1.25)
  expect_equal(sq$device_aspect, (12 / 1.25) / 8)
  # The actual point: apparent card width on the FINAL composed figure is
  # image_size * panel_width, and panel_width is squeezed to width/1.25 by
  # patchwork -- so the two should cancel to the ORIGINAL, uncompensated
  # image_size * width, matching a non-inset AEP's apparent card size exactly.
  apparent_width_inset <- sq$image_size * (12 / 1.25)
  apparent_width_plain <- 0.19 * 12
  expect_equal(apparent_width_inset, apparent_width_plain)
})

test_that("aep_diagram_squeeze also reports the effective width", {
  # aep_diagram_height() needs this directly, added 2026-08-08 so it does not
  # have to re-derive the squeeze factor by a second route.
  plain <- aep_diagram_squeeze(FALSE, 12, 8, 0.19, 0.25)
  expect_equal(plain$effective_width, 12)
  inset <- aep_diagram_squeeze(TRUE, 12, 8, 0.19, 0.25)
  expect_equal(inset$effective_width, 12 / 1.25)
})

# ---- aep_diagram_height() (2026-08-08) -----------------------------------

test_that("a sparsely spaced column stays at the floor", {
  # This is the shape the ORIGINAL working AEPs are in: a handful of nodes at
  # unit y-spacing, which already rendered fine at height = 8. The fix must
  # not inflate a diagram that was never actually crowded.
  placed <- tibble::tibble(node_id = paste0("N", 1:3), x = 0, y = 0:2)
  h <- aep_diagram_height(
    placed, effective_width = 12, image_size = 0.19, card_aspect = 0.75,
    min_height = 8
  )
  expect_equal(h, 8)
})

test_that("a densely packed column grows the canvas past the floor", {
  # Sam 2026-08-08: "10 or so organism nodes ... we can't especially afford
  # to put them in a 1x10 column" at a fixed 12x8in canvas.
  placed <- tibble::tibble(node_id = paste0("N", 1:10), x = 0, y = 0:9)
  h <- aep_diagram_height(
    placed, effective_width = 12, image_size = 0.19, card_aspect = 0.75,
    min_height = 8
  )
  # The formula itself, not a hardcoded number: hh is invariant under
  # height_in * hh (see the function's own doc), so the required height is
  # exactly 2 * (hh_at_min_height * min_height) / (min_gap * fill_fraction).
  ext <- node_card_extent(
    placed, image_size = 0.19, card_aspect = 0.75, device_aspect = 12 / 8,
    x_expand = 0.15, y_expand = 0.12
  )
  expected <- 2 * (ext$hh * 8) / (1 * 0.6)
  expect_equal(h, expected)
  expect_gt(h, 8)
})

test_that("nodes in different columns sharing close y values do not inflate height", {
  # REGRESSION. The first cut of this measured the y-gap across ALL nodes
  # regardless of x, so two DIFFERENT columns landing at similar y -- which do
  # not actually compete for space, since a card's extent is bounded in x --
  # triggered the same inflation a genuinely crowded single column would.
  placed <- tibble::tibble(node_id = c("N1", "N2"), x = c(0, 1), y = c(0, 0.1))
  h <- aep_diagram_height(
    placed, effective_width = 12, image_size = 0.19, card_aspect = 0.75,
    min_height = 8
  )
  expect_equal(h, 8)
})

test_that("fewer than two placed nodes never grows the canvas", {
  placed <- tibble::tibble(node_id = "N1", x = 0, y = 0)
  h <- aep_diagram_height(
    placed, effective_width = 12, image_size = 0.19, card_aspect = 0.75,
    min_height = 8
  )
  expect_equal(h, 8)
})

test_that("unplaced nodes are ignored rather than treated as a huge gap", {
  placed <- tibble::tibble(
    node_id = c("N1", "N2", "N3"), x = c(0, 0, 0), y = c(0, NA, 1)
  )
  h <- aep_diagram_height(
    placed, effective_width = 12, image_size = 0.19, card_aspect = 0.75,
    min_height = 8
  )
  expect_equal(h, 8)
})

# ---- aep_diagram_image_size() (2026-08-11) --------------------------------

test_that("a sparsely spaced row keeps the caller's image_size", {
  # A single row with plenty of x-spacing -- the shape most AEPs are in.
  placed <- tibble::tibble(node_id = paste0("N", 1:3), x = 0:2, y = 0)
  s <- aep_diagram_image_size(placed, image_size = 0.19)
  expect_equal(s, 0.19)
})

test_that("a densely packed row shrinks image_size below the caller's value", {
  # The L-shaped source layout (2026-08-11): six nodes at 1.3-unit x-spacing
  # in one row, added to a pool whose x already ran 0-5.2, growing rx by
  # about 50% while the LOCAL gap between adjacent nodes stayed the same --
  # exactly the shape that clipped card text against its neighbour.
  placed <- tibble::tibble(
    node_id = paste0("N", 1:8), x = seq(0, 9.1, by = 1.3), y = 0
  )
  s <- aep_diagram_image_size(placed, image_size = 0.19)
  expect_lt(s, 0.19)

  # The formula itself, not a hardcoded number: rx does not depend on
  # image_size (see the function's own doc), so it can be read straight off
  # node_card_extent() at any image_size and the required value recovered
  # directly from the row's own min gap.
  rx <- node_card_extent(placed, image_size = 0.19, x_expand = 0.15)$rx
  expected <- min(0.19, 1.3 * 0.6 / rx)
  expect_equal(s, expected)
})

test_that("aep_diagram_image_size() never grows past the caller's value", {
  # Widely spaced nodes have room to spare, but the function's job is only to
  # shrink for crowding, not to blow cards up to fill the space -- same
  # floor-not-ceiling philosophy as aep_diagram_height().
  placed <- tibble::tibble(node_id = c("N1", "N2"), x = c(0, 100), y = 0)
  s <- aep_diagram_image_size(placed, image_size = 0.19)
  expect_equal(s, 0.19)
})

test_that("image_size shrinking is measured within each row, not globally", {
  # REGRESSION, mirroring aep_diagram_height()'s own column-only regression
  # test: two nodes in DIFFERENT rows landing at close x values do not
  # actually compete for horizontal space, since a card's extent is bounded
  # in y.
  placed <- tibble::tibble(node_id = c("N1", "N2"), x = c(0, 0.1), y = c(0, 1))
  s <- aep_diagram_image_size(placed, image_size = 0.19)
  expect_equal(s, 0.19)
})

test_that("aep_diagram_image_size() degenerate cases match aep_diagram_height()'s", {
  one_node <- tibble::tibble(node_id = "N1", x = 0, y = 0)
  expect_equal(aep_diagram_image_size(one_node, image_size = 0.19), 0.19)

  with_na <- tibble::tibble(
    node_id = c("N1", "N2", "N3"), x = c(0, NA, 1), y = c(0, 1, 0)
  )
  expect_equal(aep_diagram_image_size(with_na, image_size = 0.19), 0.19)
})
