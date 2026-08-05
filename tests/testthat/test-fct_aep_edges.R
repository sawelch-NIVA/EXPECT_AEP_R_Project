# The AEP edge layer and diagram (PLAN.md Phase 4 and P5.1, added 2026-08-05).
#
# The property the whole phase rests on: putative and empirical edges must be
# visually and structurally distinguishable. PLAN.md is explicit that sparse
# edges are a finding rather than a shortfall *provided* the distinction holds,
# and that argument is what lets Sam stop on schedule.

edge_fixture <- function(...) {
  base <- tibble::tibble(
    edge_id = "E001",
    from = "N001",
    to = "N002",
    label = "test edge",
    status = "putative",
    magnitude = NA_real_,
    magnitude_unit = NA_character_,
    magnitude_n = NA_real_,
    magnitude_sd = NA_real_,
    essentiality_score = NA_real_, essentiality_justification = NA_character_,
    plausibility_score = NA_real_, plausibility_justification = NA_character_,
    evidence_score = NA_real_, evidence_justification = NA_character_,
    quantification_score = NA_real_, quantification_justification = NA_character_,
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

edge_nodes <- function() {
  tibble::tibble(
    node_id = c("N001", "N002", "N003"),
    label = c("Source", "Water", "Mussels"),
    level = c("source", "medium", "organism"),
    node_type = "empirical",
    x = c(0, 1, 2),
    y = c(0, 0, 0),
    essentiality_score = 3, plausibility_score = 3,
    evidence_score = 3, quantification_score = 3,
    external_value = NA_real_
  )
}

# ---- Reading and validation --------------------------------------------

test_that("an unknown node id is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(edge_fixture(to = "N999"), path, na = "")
  expect_error(read_aep_edges(path, nodes = edge_nodes()), "unknown node_id")
})

test_that("a self-loop is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(edge_fixture(to = "N001"), path, na = "")
  expect_error(read_aep_edges(path), "from a node to itself")
})

test_that("an unknown status is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(edge_fixture(status = "maybe"), path, na = "")
  expect_error(read_aep_edges(path), "Unrecognised status")
})

test_that("duplicate edge ids are refused and duplicate pairs warn", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::bind_rows(edge_fixture(), edge_fixture()), path, na = ""
  )
  expect_error(read_aep_edges(path), "Duplicate edge_id")

  path2 <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    dplyr::bind_rows(edge_fixture(), edge_fixture(edge_id = "E002")),
    path2, na = ""
  )
  expect_warning(read_aep_edges(path2), "more than one edge")
})

test_that("an empirical edge with no evidence is warned about", {
  # The one that matters: it asserts evidence exists without saying what, and
  # nothing downstream can tell that from a real citation.
  edges <- edge_fixture(status = "empirical")
  expect_warning(
    validate_aep_edges(edges, edge_nodes()),
    "no evidence_justification"
  )
})

test_that("a putative edge carrying a magnitude is warned about", {
  # A contradiction: putative means not evidenced here, so a number attached to
  # it will be read as one that is.
  edges <- edge_fixture(magnitude = 42, magnitude_unit = "kg/yr")
  expect_warning(validate_aep_edges(edges, edge_nodes()), "carry a magnitude")
})

test_that("a magnitude with no unit is warned about", {
  edges <- edge_fixture(
    status = "empirical", evidence_justification = "Comber et al.",
    magnitude = 42
  )
  expect_warning(validate_aep_edges(edges, edge_nodes()), "no unit")
})

test_that("nodes with no edges at all are reported", {
  edges <- edge_fixture()
  expect_warning(validate_aep_edges(edges, edge_nodes()), "no edges at all")
})

test_that("a complete edge validates silently", {
  edges <- edge_fixture(
    status = "empirical",
    evidence_justification = "Comber et al. 2008, table 3",
    magnitude = 42, magnitude_unit = "kg/yr"
  )
  nodes <- edge_nodes()[1:2, ]
  expect_no_warning(validate_aep_edges(edges, nodes))
})

# ---- Geometry -----------------------------------------------------------

test_that("edge coordinates come from the nodes' hand-placed positions", {
  coords <- aep_edge_coords(edge_fixture(), edge_nodes(), trim = 0)
  expect_equal(coords$x, 0)
  expect_equal(coords$xend, 1)
})

test_that("trimming pulls both ends in so arrows clear the labels", {
  coords <- aep_edge_coords(edge_fixture(), edge_nodes(), trim = 0.1)
  expect_gt(coords$x, 0)
  expect_lt(coords$xend, 1)
  # Symmetric.
  expect_equal(coords$x - 0, 1 - coords$xend)
})

test_that("an edge to an unplaced node is dropped rather than drawn to NA", {
  nodes <- edge_nodes()
  nodes$x[2] <- NA_real_
  nodes$y[2] <- NA_real_
  placed <- nodes[!is.na(nodes$x), ]
  coords <- aep_edge_coords(edge_fixture(), placed)
  expect_equal(nrow(coords), 0)
})

# ---- Styling and drawing ------------------------------------------------

test_that("putative and empirical are styled differently in every channel", {
  # The distinction the whole phase argument rests on, asserted rather than
  # assumed: if these ever collapse to the same style, a reader cannot tell an
  # evidenced flow from a guessed one.
  s <- aep_edge_styles()
  for (channel in names(s)) {
    expect_false(
      identical(s[[channel]][["putative"]], s[[channel]][["empirical"]]),
      info = channel
    )
  }
})

test_that("the diagram builds with a mix of edge types", {
  edges <- dplyr::bind_rows(
    edge_fixture(edge_id = "E001", from = "N001", to = "N002"),
    edge_fixture(
      edge_id = "E002", from = "N002", to = "N003", status = "empirical",
      magnitude = 42, magnitude_unit = "kg/yr",
      evidence_justification = "somewhere"
    )
  )
  p <- plot_aep(edge_nodes(), edges)
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})

test_that("the diagram builds with no edges at all", {
  # The state on day one of Phase 4.
  p <- plot_aep(edge_nodes(), empty_aep_edges())
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})

test_that("unplaced nodes give a labelled blank rather than an error", {
  nodes <- edge_nodes()
  nodes$x <- NA_real_
  nodes$y <- NA_real_
  p <- plot_aep(nodes, empty_aep_edges())
  expect_s3_class(p, "ggplot")
})

test_that("node labels gain their statistics when cards are supplied", {
  cards <- tibble::tibble(
    node_id = c("N001", "N002", "N003"),
    geo_mean = c(NA, 0.0021, 1.18),
    unit = c(NA, "mg/L", "mg/kg (wet)"),
    n = c(NA, 4969, 5498)
  )
  p <- plot_aep(edge_nodes(), empty_aep_edges(), cards = cards)
  built <- suppressWarnings(ggplot2::ggplot_build(p))
  labels <- unlist(lapply(built$data, function(d) d$label))
  expect_true(any(grepl("mg/L", labels, fixed = TRUE)))
  # A node with no statistics keeps its bare label rather than showing NA.
  expect_true(any(labels == "Source"))
})

# ---- Progress -----------------------------------------------------------

test_that("progress counts each status once", {
  # REGRESSION. tibble() evaluates sequentially with data masking, so naming the
  # first column `edges` put it in scope for every later expression and
  # `edges$status` indexed the integer just created: "$ operator is invalid for
  # atomic vectors".
  edges <- dplyr::bind_rows(
    edge_fixture(edge_id = "E001"),
    edge_fixture(edge_id = "E002", status = "empirical",
                 magnitude = 1, magnitude_unit = "kg"),
    edge_fixture(edge_id = "E003", essentiality_score = 3,
                 plausibility_score = 3, evidence_score = 2,
                 quantification_score = 1)
  )
  p <- aep_edge_progress(edges)
  expect_equal(p$edges, 3)
  expect_equal(p$empirical, 1)
  expect_equal(p$putative, 2)
  expect_equal(p$with_magnitude, 1)
  expect_equal(p$fully_scored, 1)
})

test_that("progress copes with an empty edge set", {
  p <- aep_edge_progress(empty_aep_edges())
  expect_equal(p$edges, 0)
  expect_equal(p$empirical, 0)
})
