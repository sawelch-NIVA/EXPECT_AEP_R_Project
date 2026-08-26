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

# ---- Rejected edges -----------------------------------------------------
# Added 2026-08-12. A cut edge has to be RECORDED, not deleted: the scaffolder
# proposes new edges by anti_join on from/to, so an absent row reads as "never
# proposed" and comes back on the next run. Sam lost a round of deletions to
# exactly this. These tests pin the status down at every point that has to
# agree about it.

test_that("rejected is an accepted status", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    edge_fixture(status = "rejected", notes = "Indirect; mediated by N002"),
    path, na = ""
  )
  expect_no_error(read_aep_edges(path, nodes = edge_nodes()))
})

test_that("drop_rejected_edges keeps everything that is not rejected", {
  edges <- dplyr::bind_rows(
    edge_fixture(edge_id = "E001"),
    edge_fixture(edge_id = "E002", status = "empirical"),
    edge_fixture(edge_id = "E003", status = "rejected", notes = "cut"),
    # An unfilled status is an edge not yet considered, never one that was cut.
    edge_fixture(edge_id = "E004", status = NA_character_)
  )
  expect_equal(drop_rejected_edges(edges)$edge_id, c("E001", "E002", "E004"))
})

test_that("a rejected edge with no reason is warned about", {
  # The reason is the whole content of the decision. Without it the row says a
  # judgement was made and records nothing of what it was, which is how the
  # same edge gets argued about again in three weeks.
  edges <- edge_fixture(status = "rejected")
  expect_warning(
    validate_aep_edges(edges, edge_nodes()),
    "rejected with no reason"
  )
})

test_that("whitespace does not pass as a rejection reason", {
  edges <- edge_fixture(status = "rejected", notes = "   ")
  expect_warning(
    validate_aep_edges(edges, edge_nodes()),
    "rejected with no reason"
  )
})

test_that("a rejected edge still carrying a score or magnitude is warned about", {
  # Left over from a change of mind. Anything reading the scores without
  # filtering would count a cut edge as a scored one.
  edges <- edge_fixture(
    status = "rejected", notes = "cut", plausibility_score = 3
  )
  expect_warning(
    validate_aep_edges(edges, edge_nodes()),
    "still carry a score or magnitude"
  )

  edges2 <- edge_fixture(
    status = "rejected", notes = "cut", magnitude = 42, magnitude_unit = "kg/yr"
  )
  expect_warning(
    validate_aep_edges(edges2, edge_nodes()),
    "still carry a score or magnitude"
  )
})

test_that("a rejected edge does not count as an edge for the orphan check", {
  # The check that matters most. A node whose only edges were cut is exactly
  # the node needing to be reported, and before this it looked connected.
  nodes <- edge_nodes()[1:2, ]
  edges <- edge_fixture(status = "rejected", notes = "Indirect")
  expect_warning(
    validate_aep_edges(edges, nodes),
    "no edges at all"
  )
})

test_that("a cut edge mediated by a live path validates silently", {
  # Sam's rule, 2026-08-12: a source never emits directly to an organism,
  # there is always at least one matrix between. So the rejected source ->
  # organism edge sits alongside the two live edges that carry the same flow
  # through the medium, and nothing about that arrangement should warn.
  edges <- dplyr::bind_rows(
    edge_fixture(edge_id = "E001", from = "N001", to = "N002"),
    edge_fixture(edge_id = "E002", from = "N002", to = "N003"),
    edge_fixture(
      edge_id = "E003", from = "N001", to = "N003", status = "rejected",
      notes = "Indirect; mediated by N002"
    )
  )
  expect_no_warning(validate_aep_edges(edges, edge_nodes()))
})

test_that("a rejected edge is not drawn", {
  # Structural, not visual: the built plot must contain no layer holding the
  # cut edge's geometry. Checked against the same plot with the edge live, so
  # the assertion cannot pass because nothing was drawn either way.
  live <- edge_fixture(from = "N001", to = "N002")
  cut <- edge_fixture(from = "N001", to = "N002", status = "rejected",
                      notes = "Indirect")

  n_layers <- function(edges) {
    built <- suppressWarnings(
      ggplot2::ggplot_build(plot_aep(edge_nodes(), edges))
    )
    sum(vapply(built$data, function(d) nrow(d) > 0, logical(1)))
  }

  expect_lt(n_layers(cut), n_layers(live))
})

test_that("progress reports rejected separately and excludes it from the total", {
  # Rejecting an edge IS progress. Counting cut edges in the denominator would
  # leave the figure measured against abandoned work, so it could never reach
  # completion however much real work got done.
  edges <- dplyr::bind_rows(
    edge_fixture(edge_id = "E001"),
    edge_fixture(edge_id = "E002", status = "empirical",
                 magnitude = 1, magnitude_unit = "kg"),
    edge_fixture(edge_id = "E003", status = "rejected", notes = "Indirect"),
    edge_fixture(edge_id = "E004", status = "rejected", notes = "Indirect")
  )
  p <- aep_edge_progress(edges)
  expect_equal(p$edges, 2)
  expect_equal(p$rejected, 2)
  expect_equal(p$putative, 1)
  expect_equal(p$empirical, 1)
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

# ---- Clipping arrows to the node cards ----------------------------------
# Added 2026-08-06, replacing the fractional trim recorded as a rough edge in
# PLAN.md P5.1. The bug it fixes is that a fraction scales with edge length and
# a card does not, so one trim value cannot clear cards on both a short edge and
# a long one. Every edge in figures/aep.png was wrong in one direction or the
# other.

test_that("card extent scales with the coordinate range, not the node count", {
  near <- edge_nodes()
  far <- edge_nodes()
  far$x <- far$x * 10

  e_near <- node_card_extent(near, image_size = 0.2)
  e_far <- node_card_extent(far, image_size = 0.2)

  # A card is a fixed fraction of the panel, so ten times the x range is ten
  # times the half-width in data units.
  expect_equal(e_far$hw, e_near$hw * 10)
})

test_that("a degenerate axis falls back to a unit range rather than zero", {
  # edge_nodes() has every y at 0. A zero half-height would clip nothing
  # vertically and put horizontal arrows back under the cards.
  ext <- node_card_extent(edge_nodes(), image_size = 0.2)
  expect_gt(ext$hh, 0)
  expect_true(is.finite(ext$hh))
})

test_that("clipping clears the card box by the requested gap", {
  nodes <- edge_nodes()
  hw <- 0.25
  hh <- 0.25
  gap <- 0.05
  coords <- aep_edge_coords(
    edge_fixture(), nodes, hw = hw, hh = hh, gap = gap
  )

  # N001 (0, 0) to N002 (1, 0): purely horizontal, so the segment leaves
  # through the vertical side of the box at hw, plus the gap.
  expect_equal(coords$x, hw + gap)
  expect_equal(coords$xend, 1 - hw - gap)
  expect_equal(coords$y, 0)
  expect_equal(coords$yend, 0)
})

test_that("the gap is a constant distance, not a constant fraction", {
  # THE WHOLE POINT. Under the old fractional trim the near edge cleared its
  # card and the far edge stopped a long way short of one. Both must now stop
  # the same distance from the card.
  nodes <- edge_nodes()
  short <- aep_edge_coords(
    edge_fixture(from = "N001", to = "N002"), nodes, hw = 0.2, hh = 0.2
  )
  long <- aep_edge_coords(
    edge_fixture(from = "N001", to = "N003"), nodes, hw = 0.2, hh = 0.2
  )
  expect_equal(short$x, long$x)
})

test_that("overlapping cards drop the edge rather than drawing it backwards", {
  # A reversed arrow reads as a real flow in the wrong direction, which is
  # worse than a missing one.
  nodes <- edge_nodes()
  expect_warning(
    coords <- aep_edge_coords(
      edge_fixture(), nodes, hw = 0.9, hh = 0.9
    ),
    "cards overlap"
  )
  expect_equal(nrow(coords), 0)
})

test_that("without card extents the fractional trim is unchanged", {
  # Diagrams drawn with text labels have no box to clip to, and must keep
  # working exactly as before.
  coords <- aep_edge_coords(edge_fixture(), edge_nodes(), trim = 0.1)
  expect_equal(coords$x, 0.1)
  expect_equal(coords$xend, 0.9)
})

test_that("the diagram clips to cards only when cards are actually drawn", {
  nodes <- edge_nodes()
  edges <- edge_fixture()

  # No images: text labels, fractional trim, must still build.
  expect_s3_class(plot_aep(nodes, edges), "ggplot")

  png <- withr::local_tempfile(fileext = ".png")
  ggplot2::ggsave(
    png, ggplot2::ggplot(), width = 2.4, height = 1.8, dpi = 72
  )
  imgs <- stats::setNames(rep(png, 3), nodes$node_id)
  p <- plot_aep(nodes, edges, node_images = imgs, image_size = 0.15)
  expect_s3_class(p, "ggplot")
  expect_silent(invisible(ggplot2::ggplot_build(p)))
})

# ---- Arrowheads ----------------------------------------------------------
# Added 2026-08-12. grid gives an arrowhead the line's own gpar, lty included,
# so a dashed edge produced a triangle with gaps chewed out of it. The head is
# now a separate solid layer.

test_that("arrow stubs sit at the destination end and point along the curve", {
  coords <- aep_edge_coords(edge_fixture(), edge_nodes(), trim = 0)
  stubs <- aep_edge_arrow_stubs(coords)
  expect_equal(nrow(stubs), 1)
  # The head lands exactly on the edge's own end point.
  expect_equal(stubs$xend, coords$xend)
  expect_equal(stubs$yend, coords$yend)
  # And the stub is short.
  len <- sqrt((stubs$xend - stubs$x)^2 + (stubs$yend - stubs$y)^2)
  edge_len <- sqrt((coords$xend - coords$x)^2 + (coords$yend - coords$y)^2)
  expect_lt(len, edge_len * 0.05)
  expect_gt(len, 0)
})

test_that("the stub follows the curve's tangent, not the chord", {
  # At curvature 0 the two coincide; above it they must not, or the head is
  # skewed against its own line.
  coords <- aep_edge_coords(edge_fixture(), edge_nodes(), trim = 0)
  straight <- aep_edge_arrow_stubs(coords, curvature = 0)
  curved <- aep_edge_arrow_stubs(coords, curvature = 0.15)
  expect_equal(straight$y, coords$yend)
  expect_false(isTRUE(all.equal(curved$y, straight$y)))
})

test_that("an empty edge set gives an empty stub table, not an error", {
  expect_equal(nrow(aep_edge_arrow_stubs(aep_edge_coords(
    empty_aep_edges(), edge_nodes()
  ))), 0)
})

test_that("the curve layer carries no arrow and the stub layer does", {
  edges <- edge_fixture(from = "N001", to = "N002")
  p <- plot_aep(edge_nodes(), edges)
  arrows <- vapply(p$layers, function(l) !is.null(l$geom_params$arrow),
                   logical(1))
  beziers <- vapply(p$layers, function(l) inherits(l$geom, "GeomBezier"),
                    logical(1))
  # No bezier layer may carry an arrow: that is what dashed the head.
  expect_false(any(arrows & beziers))
  expect_true(any(arrows))
})

test_that("the diagram still builds with arrowheads split out", {
  edges <- dplyr::bind_rows(
    edge_fixture(edge_id = "E001", from = "N001", to = "N002"),
    edge_fixture(edge_id = "E002", from = "N002", to = "N003",
                 status = "empirical", evidence_justification = "x")
  )
  p <- plot_aep(edge_nodes(), edges)
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})
