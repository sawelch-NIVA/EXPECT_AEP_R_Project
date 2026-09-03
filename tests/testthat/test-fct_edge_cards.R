# Edge report cards (2026-08-27).
#
# The node card with the distribution panel and the level tint removed, and a
# blank line added between the quantity and the counts. The tests are about
# structure (does it assemble, does it draw, is the blank line real) rather
# than pixels, per CLAUDE.md 2.3.1.

edge_card_fixture <- function(...) {
  base <- tibble::tibble(
    edge_id = "E001",
    from = "N012-coast-benthic-sed",
    to = "N014-mussel-soft-tissue",
    label = "Coastal benthic sediment to Coastal mussels",
    status = "putative",
    magnitude = NA_real_,
    magnitude_unit = NA_character_,
    magnitude_n = NA_real_,
    magnitude_sd = NA_real_,
    magnitude_refs = NA_real_,
    essentiality_score = NA_real_,
    plausibility_score = NA_real_,
    evidence_score = NA_real_,
    quantification_score = NA_real_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

test_that("an all-blank putative edge still assembles and draws", {
  card <- edge_card(edge_card_fixture())
  expect_s3_class(card, "patchwork")
  expect_length(card, 2)
  expect_no_error(ggplot2::ggplot_build(card[[1]]))
  expect_no_error(ggplot2::ggplot_build(card[[2]]))
})

test_that("the header carries label, quantity and counts as text rows", {
  # (The edge id is drawn as a corner grob via annotation_custom(), so it is
  # not in ggplot_build()$data -- only the three annotate() rows are.)
  h <- edge_card_header(edge_card_fixture(
    magnitude = 4.2, magnitude_unit = "mg/kg/yr",
    magnitude_n = 12, magnitude_refs = 3
  ))
  b <- ggplot2::ggplot_build(h)
  texts <- unlist(lapply(b$data, function(d) if ("label" %in% names(d)) d$label))
  expect_true(any(grepl("Coastal benthic sediment", texts)))
  expect_true(any(grepl("4\\.2 mg/kg/yr", texts)))
  expect_true(any(grepl("n = 12, refs = 3", texts)))
})

test_that("the blank line: a clear gap between the quantity and the counts", {
  # The header lays text out in a 0..10 space: label near the top (y ~ 9.6),
  # quantity at y = 3.4, counts at y = 1.0. The 2.4-unit quantity-to-counts gap
  # is the blank line Sam asked for; a packed card would sit them ~1.2 apart.
  h <- edge_card_header(edge_card_fixture(magnitude = 4.2, magnitude_unit = "x"))
  b <- ggplot2::ggplot_build(h)
  ys <- sort(unlist(lapply(b$data, function(d) {
    if ("label" %in% names(d) && "y" %in% names(d)) d$y[is.finite(d$y)]
  })))
  # Three annotate() text rows (the corner id grob is not in $data).
  expect_length(ys, 3)
  counts_y <- ys[1]
  quantity_y <- ys[2]
  expect_gt(quantity_y - counts_y, 2)
})

test_that("a missing magnitude shows as a dash, not an error", {
  h <- edge_card_header(edge_card_fixture())
  b <- ggplot2::ggplot_build(h)
  texts <- unlist(lapply(b$data, function(d) if ("label" %in% names(d)) d$label))
  expect_true(any(texts == "-"))
  expect_true(any(grepl("n = -, refs = -", texts)))
})

test_that("scores drive the badge strip, blank renders grey not '1'", {
  card <- edge_card(edge_card_fixture(
    essentiality_score = 3, plausibility_score = 2,
    evidence_score = NA_real_, quantification_score = 1
  ))
  b <- ggplot2::ggplot_build(card[[2]])
  labs <- b$data[[2]]$label
  expect_true(any(grepl("Es 3", labs)))
  expect_true(any(grepl("Ev -", labs)))
})

test_that("write_aep_edge_cards writes one PNG per live edge per AEP subdir", {
  scoped <- list(
    A001 = tibble::tibble(node_id = c(
      "N012-coast-benthic-sed", "N014-mussel-soft-tissue", "N015-fish-liver"
    )),
    A002 = tibble::tibble(node_id = c(
      "N012-coast-benthic-sed", "N014-mussel-soft-tissue"
    ))
  )
  edges <- dplyr::bind_rows(
    edge_card_fixture(edge_id = "E001"),
    edge_card_fixture(
      edge_id = "E002", to = "N015-fish-liver",
      label = "Sediment to cod liver"
    ),
    edge_card_fixture(
      edge_id = "E003", status = "rejected",
      label = "cut edge"
    )
  )
  dir <- withr::local_tempdir()
  paths <- write_aep_edge_cards(scoped, edges, dir = dir)

  # A001 has all three nodes: E001 and E002 both live (E003 rejected -> dropped).
  # A002 has only sediment + mussel: E001 only.
  expect_setequal(basename(paths), c("E001.png", "E002.png", "E001.png"))
  expect_true(file.exists(file.path(dir, "A001", "E001.png")))
  expect_true(file.exists(file.path(dir, "A001", "E002.png")))
  expect_true(file.exists(file.path(dir, "A002", "E001.png")))
  expect_false(file.exists(file.path(dir, "A001", "E003.png")))
})
