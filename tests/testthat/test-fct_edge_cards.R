# Edge report cards (2026-08-27; stripped down 2026-09-08).
#
# The card is now just the edge id over the shared EPEQ badge strip: no label,
# no magnitude/flux block (Peng et al. does not quantify inter-compartment
# flux), no level tint. Tests are about structure, not pixels (CLAUDE.md 2.3.1).

edge_card_fixture <- function(...) {
  base <- tibble::tibble(
    edge_id = "E001",
    from = "N012-coast-benthic-sed",
    to = "N014-mussel-soft-tissue",
    status = "putative",
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

test_that("the id strip carries the edge id and nothing else", {
  h <- edge_card_header(edge_card_fixture(edge_id = "E042"))
  b <- ggplot2::ggplot_build(h)
  texts <- unlist(lapply(b$data, function(d) if ("label" %in% names(d)) d$label))
  expect_equal(texts, "E042")
})

test_that("a missing edge id renders as blank, not an error", {
  h <- edge_card_header(edge_card_fixture(edge_id = NA_character_))
  b <- ggplot2::ggplot_build(h)
  texts <- unlist(lapply(b$data, function(d) if ("label" %in% names(d)) d$label))
  expect_equal(texts, "")
})

test_that("scores drive the badge strip, blank renders grey not '1'", {
  card <- edge_card(edge_card_fixture(
    essentiality_score = 3, plausibility_score = 2,
    evidence_score = NA_real_, quantification_score = 1
  ))
  b <- ggplot2::ggplot_build(card[[2]])
  labs <- b$data[[2]]$label
  expect_true(any(grepl("Es 3", labs)))
  expect_true(any(grepl("Pl 2", labs)))
  expect_true(any(grepl("Ev -", labs)))
  expect_true(any(grepl("Qn 1", labs)))
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
    edge_card_fixture(edge_id = "E002", to = "N015-fish-liver"),
    edge_card_fixture(edge_id = "E003", status = "rejected")
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
