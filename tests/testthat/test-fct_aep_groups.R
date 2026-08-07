# Declarative node grouping (2026-08-05).
#
# The property that matters: nesting is DERIVED from membership, never declared.
# A `parent` column would have to be kept consistent with the member lists by
# hand and the two would drift; containment is a fact about the lists and cannot.

groups_fixture <- function(...) {
  base <- tibble::tibble(
    group_key = c("coastal", "cod"),
    label = c("Coastal", "Cod"),
    node_ids = c("N001;N002;N003;N004;N005", "N004;N005"),
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base$members <- lapply(base$node_ids, function(x) {
    trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  })
  base
}

group_nodes <- function() {
  tibble::tibble(
    node_id = paste0("N00", 1:5),
    label = paste("Node", 1:5),
    x = c(0, 1, 2, 3, 4),
    y = c(0, 1, 2, 3, 4)
  )
}

test_that("a contained group is deeper than its container", {
  d <- aep_group_depth(groups_fixture())
  expect_equal(d, c(0L, 1L))
})

test_that("merely overlapping groups are both depth zero", {
  # Overlap without containment is honest and draws as intersecting boxes.
  g <- groups_fixture(node_ids = c("N001;N002;N003", "N003;N004;N005"))
  expect_equal(aep_group_depth(g), c(0L, 0L))
})

test_that("identical membership is not treated as containment", {
  # Strict containment only, or two groups over the same nodes would each claim
  # to be inside the other.
  g <- groups_fixture(node_ids = c("N001;N002", "N001;N002"))
  expect_equal(aep_group_depth(g), c(0L, 0L))
})

test_that("an empty group has depth zero and does not error", {
  g <- groups_fixture(node_ids = c("N001;N002;N003;N004;N005", NA))
  g$members[[2]] <- character(0)
  expect_equal(aep_group_depth(g)[2], 0L)
})

test_that("a nested box is inset inside its parent", {
  boxes <- aep_group_boxes(groups_fixture(), group_nodes())
  coastal <- boxes[boxes$group_key == "coastal", ]
  cod <- boxes[boxes$group_key == "cod", ]

  expect_gte(cod$xmin, coastal$xmin)
  expect_lte(cod$xmax, coastal$xmax)
  expect_gte(cod$ymin, coastal$ymin)
  expect_lte(cod$ymax, coastal$ymax)
})

test_that("a nested box still clears its cards' own edges", {
  # REGRESSION, 2026-08-08. Before this, depth's inset was subtracted from a
  # single combined max(pad, card_hw) term, so a nested group (depth >= 1)
  # could end up with LESS clearance than the card's own half-width -- Sam
  # found this directly: the "Cod" box (nested inside "Coastal") was smaller
  # than the node rectangles it was supposed to contain. The card clearance
  # itself must never shrink with nesting; only the extra margin beyond it may.
  card_hw <- 0.45
  card_hh <- 0.3
  boxes <- aep_group_boxes(
    groups_fixture(), group_nodes(), card_hw = card_hw, card_hh = card_hh
  )
  cod <- boxes[boxes$group_key == "cod", ]
  # Cod's members are N004, N005 at x = 3, 4 and y = 3, 4 (see group_nodes()),
  # so the box must clear card_hw/card_hh beyond BOTH the leftmost (3) and
  # rightmost (4) member on each axis.
  expect_gte(3 - cod$xmin, card_hw)
  expect_gte(cod$xmax - 4, card_hw)
  expect_gte(3 - cod$ymin, card_hh)
  expect_gte(cod$ymax - 4, card_hh)
})

test_that("shallow boxes are drawn first so nested ones sit on top", {
  boxes <- aep_group_boxes(groups_fixture(), group_nodes())
  expect_false(is.unsorted(boxes$depth))
})

test_that("a group whose nodes are unplaced is dropped, not drawn at NA", {
  nodes <- group_nodes()
  nodes$x[4:5] <- NA_real_
  nodes$y[4:5] <- NA_real_
  boxes <- aep_group_boxes(groups_fixture(), nodes[!is.na(nodes$x), ])
  expect_false("cod" %in% boxes$group_key)
  expect_true("coastal" %in% boxes$group_key)
})

test_that("no groups yields no layers rather than an error", {
  boxes <- aep_group_boxes(
    dplyr::mutate(empty_aep_node_groups(), members = list()), group_nodes()
  )
  expect_equal(nrow(boxes), 0)
  expect_length(aep_group_layers(boxes), 0)
})

test_that("an unknown node id is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  g <- groups_fixture()
  g$members <- NULL
  readr::write_csv(g, path, na = "")
  expect_error(
    read_aep_node_groups(path, nodes = group_nodes()[1:3, ]),
    "unknown node_id"
  )
})

test_that("duplicate group keys are refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  g <- groups_fixture(group_key = c("coastal", "coastal"))
  g$members <- NULL
  readr::write_csv(g, path, na = "")
  expect_error(read_aep_node_groups(path), "Duplicate group_key")
})

test_that("an absent file is a normal state, not an error", {
  g <- read_aep_node_groups(tempfile(fileext = ".csv"))
  expect_equal(nrow(g), 0)
})

test_that("the diagram builds with groups", {
  p <- plot_aep(
    group_nodes(), empty_aep_edges(), groups = groups_fixture()
  )
  expect_s3_class(p, "ggplot")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(p)))
})
