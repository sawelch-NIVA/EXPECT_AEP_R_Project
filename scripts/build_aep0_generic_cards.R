# AEP 0: a fully generic, made-up example AEP (Sam's internal name, not a
# registered aep_id in data/clean/aep/aep_manifest.csv). Eight header-only
# node cards, no values, no references, no EPEQ scoring -- a blank template
# set for illustrating AEP structure, not an assessment of anything. Hand-run,
# never a target.
#
# node_type = "external" throughout, same reasoning as
# scripts/build_fig1_example_aep.R: these are not resolved from real member
# groups, so external is the type that draws without one. Every value column
# is left NA, which sends node_card_header() down its "-" / blank branches
# (see R/fct_node_cards.R:1253) rather than printing a fabricated number.
#
# show_epeq = FALSE, show_strips = FALSE (Sam: "none of these need WoE
# assessment"): header band only, same as the real-node cards in
# build_fig1_example_aep.R. show_counts = FALSE on top of that (Sam: the
# "n = -, refs = -" line read as a missing-data bug on a card that was never
# claiming any data in the first place).
#
# Writes images/node_cards/AEP0/<node_id>.png, one per KES:
#   EX01-emission                 (source)
#   EX02-dissolved-copper         (exposure_medium)
#   EX03-particulate-copper       (exposure_medium)
#   EX04-bivalve-gills             (internal_exposure)
#   EX05-bivalve-digestive-gland   (internal_exposure)
#   EX06-fish-gills               (internal_exposure)
#   EX07-fish-intestine           (internal_exposure)
#   EX08-fish-sensory-organs      (internal_exposure)
#   EX09-algae                    (internal_exposure)
#
#   Rscript scripts/build_aep0_generic_cards.R

suppressMessages(pkgload::load_all(quiet = TRUE))

out_dir <- here::here("images/node_cards/AEP0")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

nodes <- empty_aep_nodes() |>
  tibble::add_row(
    node_id = c(
      "EX01-emission",
      "EX02-dissolved-copper",
      "EX03-particulate-copper",
      "EX04-bivalve-gills",
      "EX05-bivalve-digestive-gland",
      "EX06-fish-gills",
      "EX07-fish-intestine",
      "EX08-fish-sensory-organs",
      "EX09-algae"
    ),
    label = c(
      "Emission",
      "Dissolved copper",
      "Particulate copper",
      "Bivalve gills",
      "Bivalve digestive gland",
      "Fish gills",
      "Fish intestine",
      "Fish sensory organs",
      "Algae"
    ),
    level = c(
      "source",
      "exposure_medium", "exposure_medium",
      "internal_exposure", "internal_exposure",
      "internal_exposure", "internal_exposure", "internal_exposure",
      "internal_exposure"
    ),
    node_type = "external"
  )

# header-only card height, as a fraction of the full (header + badges +
# strips) card height -- see build_fig1_example_aep.R for why this is derived
# from node_card_heights() rather than a second, driftable number.
header_frac <- unname(node_card_heights()["header"] / sum(node_card_heights()))
card_width_in  <- 2.4   # matches write_node_cards()'s default
card_height_in <- 1.8 * header_frac

empty_members <- tibble::tibble(node_id = character(), group_id = character())
empty_data <- tibble::tibble()
cards <- aep_node_report_cards(nodes, empty_members, empty_data, NULL)

paths <- vapply(seq_len(nrow(nodes)), function(i) {
  node_row <- nodes[i, , drop = FALSE]
  card_row <- cards[cards$node_id == node_row$node_id[1], , drop = FALSE]
  p <- node_card(
    node_row, card_row,
    members = NULL, data = NULL, ids = NULL,
    show_epeq = FALSE, show_strips = FALSE, show_counts = FALSE
  )
  f <- file.path(out_dir, paste0(node_row$node_id[1], ".png"))
  ggplot2::ggsave(
    f, p,
    width = card_width_in, height = card_height_in, dpi = 300,
    device = ragg::agg_png, bg = node_card_bg_colour(node_row)
  )
  f
}, character(1))

message("wrote ", length(paths), " cards to ", out_dir)
