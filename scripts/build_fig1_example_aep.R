# Figure 1: an illustrative aggregate exposure pathway (copper -> mussel),
# drawn with the same node-card design as the real AEPs. Hand-run, never a
# target.
#
# Simplified 2026-09-11 (Sam) to the minimal three-KES AEP structure in Tan et
# al. 2018 -- Source, Compartment, Target Site Exposure -- rather than the
# five-stage split used elsewhere in this repo (aep_node_levels()). All three
# nodes are REAL AEP-001 nodes, inside Hammerfest's bounding box:
#   N042-aquaculture (source) -> N012-coast-benthic-sed (compartment)
#     -> N014-mussel-soft-tissue (target site exposure)
#
# Reworked 2026-09-11 at KET's suggestion to drop weight-of-evidence elements
# from this figure, so it stays a general illustration of AEP structure
# rather than showing a specific scored assessment:
#   - Node cards are HEADER ONLY -- no EPEQ badge band, no distribution strips
#     -- rendered fresh from the real `aep_scoped` / `aep_node_cards` targets
#     via node_card(show_epeq = FALSE, show_strips = FALSE), not reused from
#     images/node_cards/A001/ (those carry the full badges+strips card, which
#     is what this figure is deliberately not showing). Regenerate this
#     script's own cards whenever `aep_node_cards` changes; nothing rebuilds
#     them automatically since this is a hand-run script.
#   - No per-edge EPEQ badge callouts. The empirical/putative distinction is
#     kept, but carried only by arrow line style (aep_edge_styles()), not by
#     any WOE badge.
#   - No top-right geo_scope corner icon (the pin/globe marking whether a
#     node's data is bounding-box-local or Arctic-wide) and no caption text
#     under the figure (plot_aep(edge_caption = FALSE)): this is a minimal
#     illustrative diagram, not making either of those claims.
#
# Requires the `aep_scoped` and `aep_node_cards` targets to be built
# (tar_make() first if `tar_outdated()` lists either).
#
# Writes figures/fig01-example-aep.png, which _01-introduction.qmd embeds as
# {#fig-aep-intro}.
#
#   Rscript scripts/build_fig1_example_aep.R

suppressMessages(library(targets))
suppressMessages(pkgload::load_all(quiet = TRUE))

out_png <- here::here("figures/fig01-example-aep.png")

# ---- Geometry knobs -------------------------------------------------------
# Three nodes only now, so both the spacing and the canvas width are scaled
# down from the old five-node figure (which used node_x = 0,2,...,8 and
# fig_width_in = 26) to keep roughly the same per-node card size. Re-tuned by
# running this script and looking at the written PNG, not derived
# analytically.
node_x        <- c(0, 4, 8)
fig_width_in  <- 16
fill_fraction <- 0.42   # 0.6 is the default; lower => smaller cards, wider gaps

# ---- Real node cards, header band only -------------------------------------
# The three real AEP-001 nodes this figure illustrates, all inside
# Hammerfest's bounding box (see aep_membership_A001.csv).
real_node_ids <- c(
  n1 = "N042-aquaculture",
  n2 = "N012-coast-benthic-sed",
  n3 = "N014-mussel-soft-tissue"
)

scoped_a001 <- tar_read(aep_scoped)[["A001"]]
cards_a001 <- tar_read(aep_node_cards)
cards_a001 <- cards_a001[cards_a001$aep_id == "A001", , drop = FALSE]
stopifnot(all(real_node_ids %in% scoped_a001$node_id))
stopifnot(all(real_node_ids %in% cards_a001$node_id))

# header-only card height, as a fraction of the full (header + badges +
# strips) card height, from node_card_heights() so this cannot drift out of
# sync with node_card()'s own layout.
header_frac <- unname(node_card_heights()["header"] / sum(node_card_heights()))
card_width_in  <- 2.4   # matches write_node_cards()'s default
card_height_in <- 1.8 * header_frac
card_aspect    <- card_height_in / card_width_in

fig1_card_dir <- here::here("images/node_cards/A001/fig1-header-only")
dir.create(fig1_card_dir, showWarnings = FALSE, recursive = TRUE)

real_card <- vapply(names(real_node_ids), function(dummy_id) {
  node_id <- real_node_ids[[dummy_id]]
  node_row <- scoped_a001[scoped_a001$node_id == node_id, , drop = FALSE]
  card_row <- cards_a001[cards_a001$node_id == node_id, , drop = FALSE]
  # Drop geo_scope so node_card_header() draws no top-right corner icon: this
  # illustrative figure is not making a claim about any node's spatial scope
  # (local bounding box vs Arctic-wide), unlike the real AEP diagrams. NA
  # would still draw the "local" pin (geo_scope_icon_path()'s NA fallback),
  # so the column has to be absent, not merely blank.
  node_row <- node_row[, setdiff(names(node_row), "geo_scope"), drop = FALSE]
  p <- node_card(
    node_row, card_row,
    members = NULL, data = NULL, ids = NULL,
    show_epeq = FALSE, show_strips = FALSE
  )
  f <- file.path(fig1_card_dir, paste0(node_id, ".png"))
  ggplot2::ggsave(
    f, p,
    width = card_width_in, height = card_height_in, dpi = 300,
    device = ragg::agg_png, bg = node_card_bg_colour(node_row)
  )
  f
}, character(1))

# node_type = "external" throughout: these are carried-in cards, not resolved
# from member groups here, so no members/data are needed for them to draw.
# level matches the real aep_scoped rows (checked above), not the generic
# five-stage split.
nodes <- empty_aep_nodes() |>
  tibble::add_row(
    node_id = names(real_node_ids),
    label = c("Aquaculture", "Fjord sediment", "Mussel soft tissue"),
    level = c("source", "exposure_medium", "internal_exposure"),
    node_type = "external",
    x = node_x,
    y = 0,
    external_refs = 1
  )

# Edge id and label in the same "E<n>-<from slug>-to-<to slug>" form as the
# real aep_edges.csv (the node_ids here are throwaway "n1".."n3", so the slug
# comes from the node label instead).
node_slug <- node_label_slug(nodes$label)
edges <- tibble::tibble(
  edge_id = sprintf("E%03d-%s-to-%s", 1:2, node_slug[1:2], node_slug[2:3]),
  from = nodes$node_id[1:2],
  to = nodes$node_id[2:3],
  label = paste0(node_slug[1:2], "-to-", node_slug[2:3]),
  # Illustrative: aquaculture -> sediment deposition is the better-evidenced
  # link; sediment -> mussel tissue uptake skips the intermediate KESs this
  # simplified figure omits, so it is drawn putative.
  status = c("empirical", "putative"),
  magnitude = NA_real_, magnitude_unit = NA_character_,
  magnitude_n = NA_real_, magnitude_sd = NA_real_, magnitude_refs = NA_real_,
  notes = NA_character_
)

empty_members <- tibble::tibble(node_id = character(), group_id = character())
empty_data <- tibble::tibble()

# Empty data throughout, so geo_mean is NA for every node and plot_aep() does
# not draw a duplicate stats label over the real card image (which already
# carries its own headline stat).
cards <- aep_node_report_cards(nodes, empty_members, empty_data, NULL)

width <- fig_width_in
image_size <- aep_diagram_image_size(nodes, 0.22, fill_fraction = fill_fraction)
height <- aep_diagram_height(
  nodes,
  effective_width = width,
  image_size = image_size,
  card_aspect = card_aspect,
  min_height = 2.8
)

p <- plot_aep(
  nodes,
  edges,
  cards,
  node_images = real_card,
  image_size = image_size,
  card_aspect = card_aspect,
  device_aspect = width / height,
  curvature = 0,          # straight arrows
  label_edges = FALSE,    # no magnitude labels on this illustrative figure
  edge_caption = FALSE    # no caption text under the figure
)

out_height <- height
ggplot2::ggsave(
  out_png, p,
  width = width, height = out_height, dpi = 300,
  device = ragg::agg_png, bg = "white"
)
message("wrote ", out_png, "  (", width, " x ", round(out_height, 1), " in)")
