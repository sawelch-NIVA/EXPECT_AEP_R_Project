# Figure 1: an illustrative aggregate exposure pathway (copper -> fish), drawn
# with the same node-card design as the real AEPs. Hand-run, never a target.
#
# A five-node linear chain, one node per AEP stage (aep_node_levels()):
#   source -> exposure_medium -> external_exposure -> internal_exposure
#     -> target_site_exposure
#
# The first two nodes are the REAL AEP-001 node cards, reused verbatim from
# images/node_cards/A001/ (regenerate those with `tar_make(node_cards)` first):
# N042 the merged aquaculture source, and N013 the Hammerfest fjord-water
# exposure medium with its real distribution panel and "local" corner icon. The
# last three stay illustrative placeholders (inside-the-fish stages we hold no
# dataset for). Writes figures/fig01-example-aep.png, which _01-introduction.qmd
# embeds as {#fig-aep-intro}.
#
#   Rscript scripts/build_fig1_example_aep.R

suppressMessages(pkgload::load_all(quiet = TRUE))

out_png <- here::here("figures/fig01-example-aep.png")

# ---- Geometry knobs -------------------------------------------------------
# Node spacing was doubled (0:4 -> 0,2,4,...) and the cards shrunk via
# fill_fraction so the inter-node gap is ~2x what it was, leaving room to drop
# an edge card onto each arrow. Widening x alone does nothing here (a card's
# width is a fixed fraction of panel width, not inches); the lever is card size
# relative to the gap. See aep_diagram_image_size().
node_x        <- c(0, 2, 4, 6, 8)
fig_width_in  <- 26     # wider canvas keeps the cards near their old inch size
fill_fraction <- 0.42   # 0.6 is the default; lower => smaller cards, wider gaps
                        # 0.42 puts the inter-node gap at ~2x the old figure
edge_img_size <- 0.068  # edge-card width as a fraction of panel width
edge_y_gap    <- 0.035  # clear space (data units) between the arrow and the
                        # edge card's lower edge, so the arrow stays visible

# The two real AEP-001 cards, reused as-is. Their headline figures, EPEQ badges,
# distribution panel (N013) and "local" icon are already baked into the PNGs.
real_card <- c(
  n1 = here::here("images/node_cards/A001/N042-aquaculture-feed.png"),
  n2 = here::here("images/node_cards/A001/N013-coast-water-column.png")
)
stopifnot(file.exists(real_card))

nodes <- empty_aep_nodes() |>
  tibble::add_row(
    node_id = c("n1", "n2", "n3", "n4", "n5"),
    # Kept short: node_card() wraps a label at 24 characters and a wrapped
    # second line collides with the headline underneath it.
    label = c(
      "Aquaculture", "Coastal water", "Gill-surface copper",
      "Gill tissue copper", "Ion-transport site"
    ),
    level = aep_node_levels(),
    node_type = "external",
    x = node_x,
    y = 0,
    # n1, n2 are drawn from the pre-rendered real cards (real_card above), so
    # these values are placeholders only for n3-n5; n1/n2's entries are ignored.
    external_value = c(NA, NA, 0.55, 8.5, NA),
    external_unit = c(NA, NA, "ug/L", "mg/kg ww", NA),
    external_refs = 1,
    essentiality_score = c(1, 3, 3, 3, 3),
    essentiality_justification = "Illustrative example, not a scored assessment.",
    plausibility_score = 3,
    plausibility_justification = "Illustrative example, not a scored assessment.",
    drop_outliers = FALSE
  )

# Evidence and quantification are per-AEP in the real workflow (membership file,
# not aep_nodes.csv), so empty_aep_nodes() no longer carries them. Added back
# here as plain columns because node_epeq_badges() draws all four on n3-n5's
# placeholder cards. n1/n2 use the pre-rendered real cards, so their values are
# ignored.
illus <- "Illustrative example, not a scored assessment."
nodes$evidence_score <- c(1, 2, 2, 2, 1)
nodes$evidence_justification <- illus
nodes$quantification_score <- c(1, 2, 2, 2, 1)
nodes$quantification_justification <- illus

# n1/n2 carry their "local" pin baked into the pre-rendered PNG; n3-n5 get no
# icon. geo_scope_icon_path(): "" (or any unrecognised value) -> no icon.
nodes$geo_scope <- c("", "", "", "", "")

edges <- tibble::tibble(
  edge_id = sprintf("e%d", 1:4),
  from = nodes$node_id[1:4],
  to = nodes$node_id[2:5],
  label = c("release", "contact", "uptake", "internal distribution"),
  status = c("empirical", "empirical", "putative", "putative"),
  magnitude = NA_real_, magnitude_unit = NA_character_,
  magnitude_n = NA_real_, magnitude_sd = NA_real_, magnitude_refs = NA_real_,
  # Illustrative EPEQ scores so the example edge cards show a filled badge strip
  # (empirical edges better evidenced than putative ones), not four dashes.
  essentiality_score = c(2, 3, 3, 3),
  essentiality_justification = "Illustrative example, not a scored assessment.",
  plausibility_score = c(3, 3, 3, 2),
  plausibility_justification = "Illustrative example, not a scored assessment.",
  evidence_score = c(3, 3, 2, 1),
  evidence_justification = "Illustrative example, not a scored assessment.",
  quantification_score = c(2, 2, 2, 1),
  quantification_justification = "Illustrative example, not a scored assessment.",
  notes = NA_character_
)

empty_members <- tibble::tibble(node_id = character(), group_id = character())
empty_data <- tibble::tibble()

cards <- aep_node_report_cards(nodes, empty_members, empty_data, NULL)

# n3-n5 only: rendered one at a time (not through write_node_cards()). These are
# data-free, so omit_empty_strips = TRUE drops the strips band entirely and the
# card is saved shorter (node_card_short_height_frac()), matching what
# write_node_cards() now does for every distribution-less node. n1/n2 come from
# the real_card PNGs.
card_dir <- file.path(tempdir(), "fig1-cards")
dir.create(card_dir, showWarnings = FALSE, recursive = TRUE)
placeholder_i <- which(nodes$node_id %in% c("n3", "n4", "n5"))
placeholder_paths <- vapply(placeholder_i, function(i) {
  nd <- nodes[i, , drop = FALSE]
  cd <- cards[cards$node_id == nd$node_id[1], , drop = FALSE]
  pc <- node_card(
    nd, cd, empty_members, empty_data, NULL,
    limits = NULL, dpi = 300, omit_empty_strips = TRUE
  )
  h <- if (isTRUE(attr(pc, "short"))) {
    1.8 * node_card_short_height_frac()
  } else {
    1.8
  }
  f <- file.path(card_dir, paste0(nd$node_id[1], ".png"))
  ggplot2::ggsave(
    f, pc,
    width = 2.4, height = h, dpi = 300,
    device = ragg::agg_png, bg = node_card_bg_colour(nd)
  )
  f
}, character(1))

node_images <- c(
  real_card,
  stats::setNames(
    placeholder_paths, tools::file_path_sans_ext(basename(placeholder_paths))
  )
)

# ---- Example edge cards, one per linkage --------------------------------------
# Same design as the assessed AEPs' edge cards (edge_card()): the EPEQ badge
# strip over a small id line. Rendered here and dropped onto each arrow at its
# midpoint.
edge_card_dir <- file.path(tempdir(), "fig1-edge-cards")
dir.create(edge_card_dir, showWarnings = FALSE, recursive = TRUE)
edge_img_paths <- vapply(seq_len(nrow(edges)), function(i) {
  ed <- edges[i, , drop = FALSE]
  f <- file.path(edge_card_dir, paste0(ed$edge_id[1], ".png"))
  ggplot2::ggsave(
    f, edge_card(ed),
    width = 1.6, height = 0.24, dpi = 300,
    device = ragg::agg_png, bg = "white"
  )
  f
}, character(1))

width <- fig_width_in
image_size <- aep_diagram_image_size(nodes, 0.22, fill_fraction = fill_fraction)
height <- aep_diagram_height(
  nodes,
  effective_width = width,
  image_size = image_size,
  card_aspect = 1.8 / 2.4,
  min_height = 2.8
)

p <- plot_aep(
  nodes,
  edges,
  cards,
  node_images = node_images,
  image_size = image_size,
  device_aspect = width / height,
  curvature = 0,          # straight arrows
  label_edges = FALSE,    # no magnitude labels on this illustrative figure
  edge_caption = FALSE    # the edge-type key is carried by the rewritten legend
)

# Edge cards, one per arrow, at the chord midpoint but nudged UP so the card's
# lower edge clears the arrow (which runs along y = 0): the arrow stays fully
# visible, the card reads as a callout above it. Half-height of the edge card in
# data units, from its saved shape (1.6 x 0.24 in -> h/w = 0.15) and the same
# geom_image sizing maths node_card_extent() uses.
ext <- node_card_extent(
  nodes,
  image_size = image_size,
  card_aspect = 1.8 / 2.4,
  device_aspect = width / height,
  x_expand = 0.15, y_expand = 0.12,
  y_range = range(nodes$y)
)
edge_card_hh <- edge_img_size * (0.24 / 1.6) * (width / height) * ext$ry / 2
edge_mid <- tibble::tibble(
  x = (nodes$x[1:4] + nodes$x[2:5]) / 2,
  y = edge_card_hh + edge_y_gap,
  .image = unname(edge_img_paths)
)
p <- p +
  ggimage::geom_image(
    data = edge_mid,
    ggplot2::aes(x = .data$x, y = .data$y, image = .data$.image),
    size = edge_img_size,
    asp = width / height
  )

# ---- Fit check: edge card must clear its two neighbouring node cards ---------
# All widths as a fraction of panel width, so directly comparable.
node_gap_frac <- (diff(nodes$x)[1] / diff(range(nodes$x))) *
  (diff(range(nodes$x)) / (diff(range(nodes$x)) * (1 + 2 * 0.15)))
node_half_frac <- image_size / 2
gap_between <- node_gap_frac - 2 * node_half_frac
message(sprintf(
  "fit check: node card %.3f wide, gap between cards %.3f, edge card %.3f (%s)",
  image_size, gap_between, edge_img_size,
  if (edge_img_size < gap_between) "fits" else "TOO WIDE"
))

# ---- Colour legend for the five pathway stages ------------------------------
# One swatch per level, in pathway order, filled from node_level_bg_colours()
# so the strip and the cards cannot drift apart.
lvl <- names(node_level_bg_colours())
legend_df <- tibble::tibble(
  x = seq_along(lvl),
  fill = unname(node_level_bg_colours()[lvl]),
  label = c(
    "Source", "Exposure\nmedium", "External\nexposure",
    "Internal\nexposure", "Target site\nexposure"
  )
)
legend_plot <- ggplot2::ggplot(legend_df, ggplot2::aes(.data$x, 0)) +
  ggplot2::geom_tile(
    fill = legend_df$fill,
    width = 0.55, height = 0.4, colour = "grey55", linewidth = 0.3
  ) +
  ggplot2::geom_text(
    ggplot2::aes(y = -0.42, label = .data$label),
    size = 3, vjust = 1, lineheight = 0.9, colour = "grey20"
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.5)) +
  ggplot2::scale_y_continuous(limits = c(-1.1, 0.35)) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_void()

# ---- Key for the "local" corner icon --------------------------------------
local_icon <- png::readPNG(geo_scope_icon_path("local"), native = FALSE)
key_plot <- ggplot2::ggplot() +
  ggplot2::annotation_custom(
    grid::rasterGrob(
      local_icon,
      width = grid::unit(16, "pt"), height = grid::unit(16, "pt")
    ),
    xmin = 0.06, xmax = 0.11, ymin = 0.2, ymax = 0.8
  ) +
  ggplot2::annotate(
    "text",
    x = 0.115, y = 0.5, hjust = 0, vjust = 0.5, size = 3.1, colour = "grey20",
    label = paste(
      "corner icon: node summarises measured data from inside the",
      "AEP bounding box (\"local\")"
    )
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 1)) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_void()

final <- patchwork::wrap_plots(
  p, legend_plot, key_plot,
  ncol = 1, heights = c(1, 0.2, 0.06)
)

out_height <- height + 1.0
ggplot2::ggsave(
  out_png, final,
  width = width, height = out_height, dpi = 300,
  device = ragg::agg_png, bg = "white"
)
message("wrote ", out_png, "  (", width, " x ", round(out_height, 1), " in)")
