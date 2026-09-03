# Figure 1: an illustrative aggregate exposure pathway (copper -> fish), drawn
# with the same node-card design as the real AEPs. Hand-run, never a target.
#
# A five-node linear chain, one node per AEP stage (aep_node_levels()):
#   source -> exposure_medium -> external_exposure -> internal_exposure
#     -> target_site_exposure
#
# The nodes are `external` type carrying typed illustrative values, so the figure
# teaches the SHAPE of an AEP without standing in for real data. Writes
# figures/fig01-example-aep.png, which _01-introduction.qmd embeds as {#fig-aep-intro}.
#
#   Rscript scripts/build_fig1_example_aep.R

suppressMessages(pkgload::load_all(quiet = TRUE))

out_png <- here::here("figures/fig01-example-aep.png")

nodes <- empty_aep_nodes() |>
  tibble::add_row(
    node_id = c("n1", "n2", "n3", "n4", "n5"),
    # Kept short: node_card() wraps a label at 24 characters and a wrapped
    # second line collides with the headline underneath it.
    label = c(
      "Copper discharge", "Seawater", "Gill-surface copper",
      "Gill tissue copper", "Ion-transport site"
    ),
    level = aep_node_levels(),
    node_type = "external",
    x = 0:4,
    y = 0,
    # Round-number placeholders. The point is the chain, not the values.
    external_value = c(120, 0.6, 0.55, 8.5, NA),
    external_unit = c("kg/yr", "ug/L", "ug/L", "mg/kg ww", NA),
    external_refs = 1,
    essentiality_score = c(1, 3, 3, 3, 3),
    essentiality_justification = "Illustrative example, not a scored assessment.",
    plausibility_score = 3,
    plausibility_justification = "Illustrative example, not a scored assessment.",
    evidence_score = c(3, 3, 2, 2, 1),
    evidence_justification = "Illustrative example, not a scored assessment.",
    quantification_score = c(3, 3, 2, 2, 1),
    quantification_justification = "Illustrative example, not a scored assessment.",
    drop_outliers = FALSE
  )

edges <- tibble::tibble(
  edge_id = sprintf("e%d", 1:4),
  from = nodes$node_id[1:4],
  to = nodes$node_id[2:5],
  label = c("release", "contact", "uptake", "internal distribution"),
  status = c("empirical", "empirical", "putative", "putative"),
  magnitude = NA_real_, magnitude_unit = NA_character_,
  magnitude_n = NA_real_, magnitude_sd = NA_real_, magnitude_refs = NA_real_,
  essentiality_score = NA_real_, essentiality_justification = NA_character_,
  plausibility_score = NA_real_, plausibility_justification = NA_character_,
  evidence_score = NA_real_, evidence_justification = NA_character_,
  quantification_score = NA_real_, quantification_justification = NA_character_,
  notes = NA_character_
)

empty_members <- tibble::tibble(node_id = character(), group_id = character())
empty_data <- tibble::tibble()

cards <- aep_node_report_cards(nodes, empty_members, empty_data, NULL)

# Cards drawn one at a time rather than through write_node_cards(), so the
# data-free body panel comes out blank (blank_when_empty = TRUE) instead of
# carrying a "Not available" message that would read as a real absence.
card_dir <- file.path(tempdir(), "fig1-cards")
dir.create(card_dir, showWarnings = FALSE, recursive = TRUE)
img_paths <- vapply(seq_len(nrow(nodes)), function(i) {
  nd <- nodes[i, , drop = FALSE]
  cd <- cards[cards$node_id == nd$node_id[1], , drop = FALSE]
  pc <- node_card(
    nd, cd, empty_members, empty_data, NULL,
    limits = NULL, dpi = 300, blank_when_empty = TRUE
  )
  f <- file.path(card_dir, paste0(nd$node_id[1], ".png"))
  ggplot2::ggsave(
    f, pc,
    width = 2.4, height = 1.8, dpi = 300,
    device = ragg::agg_png, bg = node_card_bg_colour(nd)
  )
  f
}, character(1))
node_images <- stats::setNames(
  img_paths, tools::file_path_sans_ext(basename(img_paths))
)

width <- 18
image_size <- aep_diagram_image_size(nodes, 0.22)
height <- aep_diagram_height(
  nodes,
  effective_width = width,
  image_size = image_size,
  card_aspect = 1.8 / 2.4,
  min_height = 3.6
)

p <- plot_aep(
  nodes,
  edges,
  cards,
  node_images = node_images,
  image_size = image_size,
  device_aspect = width / height
)

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

final <- patchwork::wrap_plots(p, legend_plot, ncol = 1, heights = c(1, 0.17))

out_height <- height + 1.0
ggplot2::ggsave(
  out_png, final,
  width = width, height = out_height, dpi = 300,
  device = ragg::agg_png, bg = "white"
)
message("wrote ", out_png, "  (", width, " x ", round(out_height, 1), " in)")
