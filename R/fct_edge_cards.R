# Edge report cards (2026-08-27; stripped right down 2026-09-08).
#
# The edge counterpart to R/fct_node_cards.R: each non-rejected edge gets a
# small card to place next to its arrow when the AEP figures are assembled by
# hand.
#
# WHAT IS ON THE CARD, AND WHY IT IS SO LITTLE.
#
#   * THE EPEQ BADGE STRIP. Four coloured squares, one per criterion in the
#     fixed order essentiality / plausibility / evidence / quantification, each
#     with its 1-3 digit (or a dash) centred in it. The criterion letters were
#     dropped 2026-09-09 (Sam) to shrink the card; the order is fixed so they
#     add nothing. It mirrors the coloured H/M/L marks Peng et al. (2022) draw
#     on each KTR arrow (their Fig. 2, Fig. 4).
#   * THE EDGE ID, small and grey, as a handle for talking about the edge and
#     for lining the cards up in PowerPoint. Not information about the edge.
#     Text shrunk 1.8 -> 1.35 -> 0.81 over 2026-09-09.
#
# WHAT WAS REMOVED 2026-09-08 (Sam):
#
#   * NO DESCRIPTION AT ALL. The wrapped "X to Y" sentence was demoted to a
#     grey suffix on the id line on 2026-09-08, then dropped entirely on
#     2026-09-09 once the id itself became "E<n>-<from slug>-to-<to slug>" and
#     the label was just repeating it.
#   * NO MAGNITUDE / FLUX / COUNTS. Peng et al., the framework reference, scores
#     KTRs on the four EPEQ criteria and does not quantify inter-compartment
#     flux. We are not sourcing flux rates for submission, so the magnitude / sd
#     / n / refs block (and the deliberate blank line above it) is gone. The
#     aep_edges.csv columns stay on file; the card just no longer reads them.
#   * NO LEVEL-COLOURED BACKGROUND. An edge has no level, so the card is white.
#
# Putative vs empirical is still NOT written on the card: it is carried by the
# edge's line style in the diagram, and by the card existing at all (rejected
# edges get none). See aep_edge_statuses() / drop_rejected_edges().
#
# Far smaller than the old 2.4 x 1.6 in, and a shallow strip rather than a
# block: the default canvas is 0.8 x 0.24 in (halved 2026-09-09).

#' The Id Strip at the Top of an Edge Card
#'
#' The edge id alone, small and grey, left-aligned. A handle for referring to
#' the edge and lining cards up for hand-assembly, not information to read at a
#' glance. The `label` was dropped 2026-09-09 (Sam): after the id gained the
#' `E<n>-<from>-to-<to>` slug form, the label just repeated it.
#'
#' @param edge A one-row edges tibble carrying `edge_id`.
#' @param text_size Text size. Default 0.81 since 2026-09-09 (1.8 -> 1.35 ->
#'   0.81 over successive passes to shrink the edge card).
#' @return A ggplot.
#' @export
edge_card_header <- function(edge, text_size = 0.81) {
  id <- if (length(edge$edge_id) == 0 || is.na(edge$edge_id[1])) {
    ""
  } else {
    as.character(edge$edge_id[1])
  }

  # A 0..1 x 0..1 space with the text pinned near the left edge. theme_void, so
  # nothing else is drawn; a small inset from x = 0 keeps the glyphs off the
  # card border once the 1pt plot margin is added.
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.02, y = 0.5, hjust = 0, vjust = 0.5,
      label = id, size = text_size, colour = "grey55"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_void()
}

#' Relative Heights of an Edge Card's Two Panels
#'
#' Id strip then badges. Pulled out so anything illustrating the layout reads
#' the real ratio, same reasoning as [node_card_heights()].
#'
#' @return A named numeric vector, `c(id, badges)`.
#' @export
edge_card_heights <- function() {
  # Both bands are shallow. The badge tiles fill their band whatever its pixel
  # height, so the band height IS the badge height: kept close to the id band so
  # the badges read as a thin strip (Sam 2026-09-08, "about 1/3" of the taller
  # first cut), not tall blocks.
  c(id = 0.9, badges = 1)
}

#' Assemble One Edge Card
#'
#' The [edge_card_header()] id strip over the shared EPEQ badge strip. White, no
#' distribution panel, no label. See the header of this file.
#'
#' @param edge A one-row edges tibble.
#' @return A patchwork object.
#' @export
edge_card <- function(edge) {
  id <- edge_card_header(edge)
  # labels = FALSE: four separate coloured squares, one per criterion in the
  # fixed order essentiality / plausibility / evidence / quantification, each
  # with its digit centred in it. node_epeq_badges() already takes an edges row.
  badges <- node_epeq_badges(edge, text_size = 1.9, labels = FALSE)

  patchwork::wrap_plots(
    id,
    badges,
    ncol = 1,
    heights = edge_card_heights()
  ) &
    ggplot2::theme(
      # White, not node_card_theme()'s level tint: an edge has no level.
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "pt")
    )
}

#' Write an Edge Card per Non-Rejected Edge, per AEP
#'
#' One PNG per live edge into `dir/<aep_id>/<edge_id>.png`, mirroring
#' [write_aep_node_cards()]. An edge belongs to an AEP when both its endpoints
#' do ([aep_scope_edges()]); rejected edges are dropped ([drop_rejected_edges()]).
#'
#' The card content does not vary by AEP -- the scores are hand-entered on the
#' edge, not recomputed under a scope -- so the same edge produces an identical
#' card in each AEP it appears in. The per-AEP subdirectories exist so the
#' layout matches the node cards for hand assembly.
#'
#' @param scoped Output of [aep_scoped_nodes()].
#' @param edges The full edges table.
#' @param dir Parent directory; each AEP gets a subdirectory of it.
#' @param width,height,dpi Canvas. A shallow strip: the id line over the EPEQ
#'   badge chips. Halved to 0.8 in wide on 2026-09-09 (Sam), a fifth of the
#'   2.4 x 1.6 it started at.
#' @return The written paths, across all AEPs.
#' @export
write_aep_edge_cards <- function(
  scoped,
  edges,
  dir = here_rel("images/edge_cards"),
  width = 0.8,
  height = 0.24,
  dpi = 300
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  live <- drop_rejected_edges(edges)

  unlist(
    purrr::imap(scoped, function(nodes, id) {
      mine <- aep_scope_edges(live, nodes)
      if (nrow(mine) == 0) {
        return(character(0))
      }
      sub <- file.path(dir, id)
      dir.create(sub, showWarnings = FALSE, recursive = TRUE)

      vapply(seq_len(nrow(mine)), function(i) {
        edge <- mine[i, , drop = FALSE]
        path <- file.path(sub, paste0(edge$edge_id[1], ".png"))
        ggplot2::ggsave(
          filename = path,
          plot = edge_card(edge),
          width = width,
          height = height,
          dpi = dpi,
          device = ragg::agg_png,
          bg = "white"
        )
        path
      }, character(1))
    }),
    use.names = FALSE
  )
}
