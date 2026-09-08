# Edge report cards (2026-08-27; stripped right down 2026-09-08).
#
# The edge counterpart to R/fct_node_cards.R: each non-rejected edge gets a
# small card to place next to its arrow when the AEP figures are assembled by
# hand.
#
# WHAT IS ON THE CARD, AND WHY IT IS SO LITTLE.
#
#   * THE EPEQ BADGE STRIP. Es / Pl / Ev / Qn with the edge's four scores. This
#     is the whole point of an edge card, and it mirrors the coloured H/M/L
#     letters Peng et al. (2022) draw on each KTR arrow (their Fig. 2, Fig. 4).
#   * THE EDGE ID, small and grey, as a handle for talking about the edge and
#     for lining the cards up in PowerPoint. Not information about the edge.
#
# WHAT WAS REMOVED 2026-09-08 (Sam):
#
#   * NO STANDALONE TITLE. The wrapped "X to Y" sentence was the single biggest
#     thing on the card and it dominated it. The description is not gone, but
#     demoted: it rides along on the id line, small and grey, truncated to one
#     line (Sam 2026-09-08 wanted it off the manuscript text but kept on the
#     card as a hand-assembly aid).
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
# block: the default canvas is 1.6 x 0.24 in. The extra width over the badges'
# own footprint is for the description that rides on the id line.

#' The Id Strip at the Top of an Edge Card
#'
#' The edge id, then its "X to Y" description on the same line, small and grey,
#' left-aligned. A handle for referring to the edge and lining cards up for
#' hand-assembly, not information to read at a glance. The description is
#' truncated to one line at card width; the id leads so near-identical
#' descriptions (`Submarine tailing disposal to ...`) still disambiguate.
#'
#' @param edge A one-row edges tibble carrying `edge_id`, and ideally `label`
#'   (falls back to `from` / `to` when `label` is missing or blank).
#' @param text_size Text size.
#' @param label_chars Characters to keep of the description before an ellipsis.
#' @return A ggplot.
#' @export
edge_card_header <- function(edge, text_size = 1.8, label_chars = 34) {
  id <- if (length(edge$edge_id) == 0 || is.na(edge$edge_id[1])) {
    ""
  } else {
    as.character(edge$edge_id[1])
  }

  lab <- if (
    "label" %in% names(edge) &&
      !is.na(edge$label[1]) &&
      nzchar(edge$label[1])
  ) {
    edge$label[1]
  } else if (all(c("from", "to") %in% names(edge))) {
    paste(edge$from[1], "to", edge$to[1])
  } else {
    ""
  }
  lab <- stringr::str_trunc(lab, width = label_chars, ellipsis = "...")

  text <- if (nzchar(id) && nzchar(lab)) paste0(id, "   ", lab) else paste0(id, lab)

  # A 0..1 x 0..1 space with the text pinned near the left edge. theme_void, so
  # nothing else is drawn; a small inset from x = 0 keeps the glyphs off the
  # card border once the 1pt plot margin is added.
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0.02, y = 0.5, hjust = 0, vjust = 0.5,
      label = text, size = text_size, colour = "grey55"
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
  # text_size 1.9: the card is roughly half the linear size it used to be and
  # the badge band is a thin strip, so the badge text comes down with it.
  # node_epeq_badges() already takes an edges row.
  badges <- node_epeq_badges(edge, text_size = 1.9)

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
#' @param width,height,dpi Canvas. A shallow strip (Sam 2026-09-08): the id line
#'   with its truncated description, over the EPEQ badge strip. Far less room
#'   than the 2.4 x 1.6 it started at.
#' @return The written paths, across all AEPs.
#' @export
write_aep_edge_cards <- function(
  scoped,
  edges,
  dir = here_rel("images/edge_cards"),
  width = 1.6,
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
