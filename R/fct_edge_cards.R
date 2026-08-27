# Edge report cards (2026-08-27).
#
# The edge counterpart to R/fct_node_cards.R, built once Sam moved to assembling
# the AEP figures by hand: each edge gets a small card to place next to its
# arrow, the same way node cards are placed next to nodes.
#
# It is the node card with two panels removed and nothing added:
#
#   * NO DISTRIBUTION PANEL. An edge carries a single hand-entered magnitude,
#     not a pool of measurements, so there is no violin to draw. The magnitude
#     and its counts are text.
#   * NO LEVEL-COLOURED BACKGROUND. node_card_theme() tints a node card by
#     source / medium / organism / tse. An edge has no level, so the card is
#     plain white.
#   * A BLANK LINE between the quantity and the counts (Sam's ask). The node
#     card deliberately packs those together; the edge card deliberately does
#     not.
#
# Putative vs empirical is NOT written on the card. It is already carried by the
# edge's line style in the diagram, and by the card existing at all (rejected
# edges get none). See aep_edge_statuses() / drop_rejected_edges().
#
# The EPEQ badge strip is shared with the node card unchanged: node_epeq_badges()
# already accepts an edges row, and an edge carries all four scores.

#' The Text Block at the Top of an Edge Card
#'
#' Edge id in the corner, the wrapped label, the magnitude with its unit, a
#' blank line, then the counts. Mirrors [node_card_header()] but for a claim
#' about a flow rather than a pool of measurements.
#'
#' @param edge A one-row edges tibble, carrying `edge_id`, `label`, `magnitude`,
#'   `magnitude_unit`, `magnitude_sd`, `magnitude_n`, `magnitude_refs`.
#' @param dpi The device resolution the card will be saved at. Only affects the
#'   corner id marker, which is offset in pixels; see [node_card_header()].
#' @return A ggplot.
#' @export
edge_card_header <- function(edge, dpi = 300) {
  num <- function(x) {
    if (length(x) == 0 || is.na(x)) {
      "-"
    } else {
      trimws(formatC(x, format = "g", digits = 3))
    }
  }
  count <- function(x) {
    if (length(x) == 0 || is.na(x)) "-" else format(x, big.mark = ",")
  }

  unit <- if (length(edge$magnitude_unit) == 0 || is.na(edge$magnitude_unit)) {
    ""
  } else {
    edge$magnitude_unit[1]
  }

  # THE QUANTITY. A magnitude with its unit, and its sd where set. "-" when the
  # edge carries no number, which is every putative edge and most empirical ones
  # so far -- the card still has to render.
  mag <- edge$magnitude[1]
  sd <- if ("magnitude_sd" %in% names(edge)) edge$magnitude_sd[1] else NA_real_
  quantity <- if (length(mag) == 0 || is.na(mag)) {
    "-"
  } else {
    paste0(
      num(mag),
      if (nzchar(unit)) paste0(" ", unit) else "",
      if (!is.na(sd)) paste0(" (sd ", num(sd), ")") else ""
    )
  }

  # THE COUNTS, after a blank line. n is the observations behind the magnitude,
  # refs the references behind it -- the edge analogues of the node card's
  # `n` / `refs`. Both hand-entered on aep_edges.csv, both "-" until they are.
  mag_n <- if ("magnitude_n" %in% names(edge)) edge$magnitude_n[1] else NA_real_
  mag_refs <- if ("magnitude_refs" %in% names(edge)) {
    edge$magnitude_refs[1]
  } else {
    NA_real_
  }
  counts <- paste0("n = ", count(mag_n), ", refs = ", count(mag_refs))

  label <- if (length(edge$label) == 0 || is.na(edge$label)) {
    paste(edge$from[1], "to", edge$to[1])
  } else {
    edge$label[1]
  }

  corner_offset <- grid::unit(2 / dpi, "inches")

  # A 0..10 coordinate space. The label is anchored near the top and grows
  # downward (it can be two or three lines -- these labels are sentences, "X to
  # Y"); the quantity and counts sit lower with a deliberate gap between them.
  ggplot2::ggplot() +
    # Edge id pinned to the physical top-left corner, same treatment as the node
    # id on a node card: a handle for talking about the edge, not information
    # about it, so it sits out of the reading path.
    ggplot2::annotation_custom(
      grid::textGrob(
        edge$edge_id[1],
        x = corner_offset,
        y = grid::unit(1, "npc") - corner_offset,
        hjust = 0,
        vjust = 1,
        gp = grid::gpar(fontsize = 0.7 * 2.6 * ggplot2::.pt, col = "grey55")
      )
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 9.6, hjust = 0.5, vjust = 1,
      size = 2.9, fontface = "bold",
      label = stringr::str_wrap(label, width = 30),
      lineheight = 0.9, colour = "grey10"
    ) +
    # The gap between the quantity (y = 3.4) and the counts (y = 1.0) is the
    # blank line Sam asked for: a packed card would put the counts ~1.2 below.
    ggplot2::annotate(
      "text",
      x = 0.5, y = 3.4, hjust = 0.5, vjust = 0.5,
      size = 3.1, fontface = "bold",
      label = quantity, colour = "grey5"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5, y = 1.0, hjust = 0.5, vjust = 0.5,
      size = 2.5,
      label = counts, colour = "grey40"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    ggplot2::theme_void()
}

#' Relative Heights of an Edge Card's Two Panels
#'
#' Header then badges. Pulled out so anything illustrating the layout reads the
#' real ratio, same reasoning as [node_card_heights()].
#'
#' @return A named numeric vector, `c(header, badges)`.
#' @export
edge_card_heights <- function() {
  c(header = 2.0, badges = 0.45)
}

#' Assemble One Edge Card
#'
#' The [edge_card_header()] text block over the shared EPEQ badge strip. Plain
#' white, no distribution panel. See the header of this file.
#'
#' @param edge A one-row edges tibble.
#' @param dpi The device resolution the card will be saved at; passed to
#'   [edge_card_header()] for the corner id marker.
#' @return A patchwork object.
#' @export
edge_card <- function(edge, dpi = 300) {
  header <- edge_card_header(edge, dpi = dpi)
  # text_size 3.0: a touch under the node card's 3.4, because the edge card is
  # drawn smaller. node_epeq_badges() already takes an edges row.
  badges <- node_epeq_badges(edge, text_size = 3.0)

  patchwork::wrap_plots(
    header,
    badges,
    ncol = 1,
    heights = edge_card_heights()
  ) &
    ggplot2::theme(
      # White, not node_card_theme()'s level tint: an edge has no level.
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(2, 2, 2, 2, unit = "pt")
    )
}

#' Write an Edge Card per Non-Rejected Edge, per AEP
#'
#' One PNG per live edge into `dir/<aep_id>/<edge_id>.png`, mirroring
#' [write_aep_node_cards()]. An edge belongs to an AEP when both its endpoints
#' do ([aep_scope_edges()]); rejected edges are dropped ([drop_rejected_edges()]).
#'
#' The card content does not vary by AEP -- magnitude and scores are hand-entered
#' on the edge, not recomputed under a scope -- so the same edge produces an
#' identical card in each AEP it appears in. The per-AEP subdirectories exist so
#' the layout matches the node cards for hand assembly.
#'
#' @param scoped Output of [aep_scoped_nodes()].
#' @param edges The full edges table.
#' @param dir Parent directory; each AEP gets a subdirectory of it.
#' @param width,height,dpi Canvas. Smaller than a node card (2.4 x 1.8): the
#'   edge card has two bands, not three, and no distribution to make room for.
#' @return The written paths, across all AEPs.
#' @export
write_aep_edge_cards <- function(
  scoped,
  edges,
  dir = here_rel("figures/edge_cards"),
  width = 2.4,
  height = 1.6,
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
          plot = edge_card(edge, dpi = dpi),
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
