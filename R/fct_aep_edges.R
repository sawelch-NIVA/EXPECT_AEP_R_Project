# The AEP edge layer (PLAN.md Phase 4) and the diagram itself (P5.1).
# Added 2026-08-05.
#
# Edges are the stated highest risk in PLAN.md, and the mitigation there is
# explicit: "The mitigation is scope, not effort." Two things follow from that
# and are built in rather than left to discipline.
#
# 1. EVERY EDGE STARTS `putative`. Marking one `empirical` is a positive act
#    requiring a citation, not the default state. P4.2 time-boxes gap-filling to
#    roughly 30 minutes per edge: found, score it and cite it; not found, write
#    one sentence on what evidence WOULD settle it and move on.
#
# 2. `putative` IS DRAWN DIFFERENTLY, and that is what makes stopping defensible.
#    PLAN.md: "Sparse edges are a finding, not a shortfall, provided empirically
#    supported edges are visually distinguished from putative ones." A diagram
#    that quietly implies completeness is the failure mode; one that marks its
#    own gaps is a contribution.
#
# from/to name NODE IDS, not labels. Same reasoning as `lump_into` naming a
# group_id: a label is prose and will be reworded, an id is an identifier.

#' Permitted Edge Statuses
#'
#' * `putative` -- a flow we believe exists but have not evidenced here. The
#'   default, and not a failure.
#' * `empirical` -- supported by evidence, which must be named in
#'   `evidence_justification`.
#'
#' @return A character vector.
#' @export
aep_edge_statuses <- function() {
  c("putative", "empirical")
}

#' An Empty Edges Table
#'
#' @return A zero-row tibble.
#' @export
empty_aep_edges <- function() {
  tibble::tibble(
    edge_id = character(0),
    from = character(0),
    to = character(0),
    label = character(0),
    status = character(0),
    magnitude = numeric(0),
    magnitude_unit = character(0),
    magnitude_n = numeric(0),
    magnitude_sd = numeric(0),
    essentiality_score = numeric(0),
    essentiality_justification = character(0),
    plausibility_score = numeric(0),
    plausibility_justification = character(0),
    evidence_score = numeric(0),
    evidence_justification = character(0),
    quantification_score = numeric(0),
    quantification_justification = character(0),
    notes = character(0)
  )
}

#' Read and Validate the AEP Edges File
#'
#' @param path Where the CSV lives.
#' @param nodes Optional nodes table, to check `from` and `to` both exist.
#' @return A tibble of edges.
#' @export
read_aep_edges <- function(
  path = here_rel("data/clean/aep/aep_edges.csv"),
  nodes = NULL
) {
  if (!file.exists(path)) {
    stop(
      "No edges file at ", path,
      ". Run scripts/scaffold_aep_edges.R first."
    )
  }
  edges <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      edge_id = readr::col_character(),
      from = readr::col_character(),
      to = readr::col_character(),
      label = readr::col_character(),
      status = readr::col_character(),
      magnitude_unit = readr::col_character(),
      notes = readr::col_character()
    )
  )

  missing <- setdiff(names(empty_aep_edges()), names(edges))
  if (length(missing) > 0) {
    stop("Edges file is missing column(s): ", paste(missing, collapse = ", "))
  }

  dup <- edges$edge_id[duplicated(edges$edge_id)]
  if (length(dup) > 0) {
    stop("Duplicate edge_id(s): ", paste(unique(dup), collapse = ", "))
  }

  bad <- setdiff(stats::na.omit(unique(edges$status)), aep_edge_statuses())
  if (length(bad) > 0) {
    stop(
      "Unrecognised status(es): ", paste(sQuote(bad), collapse = ", "),
      ". Permitted: ", paste(aep_edge_statuses(), collapse = ", ")
    )
  }

  self <- edges$edge_id[!is.na(edges$from) & edges$from == edges$to]
  if (length(self) > 0) {
    stop("Edge(s) from a node to itself: ", paste(self, collapse = ", "))
  }

  # A duplicated from/to pair is almost always a copy-paste rather than a genuine
  # second flow between the same two nodes.
  pair_dup <- edges |>
    dplyr::count(.data$from, .data$to) |>
    dplyr::filter(.data$n > 1)
  if (nrow(pair_dup) > 0) {
    cli::cli_warn(c(
      "{nrow(pair_dup)} node pair{?s} {?has/have} more than one edge:",
      stats::setNames(
        paste0(pair_dup$from, " -> ", pair_dup$to),
        rep("*", nrow(pair_dup))
      )
    ))
  }

  for (col in epeq_cols()[c(TRUE, FALSE)]) {
    v <- edges[[col]]
    bad_score <- !is.na(v) & !(v %in% 1:3)
    if (any(bad_score)) {
      stop(
        sum(bad_score), " row(s) have an out-of-range ", col,
        ": scores are 1, 2 or 3, or blank if unscored."
      )
    }
  }

  if (!is.null(nodes)) {
    unknown <- setdiff(c(edges$from, edges$to), nodes$node_id)
    unknown <- unknown[!is.na(unknown)]
    if (length(unknown) > 0) {
      stop(
        "Edges name ", length(unknown), " unknown node_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", ")
      )
    }
  }

  edges
}

#' Validate the Edge Layer
#'
#' Warnings rather than errors: a half-filled edge set is the normal state
#' during Phase 4, and the pipeline must still run.
#'
#' @param edges The edges table.
#' @param nodes The nodes table.
#' @return `edges`, invisibly.
#' @export
validate_aep_edges <- function(edges, nodes) {
  problems <- character(0)

  # An empirical claim with no justification is the one that matters: it asserts
  # evidence exists without saying what it is, and nothing downstream can tell
  # that apart from a real citation.
  unevidenced <- edges$edge_id[
    edges$status %in% "empirical" &
      (is.na(edges$evidence_justification) |
        !nzchar(trimws(edges$evidence_justification)))
  ]
  if (length(unevidenced) > 0) {
    problems <- c(problems, paste0(
      length(unevidenced),
      " edge(s) marked empirical with no evidence_justification: ",
      paste(unevidenced, collapse = ", ")
    ))
  }

  # A magnitude on a putative edge is a contradiction: putative means the flow is
  # not evidenced here, so a number attached to it will be read as one that is.
  contradictory <- edges$edge_id[
    edges$status %in% "putative" & !is.na(edges$magnitude)
  ]
  if (length(contradictory) > 0) {
    problems <- c(problems, paste0(
      length(contradictory), " putative edge(s) carry a magnitude: ",
      paste(contradictory, collapse = ", "),
      " (mark them empirical, or clear the magnitude)"
    ))
  }

  bare <- edges$edge_id[!is.na(edges$magnitude) & is.na(edges$magnitude_unit)]
  if (length(bare) > 0) {
    problems <- c(problems, paste0(
      length(bare), " edge(s) have a magnitude with no unit: ",
      paste(bare, collapse = ", ")
    ))
  }

  # Nodes nothing flows into or out of. Not necessarily wrong (a source has no
  # inflow, a target site exposure no outflow), so it is reported per node and
  # left to Sam.
  orphans <- setdiff(nodes$node_id, c(edges$from, edges$to))
  if (length(orphans) > 0) {
    problems <- c(problems, paste0(
      length(orphans), " node(s) have no edges at all: ",
      paste(orphans, collapse = ", ")
    ))
  }

  if (length(problems) > 0) {
    cli::cli_warn(c(
      "AEP edge layer is incomplete:",
      stats::setNames(problems, rep("*", length(problems)))
    ))
  }

  invisible(edges)
}

#' Half-Extent of a Node Card, in Data Units
#'
#' The geometry [aep_edge_coords()] needs in order to stop an arrow at the edge
#' of a card rather than under it.
#'
#' `ggimage::geom_image()` sizes an image as a fraction of **panel width**, so
#' its extent in data units depends on three things this function has to be told:
#' the coordinate range (after the expansion [plot_aep()] applies), the shape of
#' the card image itself, and the shape of the output device. Getting any of them
#' wrong moves the clip boundary, which is why they are named arguments with
#' documented defaults rather than constants buried in the drawing code.
#'
#' Defaults match the `aep_diagram` target: `node_cards_compact` is written at
#' 2.4 x 1.8 inches (`card_aspect = 1.8 / 2.4`) onto a 12 x 8 inch canvas
#' (`device_aspect = 12 / 8`). **Change either in `_targets.R` and these defaults
#' are wrong**, so both are passed explicitly from there.
#'
#' @param nodes The nodes table, with `x` and `y`.
#' @param image_size Card width as a fraction of panel width, as passed to
#'   `ggimage::geom_image()`.
#' @param card_aspect Card height divided by card width, in inches.
#' @param device_aspect Device width divided by device height, in inches.
#' @param x_expand,y_expand The multiplicative axis expansions [plot_aep()] uses.
#' @return A list with `hw` and `hh`, half-width and half-height in data units.
#' @export
node_card_extent <- function(
  nodes,
  image_size = 0.19,
  card_aspect = 1.8 / 2.4,
  device_aspect = 12 / 8,
  x_expand = 0.15,
  y_expand = 0.12
) {
  span <- function(v, mult) {
    v <- v[is.finite(v)]
    d <- if (length(v) < 2) 0 else diff(range(v))
    # A single placed node, or all nodes in a line, leaves one axis with no
    # range. ggplot2 falls back to a unit window there, so this does too.
    if (!is.finite(d) || d == 0) {
      d <- 1
    }
    d * (1 + 2 * mult)
  }

  rx <- span(nodes$x, x_expand)
  ry <- span(nodes$y, y_expand)

  list(
    hw = image_size * rx / 2,
    # Width is a fraction of panel WIDTH, so converting to a fraction of panel
    # height picks up the device aspect. A card 0.19 of a 12in panel is 2.28in
    # wide and 1.71in tall, which over an 8in panel is 0.214 of the height.
    hh = image_size * card_aspect * device_aspect * ry / 2
  )
}

#' Edge Coordinates for Plotting
#'
#' Joins each edge to its endpoints' hand-placed coordinates, and pulls both ends
#' back so the arrow starts and stops at the boundary of the node's card.
#'
#' **Clipping to the card box replaced a fixed fractional trim on 2026-08-06**,
#' which is the rough edge PLAN.md P5.1 recorded and deferred. Trimming a
#' *fraction* of the segment scales the gap with edge length, and the cards do
#' not: a card is roughly 0.25 data units wide either way, so a fractional trim
#' of 0.12 cleared it on a long diagonal and left the arrow buried under the card
#' on a short one. Every edge in `figures/aep.png` was one or the other.
#'
#' Where `hw`/`hh` are `NULL` the old fractional trim is kept, because a diagram
#' drawn with text labels rather than cards has no box to clip to.
#'
#' @param edges The edges table.
#' @param nodes The nodes table, with `x` and `y`.
#' @param trim Fraction of the segment removed at each end. Used only when
#'   `hw`/`hh` are `NULL`.
#' @param hw,hh Half-width and half-height of a node card in data units, from
#'   [node_card_extent()].
#' @param gap Clear space in data units between the card edge and the arrow.
#' @return A tibble of edges with `x`, `y`, `xend`, `yend`.
#' @export
aep_edge_coords <- function(
  edges,
  nodes,
  trim = 0.12,
  hw = NULL,
  hh = NULL,
  gap = 0.02
) {
  coords <- nodes |> dplyr::select("node_id", "x", "y")

  out <- edges |>
    dplyr::left_join(
      coords |> dplyr::rename(from = "node_id", x0 = "x", y0 = "y"),
      by = "from"
    ) |>
    dplyr::left_join(
      coords |> dplyr::rename(to = "node_id", x1 = "x", y1 = "y"),
      by = "to"
    ) |>
    dplyr::filter(!is.na(.data$x0), !is.na(.data$x1))

  if (nrow(out) == 0) {
    return(dplyr::mutate(
      out,
      x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0)
    ))
  }

  if (is.null(hw) || is.null(hh)) {
    return(dplyr::mutate(
      out,
      x = .data$x0 + (.data$x1 - .data$x0) * trim,
      y = .data$y0 + (.data$y1 - .data$y0) * trim,
      xend = .data$x1 - (.data$x1 - .data$x0) * trim,
      yend = .data$y1 - (.data$y1 - .data$y0) * trim
    ))
  }

  dx <- out$x1 - out$x0
  dy <- out$y1 - out$y0
  len <- sqrt(dx^2 + dy^2)

  # Where the straight line leaves an axis-aligned box centred on the node,
  # as a fraction of the whole segment. Whichever side it crosses first wins.
  # Every card is the same size, so the same fraction applies at both ends.
  t_box <- pmin(
    ifelse(dx == 0, Inf, hw / abs(dx)),
    ifelse(dy == 0, Inf, hh / abs(dy))
  )
  t_gap <- ifelse(len == 0, Inf, gap / len)
  t_cut <- t_box + t_gap

  out$x <- out$x0 + dx * t_cut
  out$y <- out$y0 + dy * t_cut
  out$xend <- out$x1 - dx * t_cut
  out$yend <- out$y1 - dy * t_cut

  # Two cards close enough to overlap leave nothing to draw. Drawing it anyway
  # produces a BACKWARDS arrow, which reads as a real flow in the wrong
  # direction, so the edge is dropped and the drop is reported rather than
  # silently swallowed.
  keep <- t_cut * 2 < 1
  if (any(!keep)) {
    cli::cli_warn(c(
      "{sum(!keep)} edge{?s} dropped from the diagram: the two cards overlap, \\
       leaving no room for an arrow between them.",
      stats::setNames(out$edge_id[!keep], rep("*", sum(!keep))),
      "i" = "Move the nodes further apart in aep_nodes.csv, or reduce image_size."
    ))
  }

  out[keep, , drop = FALSE]
}

#' Styling for Empirical and Putative Edges
#'
#' The distinction PLAN.md Phase 4 rests its argument on. Putative edges are
#' dashed, grey and thin; empirical ones solid, dark and thicker. Both keep an
#' arrowhead, because both are claims about direction.
#'
#' @return A list of named vectors, `linetype`, `colour`, `linewidth`, `alpha`.
#' @export
aep_edge_styles <- function() {
  list(
    linetype = c(putative = "22", empirical = "solid"),
    colour = c(putative = "grey65", empirical = "grey15"),
    linewidth = c(putative = 0.4, empirical = 0.8),
    alpha = c(putative = 0.8, empirical = 1)
  )
}

#' Draw the AEP
#'
#' **Manual coordinates, never an automatic layout.** Vertical position carries
#' source-to-exposure meaning, so a force-directed or hierarchical layout is not
#' merely different, it is wrong: it would place nodes to minimise edge crossings
#' and in doing so destroy the one thing the y axis is for. PLAN.md P5.1.
#'
#' Plain ggplot2 rather than ggraph, on the "boring fix that ships" principle in
#' CLAUDE.md. With coordinates already fixed there is no layout algorithm left to
#' want, and a graph package would add a dependency to draw segments and text.
#'
#' @param nodes The nodes table, with `x` and `y` populated.
#' @param edges The edges table.
#' @param cards Optional report cards from [aep_node_report_cards()]. When
#'   supplied, each node label gains its geometric mean and unit.
#' @param label_edges Annotate empirical edges with their magnitude?
#' @param groups Optional node groups from [read_aep_node_groups()], drawn as
#'   labelled boxes behind everything else. Nested groups inset automatically;
#'   see [aep_group_depth()].
#' @param node_images Optional named character vector of PNG paths keyed by
#'   `node_id`, from [write_node_cards()] with `style = "compact"`. Where
#'   supplied, each node is drawn as its card instead of a text label. This is
#'   PLAN.md P5.2, and it is what makes the figure a report-card AEP rather than
#'   a labelled graph.
#' @param image_size Card width as a fraction of plot width.
#' @param card_aspect,device_aspect Card and device shape, passed to
#'   [node_card_extent()] so arrows can be clipped to the cards. Defaults match
#'   the `aep_diagram` target; pass them explicitly from anywhere else.
#' @return A ggplot.
#' @export
plot_aep <- function(nodes, edges, cards = NULL, label_edges = TRUE,
                     groups = NULL, node_images = NULL, image_size = 0.16,
                     card_aspect = 1.8 / 2.4, device_aspect = 12 / 8) {
  placed <- nodes |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))
  if (nrow(placed) == 0) {
    return(triage_empty_plot("AEP", "no nodes have x/y coordinates"))
  }

  styles <- aep_edge_styles()

  # Clip to the card box only where cards are actually drawn. With text labels
  # there is no box, and the fractional trim remains the honest approximation.
  ext <- if (!is.null(node_images) && length(node_images) > 0) {
    node_card_extent(
      placed,
      image_size = image_size,
      card_aspect = card_aspect,
      device_aspect = device_aspect
    )
  } else {
    list(hw = NULL, hh = NULL)
  }

  e <- aep_edge_coords(edges, placed, hw = ext$hw, hh = ext$hh)
  if (nrow(e) > 0) {
    e$status <- dplyr::coalesce(e$status, "putative")
  }

  node_label <- if (!is.null(cards)) {
    placed |>
      dplyr::left_join(
        cards |> dplyr::select("node_id", "geo_mean", "unit", "n"),
        by = "node_id"
      ) |>
      dplyr::mutate(
        .label = dplyr::if_else(
          is.na(.data$geo_mean),
          .data$label,
          paste0(
            .data$label, "\n",
            formatC(.data$geo_mean, format = "g", digits = 3), " ", .data$unit,
            "  (n = ", format(.data$n, big.mark = ","), ")"
          )
        )
      )
  } else {
    dplyr::mutate(placed, .label = .data$label)
  }

  p <- ggplot2::ggplot()

  # Group boxes go on FIRST, so edges and nodes draw over them. They are
  # annotation and must never occlude content.
  if (!is.null(groups) && nrow(groups) > 0) {
    p <- p + aep_group_layers(aep_group_boxes(groups, placed))
  }

  if (nrow(e) > 0) {
    for (st in names(styles$linetype)) {
      sub <- e[e$status == st, , drop = FALSE]
      if (nrow(sub) == 0) {
        next
      }
      p <- p + ggplot2::geom_segment(
        data = sub,
        ggplot2::aes(
          x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend
        ),
        linetype = styles$linetype[[st]],
        colour = styles$colour[[st]],
        linewidth = styles$linewidth[[st]],
        alpha = styles$alpha[[st]],
        arrow = ggplot2::arrow(
          length = ggplot2::unit(6, "pt"), type = "closed"
        )
      )
    }

    if (label_edges) {
      lab <- e[e$status == "empirical" & !is.na(e$magnitude), , drop = FALSE]
      if (nrow(lab) > 0) {
        p <- p + ggplot2::geom_label(
          data = lab,
          ggplot2::aes(
            x = (.data$x + .data$xend) / 2,
            y = (.data$y + .data$yend) / 2,
            label = paste0(
              formatC(.data$magnitude, format = "g", digits = 3), " ",
              .data$magnitude_unit
            )
          ),
          size = 2.4, colour = "grey15", label.size = 0,
          fill = "white", alpha = 0.85
        )
      }
    }
  }

  # NODES: cards where images were supplied, text labels otherwise. Not a mix,
  # because two visual languages for the same object on one figure is worse than
  # either alone.
  p <- if (!is.null(node_images) && length(node_images) > 0) {
    have <- node_label[node_label$node_id %in% names(node_images), , drop = FALSE]
    have$.image <- unname(node_images[have$node_id])
    missing_img <- node_label[!node_label$node_id %in% names(node_images), , drop = FALSE]

    out <- p + ggimage::geom_image(
      data = have,
      ggplot2::aes(x = .data$x, y = .data$y, image = .data$.image),
      size = image_size,
      asp = 1.5
    )
    # A node with no card still has to appear, or the diagram silently loses it.
    if (nrow(missing_img) > 0) {
      out <- out + ggplot2::geom_label(
        data = missing_img,
        ggplot2::aes(x = .data$x, y = .data$y, label = .data$.label),
        size = 2.8, lineheight = 0.95, fill = "white", colour = "grey15",
        label.padding = ggplot2::unit(4, "pt")
      )
    }
    out
  } else {
    p + ggplot2::geom_label(
      data = node_label,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$.label),
      size = 2.8,
      lineheight = 0.95,
      fill = "white",
      colour = "grey15",
      label.padding = ggplot2::unit(4, "pt")
    )
  }

  p +
    # Expanded so labels at the edge of the coordinate range are not clipped.
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::labs(
      x = NULL, y = NULL,
      caption = paste(
        "Solid arrows: empirically supported.",
        "Dashed grey: putative, not evidenced here."
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.75), colour = "grey40", hjust = 0
      )
    )
}

#' A Small Bounding-Box Locator Map
#'
#' For a geographically scoped AEP (a manifest row with any of `lat_min`,
#' `lat_max`, `lon_min`, `lon_max` set), shows where its bounding box sits
#' against the whole study area, so the diagram does not have to be read next
#' to the manifest to know what "Repparfjorden" is scoped to.
#'
#' @param base_map The whole-study-area map, e.g. the `wgs84_map` target
#'   ([create_study_area_map_wgs84()]).
#' @param lat_min,lat_max,lon_min,lon_max Bounding box; `NA` on any side means
#'   unbounded and is drawn against the full plotted extent of `base_map` on
#'   that side.
#' @return A ggplot, stripped down for use as a side panel.
#' @export
aep_bbox_inset <- function(base_map, lat_min, lat_max, lon_min, lon_max) {
  built <- ggplot2::ggplot_build(base_map)
  xr <- built$layout$panel_scales_x[[1]]$range$range
  yr <- built$layout$panel_scales_y[[1]]$range$range

  rect <- data.frame(
    xmin = dplyr::coalesce(lon_min, xr[1]),
    xmax = dplyr::coalesce(lon_max, xr[2]),
    ymin = dplyr::coalesce(lat_min, yr[1]),
    ymax = dplyr::coalesce(lat_max, yr[2])
  )

  base_map +
    ggplot2::geom_rect(
      data = rect,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      colour = "firebrick", fill = "firebrick", alpha = 0.25, linewidth = 0.6
    ) +
    ggplot2::guides(fill = "none", colour = "none", alpha = "none") +
    ggplot2::theme(legend.position = "none")
}

#' Progress Through the Edge Time-Box
#'
#' P4.2 time-boxes gap-filling to three working days. This is the number to look
#' at when deciding whether to keep going or stop: how many edges are evidenced,
#' and how many remain putative.
#'
#' @param edges The edges table.
#' @return A one-row tibble.
#' @export
aep_edge_progress <- function(edges) {
  # Every field computed BEFORE the tibble() call. Naming the first column
  # `edges` inside tibble() puts it in scope for every later expression, so
  # `edges$status` then indexed the integer column just created and failed with
  # "$ operator is invalid for atomic vectors". Sequential evaluation plus data
  # masking; the fix is to not reference the argument inside the call at all.
  n_edges <- nrow(edges)
  n_empirical <- sum(edges$status %in% "empirical")
  n_putative <- sum(edges$status %in% "putative")
  n_magnitude <- sum(!is.na(edges$magnitude))
  n_scored <- sum(
    !is.na(edges$essentiality_score) & !is.na(edges$plausibility_score) &
      !is.na(edges$evidence_score) & !is.na(edges$quantification_score)
  )

  tibble::tibble(
    edges = n_edges,
    empirical = n_empirical,
    putative = n_putative,
    with_magnitude = n_magnitude,
    fully_scored = n_scored
  )
}
