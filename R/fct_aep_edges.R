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

#' Quadratic Bezier Control Points for a Set of Edges
#'
#' One control point per edge, offset perpendicular from the chord midpoint,
#' so the curve bends away from a straight line by `curvature` (as a fraction
#' of the edge's own length). Expands each edge into three rows -- start,
#' control point, end -- which is what `ggforce::geom_bezier()` needs: it
#' infers the curve's order from how many points share a `group`, so three
#' points draws a quadratic curve and, if this is ever extended to more
#' control points per edge, the same function and the same call site would
#' draw a genuine multi-point curve with no other change. That is the reason
#' this exists as ggforce rather than `ggplot2::geom_curve()` (2026-08-05),
#' which tops out at one control point.
#'
#' Sign convention matches the old `geom_curve(curvature = ...)`: rotating the
#' edge direction 90 degrees the same way for every edge means every edge
#' bends to the same relative side of its own start-to-end direction, so the
#' diagram reads as one consistent style rather than a jumble of arbitrary
#' bends.
#'
#' @param edges A clipped edges tibble with `x`, `y`, `xend`, `yend` (from
#'   [aep_edge_coords()]) and `edge_id`.
#' @param curvature Control-point offset as a fraction of edge length. `0` is
#'   a straight line.
#' @return A tibble with one row per point, `edge_id`, `.point` (1:3, start to
#'   end) and `x`, `y`.
#' @export
aep_edge_bezier_points <- function(edges, curvature = 0.15) {
  if (nrow(edges) == 0) {
    return(tibble::tibble(
      edge_id = character(0), .point = integer(0),
      x = numeric(0), y = numeric(0)
    ))
  }

  dx <- edges$xend - edges$x
  dy <- edges$yend - edges$y
  len <- sqrt(dx^2 + dy^2)

  # Unit perpendicular, rotating (dx, dy) by +90 degrees. Zero-length edges
  # (overlapping cards) are already dropped upstream by aep_edge_coords(), but
  # guarded here too since dividing by len = 0 would otherwise produce NA
  # control points and silently drop the curve.
  perp_x <- ifelse(len == 0, 0, -dy / len)
  perp_y <- ifelse(len == 0, 0, dx / len)

  ctrl_x <- (edges$x + edges$xend) / 2 + perp_x * curvature * len
  ctrl_y <- (edges$y + edges$yend) / 2 + perp_y * curvature * len

  tibble::tibble(
    edge_id = rep(edges$edge_id, each = 3),
    .point = rep(1:3, times = nrow(edges)),
    x = as.vector(rbind(edges$x, ctrl_x, edges$xend)),
    y = as.vector(rbind(edges$y, ctrl_y, edges$yend))
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
#' @param curvature Bend applied to every edge, passed to
#'   [aep_edge_bezier_points()]. `0` is a straight line.
#' @param tile_size,tile_aspect Bare-node rectangle width (as a fraction of
#'   panel width) and height-over-width, used only when `node_images` is not
#'   supplied. Analogous to `image_size`/`card_aspect` for the real cards, and
#'   deliberately reusing [node_card_extent()] to compute them: a bare node is
#'   geometrically the same problem (a fixed-size rectangle at a data
#'   coordinate whose extent has to be known for edge clipping), just filled
#'   with `geom_tile()` instead of an image. See the note below on why this
#'   replaced `geom_label()`.
#' @return A ggplot.
#' @export
plot_aep <- function(nodes, edges, cards = NULL, label_edges = TRUE,
                     groups = NULL, node_images = NULL, image_size = 0.16,
                     card_aspect = 1.8 / 2.4, device_aspect = 12 / 8,
                     curvature = 0.15, tile_size = 0.14, tile_aspect = 0.45) {
  placed <- nodes |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))
  if (nrow(placed) == 0) {
    return(triage_empty_plot("AEP", "no nodes have x/y coordinates"))
  }

  styles <- aep_edge_styles()

  # A bare node used to be geom_label(): text with a background and a rounded
  # border, sized however wide the label happened to be. Sam 2026-08-07:
  # that isn't "any kind of geometrically explicit geometry", so an edge had
  # nothing real to clip to and fell back to the old fractional trim (see
  # aep_edge_coords()) -- not wrong exactly, but not the same box-clipping the
  # real cards get, and not why an edge missed a corner was ever obvious from
  # the bare diagram. A bare node is now geom_tile(): a real fixed-size
  # rectangle, computed by the SAME node_card_extent() the card path uses
  # (tile_size/tile_aspect standing in for image_size/card_aspect), so
  # aep_edge_coords() clips to it exactly as it does a card.
  ext <- if (!is.null(node_images) && length(node_images) > 0) {
    node_card_extent(
      placed,
      image_size = image_size,
      card_aspect = card_aspect,
      device_aspect = device_aspect
    )
  } else {
    node_card_extent(
      placed,
      image_size = tile_size,
      card_aspect = tile_aspect,
      device_aspect = device_aspect
    )
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
  #
  # ext$hw/ext$hh (computed above for arrow clipping) are passed through here
  # too: the box padding otherwise has no idea how tall a card is, and a card
  # taller than the default pad draws over both the box edge and its label.
  if (!is.null(groups) && nrow(groups) > 0) {
    p <- p + aep_group_layers(
      aep_group_boxes(groups, placed, card_hw = ext$hw, card_hh = ext$hh)
    )
  }

  if (nrow(e) > 0) {
    for (st in names(styles$linetype)) {
      sub <- e[e$status == st, , drop = FALSE]
      if (nrow(sub) == 0) {
        next
      }
      # ggforce::geom_bezier(), not ggplot2::geom_curve(): same idea (bend
      # every edge by `curvature` instead of drawing it straight), but built
      # on real control points via aep_edge_bezier_points() rather than
      # geom_curve()'s single implicit one. Sam 2026-08-07 asked for "elbow
      # joints, splines, or some other kind of more pretty line", and then
      # specifically for the ggforce route once told plain ggplot2 tops out at
      # one control point. The payoff beyond decoration: several edges here
      # cross at a shallow angle near the figure's centre, and a curve
      # visibly separates two lines that would otherwise overlap along most of
      # their length. All edges bend the same relative way (see
      # aep_edge_bezier_points()), so the diagram reads as one consistent
      # style.
      p <- p + ggforce::geom_bezier(
        data = aep_edge_bezier_points(sub, curvature = curvature),
        ggplot2::aes(x = .data$x, y = .data$y, group = .data$edge_id),
        linetype = styles$linetype[[st]],
        colour = styles$colour[[st]],
        linewidth = styles$linewidth[[st]],
        alpha = styles$alpha[[st]],
        arrow = ggplot2::arrow(
          length = ggplot2::unit(6, "pt"), type = "closed"
        )
        # No arrow.fill: 2026-08-07 briefly added one here on a hypothesis
        # that the arrowhead was rendering unfilled/invisible, then reverted
        # -- Sam confirmed arrowheads were actually visible before but
        # overplotted by the node card images on some edges, which is the
        # opposite problem, and my inspection of a rendered PNG at low
        # res/crop was not reliable enough to have caught that. See
        # node_card_extent()/aep_edge_coords() for the real mechanism: edges
        # are clipped to a box around each card BEFORE the card image is
        # drawn on top, so an undersized box is what lets an arrowhead land
        # under the image rather than short of it.
      )
    }

    if (label_edges) {
      lab <- e[e$status == "empirical" & !is.na(e$magnitude), , drop = FALSE]
      if (nrow(lab) > 0) {
        p <- p + ggplot2::geom_label(
          data = lab,
          ggplot2::aes(
            # The CHORD midpoint, not a point on the curve itself: at
            # curvature = 0.15 the two are close enough that computing the
            # true bezier midpoint is not worth the complexity it would add.
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
