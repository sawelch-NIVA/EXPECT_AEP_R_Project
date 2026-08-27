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
#' * `rejected` -- considered and cut. Not drawn, not counted, and the reason
#'   must be written in `notes`.
#'
#' `rejected` exists because **deleting the row does not work**, and that is
#' worth stating plainly since it was discovered the hard way (2026-08-12).
#' `scripts/scaffold_aep_edges.R` proposes new edges with an `anti_join` on
#' `from`/`to`, so a row that is absent is indistinguishable from one that was
#' never proposed: every deleted edge came back on the next scaffold run. That
#' is the same cache-versus-curation failure as the untracked
#' `group_decisions.csv` and the missing `imports = "STOPAEP"` -- a human
#' decision stored as an absence, which the machine then overwrites in silence.
#'
#' A rejected edge therefore **stays on file**, carrying its reason. The
#' scaffolder needs no change to respect it: its `anti_join` ignores status, so
#' the row's mere presence blocks re-proposal.
#'
#' @return A character vector.
#' @export
aep_edge_statuses <- function() {
  c("putative", "empirical", "rejected")
}

#' Edge Statuses That Are Drawn
#'
#' The complement of `rejected`. Split out rather than inlined because three
#' places need to agree on it ([plot_aep()], [validate_aep_edges()] and
#' [aep_edge_progress()]), and a diagram that draws a cut edge while the
#' progress table says it is gone is worse than either error alone.
#'
#' @return A character vector.
#' @export
aep_edge_live_statuses <- function() {
  setdiff(aep_edge_statuses(), "rejected")
}

#' Drop Rejected Edges
#'
#' `NA` status counts as live, matching [plot_aep()]'s longstanding
#' `coalesce(status, "putative")`: an unfilled cell is an edge not yet
#' considered, never one that was cut. Cutting is always a positive act.
#'
#' @param edges The edges table.
#' @return `edges` without its rejected rows.
#' @export
drop_rejected_edges <- function(edges) {
  edges[!(edges$status %in% "rejected"), , drop = FALSE]
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
    magnitude_refs = numeric(0),
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
      "No edges file at ",
      path,
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
      "Unrecognised status(es): ",
      paste(sQuote(bad), collapse = ", "),
      ". Permitted: ",
      paste(aep_edge_statuses(), collapse = ", ")
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
        sum(bad_score),
        " row(s) have an out-of-range ",
        col,
        ": scores are 1, 2 or 3, or blank if unscored."
      )
    }
  }

  if (!is.null(nodes)) {
    unknown <- setdiff(c(edges$from, edges$to), nodes$node_id)
    unknown <- unknown[!is.na(unknown)]
    if (length(unknown) > 0) {
      stop(
        "Edges name ",
        length(unknown),
        " unknown node_id(s): ",
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

  # A cut with no recorded reason is how a decision gets re-litigated in three
  # weeks. Same shape as the empirical/evidence_justification check below: the
  # status asserts a judgement was made, and `notes` is the only place the
  # judgement itself can live. `notes` rather than a dedicated column because
  # the rejection reason is prose, not a controlled vocabulary.
  unreasoned <- edges$edge_id[
    edges$status %in%
      "rejected" &
      (is.na(edges$notes) | !nzchar(trimws(edges$notes)))
  ]
  if (length(unreasoned) > 0) {
    problems <- c(
      problems,
      paste0(
        length(unreasoned),
        " edge(s) marked rejected with no reason in notes: ",
        paste(unreasoned, collapse = ", ")
      )
    )
  }

  # A rejected edge is not part of the diagram, so it must not carry the
  # scores or the magnitude of one. Left over from a change of mind, these
  # would be counted by anything that reads the scores without filtering.
  stale <- edges$edge_id[
    edges$status %in%
      "rejected" &
      (!is.na(edges$magnitude) |
        !is.na(edges$essentiality_score) |
        !is.na(edges$plausibility_score) |
        !is.na(edges$evidence_score) |
        !is.na(edges$quantification_score))
  ]
  if (length(stale) > 0) {
    problems <- c(
      problems,
      paste0(
        length(stale),
        " rejected edge(s) still carry a score or magnitude: ",
        paste(stale, collapse = ", "),
        " (clear them, or change the status)"
      )
    )
  }

  # Every check from here down asks about the diagram, and a rejected edge is
  # not in it. Most matter little either way; the orphan check matters a lot,
  # because a node whose only edges were cut is exactly the node that needs
  # reporting and would otherwise look connected.
  edges <- drop_rejected_edges(edges)

  # An empirical claim with no justification is the one that matters: it asserts
  # evidence exists without saying what it is, and nothing downstream can tell
  # that apart from a real citation.
  unevidenced <- edges$edge_id[
    edges$status %in%
      "empirical" &
      (is.na(edges$evidence_justification) |
        !nzchar(trimws(edges$evidence_justification)))
  ]
  if (length(unevidenced) > 0) {
    problems <- c(
      problems,
      paste0(
        length(unevidenced),
        " edge(s) marked empirical with no evidence_justification: ",
        paste(unevidenced, collapse = ", ")
      )
    )
  }

  # A magnitude on a putative edge is a contradiction: putative means the flow is
  # not evidenced here, so a number attached to it will be read as one that is.
  contradictory <- edges$edge_id[
    edges$status %in% "putative" & !is.na(edges$magnitude)
  ]
  if (length(contradictory) > 0) {
    problems <- c(
      problems,
      paste0(
        length(contradictory),
        " putative edge(s) carry a magnitude: ",
        paste(contradictory, collapse = ", "),
        " (mark them empirical, or clear the magnitude)"
      )
    )
  }

  bare <- edges$edge_id[!is.na(edges$magnitude) & is.na(edges$magnitude_unit)]
  if (length(bare) > 0) {
    problems <- c(
      problems,
      paste0(
        length(bare),
        " edge(s) have a magnitude with no unit: ",
        paste(bare, collapse = ", ")
      )
    )
  }

  # Nodes nothing flows into or out of. Not necessarily wrong (a source has no
  # inflow, a target site exposure no outflow), so it is reported per node and
  # left to Sam.
  orphans <- setdiff(nodes$node_id, c(edges$from, edges$to))
  if (length(orphans) > 0) {
    problems <- c(
      problems,
      paste0(
        length(orphans),
        " node(s) have no edges at all: ",
        paste(orphans, collapse = ", ")
      )
    )
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
#' Defaults match the `aep_diagrams` target: `node_cards_compact` is written at
#' 2.4 x 1.8 inches (`card_aspect = 1.8 / 2.4`) onto a 12 x 8 inch canvas
#' (`device_aspect = 12 / 8`). **Change either in `_targets.R` and these defaults
#' are wrong**, so both are passed explicitly from there.
#'
#' @param nodes The nodes table, with `x` and `y`. Ignored for an axis where
#'   `x_range`/`y_range` is supplied.
#' @param image_size Card width as a fraction of panel width, as passed to
#'   `ggimage::geom_image()`.
#' @param card_aspect Card height divided by card width, in inches.
#' @param device_aspect Device width divided by device height, in inches.
#' @param x_expand,y_expand The multiplicative axis expansions [plot_aep()] uses.
#' @param x_range,y_range Raw (pre-expansion) `c(min, max)` to use instead of
#'   `range(nodes$x)`/`range(nodes$y)`. `NULL` (the default) derives from
#'   `nodes`, exactly as before this parameter existed.
#'
#'   **Added 2026-08-08, and it is the fix for a real bug, not a nicety.**
#'   `nodes` alone is only correct if the FINAL rendered panel's data range
#'   equals the node coordinate range expanded by `x_expand`/`y_expand` -- but
#'   [plot_aep()] also draws group boxes (`aep_group_layers()`), which are
#'   real plotted data (`geom_rect()`) that auto-expand the panel range
#'   whenever a box's padding pushes past what `x_expand`/`y_expand` alone
#'   would add. When that happens, `hw`/`hh` come out smaller than the panel
#'   actually is. That is silent for `ggimage::geom_image()`, whose size is a
#'   fraction of PANEL WIDTH and does not care what the data range is -- but
#'   it directly undersizes anything sized in DATA units against the same
#'   `hw`/`hh`, which by 2026-08-07 included `geom_tile()` for bare nodes and
#'   the edge-clipping box both. Sam measured it directly: a bare node's
#'   rendered rect came out 251x162px against the real card's 310x232px, and
#'   162/251 = 0.645 while the real card's own aspect is 0.75 -- not just
#'   smaller, skewed, because the node x-range and y-range differ in how much
#'   the group box's padding exceeds `x_expand`/`y_expand`'s guess on each
#'   axis. `plot_aep()` now computes the TRUE combined range (nodes + boxes)
#'   and passes it in here, then pins the actual rendered panel to that exact
#'   window with `coord_cartesian(expand = FALSE)`, so this function's
#'   prediction and the real render can no longer disagree.
#' @return A list with `hw`, `hh` (half-width/height in data units) and `rx`,
#'   `ry` (the full expanded span each was computed from, for callers that
#'   need to reconstruct the axis window, e.g. [plot_aep()]).
#' @export
node_card_extent <- function(
  nodes,
  image_size = 0.19,
  card_aspect = 1.8 / 2.4,
  device_aspect = 12 / 8,
  x_expand = 0.15,
  y_expand = 0.12,
  x_range = NULL,
  y_range = NULL
) {
  span <- function(v, mult, override = NULL) {
    v <- if (!is.null(override)) override else v[is.finite(v)]
    d <- if (length(v) < 2) 0 else diff(range(v))
    # A single placed node, or all nodes in a line, leaves one axis with no
    # range. ggplot2 falls back to a unit window there, so this does too.
    if (!is.finite(d) || d == 0) {
      d <- 1
    }
    d * (1 + 2 * mult)
  }

  rx <- span(nodes$x, x_expand, x_range)
  ry <- span(nodes$y, y_expand, y_range)

  list(
    hw = image_size * rx / 2,
    # Width is a fraction of panel WIDTH, so converting to a fraction of panel
    # height picks up the device aspect. A card 0.19 of a 12in panel is 2.28in
    # wide and 1.71in tall, which over an 8in panel is 0.214 of the height.
    hh = image_size * card_aspect * device_aspect * ry / 2,
    rx = rx,
    ry = ry
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
      x = numeric(0),
      y = numeric(0),
      xend = numeric(0),
      yend = numeric(0)
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

#' A Node Drawn as a Fixed-Size Rectangle Plus Label
#'
#' Used for bare nodes (no card image) and for a node with no card among
#' otherwise-imaged siblings. Replaced `geom_label()` on 2026-08-07: a label's
#' background is sized to its own text, not to a shared, known extent, so an
#' edge had nothing geometrically real to clip to. A tile at `hw`/`hh` (from
#' [node_card_extent()]) is the same fixed rectangle [aep_edge_coords()]
#' already clips edges to when cards are drawn, so a bare diagram and the real
#' one agree exactly on where a node's boundary is.
#'
#' @param data A nodes-like tibble carrying `x`, `y`, `.label`.
#' @param hw,hh Half-width and half-height in data units, from
#'   [node_card_extent()].
#' @return A list of ggplot2 layers.
#' @export
aep_node_tile_layers <- function(data, hw, hh) {
  list(
    ggplot2::geom_tile(
      data = data,
      ggplot2::aes(x = .data$x, y = .data$y),
      inherit.aes = FALSE,
      width = 2 * hw,
      height = 2 * hh,
      fill = "white",
      colour = "grey30",
      linewidth = 0.4
    ),
    ggplot2::geom_text(
      data = data,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$.label),
      inherit.aes = FALSE,
      size = 2.8,
      lineheight = 0.95,
      colour = "grey15"
    )
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
      edge_id = character(0),
      .point = integer(0),
      x = numeric(0),
      y = numeric(0)
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

#' Terminal Tangent of Each Edge, for Drawing a Solid Arrowhead
#'
#' **Why arrowheads are a layer of their own** (Sam 2026-08-12: the arrows
#' "have the same dashed border as the edges, which looks noisy and silly").
#'
#' `grid` draws an arrowhead with the same `gpar` as the line it terminates,
#' and that includes `lty`. A putative edge is dashed, so its arrowhead outline
#' came out dashed too: a closed triangle with gaps chewed out of its edges,
#' which at figure scale reads as noise rather than as a dash pattern. There is
#' no per-arrow linetype override to reach for, so the fix is structural. The
#' curve is drawn with no arrow at all, and the head is carried by this short
#' **solid** segment laid along the curve's own end tangent.
#'
#' For a quadratic bezier the tangent at the end point is simply `P2 - P1`, so
#' the head points exactly along the curve rather than along the chord. That
#' distinction is visible at this curvature: on a long edge the chord direction
#' is several degrees off, and an arrowhead skewed against its own line looks
#' broken in a way that is hard to name when you see it.
#'
#' @param edges Output of [aep_edge_coords()].
#' @param curvature As [aep_edge_bezier_points()]. Must match, or the head
#'   points somewhere the curve does not go.
#' @param frac Length of the stub, as a fraction of the straight-line distance
#'   between the endpoints. Small enough to be invisible under the head itself;
#'   non-zero because a zero-length segment has no direction for `grid` to
#'   orient the arrow by.
#' @return A tibble of `edge_id`, `x`, `y`, `xend`, `yend`.
#' @export
aep_edge_arrow_stubs <- function(edges, curvature = 0.15, frac = 0.02) {
  if (nrow(edges) == 0) {
    return(tibble::tibble(
      edge_id = character(0),
      x = numeric(0), y = numeric(0),
      xend = numeric(0), yend = numeric(0)
    ))
  }

  dx <- edges$xend - edges$x
  dy <- edges$yend - edges$y
  len <- sqrt(dx^2 + dy^2)

  perp_x <- ifelse(len == 0, 0, -dy / len)
  perp_y <- ifelse(len == 0, 0, dx / len)
  ctrl_x <- (edges$x + edges$xend) / 2 + perp_x * curvature * len
  ctrl_y <- (edges$y + edges$yend) / 2 + perp_y * curvature * len

  # Tangent at the end of a quadratic bezier: P2 - P1.
  tx <- edges$xend - ctrl_x
  ty <- edges$yend - ctrl_y
  tlen <- sqrt(tx^2 + ty^2)
  ux <- ifelse(tlen == 0, 0, tx / tlen)
  uy <- ifelse(tlen == 0, 0, ty / tlen)

  step <- len * frac
  tibble::tibble(
    edge_id = edges$edge_id,
    x = edges$xend - ux * step,
    y = edges$yend - uy * step,
    xend = edges$xend,
    yend = edges$yend
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
#'   `node_id`, from [write_node_cards()]. Where
#'   supplied, each node is drawn as its card instead of a text label. This is
#'   PLAN.md P5.2, and it is what makes the figure a report-card AEP rather than
#'   a labelled graph.
#' @param image_size Card width as a fraction of plot width.
#' @param card_aspect,device_aspect Card and device shape, passed to
#'   [node_card_extent()] so arrows can be clipped to the cards. Defaults match
#'   the `aep_diagrams` target; pass them explicitly from anywhere else.
#' @param curvature Bend applied to every edge, passed to
#'   [aep_edge_bezier_points()]. `0` is a straight line.
#' @param arrow_length Arrowhead length in points. **4 since 2026-08-12**, down
#'   from 6 (Sam: "make the arrow smaller"). Drawn by a separate solid stub
#'   layer rather than by the curve's own `arrow =`; see
#'   [aep_edge_arrow_stubs()] for why.
#' @param tile_size,tile_aspect Bare-node rectangle width (as a fraction of
#'   panel width) and height-over-width, used only when `node_images` is not
#'   supplied. `NULL` (the default for both) means "the same size as the real
#'   card": falls back to `image_size`/`card_aspect`, so a bare diagram and the
#'   real one draw identically sized boxes without having to keep two sets of
#'   numbers in sync by hand. Sam 2026-08-07: the first cut of this used its
#'   own guessed defaults (0.14, 0.45) instead, and they came out roughly half
#'   the real card's size (the real compact card is 2.4in x 1.8in at 200dpi,
#'   i.e. 480x360px, `card_aspect = 1.8/2.4 = 0.75`, not the 0.45 guessed
#'   here) -- pass them explicitly only if a bare diagram should deliberately
#'   look different from the real cards.
#' @param x_expand,y_expand Axis margin as a fraction of the node coordinate
#'   span, applied before any group box is taken into account. The single
#'   source of truth for this: previously `node_card_extent()`'s defaults and
#'   this function's own axis `expand=` were two separately hardcoded 0.15/
#'   0.12 pairs that had to be kept in sync by hand and were the seed of the
#'   2026-08-07/08 tile-sizing bug (see [node_card_extent()]'s `x_range`/
#'   `y_range` doc). Now there is one value, threaded through everywhere it
#'   matters.
#' @return A ggplot.
#' @export
plot_aep <- function(
  nodes,
  edges,
  cards = NULL,
  label_edges = TRUE,
  groups = NULL,
  node_images = NULL,
  image_size = 0.16,
  card_aspect = 1.8 / 2.4,
  device_aspect = 12 / 8,
  curvature = 0.15,
  arrow_length = 4,
  tile_size = NULL,
  tile_aspect = NULL,
  x_expand = 0.15,
  y_expand = 0.12
) {
  placed <- nodes |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))
  if (nrow(placed) == 0) {
    return(triage_empty_plot("AEP", "no nodes have x/y coordinates"))
  }

  styles <- aep_edge_styles()

  card_image_size <- if (!is.null(node_images) && length(node_images) > 0) {
    image_size
  } else {
    tile_size %||% image_size
  }
  card_card_aspect <- if (!is.null(node_images) && length(node_images) > 0) {
    card_aspect
  } else {
    tile_aspect %||% card_aspect
  }

  # PASS ONE: extent from the node coordinates alone, exactly as before
  # 2026-08-08. Used only to size the group boxes' card-clearance term below --
  # a box needs *some* estimate of card size to pad against, and a small error
  # there just shifts a box's own padding slightly, which is cosmetic. It is
  # NOT used for the real extent any more; see pass two.
  ext0 <- node_card_extent(
    placed,
    image_size = card_image_size,
    card_aspect = card_card_aspect,
    device_aspect = device_aspect,
    x_expand = x_expand,
    y_expand = y_expand
  )

  boxes <- if (!is.null(groups) && nrow(groups) > 0) {
    aep_group_boxes(groups, placed, card_hw = ext0$hw, card_hh = ext0$hh)
  } else {
    NULL
  }

  # PASS TWO, and the actual fix: node_card_extent() predicts the panel's
  # final data-unit span from the node coordinates plus a fixed margin, but
  # the REAL rendered panel auto-expands to fit every plotted layer, including
  # the group box rectangles just computed -- which routinely extend further
  # than that prediction once a box's own padding is accounted for. Predicting
  # from nodes alone therefore undersizes hw/hh. So: fold the boxes' extent
  # into the range before computing the real hw/hh, then (below)
  # coord_cartesian() PINS the rendered panel to this exact window, so the
  # prediction and the render can never again disagree the way they did for
  # Sam's 251x162 (predicted) vs 310x232 (real) card measurement.
  x_all <- placed$x
  y_all <- placed$y
  if (!is.null(boxes) && nrow(boxes) > 0) {
    x_all <- c(x_all, boxes$xmin, boxes$xmax)
    y_all <- c(y_all, boxes$ymin, boxes$ymax)
  }

  ext <- node_card_extent(
    placed,
    image_size = card_image_size,
    card_aspect = card_card_aspect,
    device_aspect = device_aspect,
    x_expand = x_expand,
    y_expand = y_expand,
    x_range = range(x_all),
    y_range = range(y_all)
  )
  cx <- mean(range(x_all))
  cy <- mean(range(y_all))
  xlim <- cx + c(-1, 1) * (ext$rx / 2)
  ylim <- cy + c(-1, 1) * (ext$ry / 2)

  # Rejected edges are dropped EXPLICITLY, not left to fall out of the draw
  # loop below. They would already vanish by accident, because that loop
  # iterates over `names(styles$linetype)` and aep_edge_styles() names only the
  # live statuses -- but relying on that makes "cut edges are not drawn" an
  # emergent property of the style table, which the next person to add a style
  # entry would silently undo.
  e <- aep_edge_coords(drop_rejected_edges(edges), placed, hw = ext$hw, hh = ext$hh)
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
            .data$label,
            "\n",
            formatC(.data$geo_mean, format = "g", digits = 3),
            " ",
            .data$unit,
            "  (n = ",
            format(.data$n, big.mark = ","),
            ")"
          )
        )
      )
  } else {
    dplyr::mutate(placed, .label = .data$label)
  }

  p <- ggplot2::ggplot()

  # Group boxes go on FIRST, so edges and nodes draw over them. They are
  # annotation and must never occlude content. `boxes` was already computed
  # above (pass one) -- NOT recomputed against the pass-two `ext`, which would
  # be circular, since pass two's whole purpose is to fold these same boxes'
  # extent into the range.
  if (!is.null(boxes) && nrow(boxes) > 0) {
    p <- p + aep_group_layers(boxes)
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
      p <- p +
        ggforce::geom_bezier(
          data = aep_edge_bezier_points(sub, curvature = curvature),
          ggplot2::aes(x = .data$x, y = .data$y, group = .data$edge_id),
          linetype = styles$linetype[[st]],
          colour = styles$colour[[st]],
          linewidth = styles$linewidth[[st]],
          alpha = styles$alpha[[st]]
          # NO `arrow =` HERE. grid gives the arrowhead the line's own gpar,
          # dashed lty included, so a putative edge's head came out as a
          # triangle with gaps chewed out of it. The head is drawn by the
          # solid stub layer immediately below instead. See
          # aep_edge_arrow_stubs().
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
        ) +
        # The arrowhead, solid regardless of the edge's own linetype. Same
        # colour, linewidth and alpha as the curve so it reads as one object;
        # only `linetype` differs, and that is the entire purpose.
        ggplot2::geom_segment(
          data = aep_edge_arrow_stubs(sub, curvature = curvature),
          ggplot2::aes(
            x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend
          ),
          linetype = "solid",
          colour = styles$colour[[st]],
          linewidth = styles$linewidth[[st]],
          alpha = styles$alpha[[st]],
          arrow = ggplot2::arrow(
            length = ggplot2::unit(arrow_length, "pt"),
            type = "closed"
          )
        )
    }

    if (label_edges) {
      lab <- e[e$status == "empirical" & !is.na(e$magnitude), , drop = FALSE]
      if (nrow(lab) > 0) {
        p <- p +
          ggplot2::geom_label(
            data = lab,
            ggplot2::aes(
              # The CHORD midpoint, not a point on the curve itself: at
              # curvature = 0.15 the two are close enough that computing the
              # true bezier midpoint is not worth the complexity it would add.
              x = (.data$x + .data$xend) / 2,
              y = (.data$y + .data$yend) / 2,
              label = paste0(
                formatC(.data$magnitude, format = "g", digits = 3),
                " ",
                .data$magnitude_unit
              )
            ),
            size = 2.4,
            colour = "grey15",
            label.size = 0,
            fill = "white",
            alpha = 0.85
          )
      }
    }
  }

  # NODES: cards where images were supplied, tiles otherwise. Not a mix,
  # because two visual languages for the same object on one figure is worse than
  # either alone.
  p <- if (!is.null(node_images) && length(node_images) > 0) {
    have <- node_label[
      node_label$node_id %in% names(node_images),
      ,
      drop = FALSE
    ]
    have$.image <- unname(node_images[have$node_id])
    missing_img <- node_label[
      !node_label$node_id %in% names(node_images),
      ,
      drop = FALSE
    ]

    out <- p +
      ggimage::geom_image(
        data = have,
        ggplot2::aes(x = .data$x, y = .data$y, image = .data$.image),
        size = image_size,
        # asp corrects ggimage's render for the plot area's aspect ratio, and
        # MUST track the real device shape -- it was hardcoded to 1.5 (the
        # original fixed 12x8 canvas's ratio) until 2026-08-08, harmless while
        # device_aspect was always 12/8 for every AEP but silently wrong the
        # moment it wasn't: aep_diagram_height() makes device_aspect vary per
        # AEP, and a stale asp against a real device_aspect of e.g. 0.28 (12in
        # over a 42in-tall canvas) is what ggimage read as "compensate by
        # blowing the image up roughly 5x", confirmed by measuring the actual
        # rendered pixel footprint of a card at three different heights with
        # asp fixed: it grew with height instead of staying constant. Sam:
        # "now the node images don't scale with the plot, so they still
        # overprint."
        asp = device_aspect
      )
    # A node with no card still has to appear, or the diagram silently loses
    # it. Tiled at the CARD's own extent (ext, computed above from
    # image_size/card_aspect), so it reads as the same size as its sibling
    # cards rather than a different-shaped placeholder.
    if (nrow(missing_img) > 0) {
      out <- out + aep_node_tile_layers(missing_img, ext$hw, ext$hh)
    }
    out
  } else {
    p + aep_node_tile_layers(node_label, ext$hw, ext$hh)
  }

  p +
    # PINNED to xlim/ylim (computed above, pass two), not left to auto-range
    # plus a mult expansion: that auto-range is exactly what silently drifted
    # away from node_card_extent()'s prediction whenever a group box was
    # bigger than the node range alone, which is the 2026-08-08 tile-sizing
    # bug. coord_cartesian() windows the panel without dropping any
    # out-of-window data (unlike setting `limits` on the scale itself), so
    # this is safe even if a box or edge extends past the window.
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      caption = paste(
        "Solid arrows: empirically supported.",
        "Dashed grey: putative, not evidenced here."
      )
    ) +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.75),
        colour = "grey40",
        hjust = 0
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
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      colour = "firebrick",
      fill = "firebrick",
      alpha = 0.25,
      linewidth = 0.6
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
#' **`edges` counts live edges only, and `rejected` is reported beside it.**
#' Counting cut edges in the denominator would leave the progress figure
#' measured against work that has been deliberately abandoned, so it could never
#' reach completion however much of the real work got done. That is the opposite
#' of what a time-box needs from its progress number: rejecting an edge is
#' progress, and it should read as progress.
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
  n_rejected <- sum(edges$status %in% "rejected")
  live <- drop_rejected_edges(edges)

  n_edges <- nrow(live)
  n_empirical <- sum(live$status %in% "empirical")
  n_putative <- sum(live$status %in% "putative")
  n_magnitude <- sum(!is.na(live$magnitude))
  n_scored <- sum(
    !is.na(live$essentiality_score) &
      !is.na(live$plausibility_score) &
      !is.na(live$evidence_score) &
      !is.na(live$quantification_score)
  )

  tibble::tibble(
    edges = n_edges,
    empirical = n_empirical,
    putative = n_putative,
    rejected = n_rejected,
    with_magnitude = n_magnitude,
    fully_scored = n_scored
  )
}
