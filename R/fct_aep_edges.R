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
  path = here_rel("data/clean/aep_edges.csv"),
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

#' Edge Coordinates for Plotting
#'
#' Joins each edge to its endpoints' hand-placed coordinates, and trims both ends
#' back so an arrow stops short of the node label rather than running under it.
#'
#' @param edges The edges table.
#' @param nodes The nodes table, with `x` and `y`.
#' @param trim Fraction of the segment removed at each end.
#' @return A tibble of edges with `x`, `y`, `xend`, `yend`.
#' @export
aep_edge_coords <- function(edges, nodes, trim = 0.12) {
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

  out |>
    dplyr::mutate(
      x = .data$x0 + (.data$x1 - .data$x0) * trim,
      y = .data$y0 + (.data$y1 - .data$y0) * trim,
      xend = .data$x1 - (.data$x1 - .data$x0) * trim,
      yend = .data$y1 - (.data$y1 - .data$y0) * trim
    )
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
#' @return A ggplot.
#' @export
plot_aep <- function(nodes, edges, cards = NULL, label_edges = TRUE) {
  placed <- nodes |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))
  if (nrow(placed) == 0) {
    return(triage_empty_plot("AEP", "no nodes have x/y coordinates"))
  }

  styles <- aep_edge_styles()
  e <- aep_edge_coords(edges, placed)
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

  p +
    ggplot2::geom_label(
      data = node_label,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$.label),
      size = 2.8,
      lineheight = 0.95,
      fill = "white",
      colour = "grey15",
      label.padding = ggplot2::unit(4, "pt")
    ) +
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
