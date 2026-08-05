# Node report cards (PLAN.md 4.3, P3.2, P5.2). Added 2026-08-05.
#
# The artefact a node carries: enough to judge it at a glance, small enough to
# sit on a graph node.
#
# TWO DESIGN DECISIONS WORTH READING BEFORE CHANGING ANYTHING.
#
# 1. VALUE OWNS THE LONG EDGE. Sam's first instinct was strips resembling triage
#    panel (b), concentration by date. Panel (b) spends its x axis on date, and a
#    card cannot afford that: these distributions span up to 13 orders of
#    magnitude, and on a strip a few millimetres tall a log axis on y is a smear.
#    So a strip here is structurally one ROW of panel (c)/(d): value on x, binned
#    density, boxplot, threshold lines, count label. That also keeps the visual
#    language Sam has been reading all week, which was the stated goal.
#    Time is dropped, not encoded. A card is a summary; the trend lives on the
#    group's own panel (b), which is one click away in the notebook.
#
# 2. ONE SCALE PER UNIT, NOT PER COMPARTMENT AND NOT PER CARD. Cards must be
#    comparable, so the value axis cannot be node-local. But it also cannot be
#    global: this AEP's nodes span mg/L water and mg/kg sediment, and a shared
#    axis across incommensurable units means nothing. Unit is the widest sharing
#    that is honest, and resolve_node_data() already guarantees one unit per
#    node, so every strip on a card shares its axis by construction.

#' The Four EPEQ Criteria, Abbreviated
#'
#' **Two letters, not one, and that is forced rather than chosen.** Essentiality
#' and Evidence both begin with E, so single letters would put two different
#' criteria under the same badge. Sam suggested "just letter and numerical
#' score"; this is the smallest thing that is still unambiguous.
#'
#' @return A named character vector, score column to badge letters.
#' @export
epeq_badge_labels <- function() {
  c(
    essentiality_score = "Es",
    plausibility_score = "Pl",
    evidence_score = "Ev",
    quantification_score = "Qn"
  )
}

#' Colour per EPEQ Score
#'
#' Three levels, low to high. Deliberately NOT the threshold class palette: a
#' threshold class is a statement about contamination where red is bad, while an
#' EPEQ score is a statement about confidence where high is good. Sharing a
#' palette would invert the meaning of red halfway across the figure.
#'
#' Grey for unscored, which must not read as a low score.
#'
#' @return A named character vector keyed `"1"`, `"2"`, `"3"`, `"NA"`.
#' @export
epeq_score_colours <- function() {
  c(
    "1" = "#B5876B",
    "2" = "#C9B458",
    "3" = "#5A9367",
    "NA" = "#D9D9D9"
  )
}

#' EPEQ Badge Strip for One Node
#'
#' Four small squares, each a criterion abbreviation over its score. Programmer
#' art, per Sam's request: legible and honest, not designed.
#'
#' @param node A one-row nodes or edges tibble carrying the four score columns.
#' @param text_size Badge text size.
#' @return A ggplot.
#' @export
node_epeq_badges <- function(node, text_size = 2.6) {
  labs <- epeq_badge_labels()
  cols <- epeq_score_colours()

  scores <- vapply(
    names(labs),
    function(col) {
      v <- if (col %in% names(node)) node[[col]][1] else NA_real_
      if (is.na(v)) NA_real_ else as.numeric(v)
    },
    numeric(1)
  )

  d <- tibble::tibble(
    x = seq_along(labs),
    letter = unname(labs),
    score = scores,
    key = ifelse(is.na(scores), "NA", as.character(scores)),
    shown = ifelse(is.na(scores), "-", as.character(scores))
  )
  d$fill <- unname(cols[d$key])

  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = 0)) +
    ggplot2::geom_tile(
      fill = d$fill, colour = "white", linewidth = 0.6,
      width = 0.96, height = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$letter),
      y = 0.19, size = text_size * 0.78, colour = "grey20"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$shown),
      y = -0.20, size = text_size, fontface = "bold", colour = "grey10"
    ) +
    # Padded on the right so four badges sit as a compact block at the left
    # rather than stretching the full card width like a button bar.
    ggplot2::scale_x_continuous(limits = c(0.4, length(labs) * 2.1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.02)) +
    ggplot2::theme_void()
}

#' Horizontal Distribution Strips, One per Constituent Group
#'
#' **The point is the groups, not the pool.** A node pooling several sampling
#' groups reports one geometric mean, and that number cannot tell you whether the
#' lumping was sound. Drawing the constituents separately can: two strips sitting
#' on top of each other say the node is coherent, two strips an order apart say
#' it is not. This is the check that validates a lumping decision, and it is why
#' the card draws groups rather than the pooled distribution.
#'
#' Capped at `max_groups` because a card has to stay small. Where a node has
#' more, the largest are shown by measurement count and the remainder are named
#' in a final row, so the omission is visible rather than silent.
#'
#' @param node A one-row nodes tibble.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger.
#' @param limits Shared value limits for this node's unit. See the note at the
#'   top of this file for why the sharing is per unit.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param max_groups Most strips to draw.
#' @return A ggplot.
#' @export
node_group_strips <- function(
  node,
  members,
  data,
  ids,
  limits = NULL,
  thresholds = NULL,
  max_groups = 3
) {
  key <- triage_group_cols()
  my_groups <- members$group_id[members$node_id == node$node_id[1]]

  if (length(my_groups) == 0 || identical(node$node_type[1], "external")) {
    return(triage_empty_plot("", "no measured data", size = 2.6))
  }

  d <- resolve_node_data(node, members, data, ids)
  if (nrow(d) == 0) {
    return(triage_empty_plot("", "no data after restrictions", size = 2.6))
  }

  # Label each row with the group it came from, so the strips can be split.
  keys <- ids |>
    dplyr::filter(.data$group_id %in% my_groups) |>
    dplyr::select(dplyr::all_of(key), "group_id")
  d <- d |> dplyr::left_join(keys, by = key)

  ranked <- d |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(n = sum(.data$MEASURED_N, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$n))

  keep <- utils::head(ranked$group_id, max_groups)
  dropped <- setdiff(ranked$group_id, keep)

  d <- d |> dplyr::filter(.data$group_id %in% keep)
  d$.facet <- factor(d$group_id, levels = rev(keep))

  d <- triage_flag_by_category(d, min_n = 10)
  thr <- thresholds_for_group(thresholds, node_group_key(node, members, ids))

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$MEASURED_VALUE_STANDARD, y = .data$.facet)
  )

  bw <- category_x_binwidth(d, limits, bins = 60)
  p <- if (triage_use_points(d)) {
    p + ggplot2::geom_point(alpha = 0.6, size = 0.7)
  } else {
    p +
      ggplot2::geom_tile(
        data = count_by_category_bin(d, bw, origin = limits[1]),
        ggplot2::aes(x = .data$value_mid, y = .data$.facet, fill = .data$count),
        width = bw, height = 1, inherit.aes = FALSE
      ) +
      ggplot2::scale_fill_viridis_c(
        transform = "log10", guide = "none"
      )
  }

  p +
    triage_category_overlay(
      d, limits = limits, min_n = 10, ticks = FALSE,
      # No header: the card's own text block already says what the counts are,
      # and at card width it clipped off the canvas.
      header = FALSE, label_size = 1.6
    ) +
    triage_threshold_layers(thr, orientation = "vertical", limits = limits) +
    triage_value_scale(
      limits = limits, axis = "x",
      expand = triage_category_x_expansion()
    ) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.5, 0.5))) +
    ggplot2::labs(
      x = NULL, y = NULL,
      caption = if (length(dropped) > 0) {
        paste0("+ ", length(dropped), " smaller group(s) not shown: ",
               paste(dropped, collapse = ", "))
      } else {
        NULL
      }
    ) +
    triage_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.55), colour = "grey45"),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.6)),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.5), colour = "grey50", hjust = 0
      ),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' The Group Key a Node's Thresholds Should Match
#'
#' Thresholds are matched per sampling group, but a card draws a node. Uses the
#' node's largest constituent group, since that is the matrix the pooled
#' statistics mostly describe.
#'
#' @param node A one-row nodes tibble. @param members The membership table.
#' @param ids The group id ledger.
#' @return A one-row tibble of group-key columns, or `NULL`.
#' @export
node_group_key <- function(node, members, ids) {
  my_groups <- members$group_id[members$node_id == node$node_id[1]]
  k <- ids |> dplyr::filter(.data$group_id %in% my_groups)
  if (nrow(k) == 0) {
    return(NULL)
  }
  k[1, , drop = FALSE]
}

#' Shared Value Limits per Unit
#'
#' One scale per measured unit across every node, which is the widest sharing
#' that is not nonsense: an axis spanning mg/L water and mg/kg sediment together
#' compares incommensurable things. See the note at the top of this file.
#'
#' @param nodes The nodes table. @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger.
#' @return A named list of `c(lo, hi)`, keyed by unit.
#' @export
node_card_limits <- function(nodes, members, data, ids) {
  out <- list()
  for (i in seq_len(nrow(nodes))) {
    node <- nodes[i, , drop = FALSE]
    d <- tryCatch(
      resolve_node_data(node, members, data, ids),
      error = function(e) data[0, , drop = FALSE]
    )
    if (nrow(d) == 0) {
      next
    }
    u <- unique(d$MEASURED_UNIT_STANDARD)[1]
    v <- range(d$MEASURED_VALUE_STANDARD, na.rm = TRUE)
    out[[u]] <- if (is.null(out[[u]])) v else range(c(out[[u]], v))
  }
  out
}

#' Assemble One Node Card
#'
#' Title and statistics, the EPEQ badge strip, and one distribution strip per
#' constituent group.
#'
#' **patchwork is used here deliberately, and it is not a breach of CLAUDE.md
#' 4.4.** That rule bans composing panels in *exploratory* work, where a composed
#' figure invalidates as a unit and hides which plot changed. A card is not
#' exploratory: it is a single designed artefact whose parts have no meaning
#' apart, and PLAN.md P5.4 names composition as the right tool once figures are
#' being prepared.
#'
#' @param node A one-row nodes tibble.
#' @param card The matching row of [aep_node_report_cards()].
#' @param members,data,ids As elsewhere.
#' @param limits Shared limits for this node's unit.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param max_groups Most strips to draw.
#' @return A patchwork object.
#' @export
node_card <- function(
  node,
  card,
  members,
  data,
  ids,
  limits = NULL,
  thresholds = NULL,
  max_groups = 3
) {
  header <- node_card_header(node, card)
  badges <- node_epeq_badges(node)
  strips <- node_group_strips(
    node, members, data, ids,
    limits = limits, thresholds = thresholds, max_groups = max_groups
  )

  patchwork::wrap_plots(
    header, badges, strips,
    ncol = 1,
    heights = c(0.9, 0.5, 2.2)
  )
}

#' The Text Block at the Top of a Card
#'
#' Label, then the four aggregation levels Sam asked for (measurements, rows,
#' groups, references) and the weighted centre with its unit.
#'
#' @param node A one-row nodes tibble.
#' @param card The matching report-card row.
#' @return A ggplot.
#' @export
node_card_header <- function(node, card) {
  num <- function(x) {
    if (length(x) == 0 || is.na(x)) "-" else formatC(x, format = "g", digits = 3)
  }
  count <- function(x) {
    if (length(x) == 0 || is.na(x)) "-" else format(x, big.mark = ",")
  }

  unit <- if (length(card$unit) == 0 || is.na(card$unit)) "" else card$unit
  line2 <- paste0(
    "geo. mean ", num(card$geo_mean), if (nzchar(unit)) paste0(" ", unit) else "",
    "   GSD ", num(card$gsd)
  )
  # Every level of aggregation, per Sam 2026-08-05. n is measurements and is
  # weighted; GSD above is per row, hence both counts appearing here.
  line3 <- paste0(
    count(card$n), " measurements | ", count(card$n_rows), " rows | ",
    count(card$n_groups), " group", if (isTRUE(card$n_groups != 1)) "s" else "",
    " | ", count(card$n_sources), " ref",
    if (isTRUE(card$n_sources != 1)) "s" else ""
  )
  line4 <- if (!is.na(card$pct_arctic)) {
    paste0("Arctic ", formatC(card$pct_arctic, format = "f", digits = 1), "%")
  } else {
    ""
  }

  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 3, hjust = 0, size = 3.1, fontface = "bold",
      label = node$label[1], colour = "grey10"
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 2, hjust = 0, size = 2.5, label = line2,
      colour = "grey25"
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 1, hjust = 0, size = 2.2, label = line3,
      colour = "grey45"
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 0, hjust = 0, size = 2.2, label = line4,
      colour = "grey45"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(-0.5, 3.5)) +
    ggplot2::theme_void()
}

#' Write a Card per Node
#'
#' @param nodes,cards,members,data,ids As elsewhere.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param dir Output directory.
#' @param width,height,dpi Canvas.
#' @return The written paths.
#' @export
write_node_cards <- function(
  nodes,
  cards,
  members,
  data,
  ids,
  thresholds = NULL,
  dir = here_rel("figures/node_cards"),
  width = 3.6,
  height = 2.4,
  dpi = 150
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  limits <- node_card_limits(nodes, members, data, ids)

  paths <- character(0)
  for (i in seq_len(nrow(nodes))) {
    node <- nodes[i, , drop = FALSE]
    card <- cards[cards$node_id == node$node_id[1], , drop = FALSE]
    if (nrow(card) == 0) {
      next
    }
    lim <- limits[[card$unit[1] %||% ""]]
    p <- node_card(
      node, card, members, data, ids,
      limits = lim, thresholds = thresholds
    )
    path <- file.path(dir, paste0(node$node_id[1], ".png"))
    ggplot2::ggsave(
      filename = path, plot = p, width = width, height = height,
      dpi = dpi, device = ragg::agg_png, bg = "white"
    )
    paths <- c(paths, path)
  }
  paths
}
