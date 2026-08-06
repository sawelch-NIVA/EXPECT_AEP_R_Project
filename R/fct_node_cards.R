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
node_epeq_badges <- function(node, text_size = 2.4) {
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
    key = ifelse(is.na(scores), "NA", as.character(scores)),
    # One line, not two (Sam 2026-08-05). Stacked letter-over-score needed a
    # badge twice as tall for no extra information, and card height is the
    # scarcest thing here.
    shown = paste0(unname(labs), " ", ifelse(is.na(scores), "-", scores))
  )
  d$fill <- unname(cols[d$key])

  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = 0)) +
    ggplot2::geom_tile(
      fill = d$fill, colour = "white", linewidth = 0.6,
      width = 0.98, height = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$shown),
      size = text_size, colour = "grey10"
    ) +
    ggplot2::scale_x_continuous(limits = c(0.4, length(labs) + 1.6)) +
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
  max_groups = 3,
  style = c("full", "compact")
) {
  style <- match.arg(style)
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

  # VIOLIN, not the binned heatmap used on the triage panels (Sam 2026-08-05:
  # "having the heatmaps expand to use the full height available looks silly
  # here"). He is right, and the reason generalises: a heatmap row encodes count
  # in colour and therefore wants a fixed band height, so with one or two groups
  # it stretches to fill and implies a precision it does not have. A violin's
  # height IS the density, so it occupies exactly as much as it has earned.
  #
  # Still a summarising geom, so CLAUDE.md 4.4 is satisfied. The count encoding
  # is lost, which at card size was unreadable anyway; the per-group n is on the
  # right margin in text instead.
  #
  # Bimodality survives better than it did: two lobes read at a glance, where two
  # bright bands in a heatmap needed looking for.
  p <- if (triage_use_points(d)) {
    p + ggplot2::geom_point(alpha = 0.6, size = 0.7)
  } else {
    p +
      ggplot2::geom_violin(
        ggplot2::aes(group = .data$.facet),
        fill = "grey75", colour = "grey35", linewidth = 0.25,
        scale = "width", width = 0.9,
        # trim = FALSE would extend the kernel past the observed range, which on
        # a log axis invents concentrations nobody measured.
        trim = TRUE
      )
  }

  p +
    triage_category_overlay(
      d, limits = limits, min_n = 10, ticks = FALSE,
      # No header: the card's own text block already says what the counts are,
      # and at card width it clipped off the canvas.
      header = FALSE, label_size = 1.6,
      labels = (style == "full")
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
    ) +
    # At graph-node size the axis, the row labels and the margin counts are all
    # illegible, so they are removed rather than drawn too small to read. The
    # threshold lines and the violin shape survive, which is what a glance is
    # for.
    (if (style == "compact") {
      list(
        ggplot2::labs(caption = NULL),
        # GROUP IDS MOVE INSIDE THE PANEL, and that is an alignment fix rather
        # than a cosmetic one. A y axis pushes the strip's panel to the right,
        # while the header and badge plots above are theme_void and span the full
        # card, so the text block sat about 8% of the card left of the violins.
        # Drawing the ids inside the panel lets all three panels span the same
        # width, and then centring on x = 0.5 means the same thing everywhere.
        compact_group_labels(d, limits),
        compact_value_scale(limits),
        compact_axis_theme()
      )
    } else {
      list()
    })
}

#' Group Ids Drawn Inside the Panel
#'
#' At the left edge of the value axis, which on a log scale spanning many
#' decades is reliably empty: the data cluster near the geometric mean and the
#' lowest decade is padding. See [node_group_strips()] for why they cannot stay
#' on the axis.
#'
#' @param data The plot subset, carrying `.facet`.
#' @param limits Shared value limits.
#' @return A ggplot2 layer.
#' @export
compact_group_labels <- function(data, limits = NULL) {
  lo <- if (!is.null(limits) && all(is.finite(limits)) && all(limits > 0)) {
    limits[1]
  } else {
    suppressWarnings(min(data$MEASURED_VALUE_STANDARD, na.rm = TRUE))
  }
  d <- data.frame(
    .facet = factor(levels(data$.facet), levels = levels(data$.facet))
  )
  ggplot2::geom_text(
    data = d,
    ggplot2::aes(x = lo, y = .data$.facet, label = .data$.facet),
    inherit.aes = FALSE,
    hjust = 0, vjust = -0.9,
    size = 1.9, colour = "grey45"
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
  max_groups = 3,
  style = c("full", "compact")
) {
  style <- match.arg(style)

  # COMPACT EXISTS BECAUSE SHRINKING THE FULL CARD DOES NOT WORK.
  #
  # A card placed on a graph node is roughly 1.6in wide against the 3.6in it was
  # designed at, so every point size lands under half its intended value and the
  # body text arrives at about 1pt. Fewer, larger elements is the only way down
  # in size; scaling is not.
  #
  # What survives: the label, the headline number, the EPEQ badges, and the
  # distribution. What goes: the count line, the per-group row labels, the axis,
  # and the margin counts. All of it is still on the full card, one file away.
  if (style == "compact") {
    header <- node_card_header(node, card, style = "compact")
    badges <- node_epeq_badges(node, text_size = 3.4)
    strips <- node_group_strips(
      node, members, data, ids,
      limits = limits, thresholds = thresholds, max_groups = max_groups,
      style = "compact"
    )
    return(patchwork::wrap_plots(
      header, badges, strips,
      ncol = 1,
      # Header takes the most: a wrapped two-line title plus the headline plus
      # the counts line does not fit in the same band as a single strip.
      heights = c(1.55, 0.40, 1.4)
    ))
  }

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

#' Value Axis for a Compact Strip
#'
#' A tick at every decade, a labelled major break every third decade. Sam
#' 2026-08-06: "violin plots need _some_ kind of x axis or they're fairly
#' meaningless", and at card width a label per decade is thirteen numbers in two
#' inches.
#'
#' **Order of operations is load-bearing, and Sam flagged it before I hit it.**
#' A complete theme (`theme_void()`, `theme_minimal()`) replaces the whole theme
#' object rather than merging into it, so any axis styling added BEFORE one is
#' silently discarded. Everything here is applied after the complete theme, and
#' [compact_axis_theme()] is written to be added last.
#'
#' @param limits Shared value limits for the node's unit.
#' @param every Decades between labelled breaks.
#' @return A ggplot2 scale.
#' @export
compact_value_scale <- function(limits = NULL, every = 3) {
  decades <- -12:12
  major <- decades[decades %% every == 0]

  ggplot2::scale_x_log10(
    limits = limits,
    breaks = 10^major,
    minor_breaks = 10^decades,
    labels = function(x) {
      ifelse(is.na(x), NA_character_, formatC(x, format = "e", digits = 0))
    },
    expand = ggplot2::expansion(mult = c(0.02, 0.02)),
    guide = ggplot2::guide_axis(minor.ticks = TRUE)
  )
}

#' Axis Styling for a Compact Strip
#'
#' Must be added **after** the complete theme. See [compact_value_scale()].
#'
#' @return A ggplot2 theme.
#' @export
compact_axis_theme <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = ggplot2::rel(0.62), colour = "grey40"),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_line(colour = "grey45", linewidth = 0.3),
    axis.minor.ticks.x.bottom = ggplot2::element_line(
      colour = "grey65", linewidth = 0.22
    ),
    axis.ticks.length.x = ggplot2::unit(2.4, "pt"),
    axis.minor.ticks.length.x = ggplot2::rel(0.5),
    panel.grid = ggplot2::element_blank()
  )
}

#' Node Title: Id and Label
#'
#' `"N003 Coastal mussels"`. Sam 2026-08-06: reporting the number alongside the
#' name "will make it easier to reference them", which matters as soon as there
#' are more than a handful and the labels start sounding alike.
#'
#' @param node A one-row nodes tibble.
#' @return A single string.
#' @export
node_title <- function(node) {
  id <- node$node_id[1]
  label <- node$label[1]
  if (is.na(id) || !nzchar(id)) {
    return(label)
  }
  paste0(id, " ", label)
}

#' Does the Headline Number Deserve to be Believed?
#'
#' For a lognormal distribution the geometric mean and the median coincide
#' exactly. So when they diverge, the distribution is not lognormal, and a single
#' central number is describing something that has no single centre, usually two
#' populations stacked together.
#'
#' **This exists because the card would otherwise lie confidently.** N005 reports
#' a bold headline of 8 mg/kg (wet) for a group that is 3 correct rows near 0.2
#' and 15 rows near 3,000 from the Urban Fjord 1000x error; its median is 0.235.
#' Nothing about "8" tells the reader that.
#'
#' `tol` is in orders of magnitude. 0.5 is a factor of roughly three, which is
#' comfortably inside the noise for a real lognormal group and comfortably
#' outside it for a group holding two modes.
#'
#' @param card A one-row report card.
#' @param tol Divergence in log10 units beyond which the headline is marked.
#' @return `TRUE`, `FALSE`, or `NA` where either statistic is missing.
#' @export
headline_is_suspect <- function(card, tol = 0.5) {
  gm <- card$geo_mean[1]
  md <- card$median[1]
  if (length(gm) == 0 || is.na(gm) || is.na(md) || gm <= 0 || md <= 0) {
    return(NA)
  }
  abs(log10(gm) - log10(md)) > tol
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
node_card_header <- function(node, card, style = c("full", "compact")) {
  style <- match.arg(style)
  num <- function(x) {
    if (length(x) == 0 || is.na(x)) "-" else formatC(x, format = "g", digits = 3)
  }
  count <- function(x) {
    if (length(x) == 0 || is.na(x)) "-" else format(x, big.mark = ",")
  }
  unit <- if (length(card$unit) == 0 || is.na(card$unit)) "" else card$unit

  # THE HEADLINE NUMBER, bold and on its own line (Sam 2026-08-05).
  #
  # Geometric mean rather than arithmetic, because these distributions are
  # lognormal over orders of magnitude and the arithmetic mean sits above almost
  # every observation.
  #
  # The MEDIAN IS KEPT BESIDE IT, and not as decoration: the two agree closely on
  # a lognormal distribution and diverge when it is not one, so the gap between
  # them is a free diagnostic. N005 is the worked example, with a geometric mean
  # of 8.0 against a median of 0.235, a thirtyfold gap that says the node holds
  # two populations rather than one.
  headline <- paste0(num(card$geo_mean), if (nzchar(unit)) paste0(" ", unit) else "")
  # A marker, not a scolding: the reader still gets the number, and a reason to
  # go and look at the strips below before quoting it.
  suspect <- isTRUE(headline_is_suspect(card))
  if (suspect) {
    headline <- paste0(headline, "  (!)")
  }
  checks <- paste0("GSD ", num(card$gsd), "   median ", num(card$median))
  if (suspect) {
    checks <- paste0(checks, "
mean and median disagree; see strips")
  }
  # "n = 44, rows = 44" (Sam 2026-08-05): the words were doing no work.
  counts <- paste0(
    "n = ", count(card$n), ", rows = ", count(card$n_rows),
    ", groups = ", count(card$n_groups), ", refs = ", count(card$n_sources)
  )
  # Compact keeps the sample size and the source count and drops the rest: those
  # two are what make the headline a measurement rather than an assertion.
  compact_counts <- paste0(
    "n = ", count(card$n), ", refs = ", count(card$n_sources)
  )
  # Arctic coverage is DROPPED FROM THE CARD but still computed and carried in
  # aep_node_cards, per Sam: "remove the Arctic measure from the plot, but keep
  # the code. we can worry about it later."

  if (style == "compact") {
    return(
      ggplot2::ggplot() +
        # NODE ID IN THE TOP RIGHT, at default size (Sam 2026-08-06): it is a
        # handle for referring to the node in conversation, not information about
        # it, so it sits out of the reading path rather than inside the title.
        ggplot2::annotate(
          "text", x = 1, y = 3.0, hjust = 1, vjust = 1, size = 2.6,
          label = node$node_id[1], colour = "grey55"
        ) +
        # All three centred on the SAME anchor, x = 0.5 with hjust = 0.5. That
        # only means the same thing in each plot now that the strips below span
        # the full card too; see compact_group_labels().
        ggplot2::annotate(
          "text", x = 0.5, y = 2.35, hjust = 0.5, vjust = 1, size = 4.5,
          fontface = "bold",
          label = stringr::str_wrap(node$label[1], width = 18),
          lineheight = 0.95, colour = "grey10"
        ) +
        ggplot2::annotate(
          "text", x = 0.5, y = 0.75, hjust = 0.5, vjust = 0.5, size = 3.7,
          fontface = "bold",
          label = headline, colour = if (suspect) "#A8452F" else "grey5"
        ) +
        ggplot2::annotate(
          # Dropped further below the concentration than it was: at one line's
          # spacing the sample size read as part of the number above it.
          "text", x = 0.5, y = -0.35, hjust = 0.5, vjust = 0.5, size = 2.7,
          label = compact_counts, colour = "grey40"
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1)) +
        ggplot2::scale_y_continuous(limits = c(-0.75, 3.1)) +
        ggplot2::theme_void()
    )
  }

  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 2, hjust = 0, size = 3.0, fontface = "bold",
      label = stringr::str_wrap(node_title(node), width = 34),
      lineheight = 0.95, colour = "grey10"
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 1, hjust = 0, size = 3.4, fontface = "bold",
      label = headline, colour = if (suspect) "#A8452F" else "grey5"
    ) +
    ggplot2::annotate(
      "text", x = 1, y = 1, hjust = 1, size = 2.2, label = checks,
      colour = if (suspect) "#A8452F" else "grey45", lineheight = 0.9
    ) +
    ggplot2::annotate(
      "text", x = 0, y = 0, hjust = 0, size = 2.2, label = counts,
      colour = "grey45"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(-0.4, 2.4)) +
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
  dpi = 150,
  style = c("full", "compact")
) {
  style <- match.arg(style)
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
      limits = lim, thresholds = thresholds, style = style
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
