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
#' **Red / yellow / green since 2026-08-12** (Sam: the old bronze and gold
#' "is unintuitive as stands"). The previous ramp ran bronze `#B5876B` to gold
#' `#C9B458` to green, i.e. a medal metaphor, which reads as an award rather
#' than a warning and left 1 and 2 barely distinguishable at badge size.
#' A traffic light is the convention every reader already has.
#'
#' This does **not** collide with the threshold class palette, for the reason
#' given above: those two never appear in the same panel, and the badge always
#' carries its criterion letter and its digit, so the colour is a reinforcement
#' rather than the only channel.
#'
#' **Colourblind safety is deliberately deferred** (Sam, same date: "don't
#' worry about colourblindness for now, we can do a pass on that later"). Red
#' against green is the worst possible pairing for deuteranopia, so this needs
#' revisiting before submission; the digit inside each badge is what keeps it
#' readable in the meantime.
#'
#' @return A named character vector keyed `"1"`, `"2"`, `"3"`, `"NA"`.
#' @export
epeq_score_colours <- function() {
  c(
    "1" = "#C4453C",
    "2" = "#E8C55A",
    "3" = "#5A9367",
    "NA" = "#D9D9D9"
  )
}

#' Pastel Background Colour per Node Level
#'
#' Sam 2026-08-07: colour the card background by node type so the AEP reads at
#' a glance without following edges. Pale/pastel throughout, because these sit
#' behind data (text, violins, threshold lines) that has to stay legible on
#' top, and a saturated fill would fight it.
#'
#' Keyed on `level` (`aep_node_levels()`), not `node_type`: `node_type` is only
#' `empirical`/`external`, an internal distinction about where the numbers came
#' from, not the pathway-stage distinction the colouring is meant to carry. Sam
#' specified three of the original four: orange for `source`, blue for the
#' compartment nodes, pink for the organism nodes, with a lavender for the
#' target-site stage so every node gets a background.
#'
#' Re-keyed 2026-09-03 for the five-stage split (`aep_node_levels()`): the blue
#' carries to `exposure_medium`, the pink to `internal_exposure`, and the new
#' `external_exposure` stage gets a pale green that sits between them in the
#' pathway and in hue.
#'
#' @return A named character vector, level to hex colour.
#' @export
node_level_bg_colours <- function() {
  c(
    source = "#FBE3C7",
    exposure_medium = "#D7E6F5",
    external_exposure = "#D9EAD3",
    internal_exposure = "#F9D9E6",
    target_site_exposure = "#E4DCF2"
  )
}

#' Background Colour for One Node's Card
#'
#' @param node A one-row nodes tibble, carrying `level`.
#' @return A single hex colour. Falls back to white for an unrecognised or
#'   missing level, rather than erroring: a malformed `aep_nodes.csv` row
#'   should still produce a card.
#' @export
node_card_bg_colour <- function(node) {
  lvl <- node$level[1]
  cols <- node_level_bg_colours()
  if (length(lvl) == 0 || is.na(lvl) || !lvl %in% names(cols)) {
    return("white")
  }
  unname(cols[lvl])
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
      fill = d$fill,
      colour = "white",
      linewidth = 0.6,
      width = 0.98,
      height = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$shown),
      size = text_size,
      colour = "grey10"
    ) +
    # THE BAND MUST BE CENTRED IN ITS PANEL (Sam 2026-08-06: "misaligned with
    # the text above"). Tiles sit at x = 1..n, so the panel has to be symmetric
    # about (n + 1) / 2. The old limits, c(0.4, n + 1.6), put the panel centre
    # half a tile to the RIGHT of the tiles' centre, which on the compact card
    # threw the band out of line with the centred title and headline above it.
    # Panel width is unchanged, so the badges are the same size as before and
    # only their position moves.
    ggplot2::scale_x_continuous(
      limits = (length(labs) + 1) / 2 + c(-1, 1) * (length(labs) / 2 + 0.6)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = 0.02)) +
    ggplot2::theme_void()
}

#' An External Node's Own Time Series
#'
#' The card-body equivalent of [node_card_header()]'s "AM" label: where the
#' headline is an arithmetic mean of a typed-in `external_value`, and that
#' figure came from a (year, value) series rather than a single number, this
#' draws the series that mean was computed from, in the same panel slot a
#' distribution node would use for its violin strips.
#'
#' **Deliberately not a violin.** There is no distribution here in the sense
#' [node_group_strips()] means it -- one value per year, not repeated
#' measurements at any point in time -- so a point per year is the honest
#' summarising geom (CLAUDE.md 4.4), not a stand-in for the real thing.
#'
#' **Points and a line, not bars, since 2026-08-13.** Bars were tried first and
#' do not compose with a log axis, which this needs: a bar encodes magnitude as
#' length from a baseline, and a log axis has no baseline. `geom_col()` resolved
#' that by silently standing every bar on `y = 1` (verified with
#' `ggplot_build()`: a bar of height 100 came back `ymin = 0, ymax = 2`), and
#' even once the base was made explicit the smallest sector stayed a sliver
#' pinned to the floor, because it is genuinely 5.6 orders below the largest and
#' the axis is shared. A point sits at its value and is legible anywhere on the
#' scale. Sam 2026-08-13: "points is absolutely fine then; if anything it'll
#' work better with the avg. line", and "it's fine if all a card's plot shows is
#' that it's an absolutely negligible contribution to copper in the
#' environment".
#'
#' **The panel must occupy exactly the space a violin panel would.** Sam
#' 2026-08-12: "we're just swapping out one graph grob for another without
#' affecting the rest of the plot". It was not doing that. Measured on the
#' real cards by seeking the grid viewports after a draw (not by eye):
#'
#' | card | panel left | panel right |
#' |---|---|---|
#' | external, bars | 0.516 in | 0.055 in |
#' | empirical, violins | 0.055 in | 0.055 in |
#'
#' patchwork aligns all three of a card's panels on their panel areas, so the
#' y-axis labels this drew (`label_comma()` on values up to 58,500,000) pushed
#' the header, the badge strip and the body **all** in by 0.46 in on a 2.4 in
#' card, a fifth of its width. That is the asymmetry Sam saw on the strips, and
#' its cause was here rather than in any of the three strips.
#'
#' So there is **no y axis**. The violin panel does not have one either: it puts
#' its value axis on top, where an axis costs height rather than width, and
#' draws its group labels inside the panel. The headline number in
#' [node_card_header()] and the dashed mean line below carry the magnitude, and
#' with `limits` shared across every source node the series is a comparison
#' between cards rather than a readable scale within one.
#'
#' **A card that shows nothing but "negligible" is doing its job.** Sam
#' 2026-08-13. On the shared axis `N011-water-supply-and-waste-management` sits
#' near the floor and that is the finding, not a rendering problem to design
#' around.
#'
#' @param series A tibble with `year` (numeric or integer) and `value`
#'   columns, one row per year. Extra columns are ignored.
#' @param mean_value The node's own headline figure (`external_value`), drawn
#'   as a dashed reference line so the series can be read against the number
#'   already shown above it in [node_card_header()]. `NA` draws no line.
#' @param limits Shared `list(x = c(min, max), y = c(min, max))` from
#'   [external_series_limits()], so every source node sits on one pair of
#'   axes. `NULL` scales each card to itself, which is the pre-2026-08-12
#'   behaviour and is only useful for looking at one card alone.
#'
#'   Applied with `coord_cartesian()`, **never** `scale_*_continuous(limits=)`:
#'   a continuous scale with explicit limits censors out-of-bounds rows and
#'   silently drops the point. That exact mistake ate the triage panels' count
#'   labels once already (PLAN.md 9a).
#' @param fill Mark colour, matched to the violins' `violin_fill` (`"grey35"`).
#' @param alpha Kept for call compatibility with [node_group_strips()], which
#'   passes `violin_alpha` (0.35) through. **Not applied to the marks.** A
#'   violin is an area whose overlaps have to stay readable; these are a
#'   handful of points and a thin line, and at 0.35 on a pastel card
#'   background they wash out. Named rather than dropped so the caller does not
#'   have to know which of the two it is talking to.
#' @return A ggplot.
#' @export
node_external_series_bars <- function(
  series,
  mean_value = NA_real_,
  limits = NULL,
  fill = "grey35",
  alpha = 0.35
) {
  # A LOG AXIS CANNOT DRAW A NON-POSITIVE VALUE, and two REACH sectors have
  # them: N005 has a year at -545,000 and N007 one at -62,200, because
  # "Netto mengde" is imports plus production minus exports and can go the
  # other way. Those years are net EXPORTS, which is a real fact about copper
  # in commerce but not a quantity a source node releases, so dropping them
  # from the plot is right on the merits as well as forced by the scale.
  #
  # Dropped LOUDLY. ggplot2 would remove them anyway with a generic "removed n
  # rows containing missing values", which says nothing about which node or
  # why; this names both, and lands in tar_meta()'s warnings where Sam reads
  # them.
  dropped <- sum(!is.na(series$value) & series$value <= 0)
  if (dropped > 0) {
    cli::cli_warn(
      "{dropped} year{?s} with a non-positive value omitted from the series: \\
       a log axis cannot show them. These are net exports, not releases."
    )
    series <- series[is.na(series$value) | series$value > 0, , drop = FALSE]
  }

  # SUMMED PER YEAR. A node lumping several REACH sectors claims their sum, and
  # a series with two rows for one year would otherwise draw two points at the
  # same x with no indication that either is a part rather than the whole.
  by_year <- stats::aggregate(
    list(value = series$value), list(year = series$year), sum, na.rm = TRUE
  )
  by_year <- by_year[order(by_year$year), , drop = FALSE]

  # The line connects consecutive years and the points mark the observations.
  # Both carry the same colour as the violins' fill, at full opacity rather
  # than the violins' 0.35: a violin is an area whose overlap must stay
  # readable, while these are a handful of marks that simply need to be seen.
  ink <- fill

  p <- ggplot2::ggplot(
    by_year,
    ggplot2::aes(x = .data$year, y = .data$value)
  ) +
    ggplot2::geom_line(colour = ink, linewidth = 0.35) +
    ggplot2::geom_point(colour = ink, size = 0.7)

  if (!is.na(mean_value) && mean_value > 0 && nrow(by_year) > 0) {
    # A SEGMENT SPANNING THE NODE'S OWN YEARS, not geom_hline's full-panel
    # rule. Sam 2026-08-13. The x axis is shared across every source node, so a
    # full-width line ran on past both ends of a short series and read as a
    # threshold applying to the whole panel rather than as the mean OF THESE
    # POINTS. Clipped to the data, it visibly belongs to them.
    p <- p +
      ggplot2::annotate(
        "segment",
        x = min(by_year$year), xend = max(by_year$year),
        y = mean_value, yend = mean_value,
        # Same dashed style and rejection colour convention as the "(!)"
        # suspect marker elsewhere on the card (node_card_header()), reused
        # here for the same reason: a reference line, not new colour
        # vocabulary to learn.
        linetype = "22", colour = "#A8452F", linewidth = 0.4
      ) +
      ggplot2::annotate(
        "text",
        x = max(by_year$year), y = mean_value, label = "AM",
        # hjust = 1 anchors the text's RIGHT edge at the line's right end, so
        # it sits above the line and inside the panel. Anchoring left would
        # overflow for any node whose last year is the global last year, which
        # is most of them.
        hjust = 1, vjust = -0.45,
        size = 1.25, colour = "#A8452F"
      )
  }

  # ONE BREAK VECTOR, used by the gridlines AND the labels. They used to be
  # computed independently -- scale_y_log10()'s own default breaks for the
  # lines, scales::breaks_log() for the text -- so there was never anything
  # making them agree, and on the real cards they did not: three gridlines
  # against two labels, with no way to tell which line either label belonged
  # to (Sam 2026-08-13, "pretty inscrutable"). Sharing the vector makes the
  # correspondence structural rather than coincidental.
  brk <- external_series_breaks(limits)

  p <- p +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    # log10, matching the violins next door and the rest of this project's
    # value axes. Points have no baseline to lose, which is exactly why they
    # survive this scale where bars did not.
    ggplot2::scale_y_log10(
      breaks = if (length(brk) > 0) brk else ggplot2::waiver()
    ) +
    # Value labels go INSIDE the panel; see compact_bar_value_labels() for why
    # a real axis here misaligns the whole card.
    compact_bar_value_labels(limits, breaks = brk) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 7) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 6, colour = "grey40"),
      # The whole point of this block: no y axis text, no y ticks, no y title,
      # so the panel claims no horizontal space outside itself and lines up
      # with the violin panel exactly.
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 0, l = 2)
    )

  if (!is.null(limits)) {
    p <- p + ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y)
  }

  p
}

#' Gridline Positions for the External-Series Panel
#'
#' The single source of truth for where the horizontal lines go, so that
#' [compact_bar_value_labels()] and `scale_y_log10()` cannot disagree. They did:
#' the scale used its own default breaks and the labels used
#' `scales::breaks_log()`, which on the real cards gave **three gridlines and
#' two labels**, with nothing to say which line either label belonged to.
#'
#' Whole powers of ten only. The panel is roughly 0.6 in tall on a compact card
#' and the shared range spans six orders, so `2.5e6`-style intermediate breaks
#' are both unreadable at that size and pointless on a log axis where the
#' decades are the structure.
#'
#' @param limits Shared limits from [external_series_limits()].
#' @param n Target number of gridlines.
#' @return A numeric vector of break positions, possibly empty.
#' @export
external_series_breaks <- function(limits, n = 3) {
  if (is.null(limits) || !all(is.finite(limits$y)) || any(limits$y <= 0)) {
    return(numeric(0))
  }
  lo <- ceiling(log10(limits$y[1]))
  hi <- floor(log10(limits$y[2]))
  if (!is.finite(lo) || !is.finite(hi) || hi < lo) {
    return(numeric(0))
  }
  powers <- lo:hi
  # Thin to at most n, keeping the extremes so the labelled span still
  # describes the whole panel.
  if (length(powers) > n) {
    powers <- unique(round(seq(lo, hi, length.out = n)))
  }
  10^powers
}

#' Value Labels Drawn Inside the Bar Panel
#'
#' The same trick, and for the same reason, as [compact_group_labels()]: a real
#' y axis lives OUTSIDE the panel and so consumes horizontal width, which
#' patchwork then subtracts from all three of the card's panels and throws the
#' header and badge strip out of line with every violin card. Drawn inside, the
#' labels cost nothing and the panels stay aligned.
#'
#' Removing the axis entirely was the first fix (2026-08-12) and went too far.
#' Sam, same day: "currently the source bar charts have no y axis labels at
#' all. I don't need to tell you why this is bad."
#'
#' **Exponent-only labels** (`1e2`, `1e4`), per Sam's "do 1eXX kg, I guess".
#' The unit is not repeated on each label: it is already on the headline
#' directly above (`AM 4.06e+07 kg/y`), and at card size a six-character label
#' per gridline crowds the bars it is meant to annotate. Because the scale is
#' shared across every source node, the labels are identical on all of them,
#' which is what makes them comparable at a glance.
#'
#' @param limits Shared limits from [external_series_limits()].
#' @param breaks The break vector, from [external_series_breaks()]. **Pass the
#'   same vector the scale was given.** Recomputing it here is what produced
#'   three gridlines against two labels on the real cards; the argument exists
#'   so the two cannot drift again.
#' @param size Text size.
#' @return A list of ggplot2 layers, possibly empty.
#' @export
compact_bar_value_labels <- function(
  limits, breaks = external_series_breaks(limits), size = 1.4
) {
  if (is.null(limits) || !all(is.finite(limits$y)) || any(limits$y <= 0)) {
    return(list())
  }
  if (length(breaks) == 0) {
    return(list())
  }
  d <- data.frame(
    x = limits$x[1],
    y = breaks,
    label = paste0("1e", round(log10(breaks)))
  )
  list(
    ggplot2::geom_text(
      data = d,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      hjust = 0, vjust = -0.25,
      size = size, colour = "grey45"
    )
  )
}

#' Shared Axis Limits for Every External Node's Bars
#'
#' Sam 2026-08-12: "the source bar charts should all use the same x and y axes.
#' If that means some of the bars are very small, so be it."
#'
#' Derived from the whole `external_series` list rather than plumbed through a
#' new target, because [node_group_strips()] is already handed every node's
#' series in one object (it has to be, since one call draws many cards), so the
#' shared range is available for free at the only place that needs it.
#'
#' **The y range is over POSITIVE values only, because the axis is log10.**
#' This reverses the "always include zero" rule that stood for about an hour on
#' 2026-08-12, and the reversal is the point rather than a correction to skip
#' past: on a linear axis a bar is read from a zero baseline, so excluding zero
#' exaggerates every difference; on a log axis there is no zero to include, and
#' forcing one gives an infinite range.
#'
#' The linear version was tried first and rejected on the render. Across the
#' eight sectors the series span 74 to 5.85e7 kg/yr, nearly six orders, so on
#' one linear axis `N011-water-supply-and-waste-management`'s tallest year came
#' out at 0.00025 of the axis: not a small bar, a blank panel that reads as a
#' broken chart. Sam 2026-08-12: "absolutely a shared log axis rather than a
#' regular one. Really the y axis on the bar charts should be logged for
#' reasons of spacing anyway."
#'
#' **The bottom of the range is floored at `max_orders` below the top**, and
#' that is not cosmetic. Measured 2026-08-12: the smallest positive value in
#' the REACH series is 7.4e-5 kg/yr, and the five smallest all belong to
#' `N007-other-services-and-administration`, whose own headline is 4.06e6.
#' Sub-gram annual quantities in a net-quantity series are imports and exports
#' very nearly cancelling, not a magnitude anyone is claiming; taking them at
#' face value stretched the shared axis to **12.3 orders** and spent 40% of
#' every panel on empty space below the smallest real number. Six orders covers
#' 74 to 5.85e7, which is every sector's actual working range.
#'
#' Values below the floor are **clipped, not dropped**: `coord_cartesian()`
#' zooms rather than censors, so the row is still in the data and its bar is
#' simply off the bottom.
#'
#' @param external_series Named list of (year, value) tibbles keyed by
#'   `node_id`.
#' @param max_orders How many orders of magnitude the shared axis may span,
#'   measured down from the largest value.
#' @return `list(x = c(min, max), y = c(min, max))` with `y` strictly positive,
#'   or `NULL` where there is nothing to derive a range from.
#' @export
external_series_limits <- function(external_series, max_orders = 6) {
  if (is.null(external_series) || length(external_series) == 0) {
    return(NULL)
  }
  keep <- Filter(function(d) !is.null(d) && nrow(d) > 0, external_series)
  if (length(keep) == 0) {
    return(NULL)
  }
  years <- unlist(lapply(keep, function(d) d$year), use.names = FALSE)
  vals <- unlist(lapply(keep, function(d) d$value), use.names = FALSE)
  years <- years[is.finite(years)]
  # Positive only. See above: a log axis has no room for zero or a negative,
  # and letting one in makes the whole shared range infinite or NaN, which
  # would silently break EVERY source card rather than the one node at fault.
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(years) == 0 || length(vals) == 0) {
    return(NULL)
  }
  top <- max(vals)
  floor_at <- max(min(vals), top / 10^max_orders)
  list(
    # Half a year of padding each side so the outermost bar is not clipped in
    # half by the panel edge; geom_col's width is 0.7, so 0.5 clears it.
    x = c(min(years) - 0.5, max(years) + 0.5),
    # Multiplicative padding, because on a log axis an additive pad is a
    # different size at each end.
    y = c(floor_at / 1.6, top * 1.6)
  )
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
#' @param violin_fill,violin_alpha,violin_colour Violin styling. Translucent
#'   fill and no outline since 2026-08-10; see the comment at the geom for why.
#'   `violin_colour = NA` draws no border at all, which is not the same as
#'   matching it to the fill: a border matching a translucent fill still
#'   doubles the alpha where it lands, drawing a darker rim.
#' @param violin_width Violin width in discrete units. Below 1 the rows cannot
#'   touch; above 1 they overlap. **Leave it at 0.9.** Overlap was tried at 1.8
#'   and 3.0 on 2026-08-10 and rejected, with the renders and the reasoning
#'   kept in `docs/dev-node-card-style.qmd`. The argument stays because that
#'   section needs it to draw the rejected variants, not because widening is a
#'   live option. The y-axis expansion derives from it either way, so the
#'   clearance to the panel edge holds at any width.
#' @param external_series Named list of (year, value) tibbles keyed by
#'   `node_id`, from [node_external_series_bars()]. Only consulted for
#'   `node_type = "external"`; a missing or empty entry falls back to the
#'   plain "no measured data" placeholder, so passing `NULL` (the default)
#'   reproduces the pre-2026-08-11 behaviour exactly.
#' @param blank_when_empty When a node has no distribution to draw (an external
#'   node with no series, no member groups, or nothing left after restrictions),
#'   `FALSE` (the default) fills the panel with a "Not available" placeholder,
#'   as before this argument existed. `TRUE` leaves it blank. Used by the
#'   illustrative Figure 1 (`scripts/build_fig1_example_aep.R`), whose nodes are
#'   deliberately data-free and where the placeholder reads as a real absence.
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
  violin_fill = "grey35",
  violin_alpha = 0.35,
  violin_colour = NA,
  violin_width = 0.9,
  external_series = NULL,
  blank_when_empty = FALSE
) {
  empty_panel <- function(reason) {
    if (isTRUE(blank_when_empty)) {
      ggplot2::ggplot() + ggplot2::theme_void()
    } else {
      triage_empty_plot("", reason, size = 2.6)
    }
  }
  # AN EXTERNAL NODE HAS NO DISTRIBUTION, BUT MAY HAVE A TIME SERIES.
  # resolve_node_data() always returns zero rows for node_type = "external"
  # (there is nothing to draw a violin from), but several external sources ARE
  # natively a (year, value) series before they get collapsed to the single
  # mean/sd typed into external_value/external_sd -- REACH sector data is
  # exactly this. Where the caller supplies that series (keyed by node_id,
  # since write_node_cards() draws many nodes from one call), show it as bars
  # in the space that would otherwise say "no measured data": it is real data
  # about this node, just not the kind resolve_node_data() ever returns.
  if (identical(node$node_type[1], "external")) {
    series <- external_series[[node$node_id[1]]]
    if (!is.null(series) && nrow(series) > 0) {
      return(node_external_series_bars(
        series,
        mean_value = node$external_value[1],
        # Derived from the WHOLE list, not this node's slice, so every source
        # card shares one pair of axes (Sam 2026-08-12). See
        # external_series_limits() for what that costs the smallest sectors.
        limits = external_series_limits(external_series),
        fill = violin_fill,
        alpha = violin_alpha
      ))
    }
    return(empty_panel("no measured data"))
  }

  key <- triage_group_cols()
  my_groups <- members$group_id[members$node_id == node$node_id[1]]

  if (length(my_groups) == 0) {
    return(empty_panel("no measured data"))
  }

  d <- resolve_node_data(node, members, data, ids)
  if (nrow(d) == 0) {
    return(empty_panel("no data after restrictions"))
  }

  # Label each row with the group it came from, so the strips can be split.
  keys <- ids |>
    dplyr::filter(.data$group_id %in% my_groups) |>
    dplyr::select(dplyr::all_of(key), "group_id")
  d <- d |> dplyr::left_join(keys, by = key)

  ranked <- d |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      n = sum(.data$MEASURED_N, na.rm = TRUE),
      .groups = "drop"
    ) |>
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
  # TRANSLUCENT FILL, NO OUTLINE (Sam 2026-08-10). A violin outlined in grey35
  # at card size is mostly outline: the stroke is a visible fraction of a lobe
  # a few pixels tall, so it thickens every shape and closes up the narrow
  # tails. Dropping the border and letting the fill carry the shape is the
  # whole change.
  #
  # It is NOT here to enable overlapping rows. That was tried the same day,
  # rejected, and the evidence is kept in docs/dev-node-card-style.qmd under
  # "Why the violins do not overlap": at `violin_width` 1.8 and 3.0 the fills
  # blend perfectly well and it is the LABELS that fail, so widening buys
  # nothing. `violin_width` stays 0.9, which is why rows cannot touch.
  p <- if (triage_use_points(d)) {
    p + ggplot2::geom_point(alpha = 0.6, size = 0.7)
  } else {
    p +
      ggplot2::geom_violin(
        ggplot2::aes(group = .data$.facet),
        fill = violin_fill,
        alpha = violin_alpha,
        colour = violin_colour,
        scale = "width",
        width = violin_width,
        # trim = FALSE would extend the kernel past the observed range, which on
        # a log axis invents concentrations nobody measured.
        trim = TRUE
      )
  }

  p +
    triage_category_overlay(
      d,
      limits = limits,
      min_n = 10,
      ticks = FALSE,
      # No header: the card's own text block already says what the counts are,
      # and at card width it clipped off the canvas.
      header = FALSE,
      label_size = 1.6,
      labels = FALSE
    ) +
    # Half the default triage-panel linewidth (0.7 -> 0.35): a card is a
    # fraction of a triage panel's size, and Sam 2026-08-07 found the default
    # too heavy at that scale. Overridden here rather than in
    # triage_threshold_layers()'s own default, which is still right for the
    # full-size triage notebooks.
    triage_threshold_layers(
      thr,
      orientation = "vertical",
      limits = limits,
      linewidth = 0.35
    ) +
    triage_value_scale(
      limits = limits,
      axis = "x",
      expand = triage_category_x_expansion()
    ) +
    # CLEARANCE ABOVE AND BELOW THE OUTERMOST VIOLIN, doubled 2026-08-10.
    #
    # An expansion of exactly 0.5 against a violin 0.9 wide leaves 0.05 discrete
    # units, about six pixels at 300 dpi, between the widest point of the
    # outermost violin and the panel edge, which is also where the threshold
    # numerals and their ticks sit. 0.10 doubles it.
    #
    # Expressed as violin half-width plus clearance rather than as a bare
    # number, so that changing `violin_width` cannot silently close the gap.
    # It also keeps a deliberately overlapping layout (width > 1) honest: the
    # panel grows to fit rather than clipping the outer two violins.
    ggplot2::scale_y_discrete(
      expand = ggplot2::expansion(add = rep(violin_width / 2 + 0.10, 2))
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      caption = if (length(dropped) > 0) {
        paste0(
          "+ ",
          length(dropped),
          " smaller group(s) not shown: ",
          paste(dropped, collapse = ", ")
        )
      } else {
        NULL
      }
    ) +
    triage_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = ggplot2::rel(0.55),
        colour = "grey45"
      ),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.6)),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.5),
        colour = "grey50",
        hjust = 0
      ),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    # At graph-node size the axis, the row labels and the margin counts are all
    # illegible, so they are removed rather than drawn too small to read. The
    # threshold lines and the violin shape survive, which is what a glance is
    # for.
    ggplot2::labs(caption = NULL) +
    # GROUP IDS MOVE INSIDE THE PANEL, and that is an alignment fix rather
    # than a cosmetic one. A y axis pushes the strip's panel to the right,
    # while the header and badge plots above are theme_void and span the full
    # card, so the text block sat about 8% of the card left of the violins.
    # Drawing the ids inside the panel lets all three panels span the same
    # width, and then centring on x = 0.5 means the same thing everywhere.
    compact_group_labels(d, limits) +
    compact_value_scale(limits, thresholds = thr) +
    compact_axis_theme()
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
#' @param size Text size. 1.425 since 2026-08-10, three quarters of the
#'   original 1.9: halving it first (to 0.95) put the glyphs at 8px on a 300 dpi
#'   card, which Sam judged too small, and this splits the difference at 12px.
#'   The id is a handle for looking a group up rather than something to be read
#'   at a glance like the headline number, so it can afford to be the smallest
#'   text on the card, but not so small it stops being readable. An argument
#'   rather than a constant because the right value depends on how large the
#'   card is drawn, and the AEP diagram and the dev notebook draw it at
#'   different sizes.
#' @return A ggplot2 layer.
#' @export
compact_group_labels <- function(data, limits = NULL, size = 1.425) {
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
    hjust = 0,
    # SCALED BY `size`, and it has to be. `vjust` is in units of the text's own
    # height, so halving the size at a fixed vjust halves the offset too, and
    # the label lands on the violin's tail line instead of above it. This keeps
    # the offset that size 1.9 had at vjust -0.9, whatever the size is now.
    vjust = -0.9 * 1.9 / size,
    size = size,
    colour = "grey45"
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

#' Relative Heights of a Card's Three Panels
#'
#' Header, badges, strips, in that order. Pulled out of [node_card()] so
#' anything illustrating the card's layout (e.g. a dev notebook's box-model
#' diagram) reads the real ratio instead of a second, driftable copy of the
#' same three numbers.
#'
#' @return A named numeric vector, `c(header, badges, strips)`.
#' @export
node_card_heights <- function() {
  # Header takes the most: a wrapped two-line title plus the headline plus
  # the counts line does not fit in the same band as a single strip.
  c(header = 1.55, badges = 0.40, strips = 1.4)
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
#' @param dpi The device resolution this card will be saved at. Only affects
#'   the corner id marker; see [node_card_header()]. 300 since 2026-08-10, up
#'   from 200: nothing in the card's geometry is measured in pixels, so this is
#'   the same card with more of them. Note that `aep_diagrams` is written at
#'   `dpi = 150`, and a card occupies roughly 340px there, so beyond about 200
#'   this buys anti-aliasing on the diagram and nothing else. It is the card
#'   viewed on its own that gains.
#' @param ... Passed to [node_group_strips()], which is where every knob worth
#'   turning while styling a card lives (`violin_fill`, `violin_alpha`,
#'   `violin_colour`, `violin_width`, `external_series`).
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
  dpi = 300,
  ...
) {
  # A card placed on a graph node is roughly 1.6in wide, so every point size
  # lands under half its intended value at anything larger. Fewer, larger
  # elements is the only way down in size; scaling is not. What survives: the
  # label, the headline number, the EPEQ badges, and the distribution. What
  # goes: the count line, the per-group row labels, the axis, and the margin
  # counts.
  header <- node_card_header(node, card, dpi = dpi)
  badges <- node_epeq_badges(node, text_size = 3.4)
  strips <- node_group_strips(
    node,
    members,
    data,
    ids,
    limits = limits,
    thresholds = thresholds,
    max_groups = max_groups,
    ...
  )

  patchwork::wrap_plots(
    header,
    badges,
    strips,
    ncol = 1,
    heights = node_card_heights()
  ) &
    node_card_theme(node)
}

#' The Theme Every Panel of a Card Shares
#'
#' Applied to all three panels with patchwork's `&`, which merges into each
#' panel's theme rather than replacing it, so it survives on top of
#' `theme_void()` (header, badges) and [triage_theme()] (strips) alike.
#'
#' Exported for the same reason as [node_card_heights()]: the dev notebook
#' shows the card being assembled panel by panel, and a second hand-written
#' copy of this theme there would drift from the real one.
#'
#' **The margin is shared deliberately, and must not be cut per panel.** All
#' three panels are aligned by patchwork on their panel areas, so trimming one
#' panel's left or right margin alone widens that panel relative to the other
#' two. The card's centred title, its badge band and the group ids drawn inside
#' the strips panel all assume the three panels span the same width, and that
#' assumption is what keeps `x = 0.5` meaning the same thing on each.
#'
#' @param node A one-row nodes tibble, carrying `level`.
#' @param margin Outer margin on every side, in points. Cut from ggplot2's
#'   default 5.5 to 2 on 2026-08-10 at Sam's request. On a 1.8 in card the top
#'   and bottom defaults together took a fifth of the strips band, which is a
#'   lot of a violin to spend on white space. Not zero: at zero a violin tail
#'   or an axis numeral sits flush against the card edge, and against the
#'   neighbouring card once these are placed on the AEP diagram.
#' @return A ggplot2 theme.
#' @export
node_card_theme <- function(node, margin = 2) {
  bg <- node_card_bg_colour(node)
  ggplot2::theme(
    # Both panel and plot background are set: panel is what shows behind the
    # violin or points, plot is the margin around it, and a colour visible in
    # one but not the other would look like a rendering bug rather than a fill.
    plot.background = ggplot2::element_rect(fill = bg, colour = NA),
    panel.background = ggplot2::element_rect(fill = bg, colour = NA),
    plot.margin = ggplot2::margin(margin, margin, margin, margin, unit = "pt")
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
#' @param thresholds Output of [thresholds_for_group()], or `NULL`. Where
#'   given, adds a secondary axis on top naming each threshold's class in
#'   roman numerals (I, II, III, ...) at the value it sits on. Sam asked for
#'   the class levels on the compact card but nothing else the full secondary
#'   axis carries, so unlike [triage_threshold_sec_axis()] this omits the axis
#'   title (source and matrix): at card width there is no room for it and it
#'   would compete with the class numerals it is meant to explain.
#' @return A ggplot2 scale.
#' @export
compact_value_scale <- function(limits = NULL, every = 3, thresholds = NULL) {
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
    guide = ggplot2::guide_axis(minor.ticks = TRUE),
    sec.axis = compact_threshold_sec_axis(thresholds, limits)
  )
}

#' Compact Secondary Axis Naming Only the Threshold Classes
#'
#' A pared-down [triage_threshold_sec_axis()]: same roman-numeral breaks, no
#' axis title. See [compact_value_scale()] for why.
#'
#' @param thresholds Output of [thresholds_for_group()], or `NULL`.
#' @param limits Shared value-axis limits, used to drop off-scale breaks.
#' @return A `ggplot2::dup_axis()` specification, or `ggplot2::waiver()`.
#' @export
compact_threshold_sec_axis <- function(thresholds, limits = NULL) {
  thresholds <- thresholds_in_limits(thresholds, limits)
  if (is.null(thresholds) || nrow(thresholds) == 0) {
    return(ggplot2::waiver())
  }
  ggplot2::dup_axis(
    breaks = thresholds$THRESHOLD_VALUE_STANDARD,
    labels = threshold_axis_label(thresholds),
    name = NULL
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
    axis.text.x = ggplot2::element_text(
      size = ggplot2::rel(0.62),
      colour = "grey40"
    ),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_line(colour = "grey45", linewidth = 0.3),
    axis.minor.ticks.x.bottom = ggplot2::element_line(
      colour = "grey65",
      linewidth = 0.22
    ),
    axis.ticks.length.x = ggplot2::unit(2.4, "pt"),
    axis.minor.ticks.length.x = ggplot2::rel(0.5),
    panel.grid = ggplot2::element_blank(),
    # Roman numerals at the bottom axis's text size read as noise, per the same
    # reasoning as triage_sec_axis_theme(): bolder and a touch larger so they
    # read as the class label they are.
    axis.text.x.top = ggplot2::element_text(
      size = ggplot2::rel(0.68),
      face = "bold",
      colour = "grey25",
      # PULLED IN TO 2px, from ggplot2's default 4.95pt (23px at 300 dpi).
      # Sam 2026-08-10: the numerals floated a long way off the lines they
      # name, which on a panel this short reads as two separate rows of
      # furniture rather than one label per line. `margin` on a top axis is
      # measured on the side facing the panel, so `b` is the one that matters.
      margin = ggplot2::margin(b = 0.5, unit = "pt")
    ),
    axis.title.x.top = ggplot2::element_blank(),
    axis.ticks.x.top = ggplot2::element_line(colour = "grey45", linewidth = 0.3),
    # The tick then carries the remaining distance, so the numeral, the tick and
    # the line it names read as one mark. Halved from the shared 2.4pt for the
    # same reason: it is a connector here, not a scale reading aid.
    axis.ticks.length.x.top = ggplot2::unit(1.2, "pt")
  )
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

#' Year Range of a Node's Underlying Measurements
#'
#' `"2004-2019"`, or a single year where `date_min` and `date_max` fall in the
#' same year, or `""` where neither is known (an external node, or a node with
#' no `SAMPLING_DATE` at all).
#'
#' @param card A one-row report card, carrying `date_min`/`date_max`.
#' @return A single string, possibly empty.
#' @export
node_card_year_range <- function(card) {
  if (!all(c("date_min", "date_max") %in% names(card))) {
    return("")
  }
  lo <- card$date_min[1]
  hi <- card$date_max[1]
  if (length(lo) == 0 || length(hi) == 0 || is.na(lo) || is.na(hi)) {
    return("")
  }
  y_lo <- format(as.Date(lo), "%Y")
  y_hi <- format(as.Date(hi), "%Y")
  if (identical(y_lo, y_hi)) y_lo else paste0(y_lo, "-", y_hi)
}

#' The Text Block at the Top of a Card
#'
#' Label, then the four aggregation levels Sam asked for (measurements, rows,
#' groups, references) and the weighted centre with its unit.
#'
#' @param node A one-row nodes tibble.
#' @param card The matching report-card row.
#' @param dpi The device resolution this card will be saved at. The corner id
#'   marker is offset in PIXELS from the corner, not in points -- a physical
#'   unit (points, inches, mm) would drift in device pixels as `dpi` changes,
#'   so the offset is computed in inches from `dpi` right before use
#'   (`offset / dpi`), matching whatever raster the card is actually saved as.
#'   The offset itself is 2px as of 2026-08-10 (Sam cut it from 18); this
#'   sentence used to name 18 and had gone stale, so it now points at the
#'   constant rather than repeating it.
#' @return A ggplot.
#' @export
node_card_header <- function(node, card, dpi = 300) {
  num <- function(x) {
    if (length(x) == 0 || is.na(x)) {
      "-"
    } else {
      formatC(x, format = "g", digits = 3)
    }
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
  #
  # FALLS BACK TO card$mean FOR AN EXTERNAL NODE. node_report_card() never
  # computes geo_mean for node_type = "external" (there is no distribution to
  # take a geometric mean of; the typed-in external_value IS the figure), so
  # geo_mean is always NA there and the coalesce is load-bearing, not
  # defensive: without it every external node's headline silently rendered
  # "- <unit>" rather than its value, unnoticed until the first batch of
  # external cards (the REACH sector nodes, 2026-08-11) was actually rendered
  # and looked at, per CLAUDE.md 2.3.1.
  #
  # LABELLED "GM"/"AM" (2026-08-11), because the coalesce means the headline
  # can now be either statistic and nothing distinguished them on the card
  # itself -- a geometric and an arithmetic mean are not interchangeable
  # numbers, and a reader quoting the card has no way to tell which one they
  # have. NOT "μg"/"μa": this project has a standing, costly rule against writing a
  # micro sign anywhere it can be avoided (CLAUDE.md 4.4.-2, 18 rows of real
  # data lost silently to one), and half these cards' units are already
  # "µg/kg" or similar -- putting a look-alike Greek mu directly in front of a
  # unit that may itself start with a micro sign is exactly the collision
  # that rule exists to prevent. Plain ASCII "GM"/"AM" is unambiguous next to
  # any unit string. Omitted entirely when there is no value to label (an
  # external node with nothing entered, e.g. N001).
  headline_value <- dplyr::coalesce(card$geo_mean, card$mean)
  headline_stat <- if (length(headline_value) == 0 || is.na(headline_value)) {
    ""
  } else if (!is.na(card$geo_mean[1])) {
    "GM "
  } else {
    "AM "
  }
  headline <- paste0(
    headline_stat,
    num(headline_value),
    if (nzchar(unit)) paste0(" ", unit) else ""
  )
  # A marker, not a scolding: the reader still gets the number, and a reason to
  # go and look at the strips below before quoting it.
  suspect <- isTRUE(headline_is_suspect(card))
  if (suspect) {
    headline <- paste0(headline, "  (!)")
  }
  # Sample size, source count and the year range: those are what make the
  # headline a measurement rather than an assertion, and "when was this
  # measured" is as basic a question as "how much data".
  year_range <- node_card_year_range(card)
  compact_counts <- paste0(
    "n = ",
    count(card$n),
    ", refs = ",
    count(card$n_sources),
    if (nzchar(year_range)) paste0(", ", year_range) else ""
  )
  # Arctic coverage is DROPPED FROM THE CARD but still computed and carried in
  # aep_node_cards, per Sam: "remove the Arctic measure from the plot, but keep
  # the code. we can worry about it later."

  # 18x18 PIXELS at the card's own save resolution, not a physical unit -- see
  # the `dpi` argument doc above.
  corner_offset <- grid::unit(2 / dpi, "inches")

  # geo_scope marker for the top-right corner. NULL (no icon) unless this is a
  # scoped AEP node carrying a geo_scope; see geo_scope_icon_path().
  geo_icon <- geo_scope_icon_path(node[["geo_scope"]])

  ggplot2::ggplot() +
    # NODE ID IN THE TOP LEFT, at default size (Sam 2026-08-06, moved from
    # the top right later the same day): it is a handle for referring to the
    # node in conversation, not information about it, so it sits out of the
    # reading path rather than inside the title.
    #
    # Anchored to the card's physical corner (18x18px from the top-left)
    # rather than the data coordinates used everywhere else on this card
    # (Sam 2026-08-07: composite ids like "N003-mine-tailings" are longer
    # than the old "N003" and need to stay pinned to the corner regardless
    # of card size, not drift with the data range).
    ggplot2::annotation_custom(
      grid::textGrob(
        node$node_id[1],
        x = corner_offset,
        y = grid::unit(1, "npc") - corner_offset,
        hjust = 0,
        vjust = 1,
        gp = grid::gpar(fontsize = 0.75 * 2.6 * ggplot2::.pt, col = "grey55")
      )
    ) +
    # TOP-RIGHT: the geo_scope marker, mirror of the node id. A pin means the
    # node's data is specific to this AEP's bounding box, a globe that it is
    # drawn from a wider region (geo_scope = "arctic"). Absent on national cards.
    # ggplot drops a NULL layer silently, so no branch is needed at draw time.
    (if (!is.null(geo_icon)) {
      ggplot2::annotation_custom(card_icon_grob(geo_icon, dpi = dpi))
    }) +
    # All three centred on the SAME anchor, x = 0.5 with hjust = 0.5.
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 2.35,
      hjust = 0.5,
      vjust = 1,
      size = 4.5,
      fontface = "bold",
      # WIDTH 24, up from 18 on 2026-08-13. At 18 the longest labels wrapped to
      # two lines, and the second line dropped onto the headline underneath:
      # "Marine benthic inverts" printed over "GM 18.2 mg/kg (dry)".
      #
      # Measured rather than guessed, with grid::stringWidth() at this text's
      # own size and face: the widest label in the whole node set is that same
      # "Marine benthic inverts" at **1.887 in**, against a panel 2.289 in
      # wide. So every current label fits on one line with 0.4 in to spare, and
      # the wrap was firing on nothing. 24 characters is about 2.06 in at the
      # measured 0.086 in per character, which keeps the headroom rather than
      # spending it.
      #
      # This is a width limit, not a collision guard: a genuinely longer label
      # will still wrap, and two lines still collide with the headline. If that
      # happens the fix is the title size or the panel heights, not a bigger
      # number here.
      label = stringr::str_wrap(node$label[1], width = 24),
      lineheight = 0.95,
      colour = "grey10"
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.75,
      hjust = 0.5,
      vjust = 0.5,
      size = 3.7,
      fontface = "bold",
      label = headline,
      colour = if (suspect) "#A8452F" else "grey5"
    ) +
    ggplot2::annotate(
      # Dropped further below the concentration than it was: at one line's
      # spacing the sample size read as part of the number above it.
      "text",
      x = 0.5,
      y = -0.35,
      hjust = 0.5,
      vjust = 0.5,
      size = 2.7,
      label = compact_counts,
      colour = "grey40"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(-0.75, 3.1)) +
    ggplot2::theme_void()
}

#' Write a Card per Node
#'
#' @param nodes,cards,members,data,ids As elsewhere.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param dir Output directory.
#' @param width,height,dpi Canvas.
#' @param limits Shared value limits per unit, from [node_card_limits()]. Passed
#'   in rather than computed once AEPs exist: limits derived from one AEP's nodes
#'   are that AEP's, and two cards for the same node under different axes cannot
#'   be compared. The caller computes them across the whole node pool.
#' @param external_series Passed straight to [node_group_strips()]; see its
#'   own doc. `NULL` (the default) draws every external node's body panel as
#'   "no measured data", same as before this parameter existed.
#' @return The written paths.
#' @export
write_node_cards <- function(
  nodes,
  cards,
  members,
  data,
  ids,
  thresholds = NULL,
  dir = here_rel("images/node_cards"),
  width = 2.4,
  height = 1.8,
  dpi = 300,
  limits = NULL,
  external_series = NULL
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  if (is.null(limits)) {
    limits <- node_card_limits(nodes, members, data, ids)
  }

  paths <- character(0)
  for (i in seq_len(nrow(nodes))) {
    node <- nodes[i, , drop = FALSE]
    card <- cards[cards$node_id == node$node_id[1], , drop = FALSE]
    if (nrow(card) == 0) {
      next
    }
    lim <- limits[[card$unit[1] %||% ""]]
    p <- node_card(
      node,
      card,
      members,
      data,
      ids,
      limits = lim,
      thresholds = thresholds,
      dpi = dpi,
      external_series = external_series
    )
    path <- file.path(dir, paste0(node$node_id[1], ".png"))
    ggplot2::ggsave(
      filename = path,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      device = ragg::agg_png,
      bg = node_card_bg_colour(node)
    )
    paths <- c(paths, path)
  }
  paths
}

#' Node Report Table for One AEP (flextable)
#'
#' The tabular companion to the per-node report cards: one row per node in an
#' AEP, carrying the node's identity and type alongside summary statistics of
#' whatever numerical aspect it has (measured concentrations for an empirical
#' node, the hand-entered magnitude for an external one). First pass; expect
#' the column set to move.
#'
#' Statistics come straight from [aep_all_report_cards()] (so the centre is
#' `MEASURED_N`-weighted and the spread is per row, per `?node_statistic_weighting`);
#' the four EPEQ scores are joined from the AEP-scoped nodes table so any
#' per-AEP override is reflected. Nodes with no resolved data keep their row --
#' an empty statistics row is itself the finding that the node is hypothesised
#' but unsupported here.
#'
#' The node id (`NXXX...`) leads the table, and the last column lists every
#' distinct `REFERENCE_ID` behind the node (a dash for an external node, which
#' has no rows to draw ids from). The four EPEQ headers are single letters,
#' `E P E Q`, matching how they are named in the surrounding text.
#'
#' @param cards [aep_all_report_cards()] output, filtered to one `aep_id`.
#' @param scoped The matching element of [aep_scoped_nodes()] (e.g.
#'   `aep_scoped[["A001"]]`), for the EPEQ scores.
#' @return A flextable.
#' @export
node_report_flextable <- function(cards, scoped) {
  sig <- function(x, d = 3) ifelse(is.na(x), NA_character_, formatC(signif(x, d), format = "fg", big.mark = ","))
  yr <- function(d) ifelse(is.na(d), NA_character_, format(d, "%Y"))

  epeq <- scoped[c(
    "node_id", "essentiality_score", "plausibility_score",
    "evidence_score", "quantification_score"
  )]

  dash <- "\u2014"

  tbl <- cards |>
    dplyr::left_join(epeq, by = "node_id") |>
    dplyr::transmute(
      node_id = .data$node_id,
      node = .data$label,
      level = .data$level,
      type = .data$node_type,
      n_disp = dplyr::case_when(
        is.na(.data$n) & .data$n_rows == 0 ~ dash,
        .data$node_type == "external" ~ formatC(.data$n, format = "d", big.mark = ","),
        TRUE ~ sprintf(
          "%s (%s; %s)",
          formatC(.data$n, format = "d", big.mark = ","),
          formatC(.data$n_rows, format = "d", big.mark = ","),
          .data$n_sources
        )
      ),
      unit = dplyr::coalesce(.data$unit, dash),
      mean_sd = dplyr::case_when(
        is.na(.data$mean) ~ dash,
        is.na(.data$sd) ~ sig(.data$mean),
        TRUE ~ paste0(sig(.data$mean), " \u00b1 ", sig(.data$sd))
      ),
      median = dplyr::coalesce(sig(.data$median), dash),
      geo_mean = dplyr::coalesce(sig(.data$geo_mean), dash),
      dates = dplyr::case_when(
        is.na(.data$date_min) ~ dash,
        yr(.data$date_min) == yr(.data$date_max) ~ yr(.data$date_min),
        TRUE ~ paste0(yr(.data$date_min), "\u2013", yr(.data$date_max))
      ),
      ess = .data$essentiality_score,
      pla = .data$plausibility_score,
      evi = .data$evidence_score,
      qua = .data$quantification_score,
      references = dplyr::coalesce(.data$references, dash)
    )

  tbl |>
    flextable::flextable() |>
    flextable::set_header_labels(
      node_id = "ID", node = "Node", level = "Level", type = "Type",
      n_disp = "n (rows; refs)", unit = "Unit", mean_sd = "Mean \u00b1 SD",
      median = "Median", geo_mean = "Geo. mean", dates = "Dates",
      ess = "E", pla = "P", evi = "E", qua = "Q",
      references = "References"
    ) |>
    flextable::theme_vanilla() |>
    flextable::bold(part = "header") |>
    flextable::colformat_num(
      j = c("ess", "pla", "evi", "qua"), na_str = dash, digits = 0
    ) |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::padding(padding = 2, part = "all") |>
    flextable::autofit()
}
