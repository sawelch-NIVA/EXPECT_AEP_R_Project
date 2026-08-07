# Declarative node grouping for the AEP diagram (2026-08-05).
#
# Sam: "Having everything say 'coastal' at the start is clearly a bit silly. Some
# way to declaratively group nodes is clearly needed, so that we can put
# N001 - N005 into a box labelled 'coastal' and 4 and 5 into a box labelled cod."
#
# Right, and the redundancy is the symptom rather than the disease: a label
# repeating "Coastal" five times is a shared property that has nowhere else to
# live. Give it somewhere and the labels shorten themselves.
#
# GROUPS NEST AND MAY OVERLAP, which is what rules out the obvious cheap
# implementation. A `cluster` column on aep_nodes.csv holds one membership per
# node, and N004 needs two (coastal AND cod). So membership is many-to-many.
#
# It is stored as a semicolon-separated `node_ids` column rather than one row per
# (group, node). Normally the wrong call, but here: there will be a handful of
# groups, they are hand-edited, and nesting is far easier to SEE when the members
# of each group sit on one line. The semicolon-list convention already exists in
# this project on `exclude_references`.

#' An Empty Node-Groups Table
#'
#' @return A zero-row tibble.
#' @export
empty_aep_node_groups <- function() {
  tibble::tibble(
    group_key = character(0),
    label = character(0),
    node_ids = character(0),
    notes = character(0)
  )
}

#' Read and Validate the Node-Groups File
#'
#' @param path Where the CSV lives.
#' @param nodes Optional nodes table, to check every named node exists.
#' @return A tibble with a parsed `members` list column.
#' @export
read_aep_node_groups <- function(
  path = here_rel("data/clean/aep/aep_node_groups.csv"),
  nodes = NULL
) {
  if (!file.exists(path)) {
    return(dplyr::mutate(empty_aep_node_groups(), members = list()))
  }
  groups <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  missing <- setdiff(c("group_key", "label", "node_ids"), names(groups))
  if (length(missing) > 0) {
    stop("Node-groups file is missing column(s): ", paste(missing, collapse = ", "))
  }

  dup <- groups$group_key[duplicated(groups$group_key)]
  if (length(dup) > 0) {
    stop("Duplicate group_key(s): ", paste(unique(dup), collapse = ", "))
  }

  groups$members <- lapply(groups$node_ids, function(x) {
    if (is.na(x)) {
      return(character(0))
    }
    out <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
    out[nzchar(out)]
  })

  empty <- groups$group_key[lengths(groups$members) == 0]
  if (length(empty) > 0) {
    cli::cli_warn(
      "Node group(s) with no members: {paste(empty, collapse = ', ')}."
    )
  }

  if (!is.null(nodes)) {
    unknown <- setdiff(unlist(groups$members), nodes$node_id)
    if (length(unknown) > 0) {
      stop(
        "Node groups name ", length(unknown), " unknown node_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", ")
      )
    }
  }

  groups
}

#' Nesting Depth of Each Group
#'
#' Depth is the number of other groups that strictly contain this one. A group
#' whose members are a subset of another's is drawn inside it, with tighter
#' padding, so "Cod" sits within "Coastal" without being told to.
#'
#' **Derived from membership, not declared.** A `parent` column would have to be
#' kept consistent with the member lists by hand, and the two would drift. This
#' cannot drift: containment is a fact about the lists.
#'
#' Groups that merely overlap without containment both get depth 0 and will draw
#' as intersecting boxes, which is honest. Nothing here tries to prevent that.
#'
#' @param groups Output of [read_aep_node_groups()].
#' @return An integer vector, one per row.
#' @export
aep_group_depth <- function(groups) {
  n <- nrow(groups)
  if (n == 0) {
    return(integer(0))
  }
  vapply(
    seq_len(n),
    function(i) {
      mine <- groups$members[[i]]
      if (length(mine) == 0) {
        return(0L)
      }
      sum(vapply(
        seq_len(n),
        function(j) {
          if (i == j) {
            return(FALSE)
          }
          theirs <- groups$members[[j]]
          # Strict containment: mine inside theirs, and not identical.
          all(mine %in% theirs) && length(theirs) > length(mine)
        },
        logical(1)
      ))
    },
    integer(1)
  )
}

#' Bounding Boxes for the Node Groups
#'
#' One rectangle per group, around its members' hand-placed coordinates, inset by
#' depth so a nested group draws inside its parent rather than on top of it.
#'
#' @param groups Output of [read_aep_node_groups()].
#' @param nodes The nodes table, with `x` and `y`.
#' @param pad Padding at depth 0, in coordinate units.
#' @param inset How much padding is removed per level of nesting.
#' @param card_hw,card_hh Half-width and half-height of a node card in data
#'   units, from [node_card_extent()]. When supplied, the box is pulled out far
#'   enough to clear the card itself, not just `pad`: a fixed `pad` smaller than
#'   the card's own half-height leaves the box top, and the label above it,
#'   drawn under the topmost card's image. `NULL` (the default) keeps the old
#'   behaviour for callers with no cards to clear, e.g. a text-label diagram.
#' @param label_margin Extra clearance above the box top reserved for the
#'   label text itself, in coordinate units, on top of whatever clears the
#'   card.
#' @return A tibble of `group_key`, `label`, `xmin`, `xmax`, `ymin`, `ymax`,
#'   `depth`.
#' @export
aep_group_boxes <- function(
  groups, nodes, pad = 0.42, inset = 0.13,
  card_hw = NULL, card_hh = NULL, label_margin = 0.06
) {
  if (nrow(groups) == 0) {
    return(tibble::tibble(
      group_key = character(0), label = character(0),
      xmin = numeric(0), xmax = numeric(0),
      ymin = numeric(0), ymax = numeric(0), depth = integer(0)
    ))
  }

  depth <- aep_group_depth(groups)
  placed <- nodes |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))

  # A fixed `pad` is a guess at the card's footprint; a real one is available
  # once cards are actually being drawn, so use whichever is larger. Only the
  # top gets the extra label_margin, since that is the only edge with text
  # sitting on it.
  pad_x <- if (!is.null(card_hw)) max(pad, card_hw) else pad
  pad_y <- if (!is.null(card_hh)) max(pad, card_hh) else pad

  out <- lapply(seq_len(nrow(groups)), function(i) {
    members <- placed |> dplyr::filter(.data$node_id %in% groups$members[[i]])
    if (nrow(members) == 0) {
      return(NULL)
    }
    # Padding shrinks with depth so nested boxes sit inside their parent. Floored
    # so a deeply nested group still clears its own nodes.
    px <- max(pad_x - inset * depth[i], inset)
    py <- max(pad_y - inset * depth[i], inset)
    tibble::tibble(
      group_key = groups$group_key[i],
      label = groups$label[i],
      xmin = min(members$x) - px,
      xmax = max(members$x) + px,
      ymin = min(members$y) - py,
      ymax = max(members$y) + py + label_margin,
      depth = depth[i]
    )
  })

  out <- purrr::list_rbind(Filter(Negate(is.null), out))
  if (nrow(out) == 0) {
    return(out)
  }
  # Shallowest first, so a nested box draws over its parent rather than under.
  out |> dplyr::arrange(.data$depth)
}

#' Group Box Layers for the AEP
#'
#' Rounded rectangles with a label at the top left. Dashed grey outlines and no
#' fill, so the boxes read as annotation and never compete with the nodes or
#' the edges, which carry the actual content -- "quiet" rather than invisible;
#' see the 2026-08-07 note below on getting that balance wrong first.
#'
#' **The label sits INSIDE the box, not floating above it.** Before
#' 2026-08-07 it was drawn at `y = ymax` with `vjust = -0.5`, which pushes the
#' rendered text upward past `ymax` in device space. ggplot2's automatic axis
#' ranging only sees the DATA coordinate the text is anchored to, never the
#' rendered glyph extent, so nothing told the panel to leave room for that
#' overhang -- Sam saw "Cod" clipped to its top half, and a nested box gets the
#' least headroom of all since its own padding is already smaller by
#' construction (`aep_group_boxes()`'s depth-based inset). Anchoring inside the
#' box's own rectangle instead means the label is guaranteed visible: the rect
#' itself always contributes to the axis range because it's real plotted data,
#' not an annotation-only offset.
#'
#' @param boxes Output of [aep_group_boxes()].
#' @return A list of ggplot2 layers, possibly empty.
#' @export
aep_group_layers <- function(boxes) {
  if (nrow(boxes) == 0) {
    return(list())
  }
  list(
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$ymin, ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = NA,
      # Darker and a touch heavier than the 2026-08-05 original (grey60,
      # linewidth 0.4), and a longer dash ("42" vs "22"): Sam 2026-08-07,
      # "both the vertical and horizontal sides of the box are still (largely)
      # invisible". Still meant to read as annotation, just no longer at a
      # weight that disappears against white at figure scale.
      colour = "grey35",
      linetype = "42",
      linewidth = 0.6
    ),
    ggplot2::geom_text(
      data = boxes,
      ggplot2::aes(x = .data$xmin, y = .data$ymax, label = .data$label),
      inherit.aes = FALSE,
      # Small positive hjust/vjust, not negative: pulls the label IN from the
      # top-left corner rather than pushing it out past the box. See the
      # function doc above for why "outside the box" was the actual bug.
      hjust = 0.05,
      vjust = 1.4,
      size = 2.8,
      fontface = "italic",
      colour = "grey45"
    )
  )
}
