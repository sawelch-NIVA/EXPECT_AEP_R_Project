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
  path = here_rel("data/clean/aep_node_groups.csv"),
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
#' @return A tibble of `group_key`, `label`, `xmin`, `xmax`, `ymin`, `ymax`,
#'   `depth`.
#' @export
aep_group_boxes <- function(groups, nodes, pad = 0.42, inset = 0.13) {
  if (nrow(groups) == 0) {
    return(tibble::tibble(
      group_key = character(0), label = character(0),
      xmin = numeric(0), xmax = numeric(0),
      ymin = numeric(0), ymax = numeric(0), depth = integer(0)
    ))
  }

  depth <- aep_group_depth(groups)
  placed <- nodes |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))

  out <- lapply(seq_len(nrow(groups)), function(i) {
    members <- placed |> dplyr::filter(.data$node_id %in% groups$members[[i]])
    if (nrow(members) == 0) {
      return(NULL)
    }
    # Padding shrinks with depth so nested boxes sit inside their parent. Floored
    # so a deeply nested group still clears its own nodes.
    p <- max(pad - inset * depth[i], inset)
    tibble::tibble(
      group_key = groups$group_key[i],
      label = groups$label[i],
      xmin = min(members$x) - p,
      xmax = max(members$x) + p,
      ymin = min(members$y) - p,
      ymax = max(members$y) + p,
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
#' Rounded rectangles with a label at the top left. Deliberately quiet: dashed
#' grey outlines and no fill, so the boxes read as annotation and never compete
#' with the nodes or the edges, which carry the actual content.
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
      colour = "grey60",
      linetype = "22",
      linewidth = 0.4
    ),
    ggplot2::geom_text(
      data = boxes,
      ggplot2::aes(x = .data$xmin, y = .data$ymax, label = .data$label),
      inherit.aes = FALSE,
      hjust = -0.08,
      vjust = -0.5,
      size = 2.8,
      fontface = "italic",
      colour = "grey45"
    )
  )
}
