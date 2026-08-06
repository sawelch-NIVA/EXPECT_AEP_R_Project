# The multi-AEP layer (PLAN.md P5.3). Added 2026-08-06.
#
# WHY AEPs ARE VIEWS OVER ONE POOL OF NODES, NOT SEPARATE NODE SETS.
#
# PLAN.md P5.3 asks for "3-5 focused system AEPs plus one holistic low-detail
# AEP". The obvious implementation is an `aep_id` column on aep_nodes.csv, one
# row per node per AEP. It was rejected, and the reason is arithmetic rather than
# taste.
#
# A node carries four EPEQ scores and four written justifications. Seven nodes
# and sixteen edges is roughly ninety written judgements, and PLAN.md 9f is
# explicit that those judgements are Sam's and are the thing the deadline is
# actually bought with. Copying a node row per AEP multiplies that by the number
# of AEPs, and it multiplies it in the worst possible way: the copies start
# identical, then one gets revised and the others silently do not.
#
# So a node is defined ONCE. What varies per AEP is:
#
#   * WHICH nodes are in it        -- aep_membership.csv
#   * WHERE each node sits         -- x/y on aep_membership.csv, because a
#                                     position is a property of a node in a
#                                     particular diagram, not of the node
#   * WHAT DATA it may draw on     -- the scope columns on aep_manifest.csv
#
# The third is the one that makes this honest rather than merely convenient. The
# Repparfjorden AEP's "Benthic sediment" is the same scientific claim as the
# national one, so essentiality and plausibility genuinely do carry over. But its
# n is 806 rather than 25,024, so its card, its distribution and its geometric
# mean are all recomputed under the AEP's bounding box.
#
# EPEQ SPLITS DOWN THE MIDDLE, and the split is semantic rather than a
# convenience (Sam, 2026-08-06: "the evidence in repparfjorden for such and such
# is clearly much weaker than the overall national AEP").
#
#   * ESSENTIALITY and PLAUSIBILITY are claims about the WORLD. "Copper is
#     present in highly-conserved metabolic systems" is as true in Repparfjorden
#     as nationally. They live on aep_nodes.csv and are written once.
#
#   * EVIDENCE and QUANTIFICATION are claims about the DATASET, and the dataset
#     is exactly what an AEP scope changes. N001 is 25,024 measurements from two
#     references nationally and 806 from one inside the box; the same evidence
#     score cannot honestly describe both. They live on aep_membership.csv,
#     which is already keyed by AEP and node. See aep_scoped_epeq_cols().
#
# Blank inherits from the node, so an AEP that does not restrict anything needs
# no entries at all, and the pre-existing single-AEP files keep working.
#
# This is NOT a general per-AEP override table, which was rejected and stays
# rejected: an arbitrary exceptions table reintroduces the drift problem through
# the side door. Two named criteria, split for a stated reason, is a model of the
# thing rather than an escape hatch.
#
# SPLIT OF AUTHORITY is unchanged from the rest of this layer: the pipeline
# READS these files and never writes them.

#' An Empty AEP Manifest
#'
#' One row per AEP. The scope columns are a **bounding box**, not a latitude band
#' (Sam's call 2026-08-06, after the alternative was measured): 70-71 degrees
#' north with no longitude bound is 3,371 measurements from four references
#' spanning the whole top of Norway, whereas the box in `NBXX-reparfjorden.qmd`
#' is 1,197 measurements that are actually near Repparfjorden. The extra two
#' columns are the difference between a Repparfjorden AEP and a northern-Norway
#' one.
#'
#' Every scope column is optional. All blank means no restriction, which is what
#' the national AEP wants.
#'
#' @return A zero-row tibble.
#' @export
empty_aep_manifest <- function() {
  tibble::tibble(
    aep_id = character(0),
    label = character(0),
    scope_note = character(0),
    lat_min = numeric(0),
    lat_max = numeric(0),
    lon_min = numeric(0),
    lon_max = numeric(0),
    date_min = as.Date(character(0)),
    date_max = as.Date(character(0)),
    notes = character(0)
  )
}

#' Read and Validate the AEP Manifest
#'
#' @param path Where the CSV lives.
#' @return A tibble, one row per AEP.
#' @export
read_aep_manifest <- function(path = here_rel("data/clean/aep/aep_manifest.csv")) {
  if (!file.exists(path)) {
    stop(
      "No AEP manifest at ", path,
      ". Run scripts/scaffold_aep_manifest.R first."
    )
  }
  manifest <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      aep_id = readr::col_character(),
      label = readr::col_character(),
      scope_note = readr::col_character(),
      notes = readr::col_character(),
      # Same reasoning as read_aep_nodes(): a bare year guessed as a number and
      # compared against a Date reads as days since 1970 and empties the AEP.
      date_min = readr::col_character(),
      date_max = readr::col_character()
    )
  )

  missing <- setdiff(names(empty_aep_manifest()), names(manifest))
  if (length(missing) > 0) {
    stop("AEP manifest is missing column(s): ", paste(missing, collapse = ", "))
  }

  manifest$date_min <- parse_node_date(manifest$date_min, "min")
  manifest$date_max <- parse_node_date(manifest$date_max, "max")

  dup <- manifest$aep_id[duplicated(manifest$aep_id)]
  if (length(dup) > 0) {
    stop("Duplicate aep_id(s): ", paste(unique(dup), collapse = ", "))
  }

  blank <- is.na(manifest$aep_id) | !nzchar(trimws(manifest$aep_id))
  if (any(blank)) {
    stop(sum(blank), " manifest row(s) have no aep_id.")
  }

  # An inverted bound empties an AEP silently, exactly as an inverted date bound
  # emptied a node. Same failure, same treatment.
  for (pair in list(
    c("lat_min", "lat_max"), c("lon_min", "lon_max"), c("date_min", "date_max")
  )) {
    lo <- manifest[[pair[1]]]
    hi <- manifest[[pair[2]]]
    bad <- !is.na(lo) & !is.na(hi) & lo > hi
    if (any(bad)) {
      stop(
        sum(bad), " AEP(s) have ", pair[1], " above ", pair[2], ": ",
        paste(sQuote(manifest$aep_id[bad]), collapse = ", ")
      )
    }
  }

  manifest
}

#' An Empty AEP Membership Table
#'
#' `x` and `y` live here rather than on `aep_nodes.csv` because **a position is
#' a property of a node within one diagram**. The same sediment node sits in the
#' middle column of the national AEP and wherever the Repparfjorden layout wants
#' it. Blank falls back to the node's own `x`/`y`, so the single-AEP files that
#' predate this layer keep working unchanged.
#'
#' @return A zero-row tibble.
#' @export
empty_aep_membership <- function() {
  tibble::tibble(
    aep_id = character(0),
    node_id = character(0),
    x = numeric(0),
    y = numeric(0),
    evidence_score = numeric(0),
    evidence_justification = character(0),
    quantification_score = numeric(0),
    quantification_justification = character(0),
    notes = character(0)
  )
}

#' The EPEQ Criteria That Belong to an AEP Rather Than a Node
#'
#' Evidence and quantification are statements about the data available, and an
#' AEP scope changes what data is available. Essentiality and plausibility are
#' statements about the world and stay on the node. See the header of this file.
#'
#' @return A character vector of column names, scores and justifications
#'   interleaved, in the same shape as [epeq_cols()].
#' @export
aep_scoped_epeq_cols <- function() {
  c(
    "evidence_score", "evidence_justification",
    "quantification_score", "quantification_justification"
  )
}

#' Read and Validate the AEP Membership File
#'
#' @param path Where the CSV lives.
#' @param nodes Optional nodes table, to check every `node_id` exists.
#' @param manifest Optional manifest, to check every `aep_id` exists.
#' @return A tibble of `aep_id`, `node_id`, `x`, `y`, `notes`.
#' @export
read_aep_membership <- function(
  path = here_rel("data/clean/aep/aep_membership.csv"),
  nodes = NULL,
  manifest = NULL
) {
  if (!file.exists(path)) {
    stop(
      "No AEP membership file at ", path,
      ". Run scripts/scaffold_aep_manifest.R first."
    )
  }
  # Everything as text, then x/y coerced below. Naming parsers for columns the
  # file may legitimately not have yet warns ("named parsers don't match the
  # column names"), and this file is hand-edited and routinely half-typed.
  # Guessing is no better: an all-blank x column guesses as logical.
  membership <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  missing <- setdiff(c("aep_id", "node_id"), names(membership))
  if (length(missing) > 0) {
    stop(
      "AEP membership file is missing column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  for (col in c("x", "y")) {
    if (!col %in% names(membership)) {
      membership[[col]] <- NA_real_
    } else {
      num <- suppressWarnings(as.numeric(membership[[col]]))
      bad <- !is.na(membership[[col]]) & is.na(num)
      if (any(bad)) {
        stop(
          sum(bad), " membership row(s) have a non-numeric ", col, ": ",
          paste(sQuote(membership[[col]][bad]), collapse = ", ")
        )
      }
      membership[[col]] <- num
    }
  }
  if (!"notes" %in% names(membership)) {
    membership$notes <- NA_character_
  }

  # The two AEP-scoped EPEQ criteria. Absent columns default to blank, which
  # means "inherit from the node", so a membership file written before this
  # split still reads.
  scored <- aep_scoped_epeq_cols()
  for (col in scored[c(FALSE, TRUE)]) {
    if (!col %in% names(membership)) {
      membership[[col]] <- NA_character_
    }
  }
  for (col in scored[c(TRUE, FALSE)]) {
    if (!col %in% names(membership)) {
      membership[[col]] <- NA_real_
      next
    }
    v <- suppressWarnings(as.numeric(membership[[col]]))
    unparseable <- !is.na(membership[[col]]) & is.na(v)
    if (any(unparseable)) {
      stop(
        sum(unparseable), " membership row(s) have a non-numeric ", col, ": ",
        paste(sQuote(membership[[col]][unparseable]), collapse = ", ")
      )
    }
    # Same range check as read_aep_nodes(). A typo that survives into a figure
    # is indistinguishable from a judgement.
    bad <- !is.na(v) & !(v %in% 1:3)
    if (any(bad)) {
      stop(
        sum(bad), " membership row(s) have an out-of-range ", col,
        ": scores are 1, 2 or 3, or blank to inherit from the node."
      )
    }
    membership[[col]] <- v
  }

  # A score with no justification is the failure validate_aep_edges() already
  # guards against for edges: it asserts a judgement without saying on what.
  # Warned rather than refused, because filling the file is a work in progress.
  for (i in seq(1, length(scored), by = 2)) {
    s <- membership[[scored[i]]]
    j <- membership[[scored[i + 1]]]
    bare <- !is.na(s) & (is.na(j) | !nzchar(trimws(j)))
    if (any(bare)) {
      cli::cli_warn(
        "{sum(bare)} membership row{?s} set {.field {scored[i]}} with no \\
         {.field {scored[i + 1]}}: {.val {paste(membership$aep_id[bare],
         membership$node_id[bare], sep = '/')}}."
      )
    }
  }

  dup <- membership |>
    dplyr::count(.data$aep_id, .data$node_id) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop(
      "Duplicate membership row(s): ",
      paste(dup$aep_id, dup$node_id, sep = "/", collapse = ", ")
    )
  }

  if (!is.null(nodes)) {
    unknown <- setdiff(membership$node_id, nodes$node_id)
    if (length(unknown) > 0) {
      stop(
        "AEP membership names ", length(unknown), " unknown node_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", ")
      )
    }
  }
  if (!is.null(manifest)) {
    unknown <- setdiff(membership$aep_id, manifest$aep_id)
    if (length(unknown) > 0) {
      stop(
        "AEP membership names ", length(unknown), " unknown aep_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", "),
        ". Add them to data/clean/aep/aep_manifest.csv."
      )
    }
    # An AEP in the manifest with no members produces a blank figure. Worth a
    # warning rather than an error, because that is the normal state for a few
    # minutes while a new AEP is being written.
    orphan <- setdiff(manifest$aep_id, membership$aep_id)
    if (length(orphan) > 0) {
      cli::cli_warn(
        "AEP{?s} with no member nodes: {.val {orphan}}."
      )
    }
  }

  membership
}

#' Narrow the Node Pool to One AEP
#'
#' Selects that AEP's nodes, takes their layout coordinates from the membership
#' file, and **intersects** the AEP's scope with each node's own restrictions.
#'
#' Intersection, not replacement, and the direction matters. A node restriction
#' is a statement about the node ("only Arctic sites of this group count as this
#' node"); an AEP scope is a statement about the whole diagram ("this AEP is
#' about Repparfjorden"). Both must hold, so the bounds narrow to the tighter of
#' the two. Replacing either would let an AEP quietly widen a node past the limit
#' its own row asserts.
#'
#' The result is an ordinary nodes table, which is the point: every function
#' downstream of here ([aep_node_report_cards()], [write_node_cards()],
#' [plot_aep()]) is unchanged and does not know that AEPs exist.
#'
#' @param nodes The nodes table from [read_aep_nodes()].
#' @param membership The table from [read_aep_membership()].
#' @param manifest The table from [read_aep_manifest()].
#' @param aep_id Which AEP to scope to.
#' @return A nodes tibble, restricted and repositioned.
#' @export
aep_scope_nodes <- function(nodes, membership, manifest, aep_id) {
  # Manifest lookup FIRST. An unknown aep_id also has no membership rows, so
  # checking after the zero-row shortcut below would return an empty AEP for a
  # typo instead of refusing it.
  scope <- manifest[manifest$aep_id %in% aep_id, , drop = FALSE]
  if (nrow(scope) == 0) {
    stop("No manifest row for aep_id ", sQuote(aep_id), ".")
  }

  mine <- membership[membership$aep_id %in% aep_id, , drop = FALSE]
  out <- nodes[nodes$node_id %in% mine$node_id, , drop = FALSE]
  if (nrow(out) == 0) {
    # Still needs the longitude columns, or resolve_node_data() sees a table
    # shaped differently from every other AEP's.
    out$lon_min <- numeric(0)
    out$lon_max <- numeric(0)
    return(out)
  }

  # Layout and the AEP-scoped scores both come from the membership row, in the
  # same aligned order, falling back to the node where blank.
  row <- mine[match(out$node_id, mine$node_id), , drop = FALSE]
  out$x <- dplyr::coalesce(row$x, out$x)
  out$y <- dplyr::coalesce(row$y, out$y)

  # EVIDENCE AND QUANTIFICATION ARE THE AEP'S, NOT THE NODE'S. See the header.
  # coalesce() rather than an assignment, so a blank cell inherits and A001 needs
  # no entries at all.
  for (col in aep_scoped_epeq_cols()) {
    if (col %in% names(row) && col %in% names(out)) {
      out[[col]] <- dplyr::coalesce(row[[col]], out[[col]])
    }
  }

  # Longitude bounds exist ONLY at AEP level. Nodes carry lat_min/lat_max
  # already, and adding two more hand-edited columns to aep_nodes.csv to
  # duplicate a restriction the manifest expresses better is churn on a file
  # being edited by hand right now. resolve_node_data() reads these if present
  # and ignores them if not.
  out$lon_min <- scope$lon_min[1]
  out$lon_max <- scope$lon_max[1]

  # NOT ifelse(). It drops attributes, so a Date column came back as a bare
  # number of days and resolve_node_data() rejected it on the spot. That refusal
  # is itself a guard added after a bare year silently emptied every node
  # (see parse_node_date()), and it earned its keep within the hour.
  # coalesce() and pmax()/pmin() both preserve the class.
  tighter <- function(node_col, scope_val, f) {
    if (is.na(scope_val)) {
      return(node_col)
    }
    f(dplyr::coalesce(node_col, scope_val), scope_val)
  }
  out$lat_min <- tighter(out$lat_min, scope$lat_min[1], pmax)
  out$lat_max <- tighter(out$lat_max, scope$lat_max[1], pmin)
  out$date_min <- tighter(out$date_min, scope$date_min[1], pmax)
  out$date_max <- tighter(out$date_max, scope$date_max[1], pmin)

  out
}

#' Narrow the Edge Set to One AEP
#'
#' **An edge is drawn where both of its endpoints are in the AEP.** There is no
#' `aep_id` column on `aep_edges.csv`, deliberately: an edge is a claim about two
#' nodes, so which diagrams it belongs on follows from which diagrams they are
#' on. A column would be a second place to say the same thing, and the two would
#' disagree the first time a node was dropped from an AEP.
#'
#' @param edges The edges table.
#' @param scoped_nodes Output of [aep_scope_nodes()].
#' @return The edges whose `from` and `to` are both present.
#' @export
aep_scope_edges <- function(edges, scoped_nodes) {
  edges[
    edges$from %in% scoped_nodes$node_id & edges$to %in% scoped_nodes$node_id, ,
    drop = FALSE
  ]
}

#' Narrow the Grouping Boxes to One AEP
#'
#' A group's member list is intersected with the AEP's nodes. A box that ends up
#' with fewer than two nodes is dropped: a box around one node is not a grouping,
#' it is a second border on a card.
#'
#' @param groups Output of [read_aep_node_groups()].
#' @param scoped_nodes Output of [aep_scope_nodes()].
#' @return The groups still worth drawing, with `members` intersected.
#' @export
aep_scope_groups <- function(groups, scoped_nodes) {
  if (is.null(groups) || nrow(groups) == 0) {
    return(groups)
  }
  groups$members <- lapply(
    groups$members, function(m) intersect(m, scoped_nodes$node_id)
  )
  groups[lengths(groups$members) >= 2, , drop = FALSE]
}

#' Every AEP, Scoped
#'
#' The one place the loop over AEPs lives, so the card targets, the compact-card
#' targets and the diagram target cannot disagree about what an AEP contains.
#'
#' @param nodes,membership,manifest As above.
#' @return A named list of nodes tibbles, one per `aep_id`.
#' @export
aep_scoped_nodes <- function(nodes, membership, manifest) {
  stats::setNames(
    lapply(
      manifest$aep_id,
      function(id) aep_scope_nodes(nodes, membership, manifest, id)
    ),
    manifest$aep_id
  )
}

# ---------------------------------------------------------------------------
# Drivers: one loop over AEPs each, so a target never writes its own.
#
# NO tar_map HERE, and that is a considered choice rather than an oversight.
# tar_map needs its `values` at pipeline DEFINITION time, and the AEP list lives
# in a file the pipeline reads, which is exactly the data-dependent static
# branching that CLAUDE.md 2.3 documents as a three-pass build. Four or five AEPs
# at roughly 45 seconds of card rendering each is under four minutes to rebuild
# the lot. Paying four minutes to avoid a three-pass build is the right trade at
# this point in the calendar.
# ---------------------------------------------------------------------------

#' Report Cards for Every Node of Every AEP
#'
#' @param scoped Output of [aep_scoped_nodes()].
#' @param members,data,ids As in [aep_node_report_cards()].
#' @return A tibble of cards with a leading `aep_id` column.
#' @export
aep_all_report_cards <- function(scoped, members, data, ids) {
  purrr::list_rbind(purrr::imap(scoped, function(nodes, id) {
    cards <- aep_node_report_cards(nodes, members, data, ids)
    validate_aep_nodes(nodes, members, cards)
    # A node in two AEPs produces two cards with the same node_id and different
    # numbers, so aep_id is part of the key from here down.
    dplyr::bind_cols(tibble::tibble(aep_id = rep(id, nrow(cards))), cards)
  }))
}

#' Write Every AEP's Node Cards
#'
#' One subdirectory per AEP, because a node appears in several AEPs with
#' different data behind it and so cannot share a filename across them.
#'
#' @param scoped Output of [aep_scoped_nodes()].
#' @param cards Output of [aep_all_report_cards()].
#' @param members,data,ids,thresholds As in [write_node_cards()].
#' @param dir Parent directory; each AEP gets a subdirectory of it.
#' @param limits Shared limits per unit, computed across the whole node pool.
#' @param ... Passed to [write_node_cards()] (`width`, `height`, `dpi`, `style`).
#' @return The written paths, across all AEPs.
#' @export
write_aep_node_cards <- function(
  scoped, cards, members, data, ids,
  thresholds = NULL,
  dir = here_rel("figures/node_cards"),
  limits = NULL,
  ...
) {
  unlist(purrr::imap(scoped, function(nodes, id) {
    write_node_cards(
      nodes = nodes,
      cards = cards[cards$aep_id %in% id, , drop = FALSE],
      members = members,
      data = data,
      ids = ids,
      thresholds = thresholds,
      dir = file.path(dir, id),
      limits = limits,
      ...
    )
  }), use.names = FALSE)
}

#' Draw and Write Every AEP
#'
#' @param scoped Output of [aep_scoped_nodes()].
#' @param edges The full edges table; narrowed per AEP by [aep_scope_edges()].
#' @param cards Output of [aep_all_report_cards()].
#' @param groups Output of [read_aep_node_groups()], or `NULL`.
#' @param card_paths Compact card paths from [write_aep_node_cards()].
#' @param dir Where the figures go.
#' @param width,height,dpi Canvas, passed on to [node_card_extent()] via
#'   `plot_aep()` so arrow clipping matches the device actually used.
#' @param image_size Card width as a fraction of panel width.
#' @param card_aspect Compact card height over width, in inches.
#' @return The written paths, one per AEP.
#' @export
write_aep_diagrams <- function(
  scoped, edges, cards, groups = NULL, card_paths = character(0),
  dir = here_rel("figures"),
  width = 12, height = 8, dpi = 150,
  image_size = 0.19, card_aspect = 1.8 / 2.4
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  unlist(purrr::imap(scoped, function(nodes, id) {
    # Cards for THIS AEP only. They live in figures/<style dir>/<aep_id>/, so
    # the directory name is the key; matching on basename alone would collide
    # across AEPs, since every AEP names its cards N001.png and so on.
    mine <- card_paths[basename(dirname(card_paths)) == id]
    images <- stats::setNames(
      mine, tools::file_path_sans_ext(basename(mine))
    )

    path <- file.path(dir, paste0("aep_", id, ".png"))
    ggplot2::ggsave(
      filename = path,
      plot = plot_aep(
        nodes,
        aep_scope_edges(edges, nodes),
        cards[cards$aep_id %in% id, , drop = FALSE],
        groups = aep_scope_groups(groups, nodes),
        node_images = images,
        image_size = image_size,
        card_aspect = card_aspect,
        device_aspect = width / height
      ),
      width = width, height = height, dpi = dpi, device = ragg::agg_png
    )
    path
  }), use.names = FALSE)
}
