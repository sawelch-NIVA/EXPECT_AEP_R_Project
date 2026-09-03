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
#   * WHICH nodes are in it        -- aep_membership_<aep_id>.csv (one flat file
#                                     per AEP since 2026-08-27)
#   * WHERE each node sits         -- x/y on that file, because a position is a
#                                     property of a node in a particular
#                                     diagram, not of the node
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
#     score cannot honestly describe both. They live on the per-AEP membership
#     file, which is already keyed by AEP and node. See aep_scoped_epeq_cols().
#
# Blank inherits from the node, so an AEP that does not restrict anything needs
# no entries at all, and a membership file that predates this split still reads.
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
read_aep_manifest <- function(
  path = here_rel("data/clean/aep/aep_manifest.csv")
) {
  if (!file.exists(path)) {
    stop(
      "No AEP manifest at ",
      path,
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
    c("lat_min", "lat_max"),
    c("lon_min", "lon_max"),
    c("date_min", "date_max")
  )) {
    lo <- manifest[[pair[1]]]
    hi <- manifest[[pair[2]]]
    bad <- !is.na(lo) & !is.na(hi) & lo > hi
    if (any(bad)) {
      stop(
        sum(bad),
        " AEP(s) have ",
        pair[1],
        " above ",
        pair[2],
        ": ",
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
#' `geo_scope` (added 2026-09-02) is how a node's data footprint relates to the
#' AEP's own scope. Blank or `local` (the default) clips the node to the AEP's
#' bounding box, as before this column existed. `arctic` marks the node a
#' **regional proxy**: its footprint is *replaced*, not intersected, with
#' `LATITUDE >= arctic_circle_lat()` and no longitude bound. It exists for a
#' compartment where the AEP's own box holds too little to score, e.g. river
#' sediment inside one fjord's box. See [aep_scope_nodes()].
#'
#' @return A zero-row tibble.
#' @export
empty_aep_membership <- function() {
  tibble::tibble(
    aep_id = character(0),
    node_id = character(0),
    x = numeric(0),
    y = numeric(0),
    geo_scope = character(0),
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
    "evidence_score",
    "evidence_justification",
    "quantification_score",
    "quantification_justification"
  )
}

#' Permitted `geo_scope` Values on the Membership File
#'
#' `local` (also the meaning of a blank cell) clips a node to the AEP's
#' bounding box. `arctic` replaces that box with "north of the Arctic Circle".
#' See [empty_aep_membership()] and [aep_scope_nodes()].
#'
#' @return A character vector.
#' @export
aep_geo_scope_levels <- function() {
  c("local", "arctic")
}

#' Read and Validate the AEP Membership Files
#'
#' **One flat file per AEP** since 2026-08-27: `aep_membership_<aep_id>.csv` in
#' `data/clean/aep/`. Each AEP's membership is then a self-contained diff, and
#' editing one AEP cannot touch another's rows. The files are read, row-bound,
#' and validated exactly as the single combined file was.
#'
#' Each file must contain exactly one `aep_id`, and it must match the filename
#' suffix. A mis-named or mis-pasted file is a build failure here rather than a
#' silent re-target.
#'
#' @param paths Character vector of `aep_membership_*.csv` paths. A single
#'   directory is expanded to every matching file it holds. `NULL` (the
#'   default) globs `dir`.
#' @param nodes Optional nodes table, to check every `node_id` exists.
#' @param manifest Optional manifest, to check every `aep_id` exists.
#' @param dir Directory the default glob runs in.
#' @return A tibble of `aep_id`, `node_id`, `x`, `y`, `notes`, one row per node
#'   per AEP.
#' @export
read_aep_membership <- function(
  paths = NULL,
  nodes = NULL,
  manifest = NULL,
  dir = here_rel("data/clean/aep")
) {
  membership_glob <- function(d) {
    sort(list.files(
      d,
      pattern = "^aep_membership_.*\\.csv$",
      full.names = TRUE
    ))
  }

  if (is.null(paths)) {
    paths <- membership_glob(dir)
  } else if (length(paths) == 1 && dir.exists(paths)) {
    paths <- membership_glob(paths)
  }

  if (length(paths) == 0) {
    stop(
      "No AEP membership files (aep_membership_*.csv) in ",
      dir,
      ". Write them by hand from the schema in empty_aep_membership()."
    )
  }

  # Everything as text, then x/y coerced below. Naming parsers for columns the
  # file may legitimately not have yet warns ("named parsers don't match the
  # column names"), and this file is hand-edited and routinely half-typed.
  # Guessing is no better: an all-blank x column guesses as logical.
  per_file <- lapply(paths, function(p) {
    if (!file.exists(p)) {
      stop("No AEP membership file at ", p, ".")
    }
    mem <- readr::read_csv(
      p,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )

    # A brand-new AEP's file may exist with only a header for a few minutes.
    # The orphan-AEP warning below is the right channel for "no members yet",
    # so skip the one-aep_id assertion rather than abort on an empty file.
    if (nrow(mem) == 0) {
      return(mem)
    }

    # The filename/aep_id agreement check only means something when the file is
    # actually named aep_membership_<id>.csv (which the pipeline glob guarantees).
    # A path that does not follow the convention is not making the claim, so it
    # is not checked -- this is what lets tests and ad-hoc callers pass an
    # arbitrary path.
    if (grepl("^aep_membership_.+\\.csv$", basename(p))) {
      want <- sub("^aep_membership_(.+)\\.csv$", "\\1", basename(p))
      got <- unique(mem$aep_id[!is.na(mem$aep_id) & nzchar(trimws(mem$aep_id))])
      if (length(got) != 1L || !identical(got, want)) {
        cli::cli_abort(c(
          "{.file {basename(p)}} must hold exactly one {.field aep_id}, \\
           {.val {want}}.",
          "i" = "Found: {.val {got}}."
        ))
      }
    }
    mem
  })

  membership <- dplyr::bind_rows(per_file)

  missing <- setdiff(c("aep_id", "node_id"), names(membership))
  if (length(missing) > 0) {
    stop(
      "AEP membership files are missing column(s): ",
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
          sum(bad),
          " membership row(s) have a non-numeric ",
          col,
          ": ",
          paste(sQuote(membership[[col]][bad]), collapse = ", ")
        )
      }
      membership[[col]] <- num
    }
  }
  if (!"notes" %in% names(membership)) {
    membership$notes <- NA_character_
  }

  # geo_scope: blank / "local" (default) clips to the AEP box; "arctic" replaces
  # it with LATITUDE >= arctic_circle_lat(). Absent on files that predate this
  # column, which read as all-local. An unrecognised value is a build failure
  # rather than a silent fall-through to "local", same treatment as an unknown
  # decision or an inverted bound.
  if (!"geo_scope" %in% names(membership)) {
    membership$geo_scope <- NA_character_
  }
  gs <- trimws(membership$geo_scope)
  gs[!is.na(gs) & !nzchar(gs)] <- NA_character_
  bad_scope <- !is.na(gs) & !gs %in% aep_geo_scope_levels()
  if (any(bad_scope)) {
    stop(
      sum(bad_scope), " membership row(s) have an unrecognised geo_scope: ",
      paste(sQuote(unique(gs[bad_scope])), collapse = ", "),
      ". Permitted: ", paste(aep_geo_scope_levels(), collapse = ", "),
      ", or blank for local."
    )
  }
  membership$geo_scope <- gs

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
        sum(unparseable),
        " membership row(s) have a non-numeric ",
        col,
        ": ",
        paste(sQuote(membership[[col]][unparseable]), collapse = ", ")
      )
    }
    # Same range check as read_aep_nodes(). A typo that survives into a figure
    # is indistinguishable from a judgement.
    bad <- !is.na(v) & !(v %in% 1:3)
    if (any(bad)) {
      stop(
        sum(bad),
        " membership row(s) have an out-of-range ",
        col,
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
        "AEP membership names ",
        length(unknown),
        " unknown node_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", ")
      )
    }
  }
  if (!is.null(manifest)) {
    unknown <- setdiff(membership$aep_id, manifest$aep_id)
    if (length(unknown) > 0) {
      stop(
        "AEP membership names ",
        length(unknown),
        " unknown aep_id(s): ",
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
#' **The one exception is `geo_scope = "arctic"` on the membership row**, which
#' deliberately replaces the AEP's box for that node with `LATITUDE >=`
#' [arctic_circle_lat()] and no longitude bound. It is opt-in per node per AEP,
#' for a compartment the AEP's own footprint cannot support (river sediment
#' inside a single fjord). Date scope is still intersected; the swap is spatial.
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
    # shaped differently from every other AEP's. geo_scope likewise, so the card
    # renderer can read it off any scoped node table.
    out$lon_min <- numeric(0)
    out$lon_max <- numeric(0)
    out$geo_scope <- character(0)
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

  # Carried onto the scoped node so the card renderer can mark it (a pin for a
  # box-specific node, a globe for a regional one). Read here rather than
  # recomputed downstream: this is the one place that knows the membership row.
  # `%in% names()` guarded, same as the EPEQ columns above: read_aep_membership()
  # always supplies geo_scope, but a hand-built membership tibble in a test need
  # not, and `$` on an absent tibble column warns.
  out$geo_scope <- if ("geo_scope" %in% names(row)) {
    row$geo_scope
  } else {
    NA_character_
  }

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

  # geo_scope = "arctic" is the one place a node's footprint is REPLACED rather
  # than intersected. The node is a regional proxy: drop the AEP's longitude box
  # entirely, and pull its latitude box down to a floor of arctic_circle_lat().
  # Date scope still applies, the substitution is spatial only. Runs last so it
  # overrides the intersection above for those rows and nothing else touches
  # them afterwards. See read_aep_membership() and the header of this file.
  if ("geo_scope" %in% names(row)) {
    arctic <- !is.na(row$geo_scope) & row$geo_scope == "arctic"
    if (any(arctic)) {
      out$lon_min[arctic] <- NA_real_
      out$lon_max[arctic] <- NA_real_
      out$lat_min[arctic] <- arctic_circle_lat()
      out$lat_max[arctic] <- NA_real_
    }
  }

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
    edges$from %in% scoped_nodes$node_id & edges$to %in% scoped_nodes$node_id,
    ,
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
    groups$members,
    function(m) intersect(m, scoped_nodes$node_id)
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
#' @param ... Passed to [write_node_cards()] (`width`, `height`, `dpi`,
#'   `external_series`). The same `external_series` reaches every AEP's call
#'   unchanged: it is keyed by `node_id`, not by AEP, so a REACH node's series
#'   is the same series regardless of which AEP happens to include that node.
#' @return The written paths, across all AEPs.
#' @export
write_aep_node_cards <- function(
  scoped,
  cards,
  members,
  data,
  ids,
  thresholds = NULL,
  dir = here_rel("images/node_cards"),
  limits = NULL,
  ...
) {
  unlist(
    purrr::imap(scoped, function(nodes, id) {
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
    }),
    use.names = FALSE
  )
}

#' Squeeze Compensation for a Diagram That May Carry a Locator Inset
#'
#' An AEP with a bounding box gets `+ inset + patchwork::plot_layout(widths =
#' c(1, inset_width))` appended in [write_aep_diagrams()], which squeezes the
#' diagram's own panel down to `1 / (1 + inset_width)` of the canvas width.
#' `ggimage::geom_image()` sizes a card as a fraction of WHATEVER panel it
#' actually lands in, so calling `plot_aep()` with the nominal canvas width
#' before knowing an inset is coming undersizes every card on an inset AEP
#' relative to a plain one -- which is exactly what Sam noticed 2026-08-08
#' comparing A001 (no bounding box) against A002 (Repparfjorden, boxed):
#' "AEPs 1 and 2 use different size rectangles."
#'
#' Pulled out as its own function, pure arithmetic with no plotting or file
#' I/O, so the compensation itself can be tested directly rather than only by
#' rendering and measuring pixels.
#'
#' @param draw_inset Will an inset actually be drawn for this AEP? (Already
#'   resolved by the caller: has a bounding box AND a `bbox_map` was supplied.)
#' @param width,height Nominal canvas size in inches.
#' @param image_size Card width as a fraction of panel width, before
#'   compensation.
#' @param inset_width Width of the locator inset relative to the main panel.
#' @return A list: `image_size` (compensated), `device_aspect` (the TRUE
#'   effective width over height, for [plot_aep()]) and `effective_width`
#'   (inches, for [aep_diagram_height()] -- computing that from `device_aspect
#'   * height` again at the call site would just re-derive this number by a
#'   different route).
#' @export
aep_diagram_squeeze <- function(
  draw_inset, width, height, image_size, inset_width
) {
  squeeze <- if (isTRUE(draw_inset)) 1 + inset_width else 1
  effective_width <- width / squeeze
  list(
    image_size = image_size * squeeze,
    device_aspect = effective_width / height,
    effective_width = effective_width
  )
}

#' Canvas Height That Scales With Node Density
#'
#' A node card's PHYSICAL size (in inches) is fixed by `image_size` and the
#' panel's WIDTH alone -- see the note at the top of [node_card_extent()]'s
#' `hh` calculation: `ggimage::geom_image()` preserves the image's own aspect
#' ratio regardless of device shape, so widening or narrowing the canvas
#' HEIGHT never changes how big a card actually is. What it changes is how
#' many data-y-units fit in that fixed physical height: a taller canvas packs
#' more inches into the same y-range, i.e. more room between rows. That is the
#' whole mechanism this function uses.
#'
#' Sam 2026-08-08, once the reindexed node set gave "10 or so organism nodes"
#' stacked in one column at a fixed 12x8in canvas: "we can't especially afford
#' to put them in a 1x10 column ... it may be time to start thinking about
#' ggraph". Ruled out (see chat): vertical position is semantically meaningful
#' here (CLAUDE.md, `plot_aep()`'s own header), so an automatic layout is off
#' the table regardless of library. The actual two causes of the overlap were
#' cheaper: canvas size was a constant regardless of node count, and this
#' function fixes exactly that half, automatically, from the AEP's own layout.
#' (Widening the canvas would NOT help the companion problem of several nodes
#' sharing a column at close x-spacing -- `hw` has no such width dependency to
#' exploit, since `image_size` is already a fraction of panel width by
#' definition. That half stays a manual layout decision.)
#'
#' @param nodes The AEP's own scoped, placed nodes tibble.
#' @param effective_width Inches, from [aep_diagram_squeeze()]'s
#'   `effective_width` (i.e. already accounting for a locator inset, if any).
#' @param image_size,card_aspect As elsewhere; use the SQUEEZE-COMPENSATED
#'   `image_size` (`aep_diagram_squeeze()`'s own output), not the raw one.
#' @param x_expand,y_expand As [plot_aep()]; keep these in sync with whatever
#'   is actually passed to `plot_aep()` for the real render, since this
#'   function has to predict the same `hh` `node_card_extent()` will.
#' @param min_height Floor, inches. Never returns less than this.
#' @param fill_fraction How much of the tightest row's vertical space a card
#'   may occupy at most, leaving `1 - fill_fraction` as a visible gap between
#'   adjacent rows. `0.6` errs toward "clearly separated" over "tightly
#'   packed": the group boxes drawn behind everything add their own visual
#'   weight, so rows sitting right on top of each other read as more crowded
#'   than the same numeric gap does on a plain grid.
#' @return Height in inches, `>= min_height`.
#' @export
aep_diagram_height <- function(
  nodes, effective_width, image_size, card_aspect,
  x_expand = 0.15, y_expand = 0.12,
  min_height = 8, fill_fraction = 0.6
) {
  placed <- nodes[!is.na(nodes$x) & !is.na(nodes$y), , drop = FALSE]
  if (nrow(placed) < 2) {
    return(min_height)
  }

  # Measured WITHIN each x-column separately, not across the whole node set.
  # Two nodes sharing a y but sitting in different columns (a source and an
  # organism at the same row, say) do not compete for vertical space at all;
  # only nodes sharing an x-column really do, and that is exactly what "10 or
  # so organism nodes ... a 1x10 column" describes. Measuring globally would
  # have inflated height for AEPs that are not actually crowded, just because
  # some unrelated column happens to reuse a y value -- e.g. the very first
  # AEP, whose source/medium/organism columns each independently use y = 0,
  # 1, 2.
  col_min_gaps <- vapply(split(placed$y, placed$x), function(yv) {
    yv <- sort(unique(yv))
    if (length(yv) < 2) {
      return(Inf)
    }
    min(diff(yv))
  }, numeric(1))
  min_gap <- suppressWarnings(min(col_min_gaps))
  if (!is.finite(min_gap) || min_gap <= 0) {
    return(min_height)
  }

  # hh (half card height, data units) scales as 1/height_in -- see this
  # function's own doc above -- so hh * height_in is invariant, and evaluating
  # it once at min_height recovers that constant regardless of what min_height
  # happened to be.
  ext_ref <- node_card_extent(
    placed,
    image_size = image_size, card_aspect = card_aspect,
    device_aspect = effective_width / min_height,
    x_expand = x_expand, y_expand = y_expand
  )
  const <- ext_ref$hh * min_height
  required <- 2 * const / (min_gap * fill_fraction)
  max(min_height, required)
}

#' Card Size That Shrinks to Fit Horizontal Node Density
#'
#' The horizontal counterpart to the crowding problem [aep_diagram_height()]
#' solves for rows -- but NOT a symmetric fix, because widening the canvas
#' genuinely does nothing here. [node_card_extent()]'s `hw = image_size * rx /
#' 2` has no inches term in it at all: `image_size` is defined as a fraction of
#' panel WIDTH, so a card's width in DATA-X units is set by the data range
#' (`rx`) and that fraction alone, and stays exactly the same regardless of how
#' many physical inches the canvas happens to be. (`hh`, by contrast, involves
#' `device_aspect = width_in / height_in`, which is why taller canvas => more
#' room per row actually works for the vertical case.) Confirmed the hard way,
#' 2026-08-11: bending a tall column of source nodes into an L added columns
#' (grew `rx`) while column-to-column spacing stayed the same 1.3 units, and
#' every card past the fourth clipped its neighbour -- widening `width` in
#' `write_aep_diagrams()` would not have touched it.
#'
#' The only lever that actually shrinks `hw` relative to a fixed inter-node
#' gap is `image_size` itself, so that is what this shrinks -- never grows: a
#' loose diagram keeps the caller's intended card size rather than being
#' blown up to fill space, matching [aep_diagram_height()]'s own floor-not-
#' ceiling philosophy.
#'
#' **Run this BEFORE [aep_diagram_height()]`, not after.** `rx` here does not
#' depend on `image_size` (see the formula above), so there is no circularity
#' in computing the shrink first; but `aep_diagram_height()`'s `hh` DOES
#' depend on `image_size`, so feeding it the already-shrunk value (rather than
#' the caller's original) means a diagram that needed shrinking horizontally
#' does not then get told it needs MORE height than it actually will, once its
#' cards are smaller.
#'
#' @param nodes The AEP's own scoped, placed nodes tibble.
#' @param image_size The caller's intended card width, as a fraction of panel
#'   width. Returned unchanged if there is no horizontal crowding to fix.
#' @param x_expand As [plot_aep()]; keep in sync with the real call, same
#'   reason as [aep_diagram_height()]'s own `x_expand`.
#' @param fill_fraction As [aep_diagram_height()]: how much of the tightest
#'   row's horizontal space a card may occupy at most.
#' @return `image_size`, or smaller. Never larger than the input.
#' @export
aep_diagram_image_size <- function(
  nodes, image_size, x_expand = 0.15, fill_fraction = 0.6
) {
  placed <- nodes[!is.na(nodes$x) & !is.na(nodes$y), , drop = FALSE]
  if (nrow(placed) < 2) {
    return(image_size)
  }

  # Measured WITHIN each y-row separately, mirroring aep_diagram_height()'s
  # own per-column measurement: two nodes sharing an x but sitting in
  # different rows do not compete for horizontal space.
  row_min_gaps <- vapply(split(placed$x, placed$y), function(xv) {
    xv <- sort(unique(xv))
    if (length(xv) < 2) {
      return(Inf)
    }
    min(diff(xv))
  }, numeric(1))
  min_gap <- suppressWarnings(min(row_min_gaps))
  if (!is.finite(min_gap) || min_gap <= 0) {
    return(image_size)
  }

  # rx is independent of image_size (see this function's own doc), so the
  # value passed in here is only "whichever is at hand" -- any image_size
  # would recover the same rx.
  rx <- node_card_extent(placed, image_size = image_size, x_expand = x_expand)$rx
  required <- min_gap * fill_fraction / rx
  min(image_size, required)
}

#' Draw and Write Every AEP
#'
#' @param scoped Output of [aep_scoped_nodes()].
#' @param edges The full edges table; narrowed per AEP by [aep_scope_edges()].
#' @param cards Output of [aep_all_report_cards()].
#' @param groups Output of [read_aep_node_groups()], or `NULL`.
#' @param card_paths Compact card paths from [write_aep_node_cards()].
#' @param manifest Output of [read_aep_manifest()], or `NULL`. Supplies the
#'   title (`label`), subtitle (`scope_note`) and, where set, the bounding box
#'   drawn on the locator inset. Without it, diagrams are untitled and
#'   uninset, as before this was added.
#' @param bbox_map The whole-study-area map used as the locator inset's base,
#'   e.g. the `wgs84_map` target. Required for an inset to be drawn; an AEP
#'   with a bounding box but no `bbox_map` supplied is titled but not inset.
#' @param dir Where the figures go.
#' @param width,height,dpi Canvas, passed on to [node_card_extent()] via
#'   `plot_aep()` so arrow clipping matches the device actually used.
#'
#'   `dpi` defaults to **300 to match the compact cards** (2.4 x 1.8in at
#'   300dpi, i.e. 720 x 540px, from [write_node_cards()]). `ggimage` sizes a
#'   card as `image_size` of panel width, so at 12in the card footprint is
#'   `0.19 * 12 * dpi` pixels: 342 at the old default of 150, against a 720px
#'   source, which threw away more than half the card's resolution at
#'   placement. 300 puts the footprint at ~684px, near 1:1. Raising `dpi`
#'   further only upscales the cards, so it buys sharper vector text and
#'   blurrier cards. Change this and `write_node_cards()`'s `dpi` together.
#' @param image_size Card width as a fraction of panel width.
#' @param card_aspect Compact card height over width, in inches.
#' @param inset_width Width of the locator inset relative to the main panel
#'   (e.g. `0.25` puts it at a quarter of the main diagram's width). Ignored
#'   when `bare = TRUE`, or when an AEP has no bounding box, or when
#'   `bbox_map` is not supplied.
#' @param bare Draw text labels instead of node card images? Sam 2026-08-07:
#'   "let's draw a bare aep without images every render/targets runthrough,
#'   diagnosing stuff when the images are already drawn is tricky." A card
#'   image is opaque and draws last (see `plot_aep()`), so it can hide an
#'   edge or a group-box label that is actually mispositioned underneath --
#'   exactly what made the arrowhead-clipping bug hard to pin down by eye. The
#'   bare figure shares every geometry decision with the real one (same
#'   coordinates, same edge clipping math, same group boxes) so a problem
#'   visible there is a real geometry problem, not an image-occlusion
#'   artefact, and a problem invisible there but present in the real figure IS
#'   the occlusion. The title/subtitle/inset still apply in bare mode, since
#'   none of those depend on the card images.
#' @return The written paths, one per AEP. Named `aep_<id>.png`, or
#'   `aep_<id>_bare.png` when `bare = TRUE`.
#' @export
write_aep_diagrams <- function(
  scoped,
  edges,
  cards,
  groups = NULL,
  card_paths = character(0),
  manifest = NULL,
  bbox_map = NULL,
  dir = here_rel("figures"),
  width = 12,
  height = 8,
  dpi = 300,
  image_size = 0.19,
  card_aspect = 1.8 / 2.4,
  inset_width = 0.25,
  bare = FALSE
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  unlist(
    purrr::imap(scoped, function(nodes, id) {
      # Cards for THIS AEP only. They live in images/node_cards/<aep_id>/, so
      # the directory name is the key; matching on basename alone would collide
      # across AEPs, since every AEP names its cards N001.png and so on.
      images <- if (bare) {
        NULL
      } else {
        mine <- card_paths[basename(dirname(card_paths)) == id]
        stats::setNames(mine, tools::file_path_sans_ext(basename(mine)))
      }

      row <- if (!is.null(manifest)) {
        manifest[manifest$aep_id %in% id, , drop = FALSE]
      } else {
        NULL
      }
      title <- if (!is.null(row) && nrow(row) == 1) row$label else id
      subtitle <- if (!is.null(row) && nrow(row) == 1) row$scope_note else NULL

      has_bbox <- !is.null(row) && nrow(row) == 1 && any(!is.na(row[
        c("lat_min", "lat_max", "lon_min", "lon_max")
      ]))
      draw_inset <- has_bbox && !is.null(bbox_map)

      # See aep_diagram_squeeze(): an inset AEP's diagram panel is squeezed
      # narrower by the plot_layout() below, so plot_aep() needs to be told
      # about that BEFORE it draws, or its cards (and bare tiles, and edge
      # clipping) come out smaller than a non-inset AEP's for no reason a
      # reader could see. Sam 2026-08-08: "AEPs 1 and 2 use different size
      # rectangles. Why?" -- A002 (Repparfjorden) has a bounding box, A001
      # doesn't, so only A002 was getting squeezed.
      sq <- aep_diagram_squeeze(draw_inset, width, height, image_size, inset_width)

      # See aep_diagram_image_size(): unlike height, canvas WIDTH cannot fix
      # horizontal crowding (a card's width in data-x units is invariant to
      # how many inches the canvas is), so cards shrink instead, and this
      # must run BEFORE aep_diagram_height() -- its own hh depends on
      # image_size, so a diagram that needed shrinking horizontally should
      # not then be told it needs MORE height than its now-smaller cards
      # actually require.
      fitted_image_size <- aep_diagram_image_size(nodes, sq$image_size)

      # See aep_diagram_height(): canvas height (not width -- a card's
      # physical size doesn't depend on canvas width) scales up automatically
      # once an AEP's nodes are packed too tightly in y for the fixed
      # `height` to hold them without overlapping. `width`/`height` above are
      # therefore a FLOOR, not the final canvas size.
      this_height <- aep_diagram_height(
        nodes, effective_width = sq$effective_width,
        image_size = fitted_image_size, card_aspect = card_aspect,
        min_height = height
      )
      device_aspect <- sq$effective_width / this_height

      diagram <- plot_aep(
        nodes,
        aep_scope_edges(edges, nodes),
        cards[cards$aep_id %in% id, , drop = FALSE],
        groups = aep_scope_groups(groups, nodes),
        node_images = images,
        image_size = fitted_image_size,
        card_aspect = card_aspect,
        device_aspect = device_aspect
      )

      combined <- if (draw_inset) {
        inset <- aep_bbox_inset(
          bbox_map, row$lat_min, row$lat_max, row$lon_min, row$lon_max
        )
        diagram + inset + patchwork::plot_layout(widths = c(1, inset_width))
      } else {
        diagram
      }

      combined <- combined + patchwork::plot_annotation(
        title = title, subtitle = subtitle,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(colour = "grey30")
        )
      )

      suffix <- if (bare) "_bare" else ""
      path <- file.path(dir, paste0("aep_", id, suffix, ".png"))
      ggplot2::ggsave(
        filename = path,
        plot = combined,
        width = width,
        height = this_height,
        dpi = dpi,
        device = ragg::agg_png
      )
      path
    }),
    use.names = FALSE
  )
}
