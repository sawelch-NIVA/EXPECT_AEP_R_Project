# Species-group triage overviews.
#
# The third and innermost tier of the overview machinery:
#
#   sub-compartment   ->  by species group (or by site type / subtype, abiotic)
#   species group     ->  by species x tissue          <- this file
#   group             ->  the five per-group panels a-e
#
# Sam's framing: the lump/split work concentrates on species groups and
# individual species, and comparing Fish species against each other is a
# different question from comparing Fish against Molluscs.
#
# Bands are species x tissue, not species. Tissue moves the measured value
# further than species does: in Fish / mg/kg (wet) the median is 5.20 in liver
# against 0.228 in muscle, a factor of 23, which is larger than most
# between-species differences in the same panel. Pooling tissues would put that
# variation under a species label and invite exactly the wrong lumping decision.
#
# It also happens to be the more useful axis: a band here is very nearly a group
# key, so the panel shows the things being decided about rather than a
# projection of them.

#' Columns Identifying a Species-Group Node
#'
#' The unit is included for the same reason as in [triage_overview_node_cols()].
#'
#' @return A character vector of column names.
#' @export
triage_species_node_cols <- function() {
  c(
    "ENVIRON_COMPARTMENT",
    "ENVIRON_COMPARTMENT_SUB",
    "SPECIES_GROUP",
    "MEASURED_UNIT_STANDARD"
  )
}

#' Band Label Combining Species and Tissue
#'
#' `Gadus morhua (Liver)`. Tissue is parenthesised rather than separated by a
#' slash so the species name still reads as a species name when the labels are
#' wrapped onto two lines on a narrow axis.
#'
#' Missing tissue yields the bare species rather than `(NA)`; missing species
#' yields `Unknown species`, which keeps such rows visible instead of dropping
#' them silently in [triage_plot_by_category()]'s `!is.na()` filter.
#'
#' @param species A character vector of species names.
#' @param tissue A character vector of tissue names.
#' @return A character vector the same length as `species`.
#' @export
species_tissue_label <- function(species, tissue) {
  sp <- as.character(species)
  sp[is.na(sp) | sp == ""] <- "Unknown species"
  ti <- as.character(tissue)
  has_tissue <- !is.na(ti) & ti != ""
  ifelse(has_tissue, paste0(sp, " (", ti, ")"), sp)
}

#' Add the Species/Tissue Band Column
#'
#' @param data Rows under one species-group node.
#' @return `data` with a `.species_tissue` column.
#' @keywords internal
add_species_tissue_col <- function(data) {
  data$.species_tissue <- species_tissue_label(
    data$SAMPLE_SPECIES,
    data$SAMPLE_TISSUE
  )
  data
}

#' Stable Group Ids Behind Each Species/Tissue Band
#'
#' A band is species x tissue, but a *group* is [triage_group_cols()], eight
#' columns including the two geographic ones. So a band is usually one group and
#' occasionally several: 126 of the 139 bands drawn are one-to-one, 12 cover two
#' groups and *Mytilus edulis* (Total soft tissues) in `mg/kg (dry)` covers four.
#' Every matching id is listed, because the bands that pool groups are exactly
#' the ones a lump/split decision needs to be able to look up.
#'
#' @param data Rows under one species-group node, already carrying
#'   `.species_tissue` from [add_species_tissue_col()].
#' @param ids The `group_ids` ledger.
#' @return A tibble of `.species_tissue` and `group_ids`, one row per band.
#' @keywords internal
band_group_ids <- function(data, ids) {
  key <- triage_group_cols()

  # Joined on distinct key combinations rather than on the rows themselves, so a
  # duplicated ledger key cannot quietly multiply the data. read_group_ids()
  # checks that group_id is unique but not that the key is.
  combos <- dplyr::distinct(
    data,
    dplyr::across(dplyr::all_of(c(key, ".species_tissue")))
  )
  n_before <- nrow(combos)
  combos <- dplyr::left_join(
    combos,
    dplyr::select(ids, dplyr::all_of(c(key, "group_id"))),
    by = key
  )
  if (nrow(combos) != n_before) {
    stop(
      "band_group_ids(): the ID ledger has duplicate group keys, so the join ",
      "changed the row count from ", n_before, " to ", nrow(combos), "."
    )
  }

  combos |>
    dplyr::filter(!is.na(.data$group_id)) |>
    dplyr::group_by(.data$.species_tissue) |>
    dplyr::summarise(
      group_ids = paste(sort(unique(.data$group_id)), collapse = ", "),
      .groups = "drop"
    )
}

#' Prefix Band Labels With Their Group Ids
#'
#' `G022 Pandalus borealis (Whole body)`. Sam's request 2026-08-06: the panel
#' names species but the notebook is organised by id, so reading a band back to
#' the section discussing it meant matching on the species name by eye.
#'
#' The id goes in FRONT for the same reason it does in the notebook headings and
#' the glance table: it is the short, fixed-width part, so the ids line up down
#' the axis and can be scanned without reading the names. Bands are ordered by
#' median value, so the prefix has no effect on ordering.
#'
#' A band with no id in the ledger keeps its bare label rather than gaining an
#' empty prefix. That should not happen (every row of
#' `literature_analysis_ready` matches the ledger), but a silently mangled label
#' would be worse than a missing one.
#'
#' @param data Rows carrying `.species_tissue`.
#' @param ids The `group_ids` ledger, or `NULL` to leave the labels alone.
#' @return `data`, with `.species_tissue` prefixed.
#' @keywords internal
add_group_ids_to_bands <- function(data, ids = NULL) {
  if (is.null(ids) || nrow(ids) == 0 || nrow(data) == 0) {
    return(data)
  }
  lookup <- band_group_ids(data, ids)
  prefix <- stats::setNames(lookup$group_ids, lookup$.species_tissue)[
    data$.species_tissue
  ]
  data$.species_tissue <- ifelse(
    is.na(prefix),
    data$.species_tissue,
    paste0(prefix, " ", data$.species_tissue)
  )
  data
}

#' Species Groups Worth a By-Species Panel
#'
#' One row per compartment x sub-compartment x species group x unit carrying at
#' least `min_n` measurements across at least two distinct species/tissue bands.
#'
#' The two-band minimum is the same "must vary" rule the sub-compartment tier
#' uses: a by-species panel showing one band answers nothing. It is what drops
#' `Biota, Terrestrial / Mammals / mg/kg (dry)`, which reaches 317 measurements
#' but is *Ursus maritimus* liver and nothing else.
#'
#' @param data The `literature_analysis_ready` target. Must carry `MEASURED_N`.
#' @param min_n Minimum measurements for a node to qualify.
#' @param groups The `triage_pilot_groups` target. Species groups with no
#'   displayed group beneath them are dropped, since the notebook never opens a
#'   heading for them; see [filter_reachable_nodes()]. `NULL` skips the check.
#' @return A tibble of node columns plus `n`, `n_rows`, `n_bands`, `n_species`,
#'   `n_tissues`, `node_label` and `node_slug`.
#' @export
triage_species_nodes <- function(data, min_n = 100, groups = NULL) {
  node_cols <- triage_species_node_cols()

  present <- data |>
    dplyr::filter(
      !is.na(.data$SPECIES_GROUP),
      .data$SPECIES_GROUP != ""
    )

  if (nrow(present) == 0) {
    return(empty_species_nodes())
  }

  counts <- present |>
    add_species_tissue_col() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(node_cols))) |>
    dplyr::summarise(
      n = sum(.data$MEASURED_N),
      n_rows = dplyr::n(),
      n_bands = dplyr::n_distinct(.data$.species_tissue),
      n_species = dplyr::n_distinct(.data$SAMPLE_SPECIES),
      n_tissues = dplyr::n_distinct(.data$SAMPLE_TISSUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n >= min_n, .data$n_bands >= 2) |>
    dplyr::arrange(dplyr::desc(.data$n))

  counts <- filter_reachable_nodes(counts, groups, node_cols)

  if (nrow(counts) == 0) {
    return(empty_species_nodes())
  }

  out <- counts |>
    dplyr::mutate(
      node_label = paste(
        .data$ENVIRON_COMPARTMENT_SUB,
        .data$SPECIES_GROUP,
        .data$MEASURED_UNIT_STANDARD,
        sep = " / "
      ),
      node_slug = slugify_name(.data$node_label)
    )

  # Same collision guard as triage_overview_nodes(): make.unique() would paper
  # over a duplicate with a _1 suffix, leaving the unsuffixed slug a string
  # prefix of the suffixed one and breaking filename matching downstream.
  if (anyDuplicated(sub("_[0-9]+$", "", out$node_slug)) > 0) {
    stop(
      "triage_species_nodes(): two nodes slugged to the same name. ",
      "Node labels must be unique before slugification."
    )
  }

  # Shaped like a group so thresholds_for_group() can read it; see the same note
  # in triage_overview_nodes(). SPECIES_GROUP is a real value here rather than
  # NA, so biota thresholds do match at this tier.
  for (col in setdiff(triage_group_cols(), node_cols)) {
    out[[col]] <- NA_character_
  }
  out
}

#' Zero-Row Species Node Table
#'
#' @return A zero-row tibble with the same columns as a populated result.
#' @keywords internal
empty_species_nodes <- function() {
  cols <- c(
    triage_group_cols(),
    "n", "n_rows", "n_bands", "n_species", "n_tissues",
    "node_label", "node_slug"
  )
  out <- tibble::as_tibble(
    stats::setNames(rep(list(character(0)), length(cols)), cols)
  )
  for (col in c("n", "n_rows", "n_bands", "n_species", "n_tissues")) {
    out[[col]] <- integer(0)
  }
  out
}

#' Subset Data to One Species-Group Node
#'
#' @param data The `literature_analysis_ready` target.
#' @param node A one-row tibble from [triage_species_nodes()].
#' @return A filtered data frame.
#' @export
filter_to_species_node <- function(data, node) {
  keep <- rep(TRUE, nrow(data))
  for (col in triage_species_node_cols()) {
    want <- node[[col]][1]
    have <- data[[col]]
    keep <- keep &
      if (is.na(want)) is.na(have) else (!is.na(have) & have == want)
  }
  data[keep, , drop = FALSE]
}

#' Write the By-Species Panel for One Species Group
#'
#' @param data The `literature_analysis_ready` target.
#' @param node A one-row tibble from [triage_species_nodes()].
#' @param dir Output directory.
#' @param scale_limits Output of [compute_triage_scale_limits()], so this panel
#'   shares the value axis with everything above and below it.
#' @param thresholds The `copper_toxicity_thresholds` target, or `NULL`.
#' @param ids The `group_ids` ledger. Band labels are prefixed with their stable
#'   group ids; see [add_group_ids_to_bands()]. `NULL` gives bare species names.
#' @param max_categories Passed to [truncate_categories()].
#' @param width Figure width in inches.
#' @param height_per_category Height allowance per band.
#' @param min_height,max_height Bounds on the computed height.
#' @param dpi Resolution.
#' @return The written file path.
#' @export
write_species_overview_for_node <- function(
  data,
  node,
  dir = "triage",
  scale_limits = NULL,
  thresholds = NULL,
  ids = NULL,
  max_categories = 25,
  width = 8,
  height_per_category = 0.28,
  min_height = 3.5,
  max_height = 9,
  dpi = 150
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # Ids are added BEFORE truncation. The prefix is a function of the band, so it
  # cannot merge or split bands, and doing it here keeps the "showing the N
  # largest of M" note counting the same things the axis shows.
  node_data <- add_group_ids_to_bands(
    add_species_tissue_col(filter_to_species_node(data, node)),
    ids
  )
  trimmed <- truncate_categories(node_data, ".species_tissue", max_categories)

  subtitle <- paste(
    c(node$node_label[1], trimmed$note),
    collapse = " -- "
  )

  p <- triage_plot_by_category(
    trimmed$data,
    ".species_tissue",
    "a) Distribution by species and tissue",
    subtitle,
    # Wider than the default 15: "Gadus morhua (Liver)" is 20 characters and
    # wraps to three cramped lines at the default. Still 24 with the id prefix
    # in front: two lines is the common case, the four-id Mytilus band is the
    # only one anywhere that reaches three, and widening this steals panel width
    # from every panel to fix one label.
    wrap_width = 24,
    limits = triage_limits_for(scale_limits, node),
    thresholds = thresholds,
    grp = node
  )

  n_cat <- length(unique(trimmed$data$.species_tissue))
  height <- min(max(n_cat * height_per_category, min_height), max_height)

  path <- file.path(dir, paste0(node$node_slug[1], "_a_species.png"))
  ggplot2::ggsave(
    filename = path,
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    device = ragg::agg_png
  )
  path
}

#' Write By-Species Panels for Every Species Group
#'
#' @param data The `literature_analysis_ready` target.
#' @param nodes Output of [triage_species_nodes()].
#' @param dir Output directory.
#' @param ... Passed to [write_species_overview_for_node()], notably
#'   `scale_limits` and `thresholds`.
#' @return A character vector of written file paths, for `format = "file"`.
#' @export
write_species_overviews <- function(data, nodes, dir = "triage", ...) {
  paths <- purrr::map_chr(
    seq_len(nrow(nodes)),
    function(i) {
      node <- nodes[i, , drop = FALSE]
      message("Species panel: ", node$node_label[1])
      write_species_overview_for_node(data, node, dir = dir, ...)
    }
  )
  unname(paths)
}
