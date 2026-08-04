# Per-notebook working documents (PLAN.md P2.3, 2026-07-30).
#
# One markdown file per notebook in the P2.3 scheme. These are where Sam records
# comparisons between the groups inside a notebook: judgements that will not fit
# in a `notes` cell of group_decisions.csv, and that need to sit next to the
# plots they are about.
#
# APPEND-ONLY, AND THAT IS THE WHOLE DESIGN.
#
# The predecessor, scripts/generate_distribution_notebooks.R, was deleted in
# PLAN.md P0.2 along with its fourteen generated notebooks. The lesson recorded in
# CLAUDE.md is blunt: "Hand-written prose in the generated notebooks is not
# reproducible from anywhere else." So this generator:
#
#   * creates a file only if it does not exist;
#   * appends sections only for groups not already present;
#   * NEVER rewrites, reorders or removes anything already in a file.
#
# Group sections are detected by their `{#grp-...}` heading anchor, so a section
# stays recognised however much prose is written into it.
#
# The output is deliberately STATIC: numbers are written into the markdown rather
# than computed by chunks. These are documents to write in, not reports to
# re-run, and Sam needs the plot and his verdict on it to stay put. The cost is
# that figures go stale if the data change, which is why every file is stamped
# with its generation date and every group carries its stable id.

#' Filesystem-Safe Notebook Slug
#'
#' @param notebook A notebook name from [notebook_names()].
#' @return A lower-case, dash-separated slug.
#' @export
notebook_slug <- function(notebook) {
  notebook |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "-") |>
    stringr::str_replace_all("^-+|-+$", "") |>
    tolower()
}

#' Path to a Notebook's Working Document
#'
#' Kept in `docs/groups/` rather than alongside the `NBxx-` files, so these stay
#' clear of the notebook-numbering collapse described in CLAUDE.md 3.4 and read
#' as one obvious set. `docs/_metadata.yml` applies to this directory too, so they
#' inherit the shared formatting.
#'
#' @param notebook A notebook name.
#' @param dir Directory to place them in.
#' @return A file path.
#' @export
group_notebook_path <- function(notebook, dir = here_rel("docs/groups")) {
  file.path(dir, paste0(notebook_slug(notebook), ".qmd"))
}

#' Which Group Sections Does a File Already Contain?
#'
#' Detected by heading anchor rather than by heading text, so renaming a heading
#' or writing paragraphs under it does not make the section look absent and get
#' appended a second time.
#'
#' @param path A notebook path.
#' @return A character vector of `group_id`s already present.
#' @export
existing_group_sections <- function(path) {
  if (!file.exists(path)) {
    return(character(0))
  }
  lines <- readLines(path, warn = FALSE)
  matches <- stringr::str_match(lines, "\\{#grp-([A-Za-z0-9]+)[^}]*\\}")[, 2]
  unique(stats::na.omit(matches))
}

#' Markdown for One Group's Section
#'
#' @param row One row of the joined decisions/summary table.
#' @param plot_slug The group's `group_slug`, or `NA` if it has no panels.
#' @param captions Named vector of plot suffix to caption.
#' @return A character vector of markdown lines.
#' @export
group_section_markdown <- function(row, plot_slug = NA_character_, captions = NULL) {
  captions <- captions %||% c(
    a_density = "Overall distribution (all units)",
    b_date = "Concentration by date",
    c_campaign = "Distribution by campaign",
    d_site_type = "Distribution by site type (all geographies)",
    e_spatial = "Spatial distribution"
  )

  id <- row$group_id[1]
  label <- triage_group_label(row)
  # Quarto cross-reference ids must be lower case and start with the type prefix.
  fig <- paste0("fig-", tolower(id))
  out <- c(
    "",
    paste0("## ", id, " ", label, " {#grp-", id, "}"),
    "",
    group_summary_line(row),
    ""
  )

  if (!is.na(plot_slug)) {
    # A div id plus a per-image id makes these Quarto subfigures, so the whole
    # row is @fig-g006 and a single panel is @fig-g006-a. The closing caption
    # line is required: without it the div is a plain layout and none of the
    # references resolve.
    out <- c(out, paste0("::: {#", fig, " layout-ncol=5}"), "")
    for (i in seq_along(captions)) {
      key <- names(captions)[i]
      out <- c(
        out,
        paste0(
          "![", captions[[key]], "](",
          file.path("..", "..", "triage", paste0(plot_slug, "_", key, ".png")),
          "){#", fig, "-", substr(key, 1, 1), " group=\"", id, "\"}"
        ),
        ""
      )
    }
    out <- c(out, paste0(id, ": ", label), "", ":::", "")
  } else {
    out <- c(
      out,
      paste0(
        "*No triage panels: this group is below the `min_n` cutoff. ",
        "Add `", row$group_id[1], "` to `must_include` in the ",
        "`triage_pilot_groups` target if it needs them.*"
      ),
      ""
    )
  }

  # The point of the whole file. Left as a visible prompt rather than a blank, so
  # an unwritten verdict is obvious when skimming.
  c(out, "**Verdict:** *(unwritten)*", "")
}

#' Markdown for a Notebook's Header and Comparison Table
#'
#' The table exists so the groups in a notebook can be compared at a glance,
#' which is the question these documents are for. Per-group sections follow.
#'
#' @param notebook The notebook name.
#' @param rows All groups assigned to it, sorted.
#' @param overview A character vector of markdown lines for the higher-level
#'   panels, from [notebook_overview_markdown()], or `NULL`.
#' @return A character vector of markdown lines.
#' @export
notebook_header_markdown <- function(notebook, rows, overview = NULL) {
  # Geometric mean and GSD alongside the arithmetic ones, added 2026-08-04.
  # These concentrations are log-normal over orders of magnitude, so the
  # arithmetic mean sits above almost every observation. GSD reads as a
  # multiplicative factor: 3 means roughly threefold either side of geo_mean.
  num <- function(x) {
    if (length(x) == 0 || is.na(x) || !is.finite(x)) {
      return("")
    }
    formatC(x, format = "g", digits = 3)
  }

  tbl <- c(
    "| ID | Group | Unit | n | Mean | Median | Geo. mean | GSD | Sources | Flags |",
    "|---|---|---|---:|---:|---:|---:|---:|---:|---|"
  )
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    flags <- c(
      if (isTRUE(r$flag_multimodal[1])) "multimodal",
      if (isTRUE(r$flag_outliers[1])) "outliers"
    )
    tbl <- c(tbl, paste0(
      "| [", r$group_id[1], "](#grp-", r$group_id[1], ") ",
      "| ", triage_group_label(r),
      " | `", r$MEASURED_UNIT_STANDARD[1], "`",
      " | ", format(r$n[1], big.mark = ","),
      " | ", num(opt_col(r, "mean")),
      " | ", num(opt_col(r, "median")),
      " | ", num(opt_col(r, "geo_mean")),
      " | ", num(opt_col(r, "gsd")),
      " | ", r$n_sources[1],
      " | ", paste(flags, collapse = ", "),
      " |"
    ))
  }

  c(
    "---",
    paste0("title: \"", notebook, "\""),
    "---",
    "",
    paste0(
      "Working document for the **", notebook, "** grouping: ",
      nrow(rows), " group", if (nrow(rows) == 1) "" else "s", ", ",
      format(sum(rows$n), big.mark = ","), " measurements."
    ),
    "",
    paste0(
      "Scaffolded ", format(Sys.Date()), " by ",
      "`scripts/generate_group_notebooks.R`. **The generator is append-only**: ",
      "re-running it adds sections for new groups and never touches anything ",
      "already written here, so this file is yours to edit freely."
    ),
    "",
    paste0(
      "Figures and counts are static, written at scaffold time rather than ",
      "computed on render. Group ids are stable (`data/clean/group_ids.csv`), ",
      "so they remain the reliable way to refer back to a group even if the ",
      "numbers here go stale."
    ),
    "",
    overview,
    "# Groups at a glance",
    "",
    tbl,
    "",
    # Quarto turns a table into a cross-referenceable one via a caption line
    # starting with the id, so this is @tbl-glance in the prose.
    paste0(": Groups in the ", notebook, " notebook. {#tbl-glance}"),
    "",
    "# Comparison",
    "",
    "*(Notes on how these groups relate: what should be lumped, split, or",
    "dropped, and why. This is the question the document exists to answer.)*",
    "",
    "# Groups",
    ""
  )
}

#' Higher-Level Panels for a Notebook
#'
#' The sub-compartment and species-group comparison panels, placed at the top of
#' a notebook, above the groups they compare. Sam's request: "at the top of the
#' fish table it would be nice to have the whole plot of fish".
#'
#' **The mapping is not one to one.** A notebook is defined by the P2.3 scheme,
#' which lumps: *Crustaceans and Invertebrates* covers three species groups, and
#' *Other Waters* covers five sub-compartments. Panels are also split by unit. So
#' every panel whose key falls within the notebook is included, which for a lumped
#' notebook is several.
#'
#' These panels letter their own figures **a) and b)**, restarting rather than
#' continuing the per-group a-e. They were referred to as "f and g" in earlier
#' prose, which matched nothing on screen and is not used any more.
#'
#' @param rows The notebook's groups.
#' @param overview_nodes,species_nodes The overview and species node tables.
#' @param overview_paths,species_paths Their written file paths.
#' @return A character vector of markdown lines, possibly empty.
#' @export
notebook_overview_markdown <- function(
  rows,
  overview_nodes = NULL,
  species_nodes = NULL,
  overview_paths = character(0),
  species_paths = character(0)
) {
  out <- character(0)

  # Overview and species panels are NOT the same shape, and treating them alike
  # silently produced no species panels at all. Overview nodes carry `level_1` /
  # `level_2` and write one file per level as `<slug>_<a|b>_overview_<level>.png`.
  # Species nodes have no level columns and write a single
  # `<slug>_a_species.png`. See triage_species_plots vs triage_overview_plots.
  emit_overview <- function(nodes) {
    lines <- character(0)
    for (i in seq_len(nrow(nodes))) {
      node <- nodes[i, , drop = FALSE]
      levels <- c(node$level_1[1], node$level_2[1])
      levels <- levels[!is.na(levels)]
      if (length(levels) == 0) {
        next
      }
      fig <- paste0("fig-", tolower(gsub("_", "-", node$node_slug[1])))
      body <- character(0)
      for (j in seq_along(levels)) {
        p <- file.path("triage", paste0(
          node$node_slug[1], "_", c("a", "b")[j], "_overview_",
          tolower(levels[j]), ".png"
        ))
        if (!p %in% overview_paths) {
          next
        }
        body <- c(body, paste0(
          "![", c("a", "b")[j], ") Distribution by ",
          triage_level_label(levels[j]), "](",
          file.path("..", "..", p), "){#", fig, "-", c("a", "b")[j], "}"
        ), "")
      }
      if (length(body) == 0) {
        next
      }
      lines <- c(
        lines,
        paste0(
          "**Overview, `", node$MEASURED_UNIT_STANDARD[1], "`.** `n` = ",
          format(node$n[1], big.mark = ","), " measurements in ",
          node$n_groups[1], " groups below this sub-compartment."
        ),
        "",
        paste0("::: {#", fig, " layout-ncol=", length(levels), "}"),
        "",
        body,
        paste0(node$node_label[1]),
        "",
        ":::",
        ""
      )
    }
    lines
  }

  emit_species <- function(nodes) {
    lines <- character(0)
    for (i in seq_len(nrow(nodes))) {
      node <- nodes[i, , drop = FALSE]
      p <- file.path("triage", paste0(node$node_slug[1], "_a_species.png"))
      if (!p %in% species_paths) {
        next
      }
      fig <- paste0("fig-species-", tolower(gsub("_", "-", node$node_slug[1])))
      lines <- c(
        lines,
        paste0(
          "**By species, `", node$MEASURED_UNIT_STANDARD[1], "`.** `n` = ",
          format(node$n[1], big.mark = ","), " measurements across ",
          node$n_species[1], " species and ", node$n_tissues[1], " tissues."
        ),
        "",
        paste0("::: {#", fig, "}"),
        "",
        paste0(
          "![By species and tissue](", file.path("..", "..", p), ")"
        ),
        "",
        paste0(node$node_label[1]),
        "",
        ":::",
        ""
      )
    }
    lines
  }

  key <- c("ENVIRON_COMPARTMENT", "ENVIRON_COMPARTMENT_SUB")
  if (!is.null(overview_nodes) && nrow(overview_nodes) > 0) {
    keep <- do.call(paste, overview_nodes[key]) %in% do.call(paste, rows[key])
    out <- c(out, emit_overview(overview_nodes[keep, , drop = FALSE]))
  }
  skey <- c(key, "SPECIES_GROUP")
  if (!is.null(species_nodes) && nrow(species_nodes) > 0) {
    keep <- do.call(paste, species_nodes[skey]) %in% do.call(paste, rows[skey])
    out <- c(out, emit_species(species_nodes[keep, , drop = FALSE]))
  }

  if (length(out) == 0) {
    return(character(0))
  }
  c("# Overview", "", out)
}

#' Generate or Extend the Per-Notebook Working Documents
#'
#' @param decisions The `group_decisions` target, carrying `notebook`.
#' @param groups The `triage_pilot_groups` target, for plot slugs.
#' @param dir Output directory.
#' @param notebooks Which notebooks to generate. Defaults to all present.
#' @param verbose Report what changed?
#' @return A tibble of per-notebook actions, invisibly.
#' @export
generate_group_notebooks <- function(
  decisions,
  groups,
  dir = here_rel("docs/groups"),
  notebooks = NULL,
  summary_data = NULL,
  overview_nodes = NULL,
  species_nodes = NULL,
  overview_paths = character(0),
  species_paths = character(0),
  verbose = TRUE
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  decisions <- decisions |>
    dplyr::filter(!is.na(.data$notebook), .data$notebook != "")

  # Statistics are joined from the summary rather than read out of the decisions
  # CSV. They are machine-derived, so this keeps them fresh and keeps the
  # hand-edited file about decisions.
  if (!is.null(summary_data)) {
    stat_cols <- intersect(
      c("mean", "median", "geo_mean", "gsd"),
      names(summary_data)
    )
    if (length(stat_cols) > 0) {
      key <- triage_group_cols()
      n_before <- nrow(decisions)
      decisions <- decisions |>
        dplyr::select(-dplyr::any_of(stat_cols)) |>
        dplyr::left_join(
          summary_data |> dplyr::select(dplyr::all_of(c(key, stat_cols))),
          by = key
        )
      if (nrow(decisions) != n_before) {
        stop(
          "Joining summary statistics changed the row count from ",
          n_before, " to ", nrow(decisions), "."
        )
      }
    }
  }
  notebooks <- notebooks %||% sort(unique(decisions$notebook))

  slug_lookup <- stats::setNames(groups$group_slug, groups$group_id)
  report <- list()

  for (nb in notebooks) {
    rows <- decisions |>
      dplyr::filter(.data$notebook == nb) |>
      dplyr::arrange(dplyr::desc(.data$n))
    path <- group_notebook_path(nb, dir)
    present <- existing_group_sections(path)

    if (!file.exists(path)) {
      overview <- notebook_overview_markdown(
        rows, overview_nodes, species_nodes, overview_paths, species_paths
      )
      writeLines(notebook_header_markdown(nb, rows, overview), path)
      created <- TRUE
    } else {
      # The header, its glance table and its overview panels are written once
      # with the file. Regenerating them would mean rewriting the top of a
      # document Sam is editing, which is exactly what this generator refuses to
      # do. New statistics therefore reach new files only; retrofitting them into
      # an existing one is a migration, not a re-run.
      created <- FALSE
    }

    todo <- rows |> dplyr::filter(!.data$group_id %in% present)
    if (nrow(todo) > 0) {
      new_lines <- unlist(lapply(
        seq_len(nrow(todo)),
        function(i) {
          r <- todo[i, , drop = FALSE]
          group_section_markdown(
            r,
            plot_slug = unname(slug_lookup[r$group_id[1]])
          )
        }
      ))
      # Appended, never rewritten. Everything already in the file is untouched.
      cat(paste0(new_lines, collapse = "\n"), file = path, append = TRUE, sep = "")
      cat("\n", file = path, append = TRUE)
    }

    report[[nb]] <- tibble::tibble(
      notebook = nb,
      path = path,
      created = created,
      groups = nrow(rows),
      appended = nrow(todo),
      already_present = length(present)
    )
  }

  out <- dplyr::bind_rows(report)
  if (verbose) {
    message(
      "Group notebooks: ", sum(out$created), " created, ",
      sum(out$appended), " section(s) appended, ",
      sum(out$already_present), " left untouched"
    )
  }
  invisible(out)
}
