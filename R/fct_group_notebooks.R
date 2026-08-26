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
  # [A-Za-z0-9-]+, not just [A-Za-z0-9]+ (Sam 2026-08-08): a composite group
  # id like "G014-Bf-Cnr-G-mor-Liv-Mw" contains hyphens, and the narrower
  # class used to stop at the first one, silently truncating the captured id
  # to "G014" rather than failing loudly.
  matches <- stringr::str_match(lines, "\\{#grp-([A-Za-z0-9-]+)[^}]*\\}")[, 2]
  unique(stats::na.omit(matches))
}

#' The Five Triage Panels, in Reading Order
#'
#' Suffix to caption. The letter prefix on each suffix is what the notebooks key
#' their subfigure ids on, so `a_density` becomes `@fig-g006-a`.
#'
#' @return A named character vector.
#' @export
triage_panel_captions <- function() {
  c(
    a_density = "Overall distribution (all units)",
    b_date = "Concentration by date",
    c_source = "Distribution by campaign or reference",
    d_site_type = "Distribution by site type (all geographies)",
    e_spatial = "Spatial distribution"
  )
}

#' The Placeholder Written Where a Group Has No Panels
#'
#' Generated in one place because [refresh_group_panels()] matches it
#' **verbatim** to decide whether a section is safe to repair. If the two ever
#' drifted apart, the repair would silently stop finding anything.
#'
#' @param id A `group_id`.
#' @return A single markdown line.
#' @export
no_panels_placeholder <- function(id) {
  paste0(
    "*No triage panels: this group is below the `min_n` cutoff. ",
    "Add `", id, "` to `must_include` in the ",
    "`triage_pilot_groups` target if it needs them.*"
  )
}

#' The Figure Block for One Group
#'
#' Empty cross-reference bullets, then the subfigure div. Shared by
#' [group_section_markdown()] (writing a new section) and
#' [refresh_group_panels()] (repairing one that has gained panels), so a section
#' repaired later is byte-identical to one written fresh.
#'
#' One empty bullet per panel sits ABOVE the div, carrying the cross-reference
#' already written out. Sam's request 2026-08-05: he was typing
#' `- @fig-g013-a:` by hand for every panel of every group, which is five
#' references x 245 groups of clerical work, and a mistyped id fails silently as
#' an unresolved reference.
#'
#' @param id A `group_id`. @param label The group label, for the div caption.
#' @param plot_slug The group's `group_slug`.
#' @param captions Named vector of plot suffix to caption.
#' @return A character vector of markdown lines.
#' @export
panel_block_markdown <- function(id, label, plot_slug, captions = NULL) {
  captions <- captions %||% triage_panel_captions()
  fig <- paste0("fig-", tolower(id))

  out <- c(
    vapply(
      names(captions),
      function(key) paste0("- @", fig, "-", substr(key, 1, 1), ":"),
      character(1),
      USE.NAMES = FALSE
    ),
    ""
  )

  # A div id plus a per-image id makes these Quarto subfigures, so the whole row
  # is @fig-g006 and a single panel is @fig-g006-a. The closing caption line is
  # required: without it the div is a plain layout and none of the references
  # resolve.
  out <- c(out, paste0("::: {#", fig, " layout-ncol=5}"), "")
  for (key in names(captions)) {
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
  c(out, paste0(id, ": ", label), "", ":::", "")
}

#' Markdown for One Group's Section
#'
#' @param row One row of the joined decisions/summary table.
#' @param plot_slug The group's `group_slug`, or `NA` if it has no panels.
#' @param captions Named vector of plot suffix to caption.
#' @return A character vector of markdown lines.
#' @export
group_section_markdown <- function(row, plot_slug = NA_character_, captions = NULL) {
  captions <- captions %||% triage_panel_captions()

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

  out <- c(out, if (!is.na(plot_slug)) {
    panel_block_markdown(id, label, plot_slug, captions)
  } else {
    c(no_panels_placeholder(id), "")
  })

  # The point of the whole file, and the ONLY thing in a section Sam edits --
  # everything above is machine-written and gets left alone by both this
  # generator (append-only) and refresh_group_panels(). A callout marks that
  # boundary visually (Sam 2026-08-08: "move the areas of the QMDs that
  # expect human writing into callouts... make it a lot more clear what's
  # safe to change and what isn't"). "**Verdict:**" itself stays literal
  # text inside the callout, not folded into the callout title, so existing
  # verdicts (grep-matched on that prefix elsewhere) keep meaning the same
  # thing.
  c(out, "::: {.callout-note}", "**Verdict:** *(unwritten)*", ":::", "")
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
      "already written here. The callout boxes (:::  {.callout-note}) mark ",
      "what it expects YOU to write -- everything else is machine-generated ",
      "and will be silently out of sync with the data if hand-edited."
    ),
    "",
    paste0(
      "Figures and counts are static, written at scaffold time rather than ",
      "computed on render. Group ids are stable (`data/clean/decisions/group_ids.csv`), ",
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
    "::: {.callout-note}",
    "*(Notes on how these groups relate: what should be lumped, split, or",
    "dropped, and why. This is the question the document exists to answer.)*",
    ":::",
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
  refresh_panels = TRUE,
  verbose = TRUE
) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # Bring existing sections up to date with panels that have since been built,
  # BEFORE appending new ones. Without this, adding a group to `must_include` and
  # re-running the pipeline leaves its section reading "No triage panels"
  # forever, because append-only skips any anchor already in the file. Sam hit
  # exactly that with G047 on 2026-08-05 after correctly editing the target and
  # re-running twice.
  #
  # Narrowly scoped: see refresh_group_panels(). It replaces one machine-written
  # line and cannot touch prose.
  if (refresh_panels) {
    refresh_group_panels(groups, dir = dir, decisions = decisions, verbose = verbose)
  }

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

#' Give Panels to Sections That Have Since Gained Them
#'
#' **The gap this closes.** Adding a group to `must_include` and re-running the
#' pipeline writes its five PNGs, but the notebook section still reads "No triage
#' panels: this group is below the `min_n` cutoff". The pipeline never touches
#' `docs/groups/*.qmd` at all, and [generate_group_notebooks()] cannot help
#' either: it appends sections only for group anchors ABSENT from the file, so a
#' section that already exists is skipped whatever has changed underneath it.
#'
#' Sam hit exactly this with G047 on 2026-08-05, having correctly edited the
#' target and re-run the pipeline twice.
#'
#' **Append-only is not weakened, and the scoping is the whole argument.** This
#' replaces one line, and only where that line is byte-identical to
#' [no_panels_placeholder()] for the group whose section it sits in. That string
#' is machine-written boilerplate that no one would type. If Sam has written a
#' single character into it, the match fails and the section is left alone. It
#' cannot touch prose, a verdict, or a section that already has a figure block.
#'
#' Idempotent: after a repair the placeholder is gone, so re-running does
#' nothing.
#'
#' @param groups The `triage_pilot_groups` target, supplying `group_id` and
#'   `group_slug`.
#' @param dir Where the notebooks live.
#' @param decisions Optional decisions table, used only for the group label in
#'   the figure caption. Falls back to the label already in the heading.
#' @param verbose Report what changed?
#' @return A tibble of `file`, `group_id`, `repaired`, invisibly.
#' @export
refresh_group_panels <- function(
  groups,
  dir = here_rel("docs/groups"),
  decisions = NULL,
  verbose = TRUE
) {
  files <- list.files(dir, pattern = "[.]qmd$", full.names = TRUE)
  log <- list()

  for (path in files) {
    lines <- readLines(path, warn = FALSE)
    # See existing_group_sections() above for why this is [A-Za-z0-9-]+.
    anchors <- stringr::str_match(lines, "\\{#grp-([A-Za-z0-9-]+)")[, 2]
    changed <- FALSE

    # Walk backwards so earlier line numbers stay valid as the file grows.
    placeholders <- rev(which(startsWith(lines, "*No triage panels:")))

    for (i in placeholders) {
      owner <- utils::tail(stats::na.omit(anchors[seq_len(i)]), 1)
      if (length(owner) != 1) {
        next
      }
      # Verbatim match against the placeholder for THIS group. A placeholder
      # naming a different group id means the file has been hand-edited in a way
      # this function must not second-guess.
      if (!identical(lines[i], no_panels_placeholder(owner))) {
        next
      }
      # which(), not the bare logical: `groups$group_id` can carry NA for a
      # group that entered triage before it was given a stable id, and base-R
      # logical subsetting turns each NA index into an extra NA element. That
      # made `length(slug)` 2 for EVERY group, so this guard sent every
      # candidate to `next` and the whole function became a silent no-op.
      # Found 2026-08-26, after G043 sat unrepaired despite its panels
      # existing. One unidentified group must not disable repair for the rest.
      slug <- groups$group_slug[which(groups$group_id == owner)]
      if (length(slug) != 1 || is.na(slug)) {
        next
      }

      label <- if (!is.null(decisions) && owner %in% decisions$group_id) {
        triage_group_label(decisions[which(decisions$group_id == owner), ])
      } else {
        # From the heading itself: "## G047 Fish / Gadus morhua / ... {#grp-G047}"
        heading <- utils::tail(
          grep(paste0("\\{#grp-", owner, "\\}"), lines), 1
        )
        sub(
          paste0("^## ", owner, " (.*) \\{#grp-", owner, "\\}$"), "\\1",
          lines[heading]
        )
      }

      lines <- append(
        lines[-i],
        panel_block_markdown(owner, label, slug),
        after = i - 1
      )
      changed <- TRUE
      log[[length(log) + 1]] <- tibble::tibble(
        file = basename(path), group_id = owner, repaired = TRUE
      )
    }

    if (changed) {
      writeLines(lines, path)
    }
  }

  out <- if (length(log)) purrr::list_rbind(log) else {
    tibble::tibble(
      file = character(0), group_id = character(0), repaired = logical(0)
    )
  }

  if (verbose) {
    if (nrow(out) == 0) {
      message("No sections needed panels adding.")
    } else {
      message(
        "Added panels to ", nrow(out), " section(s): ",
        paste(out$group_id, collapse = ", ")
      )
    }
  }
  invisible(out)
}
