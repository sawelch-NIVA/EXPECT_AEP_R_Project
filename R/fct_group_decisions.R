# The grouping decision layer (PLAN.md P2.2).
#
# One hand-edited CSV is the interface between the machinery and Sam's judgement.
# The pipeline SCAFFOLDS it and READS it. It never decides anything: what to lump,
# split, or drop is a scientific judgement and stays manual (CLAUDE.md working
# agreements).
#
# The same cache-versus-curation split described in PLAN.md section 10 applies
# here, and is the reason scaffold_group_decisions() is a merge rather than a
# write: the context columns (n, flags, coverage) are machine-derived and get
# refreshed on every scaffold, while `decision`, `lump_into` and `notes` are
# human and are never touched once set. That means the scaffold can be re-run
# safely when new data arrives.

#' Permitted Decision Values
#'
#' * `keep` -- reviewed, usable as it stands.
#' * `lump` -- merge with another **group**, named by its `group_id` in
#'   `lump_into`. This is a group-to-group merge *within* a notebook; which
#'   notebook a group belongs to is the separate `notebook` column.
#' * `split` -- the group key is too coarse; needs subdividing before use.
#' * `drop` -- not usable, with the reason in `notes`.
#'
#' An empty string means undecided. `keep` therefore means "looked at it, it is
#' fine", which is a different and more useful statement than silence.
#'
#' **`own_notebook` was removed on 2026-07-30**, when `notebook` became its own
#' column and made it redundant. It had also become actively ambiguous: eleven
#' rows carried `own_notebook` *and* a `lump_into` value, which are opposite
#' instructions under a single-column schema.
#'
#' @return A character vector of permitted values.
#' @export
group_decision_levels <- function() {
  c("keep", "lump", "split", "drop")
}

#' Columns Owned by the Human
#'
#' Never overwritten by [scaffold_group_decisions()].
#'
#' `notebook` is here rather than machine-derived because, although
#' [apply_notebook_scheme()] can fill it mechanically, the assignment is a
#' judgement and Sam must be able to override any row without the next scaffold
#' undoing it.
#'
#' @return A character vector of column names.
#' @export
group_decision_human_cols <- function() {
  c("notebook", "decision", "lump_into", "notes")
}

#' Add Measurement-Coverage Columns
#'
#' `cum_pct` is the running share of all measurements accounted for by this group
#' and every larger one. It exists because the distribution is brutally skewed:
#' as of 2026-07-30 the top 2 groups of 245 carry 50% of all measurements, the top
#' 7 carry 90%, and 183 groups have `n < 30`. Without the cumulative column it is
#' impossible to see where a sensible stopping point is.
#'
#' `tier` buckets that into the thresholds worth reasoning about. Note what it
#' does **not** mean: a low tier is not "unimportant". Measurement count is
#' dominated by Vannmiljø water and sediment monitoring, while most of the biota
#' groups an AEP needs as nodes are small. See the note in PLAN.md P2.3.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @return `summary_data` with `rank`, `cum_pct` and `tier`, sorted by `n`
#'   descending.
#' @export
add_coverage_columns <- function(summary_data) {
  out <- summary_data |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      cum_pct = cumsum(.data$n) / sum(.data$n)
    )

  # Each tier is the MINIMAL set of groups reaching its threshold, found by rank
  # rather than by comparing a derived cumulative against a constant.
  #
  # Two earlier attempts were wrong. Tiering on `cum_pct <= 0.90` excludes
  # whichever group crosses the line, putting top90 at 6 groups covering 88% and
  # disagreeing with the wording in PLAN.md P2.2. Tiering on the coverage *before*
  # each group fixed that but landed on a floating-point boundary: 0.96 - 0.06
  # evaluates to 0.8999999999999999, so a group sitting exactly on a threshold was
  # tiered by rounding noise. Finding the crossing rank once avoids both.
  cut_rank <- function(p) {
    hit <- which(out$cum_pct >= p)
    if (length(hit) == 0) nrow(out) else hit[1]
  }
  k90 <- cut_rank(0.90)
  k95 <- cut_rank(0.95)
  k99 <- cut_rank(0.99)

  out |>
    dplyr::mutate(
      tier = dplyr::case_when(
        .data$rank <= k90 ~ "top90",
        .data$rank <= k95 ~ "top95",
        .data$rank <= k99 ~ "top99",
        .default = "tail"
      )
    )
}

#' Scaffold or Refresh the Group Decisions File
#'
#' Idempotent by design. Run it as often as you like: machine-derived context is
#' refreshed, human decisions are preserved untouched, and new groups are appended
#' as undecided.
#'
#' Refusing to overwrite `decision` is the whole point. A scaffold that clobbered
#' the decisions would make the file unsafe to regenerate, which in turn makes it
#' unsafe to add new data.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @param path Where the CSV lives.
#' @param verbose Report what changed?
#' @return The written tibble, invisibly.
#' @export
scaffold_group_decisions <- function(
  summary_data,
  path = here_rel("data/clean/decisions/group_decisions.csv"),
  ids = read_group_ids(),
  verbose = TRUE
) {
  key <- triage_group_cols()

  context <- summary_data |>
    add_coverage_columns() |>
    attach_group_ids(ids) |>
    dplyr::transmute(
      # First column, because it is the thing you look up and type.
      group_id = .data$group_id,
      dplyr::across(dplyr::all_of(key)),
      species_common_name = .data$species_common_name,
      rank = .data$rank,
      n = .data$n,
      n_sources = .data$n_sources,
      # Summary statistics are deliberately NOT carried here. They are
      # machine-derived and live in summarise_literature_data; the glance tables
      # join them at generation time. Keeping them out means this file stays
      # about decisions, and adding a statistic does not require every user to
      # re-scaffold a file they may have open.
      #
      # Carried so the flag text can quote the dip test p value. `any_of` because
      # the test fixtures supply a bare summary without it.
      #
      # `n_rows` is deliberately NOT here: it is computed against
      # literature_analysis_ready in sample_triage_groups(), not by
      # summarise_literature_data, and group_summary_line() drops the rows clause
      # when it is absent.
      dplyr::across(dplyr::any_of("dip_p")),
      cum_pct = round(.data$cum_pct, 4),
      tier = .data$tier,
      flag_multimodal = .data$flag_multimodal,
      flag_outliers = .data$flag_outliers
    )

  existing <- if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(
      .default = readr::col_guess(),
      decision = readr::col_character(),
      lump_into = readr::col_character(),
      notes = readr::col_character()
    ))
  } else {
    NULL
  }

  if (is.null(existing)) {
    out <- context |>
      dplyr::mutate(notebook = "", decision = "", lump_into = "", notes = "")
    added <- nrow(out)
    orphaned <- 0L
  } else {
    human <- existing |>
      dplyr::select(dplyr::all_of(key), dplyr::any_of(group_decision_human_cols()))
    out <- context |> dplyr::left_join(human, by = key)
    for (col in group_decision_human_cols()) {
      if (!col %in% names(out)) {
        out[[col]] <- ""
      }
      out[[col]] <- dplyr::coalesce(out[[col]], "")
    }
    added <- sum(!do.call(paste, context[key]) %in% do.call(paste, existing[key]))
    orphaned <- sum(!do.call(paste, existing[key]) %in% do.call(paste, context[key]))
  }

  # Orphans are reported, never silently dropped: a group vanishing usually means
  # an upstream key changed (a species rename, a unit fix), and the decision that
  # was attached to it is still worth re-reading rather than losing.
  if (orphaned > 0 && !is.null(existing)) {
    lost <- existing |>
      dplyr::filter(
        !do.call(paste, existing[key]) %in% do.call(paste, context[key]),
        .data$decision != ""
      )
    if (nrow(lost) > 0) {
      cli::cli_warn(c(
        "{nrow(lost)} decided group(s) no longer exist in the data.",
        "i" = "Their decisions will be dropped from {.path {path}}.",
        "i" = "Usually an upstream key changed: a species rename or a unit fix."
      ))
    }
  }

  readr::write_csv(out, path, na = "")

  if (verbose) {
    message(
      "group_decisions.csv: ", nrow(out), " groups (",
      added, " new, ", orphaned, " orphaned, ",
      sum(out$decision != ""), " already decided)"
    )
  }
  invisible(out)
}

#' Read and Validate the Group Decisions File
#'
#' Validates rather than trusts: this file is hand-edited, so a typo in a
#' `decision` value or a `lump_into` pointing at nothing must fail loudly here
#' rather than silently produce an empty group downstream.
#'
#' @param path Where the CSV lives.
#' @param summary_data Optional. When supplied, reports groups present in the data
#'   but absent from the file, which is how a stale decisions file is caught after
#'   new data arrives.
#' @return A tibble of decisions.
#' @export
read_group_decisions <- function(
  path = here_rel("data/clean/decisions/group_decisions.csv"),
  summary_data = NULL
) {
  if (!file.exists(path)) {
    stop(
      "No decisions file at ", path, ". ",
      "Run scripts/scaffold_group_decisions.R first."
    )
  }
  key <- triage_group_cols()
  decisions <- readr::read_csv(path, show_col_types = FALSE)

  missing_cols <- setdiff(
    c(key, group_decision_human_cols()),
    names(decisions)
  )
  if (length(missing_cols) > 0) {
    stop("Decisions file is missing column(s): ", paste(missing_cols, collapse = ", "))
  }

  decisions <- decisions |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(group_decision_human_cols()),
        ~ dplyr::coalesce(as.character(.x), "")
      )
    )

  bad <- setdiff(unique(decisions$decision), c("", group_decision_levels()))
  if (length(bad) > 0) {
    stop(
      "Unrecognised decision value(s): ",
      paste(sQuote(bad), collapse = ", "),
      ". Permitted: ",
      paste(group_decision_levels(), collapse = ", ")
    )
  }

  # Controlled vocabulary on `notebook`, so a typo fails here rather than quietly
  # creating a fifteenth notebook. This is not hypothetical: hand-typed
  # "Birds & mammals" and scheme-written "Birds and Mammals" split one notebook in
  # two on 2026-07-30, and nothing complained.
  bad_nb <- setdiff(unique(decisions$notebook), c("", notebook_names()))
  if (length(bad_nb) > 0) {
    stop(
      "Unrecognised notebook name(s): ",
      paste(sQuote(bad_nb), collapse = ", "),
      ". Permitted: ",
      paste(notebook_names(), collapse = ", ")
    )
  }

  # A lump with nowhere to lump into is a half-finished thought, not a decision.
  incomplete <- decisions |>
    dplyr::filter(.data$decision == "lump", .data$lump_into == "")
  if (nrow(incomplete) > 0) {
    cli::cli_warn(
      "{nrow(incomplete)} row(s) marked {.val lump} with an empty {.field lump_into}."
    )
  }

  # lump_into now names a GROUP by its id, not a notebook and not free text. An id
  # pointing at nothing is worse than a blank, because it looks decided.
  if ("group_id" %in% names(decisions)) {
    targets_named <- setdiff(unique(decisions$lump_into), "")
    unknown <- setdiff(targets_named, decisions$group_id)
    if (length(unknown) > 0) {
      stop(
        "lump_into names ", length(unknown),
        " unknown group id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", "),
        ". It must be a group_id such as 'G042'."
      )
    }
    self <- decisions$group_id[decisions$lump_into == decisions$group_id &
      decisions$lump_into != ""]
    if (length(self) > 0) {
      stop("Group(s) lumped into themselves: ", paste(self, collapse = ", "))
    }
  }

  if (!is.null(summary_data)) {
    in_data <- do.call(paste, summary_data[key])
    in_file <- do.call(paste, decisions[key])
    n_missing <- sum(!in_data %in% in_file)
    if (n_missing > 0) {
      cli::cli_warn(c(
        "{n_missing} group(s) in the data are absent from {.path {basename(path)}}.",
        "i" = "Re-run scripts/scaffold_group_decisions.R to append them."
      ))
    }
  }

  decisions
}

#' Notebook Assignment Scheme
#'
#' Sam's grouping decision, 2026-07-30, as data rather than as prose. Ten
#' notebooks. Every one of the 245 groups lands in exactly one, and
#' [assign_notebook()] errors if that stops being true.
#'
#' Abiotic groups map on `ENVIRON_COMPARTMENT_SUB`; biota map on `SPECIES_GROUP`
#' and **ignore the `Biota, Aquatic` / `Biota, Terrestrial` split**. That split
#' cannot be the biota axis because Mammals straddle it (17 groups aquatic, 6
#' terrestrial), so "Birds and Mammals" would otherwise be cut across two parent
#' notebooks. It is also a shaky label: *Ursus maritimus* is filed as Terrestrial,
#' which is arguable for a marine mammal.
#'
#' The small abiotic compartments are lumped because a notebook for two
#' measurements is not a notebook: Groundwater has 2 measurements, Porewater 11,
#' Sludge 11.
#'
#' @return A named list with `abiotic` and `biota` lookups.
#' @export
notebook_scheme <- function() {
  list(
    abiotic = c(
      "Freshwater" = "Freshwater",
      "Aquatic Sediment" = "Aquatic Sediment",
      # Brackish takes the marine classification thresholds too; see
      # threshold_compartment_map().
      "Marine/Salt Water" = "Marine and Brackish Water",
      "Brackish/Transitional Water" = "Marine and Brackish Water",
      "Stormwater" = "Other Waters",
      "Wastewater" = "Other Waters",
      "Groundwater" = "Other Waters",
      "Porewater" = "Other Waters",
      "Sludge" = "Other Waters",
      "Soil O Horizon (Organic)" = "Terrestrial Soil"
    ),
    biota = c(
      "Fish" = "Fish",
      "Molluscs" = "Molluscs",
      "Crustaceans" = "Crustaceans and Invertebrates",
      "Invertebrates" = "Crustaceans and Invertebrates",
      "Worms" = "Crustaceans and Invertebrates",
      "Birds" = "Birds and Mammals",
      "Mammals" = "Birds and Mammals",
      "Algae" = "Algae and Plants",
      "Moss/Hornworts" = "Algae and Plants",
      # The fall-through: Zooplankton epilimnion, Bunndyr, and one group with no
      # species group at all. Given a notebook rather than dropped, so the
      # decision to exclude them (if that is the decision) is visible and
      # justified rather than implicit.
      "Ecosystem" = "Unclassified Biota",
      "Other" = "Unclassified Biota"
    )
  )
}

#' Every Permitted Notebook Name
#'
#' The controlled vocabulary for the `notebook` column.
#' @return A character vector, sorted.
#' @export
notebook_names <- function() {
  scheme <- notebook_scheme()
  sort(unique(c(scheme$abiotic, scheme$biota)))
}

#' Assign Each Group to a Notebook
#'
#' Errors rather than returning `NA` for an unmapped group. A silently unassigned
#' group is a group that quietly vanishes from every notebook, which is exactly
#' the failure this layer exists to prevent.
#'
#' @param data A data frame with the group-key columns.
#' @return A character vector of notebook names, one per row.
#' @export
assign_notebook <- function(data) {
  scheme <- notebook_scheme()
  is_biota <- data$ENVIRON_COMPARTMENT == "Biota"

  out <- ifelse(
    is_biota,
    unname(scheme$biota[as.character(data$SPECIES_GROUP)]),
    unname(scheme$abiotic[as.character(data$ENVIRON_COMPARTMENT_SUB)])
  )
  # Biota with no species group at all: the lookup returns NA for an NA key, and
  # that is a real group (9 measurements), not an error.
  out[is_biota & is.na(data$SPECIES_GROUP)] <- "Unclassified Biota"

  if (any(is.na(out))) {
    unmapped <- unique(paste(
      data$ENVIRON_COMPARTMENT[is.na(out)],
      data$ENVIRON_COMPARTMENT_SUB[is.na(out)],
      data$SPECIES_GROUP[is.na(out)],
      sep = " / "
    ))
    stop(
      "No notebook for ", sum(is.na(out)), " group(s): ",
      paste(unmapped, collapse = "; "),
      ". Add them to notebook_scheme()."
    )
  }
  out
}

#' Apply the Notebook Scheme to the Decisions File
#'
#' Fills the `notebook` column mechanically from [notebook_scheme()]. It does not
#' touch `decision`, `lump_into` or `notes`: those are judgement, and the notebook
#' assignment is a filing question.
#'
#' **Only fills blank `notebook` cells** unless `overwrite = TRUE`, so running it
#' after hand-editing cannot destroy an override.
#'
#' An earlier version keyed "should I fill this row?" on `decision` alone, and so
#' overwrote the `lump_into` of a row that had a note but no decision. It now keys
#' on the column it is actually writing.
#'
#' @param path The decisions CSV.
#' @param overwrite Replace non-empty notebook assignments too?
#' @param verbose Report what changed?
#' @return The written tibble, invisibly.
#' @export
apply_notebook_scheme <- function(
  path = here::here("data/clean/decisions/group_decisions.csv"),
  overwrite = FALSE,
  verbose = TRUE
) {
  decisions <- read_group_decisions(path)
  notebook <- assign_notebook(decisions)

  fillable <- if (overwrite) {
    rep(TRUE, nrow(decisions))
  } else {
    decisions$notebook == ""
  }
  n_kept <- sum(!fillable)

  decisions$notebook[fillable] <- notebook[fillable]
  readr::write_csv(decisions, path, na = "")

  if (verbose) {
    message(
      "Assigned ", sum(fillable), " group(s) across ",
      length(unique(decisions$notebook)), " notebooks",
      if (n_kept > 0) {
        paste0("; left ", n_kept, " existing assignment(s) alone")
      } else {
        ""
      }
    )
  }
  invisible(decisions)
}

#' Progress Report on the Decisions
#'
#' What is left to decide, by coverage tier. The Friday target in PLAN.md is
#' every `top90` group decided.
#'
#' @param decisions Output of [read_group_decisions()].
#' @return A tibble of counts per tier.
#' @export
group_decision_progress <- function(decisions) {
  decisions |>
    dplyr::mutate(
      tier = factor(.data$tier, levels = c("top90", "top95", "top99", "tail"))
    ) |>
    dplyr::group_by(.data$tier) |>
    dplyr::summarise(
      groups = dplyr::n(),
      decided = sum(.data$decision != ""),
      undecided = sum(.data$decision == ""),
      measurements = sum(.data$n),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$tier)
}
