# Stable identifiers for sample groups (2026-07-30).
#
# A group is defined by an eight-column key, which is unusable as a reference: you
# cannot write it in a note, a commit message, or a `lump_into` cell. This gives
# every group a short accession number instead.
#
# THE PROPERTY THAT MATTERS IS STABILITY. These IDs end up in hand-written notes
# and, eventually, in the manuscript. An ID whose value depends on what else is in
# the set silently re-points every existing reference when the data change. That
# rules out anything derived from rank or row order: rank moves whenever `n`
# moves, so a rank-derived `G001` means Freshwater today and something else after
# the next Vannmiljø pull, with nothing erroring.
#
# So IDs are ALLOCATED ONCE AND STORED, never recomputed. The ledger is the
# authority; the code only appends to it. Same cache-versus-curation split as the
# decisions file: `data/clean/decisions/group_ids.csv` is append-only identity, and
# `group_decisions.csv` is judgement.
#
# Deliberately a separate file from the decisions. Identity and judgement have
# different lifecycles, and keeping them apart means the summary table and the
# triage notebook can carry IDs without depending on whether anyone has made a
# decision yet.

#' Format a Group ID
#'
#' `G` prefix so IDs are greppable and cannot be mistaken for `n` or `rank`.
#' Zero-padded to three digits so they sort lexically; 999 is ample headroom
#' against the current 245 groups.
#'
#' @param i Integer vector.
#' @return A character vector.
#' @export
format_group_id <- function(i) {
  sprintf("G%03d", as.integer(i))
}

#' Read the Group ID Ledger
#'
#' @param path Path to the ledger CSV.
#' @return A tibble of the group key plus `group_id`, or a zero-row tibble.
#' @export
read_group_ids <- function(
  path = here::here("data/clean/decisions/group_ids.csv")
) {
  key <- triage_group_cols()
  if (!file.exists(path)) {
    empty <- as.data.frame(
      stats::setNames(rep(list(character(0)), length(key)), key)
    )
    return(tibble::as_tibble(cbind(empty, group_id = character(0))))
  }
  ids <- readr::read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(c(key, "group_id"), names(ids))
  if (length(missing_cols) > 0) {
    stop(
      "ID ledger at ", path, " is missing column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (anyDuplicated(ids$group_id) > 0) {
    stop("ID ledger contains duplicate group_id values.")
  }
  ids
}

#' Allocate IDs to Any Groups That Lack One
#'
#' Append-only. Existing IDs are never changed and **retired IDs are never
#' reused**: the next ID is always one past the highest ever issued, not one past
#' the highest currently in use. Reuse is how a note written in March ends up
#' pointing at a different group in September.
#'
#' Allocation order for a fresh ledger is by `n` descending, which is arbitrary
#' but harmless because it is frozen immediately.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @param path Path to the ledger CSV.
#' @param verbose Report what changed?
#' @return The full ledger, invisibly.
#' @export
allocate_group_ids <- function(
  summary_data,
  path = here::here("data/clean/decisions/group_ids.csv"),
  verbose = TRUE
) {
  key <- triage_group_cols()
  ledger <- read_group_ids(path)

  wanted <- summary_data |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::select(dplyr::all_of(key)) |>
    dplyr::distinct()

  have <- do.call(paste, ledger[key])
  need <- wanted[!do.call(paste, wanted[key]) %in% have, , drop = FALSE]

  if (nrow(need) > 0) {
    highest <- if (nrow(ledger) == 0) {
      0L
    } else {
      max(as.integer(sub("^G", "", ledger$group_id)))
    }
    need$group_id <- format_group_id(highest + seq_len(nrow(need)))
    ledger <- dplyr::bind_rows(ledger, need)
    readr::write_csv(ledger, path, na = "")
  }

  retired <- sum(!have %in% do.call(paste, wanted[key]))
  if (verbose) {
    message(
      "group_ids.csv: ", nrow(ledger), " IDs (",
      nrow(need), " newly allocated, ", retired,
      " retired but kept)"
    )
  }
  invisible(ledger)
}

#' Attach Group IDs to a Table
#'
#' Left join on the full group key, with an assertion that the row count did not
#' change: a ledger with duplicate keys would otherwise multiply the data
#' silently.
#'
#' @param data Any table carrying the group-key columns.
#' @param ids The ledger, from [read_group_ids()].
#' @param warn_missing Warn about groups with no ID yet?
#' @return `data` with a `group_id` column.
#' @export
attach_group_ids <- function(data, ids, warn_missing = TRUE) {
  key <- triage_group_cols()
  n_before <- nrow(data)

  out <- data |>
    dplyr::left_join(
      ids |> dplyr::select(dplyr::all_of(key), "group_id"),
      by = key
    )

  if (nrow(out) != n_before) {
    stop(
      "attach_group_ids() changed the row count from ", n_before, " to ",
      nrow(out), ": the ID ledger has duplicate group keys."
    )
  }
  if (warn_missing && any(is.na(out$group_id))) {
    cli::cli_warn(c(
      "{sum(is.na(out$group_id))} group(s) have no ID yet.",
      "i" = "Run scripts/allocate_group_ids.R to append them."
    ))
  }
  out
}
