# Record which rows each unit correction touches. Hand-run, never a target.
#
# The pipeline requires every correction's selector to match exactly the
# `row_ids` recorded beside it (see R/fct_unit_corrections.R). This script is
# what puts them there: you write the selector and the reasoning, run this, and
# it resolves the selector against the current data and fills the cell.
#
# It is NOT part of the pipeline on purpose. Resolving in the pipeline would
# make the recorded ids track the data automatically, which destroys the entire
# point: the check only detects drift because the ids are a snapshot of what you
# reviewed, frozen at the moment you reviewed it.
#
# Usage:
#   Rscript scripts/scaffold_unit_corrections.R            # fill blanks only
#   Rscript scripts/scaffold_unit_corrections.R --refresh  # re-record all
#
# --refresh is how you accept drift the pipeline has flagged, AFTER looking at
# what changed. Reach for it only when the build has told you what it would
# rewrite, never as a first move.

suppressMessages(pkgload::load_all(quiet = TRUE))
suppressMessages(library(dplyr))

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
path <- here::here("data/clean/unit_corrections.csv")

if (!file.exists(path)) {
  write_unit_corrections_template(path)
  message("Created empty ", path, ". Add corrections, then re-run.")
  quit(save = "no")
}

corrections <- read_unit_corrections(path)
if (nrow(corrections) == 0) {
  message("No corrections in ", path, ". Nothing to record.")
  quit(save = "no")
}

# The UNCORRECTED table, deliberately: selectors are written against what the
# source says, and value bounds in particular would not survive being resolved
# against already-corrected values.
data <- targets::tar_read(load_literature_pqt)
ids <- targets::tar_read(group_ids)

todo <- if (refresh) {
  seq_len(nrow(corrections))
} else {
  which(vapply(corrections$row_ids, function(x) length(split_row_ids(x)) == 0, logical(1)))
}

if (length(todo) == 0) {
  message("Every correction already records its rows. Use --refresh to re-record.")
  quit(save = "no")
}

for (i in todo) {
  this <- corrections[i, ]
  cid <- this$correction_id[1]
  hit <- match_unit_correction(data, this, ids = ids)

  if (!any(hit)) {
    warning(cid, " matches no rows. Left blank; check the selector.", call. = FALSE)
    next
  }

  matched <- sort(as.character(data$row_id[hit]))
  previous <- sort(split_row_ids(this$row_ids[1]))

  corrections$row_ids[i] <- paste(matched, collapse = ";")

  vals <- data$MEASURED_VALUE_STANDARD[hit]
  message(sprintf(
    "%s: %d row(s), %d measurement(s), values %.4g to %.4g, factor %g",
    cid, length(matched), sum(data$MEASURED_N[hit], na.rm = TRUE),
    min(vals, na.rm = TRUE), max(vals, na.rm = TRUE), this$factor[1]
  ))

  if (length(previous) > 0 && !identical(matched, previous)) {
    message(
      "  re-recorded: +", length(setdiff(matched, previous)),
      " / -", length(setdiff(previous, matched))
    )
  }
}

readr::write_csv(corrections, path, na = "")
message("\nWrote ", path, ". Review the diff before rebuilding.")
