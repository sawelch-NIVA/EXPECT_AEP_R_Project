# One-off: rewrite group_id from bare "G014" to composite "G014-Bf-Cnr-..."
# (2026-08-08).
#
#   Rscript scripts/migrate_group_ids_to_composite.R
#
# Run by hand, once. This is NOT part of the normal group_ids.csv contract
# (append-only, existing values never change -- see R/fct_group_ids.R's
# header) and must never become a target: it is a deliberate one-time
# identity change, made because Sam decided the composite form is worth
# using as the real id everywhere rather than a display-only label layered
# on top of it (see the "let's see" conversation, 2026-08-08).
#
# The composite VALUE can still change later if a group's compartment,
# geography, species or tissue gets hand-corrected -- this script does not
# make the id live-derived, it freezes today's composite form into the
# ledger, exactly once, the same way allocate_group_ids() freezes a bare
# number. Re-running this script is NOT idempotent in that sense: running it
# twice would try to re-derive composite codes from what are now already
# composite ids and mangle them. It is intentionally not written to guard
# against that -- if you need to re-run it, restore group_ids.csv from git
# first.
#
# Rewrites, in this order:
#   1. data/clean/decisions/group_ids.csv       -- group_id (the ledger itself)
#   2. data/clean/decisions/group_decisions.csv -- group_id and lump_into
#   3. data/clean/aep/aep_nodes.csv              -- bare "Gnnn" mentions in notes
#
# Does NOT touch docs/groups/*.qmd (headings, anchors, image paths) or
# _targets.R's must_include -- those need their own pass, since the qmd files
# also need the hand-written-vs-generated callout restructuring done at the
# same time, and are not safe to touch mechanically here.

suppressMessages({
  library(here)
  library(dplyr)
  library(readr)
})
here::i_am("Readme.md")
pkgload::load_all(quiet = TRUE)

ledger_path <- here("data/clean/decisions/group_ids.csv")
decisions_path <- here("data/clean/decisions/group_decisions.csv")
aep_nodes_path <- here("data/clean/aep/aep_nodes.csv")

# ---- 1. The ledger itself ------------------------------------------------

ledger <- read_group_ids(ledger_path)
old_ids <- ledger$group_id
new_ids <- withCallingHandlers(
  format_composite_group_id(ledger),
  warning = function(w) {
    message("Expected (see misc-todo.md items 13-14): ", conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)

if (anyDuplicated(new_ids) > 0) {
  stop(
    "Refusing to write: composite ids are not unique. Duplicated: ",
    paste(unique(new_ids[duplicated(new_ids)]), collapse = ", ")
  )
}

id_map <- setNames(new_ids, old_ids)

ledger$group_id <- new_ids
write_csv(ledger, ledger_path, na = "")

n_composite <- sum(new_ids != old_ids)
message(
  "group_ids.csv: ", n_composite, "/", length(old_ids),
  " ids composited, ", length(old_ids) - n_composite, " left bare (no compartment/geography code yet)"
)

# ---- 2. group_decisions.csv ----------------------------------------------

decisions <- read_csv(decisions_path, show_col_types = FALSE)
unknown <- setdiff(decisions$group_id, names(id_map))
if (length(unknown) > 0) {
  stop(
    "group_decisions.csv has group_id(s) not in the ledger: ",
    paste(unknown, collapse = ", ")
  )
}
decisions$group_id <- unname(id_map[decisions$group_id])
# lump_into is free text naming another group_id (validated in
# validate_group_decisions()); remapped the same way, entry by entry, in
# case it is ever populated before this script is next needed.
decisions$lump_into <- ifelse(
  decisions$lump_into %in% names(id_map),
  unname(id_map[decisions$lump_into]),
  decisions$lump_into
)
write_csv(decisions, decisions_path, na = "")
message("group_decisions.csv: group_id and lump_into remapped.")

# ---- 3. aep_nodes.csv notes -----------------------------------------------

aep_nodes <- read_csv(aep_nodes_path, show_col_types = FALSE)
remap_notes <- function(notes) {
  for (old in names(id_map)) {
    notes <- gsub(paste0("\\b", old, "\\b"), id_map[[old]], notes)
  }
  notes
}
n_before <- sum(!is.na(aep_nodes$notes) & grepl("\\bG[0-9]{3}\\b", aep_nodes$notes))
aep_nodes$notes <- ifelse(
  is.na(aep_nodes$notes), aep_nodes$notes, remap_notes(aep_nodes$notes)
)
write_csv(aep_nodes, aep_nodes_path, na = "")
message("aep_nodes.csv: ", n_before, " notes cell(s) had bare group id mentions, remapped.")

message("\nDone. Still needed: docs/groups/*.qmd anchors/links/image paths, and _targets.R's must_include.")
