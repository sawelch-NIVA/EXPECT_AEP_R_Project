# Fill data/clean/decisions/group_decisions.csv from the notebook scheme (PLAN.md P2.3).
#
#   Rscript scripts/apply_notebook_scheme.R
#
# Mechanical: it applies notebook_scheme() and nothing else. It will not overwrite
# a decision that is already filled in, so it is safe to re-run after hand
# editing. Review the result as a git diff rather than by reading 245 rows.
#
# Run scripts/scaffold_group_decisions.R first if new data has arrived, so that
# every current group has a row to be assigned.

suppressMessages({
  library(targets)
  library(here)
  library(dplyr)
})
here::i_am("Readme.md")
pkgload::load_all(quiet = TRUE)

decisions <- apply_notebook_scheme(here("data/clean/decisions/group_decisions.csv"))

message("\nGroups and measurements per notebook:")
decisions |>
  group_by(notebook) |>
  summarise(
    groups = n(),
    species = n_distinct(SAMPLE_SPECIES[!is.na(SAMPLE_SPECIES)]),
    measurements = sum(n),
    .groups = "drop"
  ) |>
  arrange(desc(measurements)) |>
  as.data.frame() |>
  print()

message("\nRemaining undecided, by coverage tier:")
print(as.data.frame(group_decision_progress(decisions)))
