# Scaffold or refresh data/clean/group_decisions.csv (PLAN.md P2.2).
#
# Run this by hand, not from the pipeline. The pipeline READS the decisions file
# and must never write it; writing a hand-edited file from a target is how
# somebody's afternoon of judgement gets silently overwritten by a rebuild.
#
#   Rscript scripts/scaffold_group_decisions.R
#
# Safe to re-run at any time. Machine-derived context (n, coverage, flags) is
# refreshed; `decision`, `lump_into` and `notes` are never touched once set. Run
# it whenever new data arrives, or whenever read_group_decisions() warns that
# groups in the data are missing from the file.

suppressMessages({
  library(targets)
  library(here)
})
here::i_am("Readme.md")
pkgload::load_all(quiet = TRUE)

summary_data <- tar_read(summarise_literature_data)

decisions <- scaffold_group_decisions(
  summary_data,
  path = here_rel("data/clean/group_decisions.csv")
)

message("\nWhat is left to decide, by coverage tier:")
print(as.data.frame(group_decision_progress(decisions)))

message(
  "\nThe Friday target (PLAN.md P2.2) is every `top90` group decided.\n",
  "Read docs/NBXX-Sample-Groups.qmd, work down from the largest n, and fill in\n",
  "the `decision` column: ",
  paste(group_decision_levels(), collapse = " / "),
  "."
)
