# Scaffold the per-notebook working documents (PLAN.md P2.3).
#
#   Rscript scripts/generate_group_notebooks.R
#
# Run by hand, never from the pipeline. These files hold hand-written prose that
# exists nowhere else, which is exactly what got lost when the fourteen generated
# distributions notebooks were deleted in P0.2.
#
# APPEND-ONLY. Re-running creates any missing file, appends sections for groups
# that have appeared since, and leaves every existing section exactly as written.
# It is safe to run whenever new data or new groups arrive.

suppressMessages({
  library(targets)
  library(here)
  library(dplyr)
})
here::i_am("Readme.md")
pkgload::load_all(quiet = TRUE)

report <- generate_group_notebooks(
  decisions = tar_read(group_decisions),
  groups = tar_read(triage_pilot_groups),
  dir = here("docs/groups"),
  # Statistics come from the summary, not the decisions CSV: they are
  # machine-derived, so joining them here keeps them fresh and keeps the
  # hand-edited file about decisions.
  summary_data = tar_read(summarise_literature_data),
  # The sub-compartment and species-group comparison panels, placed above the
  # groups they compare. Not one per notebook: a lumped notebook such as
  # "Crustaceans and Invertebrates" spans three species groups, and panels split
  # by unit as well, so it gets several.
  overview_nodes = tar_read(triage_overview_node_table),
  species_nodes = tar_read(triage_species_node_table),
  overview_paths = tar_read(triage_overview_plots),
  species_paths = tar_read(triage_species_plots)
)

print(as.data.frame(report[c("notebook", "groups", "appended", "already_present")]))

message(
  "\nWrite your comparisons under '# Comparison', and your per-group calls\n",
  "under each group's '**Verdict:**'. Nothing here is regenerated.\n\n",
  "These are not in the _quarto.yml render list, so a project build ignores\n",
  "them. Render one by hand with: quarto render docs/groups/<file>.qmd"
)
