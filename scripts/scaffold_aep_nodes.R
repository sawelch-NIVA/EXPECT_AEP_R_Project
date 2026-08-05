# Scaffold the AEP node layer (PLAN.md P3.3). Hand-run, never a target.
#
# Writing a hand-edited file from a target is how an afternoon of judgement gets
# silently overwritten by a rebuild. Same reasoning, and the same append-only
# contract, as scripts/scaffold_group_decisions.R.
#
# WHAT THIS DOES AND DOES NOT DO
#
# It creates the two files if they are absent, with a worked example row so the
# schema is legible rather than a header line. If they already exist it leaves
# them completely alone and just reports where the node layer has got to.
#
# It does NOT propose nodes. Which groups belong together is the scientific
# judgement this whole layer exists to record (CLAUDE.md working agreements:
# "Automating the ranking of which groups deserve scrutiny is welcome.
# Automating the decision about what to lump, split, or drop is not.").
#
# Usage:
#   Rscript scripts/scaffold_aep_nodes.R

suppressMessages(pkgload::load_all(quiet = TRUE))

nodes_path <- here::here("data/clean/aep_nodes.csv")
members_path <- here::here("data/clean/aep_node_members.csv")

# ---- Nodes ---------------------------------------------------------------

if (file.exists(nodes_path)) {
  message("aep_nodes.csv exists, leaving it alone.")
} else {
  # One worked example, drawn from the marine node in docs/NBXX-algae.qmd so the
  # columns are filled with something recognisable rather than with placeholders.
  # Its scores and justifications are Sam's own, transcribed from
  # tbl-epeq-marine. Delete the row or edit it; it is a template, not data.
  example <- tibble::tibble(
    node_id = "N001",
    label = "Marine water",
    level = "medium",
    node_type = "empirical",
    x = 0,
    y = 3,
    lat_min = NA_real_,
    lat_max = NA_real_,
    date_min = as.Date(NA),
    date_max = as.Date(NA),
    exclude_references = NA_character_,
    drop_outliers = FALSE,
    # Blank, and they must stay blank on an empirical node: its magnitude is
    # computed from its member groups. read_aep_nodes() stops if they are set
    # here. See external_value_cols().
    external_value = NA_real_,
    external_sd = NA_real_,
    external_n = NA_real_,
    external_unit = NA_character_,
    essentiality_score = 3,
    essentiality_justification = paste(
      "Copper is known to be naturally present in the earth's crust and",
      "aquatic ecosystems."
    ),
    plausibility_score = 3,
    plausibility_justification = "As above.",
    evidence_score = 3,
    evidence_justification = paste(
      "The studied dataset contains substantial evidence on copper",
      "concentrations in marine water."
    ),
    quantification_score = 2,
    quantification_justification = paste(
      "Copper kinetics in marine environments are generally well studied and",
      "modelled."
    ),
    notes = "Example row transcribed from docs/NBXX-algae.qmd tbl-epeq-marine."
  )
  readr::write_csv(example, nodes_path, na = "")
  message("Wrote ", nodes_path, " with 1 example node.")
}

# ---- Membership ----------------------------------------------------------

if (file.exists(members_path)) {
  message("aep_node_members.csv exists, leaving it alone.")
} else {
  ids <- read_group_ids()
  # The Marine/Salt Water groups, so the example node resolves to real data on a
  # first run and the reader can see what a membership row looks like.
  marine <- ids |>
    dplyr::filter(
      .data$ENVIRON_COMPARTMENT_SUB == "Marine/Salt Water",
      .data$MEASURED_UNIT_STANDARD == "mg/L"
    ) |>
    dplyr::slice_head(n = 1)

  members <- tibble::tibble(
    node_id = "N001",
    group_id = if (nrow(marine) > 0) marine$group_id[1] else NA_character_,
    notes = "Example membership row."
  )
  readr::write_csv(members, members_path, na = "")
  message("Wrote ", members_path, " with ", nrow(members), " membership row(s).")
}

# ---- Where the node layer has got to -------------------------------------

nodes <- read_aep_nodes(nodes_path)
members <- read_aep_node_members(members_path, nodes = nodes)
ids <- read_group_ids()
data <- targets::tar_read(literature_analysis_ready)
summary_data <- targets::tar_read(summarise_literature_data)

cards <- aep_node_report_cards(nodes, members, data, ids)
validate_aep_nodes(nodes, members, cards)

coverage <- node_coverage(members, summary_data, ids)
cov <- node_coverage_summary(coverage)

message("")
message("Nodes: ", nrow(nodes), " (", sum(nodes$node_type == "empirical"),
        " empirical, ", sum(nodes$node_type == "external"), " external)")
message("Groups claimed: ", cov$groups_claimed, " of ", cov$groups,
        "  |  measurements claimed: ", cov$pct_measurements_claimed, "%")
message("")
message("Largest unclaimed groups:")
print(
  as.data.frame(
    coverage |>
      dplyr::filter(!.data$claimed) |>
      dplyr::select("group_id", "n", "tier", "ENVIRON_COMPARTMENT_SUB",
                    "SAMPLE_SPECIES", "SAMPLE_TISSUE", "MEASURED_UNIT_STANDARD") |>
      head(15)
  ),
  row.names = FALSE
)
message("")
message("Pick a node, add its groups to aep_node_members.csv, score it in ",
        "aep_nodes.csv.")
