# Scaffold the AEP edge layer (PLAN.md P4.1). Hand-run, never a target.
#
# Proposes every DOWNWARD flow between placed nodes, all defaulting to
# `putative`, and appends only pairs not already in the file.
#
# WHY DOWNWARD ONLY, AND WHY THAT IS A PROPOSAL NOT A DECISION.
#
# `level` orders the pathway: source -> medium -> organism -> tse. A flow that
# runs down that order is plausible enough to be worth a row; one that runs back
# up (biota returning copper to sediment, say) is real but is a judgement Sam
# should make deliberately rather than find pre-typed. So this proposes the
# downward set as a starting grid and never removes or reverses anything.
#
# Proposing every downward pair is deliberately over-generous. P4.2's time-box
# works by having the candidate list in front of you and crossing edges off, not
# by remembering which ones you meant to consider.
#
# Usage:
#   Rscript scripts/scaffold_aep_edges.R

suppressMessages(pkgload::load_all(quiet = TRUE))
suppressMessages(library(dplyr))

path <- here::here("data/clean/aep/aep_edges.csv")

nodes <- read_aep_nodes()
placed <- nodes |> filter(!is.na(x), !is.na(y))

if (nrow(placed) < 2) {
  stop(
    "Fewer than two nodes have x/y coordinates. ",
    "Place them in aep_nodes.csv first: the edge grid is derived from `level`, ",
    "and an unplaced node cannot be drawn."
  )
}

rank <- setNames(seq_along(aep_node_levels()), aep_node_levels())

candidates <- tidyr::expand_grid(
  from = placed$node_id,
  to = placed$node_id
) |>
  filter(from != to) |>
  left_join(placed |> select(from = node_id, from_level = level), by = "from") |>
  left_join(placed |> select(to = node_id, to_level = level), by = "to") |>
  filter(rank[from_level] < rank[to_level]) |>
  select(from, to)

existing <- if (file.exists(path)) {
  read_aep_edges(path, nodes = nodes)
} else {
  empty_aep_edges()
}

new_pairs <- candidates |>
  anti_join(existing |> select(from, to), by = c("from", "to"))

if (nrow(new_pairs) == 0) {
  message("No new node pairs to propose. ", nrow(existing), " edge(s) on file.")
} else {
  labels <- setNames(placed$label, placed$node_id)
  n_existing <- nrow(existing)

  additions <- new_pairs |>
    mutate(
      edge_id = sprintf("E%03d", n_existing + row_number()),
      label = paste(labels[from], "to", labels[to]),
      # EVERY edge starts putative. Marking one empirical is a positive act
      # requiring a citation, not the default state. PLAN.md Phase 4.
      status = "putative",
      magnitude = NA_real_,
      magnitude_unit = NA_character_,
      magnitude_n = NA_real_,
      magnitude_sd = NA_real_,
      essentiality_score = NA_real_,
      essentiality_justification = NA_character_,
      plausibility_score = NA_real_,
      plausibility_justification = NA_character_,
      evidence_score = NA_real_,
      evidence_justification = NA_character_,
      quantification_score = NA_real_,
      quantification_justification = NA_character_,
      notes = NA_character_
    ) |>
    select(all_of(names(empty_aep_edges())))

  out <- bind_rows(existing, additions)
  readr::write_csv(out, path, na = "")
  message(
    "Proposed ", nrow(additions), " new edge(s); ",
    nrow(out), " on file."
  )
}

edges <- read_aep_edges(path, nodes = nodes)
validate_aep_edges(edges, placed)

message("")
print(as.data.frame(aep_edge_progress(edges)), row.names = FALSE)
message("")
message("Work down the putative edges. For each, spend at most ~30 minutes ",
        "looking for support (PLAN.md P4.2):")
message("  found     -> set status = empirical, score it, cite it in ",
        "evidence_justification")
message("  not found -> leave it putative, write one sentence in notes on what ",
        "evidence would settle it")
