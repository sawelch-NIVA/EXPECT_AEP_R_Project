# Propose PRTR source nodes for the AEP node layer. Hand-run, never a target.
#
# Same append-only contract as scripts/scaffold_aep_nodes.R and
# scripts/scaffold_group_decisions.R: it APPENDS rows for sources not yet
# present, and never rewrites or reorders a row that already exists. Writing a
# hand-edited file from a target is how an afternoon of judgement gets silently
# overwritten by a rebuild.
#
# WHAT IT FILLS AND WHAT IT DELIBERATELY LEAVES BLANK
#
# Filled: node_id, label, level, node_type, and the four external_* columns,
#   because those are arithmetic over data/clean/derived/prtr_emissions_summary.csv and
#   re-deriving them by hand would only introduce transcription errors.
#
# Blank: every EPEQ score and justification, and x/y. Those are the judgement
#   this layer exists to record. CLAUDE.md is explicit that automating the
#   ranking of what deserves scrutiny is welcome and automating the decision is
#   not, and a scaffolded score would be indistinguishable from a real one.
#
# THE MAGNITUDE IS AN ANNUAL TOTAL, in kg/yr: the sum across facilities within a
# year, averaged over reported years. NOT the mean facility-year value, which is
# 25x smaller for land-based industry and would make the source look negligible.
# See the header of scripts/summarise_prtr_emissions.R.
#
# Usage:
#   Rscript scripts/summarise_prtr_emissions.R   # refresh the aggregates first
#   Rscript scripts/scaffold_prtr_nodes.R
#   Rscript scripts/scaffold_prtr_nodes.R --aep A002   # regional sources only

suppressMessages({
  pkgload::load_all(quiet = TRUE)
  library(dplyr)
  library(readr)
  library(here)
})

here::i_am("README.md")

args <- commandArgs(trailingOnly = TRUE)
want_aep <- if ("--aep" %in% args) args[which(args == "--aep") + 1] else NULL

summary_path <- here("data/clean/derived/prtr_emissions_summary.csv")
nodes_path <- here("data/clean/aep/aep_nodes.csv")
membership_path <- here("data/clean/aep/aep_membership.csv")

if (!file.exists(summary_path)) {
  stop(
    "No PRTR summary at ", summary_path,
    ". Run scripts/summarise_prtr_emissions.R first."
  )
}

prtr <- read_csv(summary_path, show_col_types = FALSE)
nodes <- read_aep_nodes(nodes_path)

# ---- Which sources are worth a node -------------------------------------
#
# National sector totals, and the Repparfjorden facility aggregate. Facilities
# individually are too fine for an AEP node: five Hammerfest facilities summing
# to 0.6 kg/yr is one source, not five, and five near-identical cards would
# crowd the diagram without adding a distinction anyone would use.

proposed <- bind_rows(
  prtr |>
    filter(.data$scope == "National") |>
    transmute(
      aep_id = "A001",
      label = paste0(.data$source_category, " releases to ", tolower(.data$medium)),
      external_value = .data$total_kg_yr,
      external_sd = .data$sd_total_kg_yr,
      external_n = .data$n_years,
      external_unit = "kg/yr",
      provenance = paste0(
        "Norwegian PRTR (norske utslipp), ", .data$n_facilities,
        " facilities, ", .data$year_min, "-", .data$year_max,
        ". Annual national total, averaged over reported years."
      )
    ),
  prtr |>
    filter(
      grepl("^Repparfjorden", .data$scope),
      .data$region == "All facilities"
    ) |>
    transmute(
      aep_id = "A002",
      label = paste0("Reported point sources to ", tolower(.data$medium)),
      external_value = .data$total_kg_yr,
      external_sd = .data$sd_total_kg_yr,
      external_n = .data$n_years,
      external_unit = "kg/yr",
      provenance = paste0(
        "Norwegian PRTR (norske utslipp), Hammerfest kommune, ",
        .data$n_facilities, " facilities, ", .data$year_min, "-",
        .data$year_max,
        ". EXCLUDES the historic submarine tailings deposit, which is not a ",
        "reported release. See the separate STD node."
      )
    )
)

if (!is.null(want_aep)) {
  proposed <- proposed |> filter(.data$aep_id == want_aep)
}

# Matched on LABEL, because node_id is allocated here and so cannot be the key
# on a re-run. A label that already exists is left completely alone, including
# its magnitude: re-running after Sam has revised a number must not undo the
# revision. If a PRTR figure genuinely changes, delete the row and re-run.
already <- proposed$label %in% nodes$label
if (any(already)) {
  message(
    sum(already), " proposed source(s) already in aep_nodes.csv, untouched: ",
    paste(proposed$label[already], collapse = "; ")
  )
}
new <- proposed[!already, , drop = FALSE]

if (nrow(new) == 0) {
  message("Nothing to add. aep_nodes.csv already has every proposed PRTR node.")
} else {
  # Continue the existing numbering rather than restarting. read_aep_nodes()
  # refuses duplicate ids, so a collision would abort the next build.
  existing_n <- suppressWarnings(as.integer(sub("^N", "", nodes$node_id)))
  next_n <- max(c(0L, existing_n[!is.na(existing_n)])) + 1L
  new$node_id <- sprintf("N%03d", seq(next_n, length.out = nrow(new)))

  rows <- tibble::tibble(
    node_id = new$node_id,
    label = new$label,
    level = "source",
    # external, not empirical: there is no measured concentration in this
    # dataset to compute from. The magnitude is a release rate carried in from
    # an assessment made elsewhere. See aep_node_types().
    node_type = "external",
    # BLANK ON PURPOSE. A node with no coordinates is not drawn, so nothing
    # appears on a figure until it has been placed deliberately.
    x = NA_real_,
    y = NA_real_,
    lat_min = NA_real_,
    lat_max = NA_real_,
    date_min = NA_character_,
    date_max = NA_character_,
    exclude_references = NA_character_,
    exclude_campaigns = NA_character_,
    drop_outliers = FALSE,
    external_value = new$external_value,
    external_sd = new$external_sd,
    external_n = new$external_n,
    external_unit = new$external_unit,
    essentiality_score = NA_real_,
    essentiality_justification = NA_character_,
    plausibility_score = NA_real_,
    plausibility_justification = NA_character_,
    evidence_score = NA_real_,
    evidence_justification = NA_character_,
    quantification_score = NA_real_,
    quantification_justification = NA_character_,
    notes = new$provenance
  )

  # Read the raw file rather than reusing `nodes`, so the dates Sam typed come
  # back out in the form he typed them. read_aep_nodes() converts a bare year
  # to a Date, and writing that back would rewrite "1900" as "1900-01-01" on
  # every existing row: a diff across the whole file for no reason.
  raw <- read_csv(nodes_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  out <- bind_rows(raw, mutate(rows, across(everything(), as.character)))

  # write_excel_csv() throughout: these carry Norwegian facility names, and
  # readr's BOM-less UTF-8 renders as mojibake in Excel on Windows.
  write_excel_csv(out, nodes_path, na = "")
  message("Appended ", nrow(rows), " source node(s) to aep_nodes.csv:")
  for (i in seq_len(nrow(rows))) {
    message(
      "  ", rows$node_id[i], "  ", rows$label[i], "  ",
      signif(rows$external_value[i], 4), " ", rows$external_unit[i]
    )
  }

  # Membership too, or the node exists and appears in no AEP. N008 was added by
  # hand today and hit exactly that: a node with no membership row and no x/y is
  # invisible on every figure and silent about it.
  if (file.exists(membership_path)) {
    mem_raw <- read_csv(
      membership_path, show_col_types = FALSE,
      col_types = cols(.default = col_character())
    )
    add <- tibble::tibble(aep_id = new$aep_id, node_id = new$node_id)
    for (col in setdiff(names(mem_raw), names(add))) {
      add[[col]] <- NA_character_
    }
    write_excel_csv(
      bind_rows(mem_raw, add[, names(mem_raw)]), membership_path, na = ""
    )
    message("Appended ", nrow(add), " membership row(s).")
  }
}

# ---- What is still missing ----------------------------------------------

message("")
placed <- nodes |> filter(!is.na(.data$x), !is.na(.data$y))
unplaced <- setdiff(nodes$node_id, placed$node_id)
if (length(unplaced) > 0) {
  message(
    "Nodes with no x/y, which will NOT be drawn: ",
    paste(unplaced, collapse = ", ")
  )
}
if (file.exists(membership_path)) {
  mem <- read_aep_membership(membership_path)
  orphan <- setdiff(nodes$node_id, mem$node_id)
  if (length(orphan) > 0) {
    message(
      "Nodes in no AEP at all: ", paste(orphan, collapse = ", ")
    )
  }
}
message(
  "Scores and coordinates are left blank on purpose. Fill them in ",
  "data/clean/aep/aep_nodes.csv and data/clean/aep/aep_membership.csv."
)
