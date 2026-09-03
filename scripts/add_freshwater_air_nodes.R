# One-shot: add the non-coastal matrix nodes and shift the layout to make room.
# Sam 2026-08-12. Hand-run, never a target, and not idempotent -- it is a
# recorded edit to hand-edited files, kept in scripts/ so the change is
# reviewable rather than appearing as an unexplained diff.
#
# WHAT IT DOES
#
# 1. Shifts every medium and organism node right by one column (1.3), freeing
#    x = 1.3 for the freshwater column. Source nodes do not move: the column at
#    x = 0 and the row along y = 5 are both sources and both stay put.
#    Total x span is unchanged at 7.8 (the source row already reached it), so
#    aep_diagram_image_size() computes the same card size as before and no
#    edge geometry changes.
# 2. Adds N027 river water column, N028 river benthic sediment, N029 air.
# 3. Boxes the two freshwater nodes.
#
# NO EDGES ARE ADDED. Deliberate, and worth stating: running
# scaffold_aep_edges.R here would propose 11 sources x 3 new media plus 3 new
# media x 13 organisms, i.e. ~72 fresh putative edges, undoing the cut from
# 87 to 38 made earlier the same day. Which flows are real is Sam's judgement
# (CLAUDE.md 5: scientific judgement stays manual), so the nodes arrive
# unconnected and validate_aep_edges() reports them as orphans, which is the
# honest signal rather than a silent gap.
#
# ONE STRUCTURAL NOTE FOR WHOEVER ADDS THOSE EDGES. Every organism node in the
# set is marine (mussel, cod, crab, marine crustaceans/zooplankton/benthic
# inverts/macroalgae, seabird, eider). So the freshwater nodes have no organism
# to flow INTO; their outward flow is river -> coast, which is medium ->
# medium. scaffold_aep_edges.R only proposes strictly downward level pairs, so
# it will never suggest that edge. It has to be typed by hand. Nothing forbids
# it: read_aep_edges() and validate_aep_edges() do not constrain level order.

suppressMessages(pkgload::load_all(quiet = TRUE))
suppressMessages(library(dplyr))

nodes_path <- here::here("data/clean/aep/aep_nodes.csv")
members_path <- here::here("data/clean/aep/aep_node_members.csv")
groups_path <- here::here("data/clean/aep/aep_node_groups.csv")
# One flat file per AEP since 2026-08-27. This script only ever adds A001 rows.
membership_path <- here::here("data/clean/aep/aep_membership_A001.csv")

nodes <- readr::read_csv(nodes_path, show_col_types = FALSE)
stopifnot(!any(c("N027-river-water-column", "N028-river-benthic-sed",
                 "N029-air") %in% nodes$node_id))

# ---- 1. shift media and organisms right ---------------------------------
shift <- nodes$level %in% c("exposure_medium", "internal_exposure") & !is.na(nodes$x)
nodes$x[shift] <- nodes$x[shift] + 1.3

# ---- 2. the new nodes ----------------------------------------------------
blank <- nodes[0, ]
new <- tibble::tibble(
  node_id = c("N027-river-water-column", "N028-river-benthic-sed", "N029-air"),
  label = c("River water", "River sediment", "Air"),
  level = "exposure_medium",
  # Air is external because the pipeline HAS NO AIR DATA: the raw Vannmiljo
  # export carries 1,123 ng/m3 rows and vm_filter_compartments() drops them
  # before anything downstream sees them (CLAUDE.md 9c). Making it empirical
  # would need the compartment filter widened, a mass-per-volume-of-air
  # dimension standardise_measured_units() has never handled, and a full
  # rebuild. Sam 2026-08-12: "air having no data is fine; it should be
  # external."
  node_type = c("empirical", "empirical", "external"),
  x = c(1.3, 1.3, 1.3),
  y = c(2, 1, 0),
  lat_min = NA_real_, lat_max = NA_real_,
  date_min = 1900, date_max = 2100,
  exclude_references = NA_character_, exclude_campaigns = NA_character_,
  drop_outliers = FALSE,
  external_value = NA_real_, external_sd = NA_real_, external_n = NA_real_,
  # Left blank rather than guessed. PRTR reports copper to air by sector
  # (land-based industry ~3,261 kg/yr, Hammerfest LNG 19 kg/yr, PLAN.md 9g),
  # so a magnitude is available, but it is a release TO air rather than a
  # concentration IN it, and which of those this node represents is a
  # modelling decision, not a lookup.
  external_unit = NA_character_,
  essentiality_score = NA_real_, essentiality_justification = NA_character_,
  plausibility_score = NA_real_, plausibility_justification = NA_character_,
  evidence_score = NA_real_, evidence_justification = NA_character_,
  quantification_score = NA_real_, quantification_justification = NA_character_,
  notes = c(
    "Aquatic / Freshwater, River, stream, canal, water column. Group G001-Wfw-Rwc-C: 41,831 measurements, the largest group in the dataset, geometric mean 1.52e-3 mg/L against 8.48e-4 for coastal water.",
    "Aquatic / Aquatic Sediment, River, stream, canal, water benthos. Group G008-Wsd-Rwb-Md: 857 measurements, geometric mean 20.5 mg/kg dry, which is indistinguishable from coastal sediment at 20.6.",
    "No measured data: air is dropped by vm_filter_compartments() before the pipeline sees it. Scores and magnitude to be filled by hand. See PLAN.md 9g on PRTR copper to air."
  )
)
# Fill any column this script does not name, so a later schema change does not
# silently produce short rows.
for (nm in setdiff(names(blank), names(new))) new[[nm]] <- blank[[nm]][NA_integer_]
new <- new[, names(nodes)]

nodes <- bind_rows(nodes, new)
readr::write_excel_csv(nodes, nodes_path, na = "")

# ---- 3. group memberships (which sampling groups resolve each node) ------
members <- readr::read_csv(members_path, show_col_types = FALSE)
members <- bind_rows(members, tibble::tibble(
  node_id = c("N027-river-water-column", "N028-river-benthic-sed"),
  group_id = c("G001-Wfw-Rwc-C", "G008-Wsd-Rwb-Md"),
  notes = c("River/stream/canal water column", "River/stream/canal sediment")
))
readr::write_excel_csv(members, members_path, na = "")

# ---- 4. the dashed box ---------------------------------------------------
# Group boxes are already dashed and grey for every group (aep_group_layers(),
# linetype "42"), so this needs no styling, only a row. Air is deliberately
# NOT a member: it is not part of the freshwater system, it just sits above it.
groups <- readr::read_csv(groups_path, show_col_types = FALSE)
groups <- bind_rows(groups, tibble::tibble(
  group_key = "freshwater",
  label = "Freshwater",
  node_ids = "N027-river-water-column;N028-river-benthic-sed",
  notes = "Riverine compartments, upstream of the coastal system."
))
readr::write_excel_csv(groups, groups_path, na = "")

# ---- 5. AEP membership ---------------------------------------------------
# A001 (national) only. A002 Repparfjorden and A003 Sorfjorden both have
# freshwater inside their boxes (90 river measurements in Repparfjorden alone,
# PLAN.md 9g), so they are plausible members, but adding a node to a case-study
# AEP is a scoping decision and those two also want their own evidence and
# quantification scores in their own aep_membership_A00x.csv. Left for Sam.
membership <- readr::read_csv(membership_path, show_col_types = FALSE)
add <- tibble::tibble(
  aep_id = "A001",
  node_id = c("N027-river-water-column", "N028-river-benthic-sed", "N029-air"),
  notes = "Added 2026-08-12 with the non-coastal matrix nodes."
)
for (nm in setdiff(names(membership), names(add))) {
  add[[nm]] <- membership[[nm]][NA_integer_]
}
membership <- bind_rows(membership, add[, names(membership)])
readr::write_excel_csv(membership, membership_path, na = "")

message("nodes: ", nrow(nodes), " | members: ", nrow(members),
        " | groups: ", nrow(groups), " | membership: ", nrow(membership))
