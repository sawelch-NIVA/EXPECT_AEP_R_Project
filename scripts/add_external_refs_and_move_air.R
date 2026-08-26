# One-shot, Sam 2026-08-12/13. Two edits to aep_nodes.csv:
#
# 1. Adds the `external_refs` column (see external_value_cols()) and sets it to
#    1 for N004-N011, the REACH sector nodes, which all come from one extract.
#    Left blank elsewhere: N003 mine tailings still wants a figure from Sternal
#    or Pedersen, and N001 and N029 have no source at all yet, so a 1 there
#    would be a claim rather than a record.
#
# 2. Moves N029-air to sit ABOVE both water columns rather than below the
#    river sediment. Sam: "Air, logically enough, should be positioned above
#    river water and coastal water column."
#
#    x = 1.95 is deliberately OFF the 1.3 grid: it is the midpoint of the
#    freshwater column (1.3) and the coastal column (2.6), so air sits above
#    both rather than above one and diagonal to the other. Checked against
#    aep_diagram_image_size(), which measures the minimum gap WITHIN each
#    y-row: row y = 3 becomes x = {0, 1.95, 3.9, 5.2}, whose smallest gap is
#    1.3 (3.9 to 5.2), unchanged. So the cards do not shrink.

suppressMessages(pkgload::load_all(quiet = TRUE))
suppressMessages(library(dplyr))

path <- here::here("data/clean/aep/aep_nodes.csv")
nodes <- readr::read_csv(path, show_col_types = FALSE)

reach <- c(
  "N004-manufacturing",
  "N005-agriculture-forestry-and-fishing",
  "N006-mining-and-quarrying",
  "N007-other-services-and-administration",
  "N008-wholesale-and-retail-trade",
  "N009-construction-and-real-estate",
  "N010-transportation-and-storage",
  "N011-water-supply-and-waste-management"
)
stopifnot(all(reach %in% nodes$node_id))

if (!"external_refs" %in% names(nodes)) {
  nodes$external_refs <- NA_real_
}
nodes$external_refs[nodes$node_id %in% reach] <- 1

# Keep the column next to its siblings rather than appended at the end, so the
# file still reads as grouped by concern when opened in a spreadsheet.
nodes <- nodes |>
  relocate("external_refs", .after = "external_unit")

nodes$x[nodes$node_id == "N029-air"] <- 1.95
nodes$y[nodes$node_id == "N029-air"] <- 3

readr::write_excel_csv(nodes, path, na = "")

# Verify against the real readers rather than trusting the write.
n <- read_aep_nodes()
p <- n[!is.na(n$x) & !is.na(n$y), ]
cat("external_refs set on:", sum(!is.na(n$external_refs)), "nodes\n")
cat("air at: x =", n$x[n$node_id == "N029-air"],
    " y =", n$y[n$node_id == "N029-air"], "\n")
cat("river water y =", n$y[n$node_id == "N027-river-water-column"],
    "| coastal water y =", n$y[n$node_id == "N013-coast-water-column"], "\n")
cat("image_size:", round(aep_diagram_image_size(p, image_size = 0.19), 4),
    "(was 0.0769)\n")
