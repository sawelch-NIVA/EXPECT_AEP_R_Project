# Allocate stable IDs to any sample groups that lack one (2026-07-30).
#
#   Rscript scripts/allocate_group_ids.R
#
# Run by hand, not from the pipeline, for the same reason as the decisions
# scaffold: the ledger is the authority for what a reference means, and a target
# that rewrites it on every build could silently re-point IDs already written into
# notes.
#
# Append-only and safe to re-run. Existing IDs never change; retired IDs are never
# reused. Run it whenever `read_group_ids()` warns that groups have no ID.

suppressMessages({
  library(targets)
  library(here)
  library(dplyr)
})
here::i_am("Readme.md")
pkgload::load_all(quiet = TRUE)

ledger <- allocate_group_ids(
  tar_read(summarise_literature_data),
  path = here("data/clean/decisions/group_ids.csv")
)

message("\nFirst few, by allocation order:")
ledger |>
  head(8) |>
  transmute(
    group_id,
    group = substr(paste(
      ENVIRON_COMPARTMENT_SUB,
      coalesce(SAMPLE_SPECIES, ""),
      coalesce(SAMPLE_TISSUE, ""),
      sep = " / "
    ), 1, 46),
    unit = MEASURED_UNIT_STANDARD
  ) |>
  as.data.frame() |>
  print()
