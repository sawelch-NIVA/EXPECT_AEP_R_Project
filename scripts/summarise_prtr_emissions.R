# Aggregate the Norwegian PRTR (norske utslipp) and REACH declaration files into
# numbers that can be transcribed into aep_nodes.csv as `external` nodes.
#
# DELIBERATELY NOT A TARGET, per PLAN.md section 10, which defers pipeline
# integration of the emissions and REACH data until after submission. PLAN.md
# P3.6 makes the matching point about the WoE work: those assessments are already
# written as prose in docs/NBXX-norske-utslipp.qmd and need TRANSCRIBING, not
# re-deriving. Same contract as scripts/scaffold_unit_corrections.R.
#
# The reading and aggregation live in R/fct_prtr_emissions.R rather than here,
# because docs/NBXX-emissions-prtr.qmd needs them too and two copies of a
# spreadsheet reshaping are two chances to reshape it differently.
#
# Run:
#   Rscript scripts/summarise_prtr_emissions.R
#
# Writes data/clean/derived/prtr_emissions_summary.csv, which is a REPORT rather than an
# input: nothing in the pipeline reads it. For the plotted version, which is what
# you actually want before deciding on nodes, render
# docs/NBXX-emissions-prtr.qmd.

suppressMessages({
  pkgload::load_all(quiet = TRUE)
  library(dplyr)
  library(readr)
  library(here)
})

here::i_am("README.md")
emissions_dir <- here("inst/extdata/emissions")

prtr <- read_prtr_long(emissions_dir)

# INCOMPLETE YEARS ARE EXCLUDED, not merely flagged. See prtr_complete_years():
# the most recent year is routinely partial because facilities report on a lag,
# and a partial year enters the mean as a low-emission year. Land-based industry
# reported 47 kg to water in 2024 against roughly 7,000 kg in 2023.
incomplete <- prtr_complete_years(prtr) |> filter(!.data$complete)

national <- summarise_prtr_releases(prtr, c("source_category", "medium")) |>
  mutate(scope = "National", region = NA_character_, .before = 1)

by_fylke <- summarise_prtr_releases(prtr, c("fylke", "source_category", "medium")) |>
  rename(region = "fylke") |>
  mutate(scope = "Fylke", .before = 1)

# The A002 bounding box sits inside Hammerfest kommune, which absorbed Kvalsund
# in the 2020 municipal reform. Kommune rather than a coordinate join, because
# the PRTR files carry no coordinates and the kommune-to-point join is unbuilt
# (PLAN.md section 10).
hf <- filter_prtr_kommune(prtr)

repparfjorden <- summarise_prtr_releases(hf, c("facility", "source_category", "medium")) |>
  rename(region = "facility") |>
  mutate(scope = "Repparfjorden (Hammerfest kommune)", .before = 1)

repparfjorden_total <- summarise_prtr_releases(hf, "medium") |>
  mutate(
    scope = "Repparfjorden (Hammerfest kommune)",
    region = "All facilities",
    source_category = "All",
    .before = 1
  )

out <- bind_rows(national, by_fylke, repparfjorden, repparfjorden_total) |>
  relocate("scope", "region", "source_category", "medium")

path <- here("data/clean/derived/prtr_emissions_summary.csv")

# write_excel_csv(), NOT write_csv(). Facility names carry Norwegian characters
# ("deponi for borekaks på ulveryggen") and readr writes valid UTF-8 with no BOM,
# which Excel on Windows reads as CP1252 and renders as "pÃ¥". The file was never
# corrupt; it only looked it, which is the worse failure because there is nothing
# to find when you go looking. write_excel_csv() prepends a UTF-8 BOM.
#
# Same family as the micro-sign trap in CLAUDE.md 4.4.-2: CSV plus Windows plus
# Excel will mangle a non-ASCII character eventually. Anything written for a
# human to open in Excel should use write_excel_csv().
write_excel_csv(out, path)
cat("Wrote", path, "with", nrow(out), "rows.\n")

if (nrow(incomplete) > 0) {
  cat("\n--- Years EXCLUDED as incomplete reporting ---\n")
  print(
    as.data.frame(incomplete |> select(-"complete")),
    row.names = FALSE, digits = 4
  )
}

cat("\n--- National releases, kg/yr (annual total, incomplete years dropped) ---\n")
print(
  as.data.frame(
    national |>
      select(
        "source_category", "medium", "total_kg_yr", "sd_total_kg_yr",
        "n_facilities", "n_years", "n_dropped", "year_min", "year_max",
        "mean_kg_yr"
      )
  ),
  row.names = FALSE, digits = 4
)

cat("\n--- Repparfjorden (Hammerfest kommune), kg/yr ---\n")
print(
  as.data.frame(
    bind_rows(repparfjorden, repparfjorden_total) |>
      select("region", "source_category", "medium", "total_kg_yr", "n_years")
  ),
  row.names = FALSE, digits = 4
)

# ---------------------------------------------------------------------------
# REACH: copper in commerce, tonnes per year. A DIFFERENT KIND OF NUMBER from
# everything above; do not add it to a release total.
# ---------------------------------------------------------------------------

reach <- read_reach_declarations(file.path(emissions_dir, "REACH_copper_prtd.xlsx"))
ry <- reach_complete_years(reach)

cat("\n--- REACH declared copper, net tonnes/yr (commerce, not release) ---\n")
print(as.data.frame(ry), row.names = FALSE, digits = 5)

ok <- ry |> filter(.data$complete)
cat(
  "\nPlausibly complete years (", paste(ok$year, collapse = ", "), ") mean: ",
  format(mean(ok$tonnes), big.mark = ",", digits = 5), " tonnes/yr (sd ",
  format(stats::sd(ok$tonnes), big.mark = ",", digits = 4), ")\n",
  sep = ""
)
cat(
  "Years ", paste(ry$year[!ry$complete], collapse = ", "),
  " sit far below the median and are a STEP, not a trend.",
  " Check reporting completeness before using any mean across them.\n",
  sep = ""
)
