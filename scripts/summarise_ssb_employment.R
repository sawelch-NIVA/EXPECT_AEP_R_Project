# How much of Norway's extractive-sector workforce sits in Hammerfest?
#
# Pulls register-based employment (employed persons per 4th quarter, by PLACE OF
# WORK) from Statistics Norway's PxWeb API and expresses Hammerfest kommune as a
# share of the national total, per industry. The headline for the manuscript's
# study-area section is Hammerfest's share of "Mining and quarrying" (SN2007
# section B, divisions 05-09), which bundles offshore oil & gas extraction (06),
# metal-ore mining (07) and mining support services (09).
#
# DELIBERATELY NOT A TARGET. This is a one-off descriptive statistic for
# index.qmd (Materials & Methods > Study area); nothing downstream reads it.
# Same contract as scripts/summarise_prtr_emissions.R.
#
# Two SSB tables, because they trade detail against disclosure:
#   07984  section-level industry (05-09 as one code). Hammerfest's mining &
#          quarrying total IS disclosed here -- this is the table the paper uses.
#   08536  division-level industry (06, 07, 08, 09 separately). At kommune
#          resolution almost every Hammerfest extractive cell is ":" (suppressed
#          for confidentiality, i.e. too few establishments). Fetched only so the
#          suppression is on the record rather than a surprise.
#
# Kommune-number history matters: Hammerfest is 2004 up to 2019, 5406 for
# 2020-2023 (Troms og Finnmark), 5603 from 2024 (Finnmark again). ham_code()
# picks the code that applies to each year so the series is continuous.
#
# Run:
#   Rscript scripts/summarise_ssb_employment.R
#
# Writes data/clean/derived/ssb_employment_hammerfest.csv (a REPORT, not a
# pipeline input). Raw value strings are kept alongside the numerics so a ":"
# (suppressed) or "0" (code not in use that year) stays visible.

suppressMessages({
  library(jsonlite)
  library(curl)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})

here::i_am("README.md")

api <- "https://data.ssb.no/api/v0/en/table/"

ssb_fetch <- function(table, query) {
  h <- curl::new_handle()
  curl::handle_setopt(h, copypostfields = jsonlite::toJSON(query, auto_unbox = TRUE))
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  r <- curl::curl_fetch_memory(paste0(api, table), handle = h)
  if (r$status_code != 200L) {
    stop("SSB ", table, " returned HTTP ", r$status_code, ": ", rawToChar(r$content))
  }
  d <- utils::read.csv(
    text = rawToChar(r$content), colClasses = "character",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  names(d)[ncol(d)] <- "raw"
  d$value <- suppressWarnings(as.numeric(d$raw))
  d
}

sel <- function(code, values) {
  list(code = code, selection = list(filter = "item", values = as.list(values)))
}

years <- as.character(2015:2025)
ham_code <- function(y) ifelse(y <= 2019L, "2004", ifelse(y <= 2023L, "5406", "5603"))

# --- 07984: section-level, the numbers the paper reports --------------------
nace7 <- c(
  "00-99" = "All industries",
  "05-09" = "Mining and quarrying",
  "10-33" = "Manufacturing",
  "35-39" = "Electricity, water supply, sewerage, waste"
)
d7 <- ssb_fetch("07984", list(
  query = list(
    sel("Region", c("0", "5603", "5406", "2004")),
    sel("NACE2007", names(nace7)),
    sel("Kjonn", "0"),
    sel("Alder", "15-74"),
    sel("ContentsCode", "SysselsatteArb"), # by place of work
    sel("Tid", years)
  ),
  response = list(format = "csv3")
))

d7 <- d7 |>
  mutate(year = as.integer(.data$Tid)) |>
  filter(.data$Region == "0" | .data$Region == ham_code(.data$year)) |>
  mutate(who = if_else(.data$Region == "0", "norway", "hammerfest"))

shares <- d7 |>
  select(year, nace = NACE2007, who, raw, value) |>
  pivot_wider(names_from = who, values_from = c(raw, value)) |>
  transmute(
    year,
    nace,
    industry = unname(nace7[nace]),
    contents = "Employed persons by place of work",
    norway_raw = raw_norway,
    hammerfest_raw = raw_hammerfest,
    norway = value_norway,
    hammerfest = value_hammerfest,
    hammerfest_share_pct = if_else(
      !is.na(value_norway) & !is.na(value_hammerfest) & value_norway > 0,
      round(100 * value_hammerfest / value_norway, 3), NA_real_
    ),
    source_table = "SSB 07984"
  ) |>
  arrange(nace, year)

# --- 08536: division detail within Mining & quarrying (mostly suppressed) ---
nace8 <- c(
  "06" = "Extraction of oil and natural gas",
  "07" = "Mining of metal ores",
  "08" = "Other mining and quarrying",
  "09" = "Mining support service activities"
)
d8 <- ssb_fetch("08536", list(
  query = list(
    sel("Region", c("0", "5603", "5406", "2004")),
    sel("NACE2007", names(nace8)),
    sel("Kjonn", "0"),
    sel("ContentsCode", "SysselArbsted"), # by place of work
    sel("Tid", years)
  ),
  response = list(format = "csv3")
))

detail <- d8 |>
  mutate(year = as.integer(.data$Tid)) |>
  filter(.data$Region == "0" | .data$Region == ham_code(.data$year)) |>
  mutate(who = if_else(.data$Region == "0", "norway", "hammerfest")) |>
  select(year, nace = NACE2007, who, raw, value) |>
  pivot_wider(names_from = who, values_from = c(raw, value)) |>
  transmute(
    year,
    nace,
    industry = unname(nace8[nace]),
    contents = "Employed persons by place of work",
    norway_raw = raw_norway,
    hammerfest_raw = raw_hammerfest,
    norway = value_norway,
    hammerfest = value_hammerfest,
    hammerfest_share_pct = if_else(
      !is.na(value_norway) & !is.na(value_hammerfest) & value_norway > 0,
      round(100 * value_hammerfest / value_norway, 3), NA_real_
    ),
    source_table = "SSB 08536"
  ) |>
  arrange(nace, year)

out <- bind_rows(shares, detail)

dest <- here("data/clean/derived/ssb_employment_hammerfest.csv")
readr::write_csv(out, dest)
message("wrote ", dest, " (", nrow(out), " rows)")

latest <- max(shares$year)
cat("\n--- ", latest, ", employed persons by place of work (SSB 07984) ---\n", sep = "")
shares |>
  filter(year == latest) |>
  mutate(across(c(norway, hammerfest), ~ formatC(.x, format = "d", big.mark = ","))) |>
  select(industry, norway, hammerfest, hammerfest_share_pct) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n--- Mining and quarrying (B): Hammerfest share of Norway, 2015-", latest, " ---\n", sep = "")
shares |>
  filter(nace == "05-09") |>
  select(year, norway, hammerfest, hammerfest_share_pct) |>
  as.data.frame() |>
  print(row.names = FALSE)
