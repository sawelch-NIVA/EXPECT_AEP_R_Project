# read_emissions.R
# Reads and harmonises all Norwegian copper emission Excel files into a single
# long-format tibble.

library(readxl)
library(tidyverse)
library(stringr)


# ==============================================================================
# 1. Helper functions ----
# ==============================================================================

# Pivot an individual-facility file to long format.
# Handles variable column presence (air, water, sea).
#' @param df  Wide tibble as read from Excel (after skip + rename)
#' @param category  String label for source_category column
pivot_individual <- function(df, category) {
  # Identify emission value columns (those containing "Årlig utslipp")
  emission_cols <- names(df)[str_detect(names(df), "Årlig utslipp til")]

  # Build a named vector: medium label -> column name
  medium_map <- c(
    "luft" = "Årlig utslipp til luft",
    "vann" = "Årlig utslipp til vann",
    "sjø" = "Årlig utslipp til sjø" # present in oil/marine
  )

  # Keep only those that exist in this file
  medium_map <- medium_map[medium_map %in% emission_cols]

  df |>
    select(
      facility_name = any_of("Anleggsnavn"),
      fylke = any_of("Fylke"),
      kommune = any_of("Kommune"),
      year = any_of("År"),
      all_of(unname(medium_map))
    ) |>
    pivot_longer(
      cols = all_of(unname(medium_map)),
      names_to = "medium_raw",
      values_to = "emission_kg"
    ) |>
    mutate(
      source_category = category,
      medium = names(medium_map)[match(medium_raw, medium_map)],
      year = as.integer(year)
    ) |>
    select(-medium_raw) |>
    filter(!is.na(emission_kg))
}

# Read and reshape an aggregate (sector-level) file.
# These have 3–4 junk header rows, then År + one or more medium columns.
#' @param path      Path to xlsx
#' @param category  String label
#' @param media     Named character vector: medium_label -> col name in file
read_aggregate <- function(path, category, media) {
  # Find the real header row: first row where col 1 == "År"
  raw <- read_excel(path, col_names = FALSE)
  header_row <- which(raw[[1]] == "År") - 1

  df <- read_excel(path, skip = header_row, col_names = TRUE)

  # Rename first col to year; remaining cols per media map
  names(df)[1] <- "year"
  for (medium_label in names(media)) {
    col_name <- media[[medium_label]]
    if (col_name %in% names(df)) {
      names(df)[names(df) == col_name] <- medium_label
    }
  }
  df |>
    select(year, any_of(names(media))) |>
    pivot_longer(
      cols = -year,
      names_to = "medium",
      values_to = "emission_kg"
    ) |>
    mutate(
      source_category = category,
      facility_name = NA_character_,
      fylke = NA_character_,
      kommune = NA_character_,
      year = as.integer(year),
      emission_kg = as.numeric(emission_kg) # TODO: This warns on conversion of sentinel strings (e.g. "IT", but could be safer)
    ) |>
    filter(!is.na(emission_kg), !is.na(year))
}


# ==============================================================================
# 2. Read individual-facility files ----
# ==============================================================================

emissions_land_industry <- read_excel(
  "inst/extdata/emissions/norske_utslipp_copper_land_industries_individ.xlsx",
  skip = 1
) |>
  pivot_individual("land_industry")

emissions_landfills <- read_excel(
  "inst/extdata/emissions/norske_utslipp_copper_landfills_individ.xlsx",
  skip = 1
) |>
  pivot_individual("landfill")

emissions_oil_marine <- read_excel(
  "inst/extdata/emissions/norske_utslipp_oil_marine.xlsx",
  skip = 1
) |>
  pivot_individual("oil_marine")

emissions_water_treatment <- read_excel(
  "inst/extdata/emissions/norske_utslipp_water_treatment.xlsx",
  skip = 1
) |>
  pivot_individual("water_treatment")


# ==============================================================================
# 3. Read aggregate sector files ----
# ==============================================================================

emissions_households <- read_aggregate(
  "inst/extdata/emissions/norske_utslipp_households.xlsx",
  category = "households",
  media = c(luft = "Utslipp til luft")
)

emissions_transport <- read_aggregate(
  "inst/extdata/emissions/norske_utslipp_transport.xlsx",
  category = "transport",
  media = c(luft = "Utslipp til luft")
)

# it's completely unclear what this one represents, so we'll remove it from the final aggregation
emissions_various_sectors <- read_aggregate(
  "inst/extdata/emissions/norske_utslipp_various_sectors.xlsx",
  category = "various_sectors",
  media = c(luft = "Utslipp til luft")
)

emissions_products <- read_aggregate(
  "inst/extdata/emissions/norske_utslipp_products.xlsx",
  category = "products",
  media = c(
    luft = "Utslipp til luft",
    vann = "Utslipp til vann",
    jord = "Utslipp til jord"
  )
)


# ==============================================================================
# 4. Bind into single table ----
# ==============================================================================

copper_emissions <- bind_rows(
  emissions_land_industry,
  emissions_landfills,
  emissions_oil_marine,
  emissions_water_treatment,
  emissions_households,
  emissions_transport,
  # emissions_various_sectors,
  emissions_products
) |>
  select(
    source_category,
    facility_name,
    fylke,
    kommune,
    year,
    medium,
    emission_kg
  ) |>
  arrange(source_category, year) |>
  mutate(
    medium = recode_values(
      medium,
      "vann" ~ "Aquatic",
      "jord" ~ "Terrestrial",
      "luft" ~ "Atmospherice"
    )
  )

copper_emissions_aggregated <- copper_emissions |>
  # aggregate to national level
  reframe(
    .by = c("year", "source_category", "medium"),
    sum_emissions_kg = sum(emission_kg)
  ) |>
  mutate(
    z_score = scale(sum_emissions_kg)[, 1],
    .by = c("source_category", "medium")
  ) |>
  ungroup() |>
  # TODO: This doesn't catch the unlikely changes in products or land_industry
  mutate(outlier = (z_score > 2)) # use z = 2 as a cutoff

copper_emissions_plot <- copper_emissions_aggregated |>
  ggplot(
    mapping = aes(
      x = year,
      y = sum_emissions_kg,
      colour = source_category
    )
  ) +
  geom_point(mapping = aes(alpha = !outlier)) +
  geom_line() +
  facet_grid(rows = vars(medium)) +
  scale_y_log10() +
  scale_colour_brewer(palette = "Set1", name = "Source Category") +
  theme_minimal() +
  labs(
    title = "Emissions of Copper Reported Under REACH By Year and Recieving Compartment",
    subtitle = "1985 - 2024, via norskeutslipp.no. Outliers shown without points",
    x = "Year",
    y = "Reported Emissions (kg)"
  ) +
  theme(strip.placement = "top")


# Aside from some highly suspicious outliers it looks like reported copper emissions to various compartments have more
# or less held steady. it seems reasonable to take averages. of course, we don't know where all these sites are...

copper_emissions_by_source_summarised <- copper_emissions_aggregated |>
  mutate(
    z_score = scale(sum_emissions_kg)[, 1],
    .by = c("source_category", "medium")
  ) |>
  ungroup() |>
  filter(abs(z_score) < 2) |> # use z = 2 as a cutoff
  reframe(
    mean_sum_emissions_kg = mean(sum_emissions_kg),
    sd_sum_emissions_kg = sd(sum_emissions_kg),
    n_years_reported = n(),
    .by = c("source_category", "medium")
  ) |>
  distinct() |>
  arrange(desc(mean_sum_emissions_kg))


copper_emissions_by_source_summarised
#   source_category mean_sum_emissions_kg sd_sum_emissions_kg n_years_reported NACE
#   <chr>                           <dbl>               <dbl>            <int>
# 1 products                      214308.            308064.                53 ?
# 2 various_sectors                26038.              2100.                28 ignore
# 3 transport                      19287.              2696.                28 H
# 4 land_industry                   6889.              4923.                71 C?
# 5 water_treatment                 4935.              1024.                22 E
# 6 oil_marine                       434.               565.                26 ?
# 7 households                       426.                58.4               28 ?
# 8 landfill                         324.               155.                15 E

# TODO:
# Match emissions to fylke/site coordinates
# Translate medium to ENVIRON_COMPARTMENT
# Get average emissions for kommune, country
# Pull out (hopefully) case study regions for focused AEPs
# Make a flow diagram for average copper emissions to compartments per year, from industries
# How do we then link this to concentrations in water? can we get rough volumes for receiving water?
