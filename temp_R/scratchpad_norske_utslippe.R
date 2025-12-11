library(readxl)
library(dplyr)
library(esquisse)

copper_industrial_emissions <- read_excel(
  "data/raw/norske_utslipp/norske_utslipp_copper_land_industries_individ.xlsx",
  skip = 2 # Skips first 2 rows, starts reading from row 3
) %>%
  rename(
    facility_name = Anleggsnavn,
    county = Fylke,
    municipality = Kommune,
    nace_code = `NACE Kode`,
    year = År,
    annual_emission_air = `Årlig utslipp til luft`,
    basis_air = `Grunnlagsverdi for luft`,
    annual_emission_water = `Årlig utslipp til vann`,
    basis_water = `Grunnlagsverdi for vann`,
    unit = Enhet,
    facility_id = AnleggId,
    org_number = `Org.nr.`
  )

# lots of NAs?
copper_industrial_emissions_categorized <- copper_industrial_emissions |>
  mutate(
    missing_pattern = case_when(
      is.na(annual_emission_air) &
        is.na(annual_emission_water) ~ "Both missing",
      is.na(annual_emission_air) ~ "Air missing",
      is.na(annual_emission_water) ~ "Water missing",
      TRUE ~ "Complete data"
    )
  )

# Summarize by year and missing pattern
emissions_summary <- copper_industrial_emissions_categorized |>
  group_by(year, missing_pattern) |>
  reframe(count = n())

# Create the plot
ggplot(emissions_summary, aes(x = year, y = count, fill = missing_pattern)) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Complete data" = "#2E7D32",
      "Air missing" = "#FB8C00",
      "Water missing" = "#1976D2",
      "Both missing" = "#C62828"
    )
  ) +
  labs(
    title = "Industrial Copper Emissions Data Completeness Over Time",
    x = "Year",
    y = "Number of Records",
    fill = "Data Status"
  ) +
  theme_minimal()

copper_industrial_emissions_by_year_municipality <-
  copper_industrial_emissions |>
  group_by(municipality, year) |>
  reframe(
    emissions_kg_per_year_air = sum(annual_emission_air, na.rm = TRUE),
    emissions_kg_per_year_water = sum(annual_emission_water, na.rm = TRUE)
  )

# Get top 10 municipalities by total emissions (air + water combined)
top_municipalities <- copper_industrial_emissions |>
  group_by(municipality) |>
  summarise(
    total_emissions = sum(
      annual_emission_air,
      annual_emission_water,
      na.rm = TRUE
    )
  ) |>
  slice_max(total_emissions, n = 10) |>
  pull(municipality)

# Filter and reshape data for plotting
emissions_plot_data <- copper_industrial_emissions_by_year_municipality |>
  filter(municipality %in% top_municipalities) |>
  tidyr::pivot_longer(
    cols = starts_with("emissions_kg"),
    names_to = "emission_type",
    values_to = "kg_per_year"
  ) |>
  mutate(
    emission_type = case_when(
      emission_type == "emissions_kg_per_year_air" ~ "Air",
      emission_type == "emissions_kg_per_year_water" ~ "Water"
    )
  )

# Create faceted plot
top_municipalities_plot <- ggplot(
  emissions_plot_data,
  aes(x = year, y = kg_per_year, color = emission_type)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~municipality, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Air" = "#FB8C00", "Water" = "#1976D2")) +
  labs(
    title = "Top 10 Municipalities: Copper Emissions Over Time",
    x = "Year",
    y = "Emissions (kg/year)",
    color = "Emission Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
