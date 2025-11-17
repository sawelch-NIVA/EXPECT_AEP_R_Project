library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)


import_norske_utslipp <- function(path) {
  emission_data <- readxl::read_xlsx(path = path, col_names = FALSE)

  # Find first row with "År" in any column
  year_row <- emission_data |>
    dplyr::mutate(row_num = dplyr::row_number()) |>
    dplyr::filter(dplyr::if_any(
      dplyr::everything(),
      ~ stringr::str_detect(.x, "År")
    )) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(row_num)

  # Slice from that row to end
  emission_data <- emission_data |>
    dplyr::slice(year_row:nrow(emission_data)) |>
    # Extract first row as new column names ----
    janitor::row_to_names(row_number = 1, remove_row = TRUE)
}

# households
norske_utslipp_households <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_households.xlsx"
) |>
  mutate(
    Year = År,
    emission = as.numeric(`Utslipp til luft`),
    unit = "kg",
    compartment = "air",
    source = "Households",
    .keep = "none"
  )

# land industry
norske_utslipp_land_industry <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_land_industry.xlsx"
) |>
  mutate(
    Year = År,
    emission = as.numeric(`Årlig utslipp til vann`),
    unit = "kg",
    compartment = "water",
    source = "Terrestrial industry",
    .keep = "none"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# landfills
norske_utslipp_landfills <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_landfills.xlsx"
) |>
  mutate(
    Year = År,
    emission = as.numeric(`Årlig utslipp til vann`),
    unit = "kg",
    compartment = "water",
    source = "Landfills",
    .keep = "none"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# marine oil
norske_utslipp_oil_marine <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_oil_marine.xlsx"
) |>
  mutate(
    Year = År,
    water = as.numeric(`Årlig utslipp til vann`),
    air = as.numeric(`Årlig utslipp til luft`),
    underground = as.numeric(`Årlig utslipp til undergrunn`),
    unit = "kg",
    source = "Marine Oil & Gas",
    .keep = "none"
  ) |>
  tidyr::pivot_longer(
    cols = c(water, air, underground),
    values_to = "emission",
    names_to = "compartment"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# products
norske_utslipp_products <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_products.xlsx"
) |>
  mutate(
    Year = År,
    water = as.numeric(`Utslipp til vann`),
    air = as.numeric(`Utslipp til luft`),
    soil = as.numeric(`Utslipp til jord`),
    unit = "kg",
    source = "Products",
    .keep = "none"
  ) |>
  tidyr::pivot_longer(
    cols = c(water, air, soil),
    values_to = "emission",
    names_to = "compartment"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# transport
norske_utslipp_transport <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_transport.xlsx"
) |>
  mutate(
    Year = År,
    emission = as.numeric(`Utslipp til luft`),
    unit = "kg",
    source = "Transport",
    compartment = "air",
    .keep = "none"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# various sectors
norske_utslipp_various_sectors <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_various_sectors.xlsx"
) |>
  mutate(
    Year = År,
    emission = as.numeric(`Utslipp til luft`),
    unit = "kg",
    source = "various_sectors",
    compartment = "air",
    .keep = "none"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# water treatment
norske_utslipp_water_treatment <- import_norske_utslipp(
  path = "data/raw/norske_utslipp/norske_utslipp_water_treatment.xlsx"
) |>
  mutate(
    Year = År,
    emission = as.numeric(`Årlig utslipp til vann`),
    unit = "kg",
    source = "Wastewater Treatment Plants",
    compartment = "water",
    .keep = "none"
  ) |>
  group_by(Year, compartment, source, unit) |>
  summarise(emission = sum(emission))

# summarise
total_norske_utslipp_emissions <- bind_rows(
  norske_utslipp_households,
  norske_utslipp_land_industry,
  norske_utslipp_landfills,
  norske_utslipp_oil_marine,
  norske_utslipp_products,
  norske_utslipp_transport,
  norske_utslipp_water_treatment
) |>
  filter(compartment != "underground") |>
  mutate(Year = as.numeric(Year))

ggplot(
  data = total_norske_utslipp_emissions,
  mapping = aes(x = Year, y = emission, color = source, group = source)
) +
  geom_point() +
  geom_path() +
  scale_y_log10() +
  facet_wrap(facets = vars((compartment)), ncol = 1)

ggplot(
  data = total_norske_utslipp_emissions,
  mapping = aes(x = Year, y = emission / 1000, fill = source, group = source)
) +
  geom_col() +
  facet_wrap(
    facets = vars((compartment)),
    ncol = 1,
    scales = "free_y",
    shrink = FALSE
  ) +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5), limits = c(1985, 2025)) +
  scale_fill_hue(name = "Reporting Source") +
  theme_ipsum() +
  labs(
    x = "Year",
    y = "reported emissions (mg)",
    title = "Emissions of Copper (7440-50-8) reported to Norske Utslipp",
    subtitle = "Classified by reporting source and receiving compartment."
  )
