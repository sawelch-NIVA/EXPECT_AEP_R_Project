getwd()

# read env con
Vm_2025_Q1 <- readxl::read_excel(
  path = "data/raw/vannmiljo/Vm_Output_Data_Q1_2025.xlsx",
  sheet = 1,
  guess_max = 10000,
  # n_max = 100
) |>
  # remove columns that don't contain useful info
  janitor::remove_empty(which = "cols") |>
  dplyr::select(
    !contains(c("Not relevant")),
    !MEASURED_REFERENCE_ID,
    !STRESSOR_NAME,
    !CAS_RN,
    !ENTERED_BY,
    !LOD_VALUE
  )

# read sites
vm_sites <- readxl::read_excel(
  path = "data/raw/vannmiljo/complete_dataset_20250226_1807.xlsx",
  sheet = 2,
  guess_max = 100,
  # n_max = 100
) |>
  # remove columns that don't contain useful info
  janitor::remove_empty(which = "cols") |>
  dplyr::select(
    !contains(c("Not relevant")),
    !OCEAN_IHO,
    !ENTERED_BY,
    !ENTERED_DATE
  )


vm_campaigns <- readr::read_csv(
  file = "data/raw/vannmiljo/Vm_lookup_campaigns_SAW_2024.csv"
) |>
  dplyr::mutate(
    CAMPAIGN_NAME = paste0("Vm_", CAMPAIGN),
    CAMPAIGN_DESCRIPTION_EN,
    .keep = "none"
  )

# missing campaigns?
library(dplyr)
camp_missing <- vm_env_con |>
  count(CAMPAIGN, sort = TRUE) |>
  dplyr::left_join(
    y = vm_campaigns,
    by = c("CAMPAIGN" = "CAMPAIGN_NAME")
  ) |>
  filter(is.na(CAMPAIGN_DESCRIPTION_EN))
camp_missing
if (nrow(camp_missing) > 0) {
  warning("Some campaigns are missing descriptions.")
}


vm_joined <- dplyr::left_join(
  x = vm_env_con,
  y = vm_sites,
  by = "SITE_CODE"
) |>
  dplyr::left_join(
    y = vm_campaigns,
    by = c("CAMPAIGN" = "CAMPAIGN_NAME")
  ) |>
  dplyr::mutate(SAMPLE_DATE = lubridate::dmy(SAMPLE_DATE))

# missing important fields?
vm_joined_na <- vm_joined |>
  filter(if_any(c(ENVIRON_COMPARTMENT, ENVIRON_COMPARTMENT_SUB), is.na))
# wtf?

arrow::write_parquet(vm_joined, sink = "data/clean/vm_copper_to_2024.parquet")
