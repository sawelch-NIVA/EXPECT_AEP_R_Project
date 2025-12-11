library(dm)
library(targets)
source("_targets.R")

# from https://dm.cynkra.com/articles/howto-dm-df.html

literature_merged_data <- tar_read(load_literature_pqt)
literature_reference_data <- tar_read(reference_data)
literature_campaign_data <- tar_read(campaign_data)
literature_sites_data <- tar_read(sites_data)
literature_measurements_data <- tar_read(measurements_data)
literature_qc <- tar_read(data_quality_report)
wgs84_geo <- tar_read(wgs84_geography)

eData_no_keys <- dm(
  literature_reference_data,
  literature_measurements_data,
  literature_sites_data,
  literature_campaign_data
)

eData_no_keys

dm_enum_pk_candidates(
  dm = eData_no_keys,
  table = literature_sites_data
)
# add a primary key to a named table
eData_pk <- eData_no_keys |>
  dm_add_pk(table = literature_sites_data, column = SITE_CODE) |>
  dm_add_pk(table = literature_measurements_data, column = SAMPLE_ID)

# set it up as a foreign key
dm_enum_fk_candidates(
  dm = eData_no_keys,
  table = literature_sites_data,
  ref_table = literature_measurements_data
)
