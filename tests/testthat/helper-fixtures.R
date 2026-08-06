# Shared test fixtures.
#
# In a helper- file rather than inside a test file, because testthat sources
# helpers for EVERY run including `filter=` runs, while a test file is sourced
# only when it matches. Defining these in test-fct_aep_nodes.R meant
# test-fct_node_cards.R passed in a full run and failed under a filter, which is
# the worst way round: green locally, red in CI.

node_fixture <- function(...) {
  base <- tibble::tibble(
    node_id = "N001",
    label = "Test node",
    level = "medium",
    node_type = "empirical",
    x = 0, y = 1,
    lat_min = NA_real_, lat_max = NA_real_,
    date_min = as.Date(NA), date_max = as.Date(NA),
    exclude_references = NA_character_,
    exclude_campaigns = NA_character_,
    drop_outliers = FALSE,
    external_value = NA_real_, external_sd = NA_real_,
    external_n = NA_real_, external_unit = NA_character_,
    essentiality_score = 3, essentiality_justification = "x",
    plausibility_score = 3, plausibility_justification = "x",
    evidence_score = 2, evidence_justification = "x",
    quantification_score = 2, quantification_justification = "x",
    notes = NA_character_
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

ids_fixture <- function() {
  tibble::tibble(
    ENVIRON_COMPARTMENT = c("Aquatic", "Aquatic", "Biota"),
    ENVIRON_COMPARTMENT_SUB = c("Freshwater", "Freshwater", "Biota, Aquatic"),
    SPECIES_GROUP = c(NA, NA, "Fish"),
    SAMPLE_SPECIES = c(NA, NA, "Gadus morhua"),
    SAMPLE_TISSUE = c(NA, NA, "Liver"),
    SITE_GEOGRAPHIC_FEATURE = c("River, stream, canal", "Lake, pond, pool, reservoir", "Coastal, fjord"),
    SITE_GEOGRAPHIC_FEATURE_SUB = c("Water column, pelagic zone", "Water column, pelagic zone", "Not reported"),
    MEASURED_UNIT_STANDARD = c("mg/L", "mg/L", "mg/kg (wet)"),
    group_id = c("G001", "G002", "G003")
  )
}

data_fixture <- function() {
  ids <- ids_fixture()
  # 10 rows per group, latitudes straddling the Arctic Circle, two references.
  purrr::list_rbind(purrr::map(seq_len(nrow(ids)), function(i) {
    row <- ids[i, ]
    tibble::tibble(
      ENVIRON_COMPARTMENT = row$ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB = row$ENVIRON_COMPARTMENT_SUB,
      SPECIES_GROUP = row$SPECIES_GROUP,
      SAMPLE_SPECIES = row$SAMPLE_SPECIES,
      SAMPLE_TISSUE = row$SAMPLE_TISSUE,
      SITE_GEOGRAPHIC_FEATURE = row$SITE_GEOGRAPHIC_FEATURE,
      SITE_GEOGRAPHIC_FEATURE_SUB = row$SITE_GEOGRAPHIC_FEATURE_SUB,
      MEASURED_UNIT_STANDARD = row$MEASURED_UNIT_STANDARD,
      MEASURED_VALUE_STANDARD = seq(1, 10) * i,
      MEASURED_N = 1L,
      LATITUDE = seq(60, 78, length.out = 10),
      LONGITUDE = 10,
      SAMPLING_DATE = seq(as.Date("2010-01-01"), by = "year", length.out = 10),
      REFERENCE_ID = rep(c("RefA", "RefB"), 5),
      # Deliberately crossed with REFERENCE_ID rather than nested, so a test
      # excluding a campaign cannot pass by accidentally excluding a reference.
      CAMPAIGN_NAME_SHORT = rep(c("Camp X (a)", "Camp X (a)", "Camp Y (b)"), length.out = 10)
    )
  }))
}

members_fixture <- function(group_ids = "G001", node_id = "N001") {
  tibble::tibble(
    node_id = node_id,
    group_id = group_ids,
    notes = NA_character_
  )
}

summary_fixture <- function() {
  ids <- ids_fixture()
  dplyr::bind_cols(
    ids |> dplyr::select(-"group_id"),
    tibble::tibble(
      n = c(1000, 500, 100),
      n_sources = 1L,
      species_common_name = NA_character_,
      flag_multimodal = FALSE,
      flag_outliers = FALSE
    )
  )
}
