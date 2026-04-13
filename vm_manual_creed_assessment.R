# # Manual CREED Assessment Script ----
# # For assessing datasets too large for the Shiny app
# #
# # WORKFLOW:
# # 1. Load data from targets
# # 2. Review auto-populated fields (printed to console)
# # 3. Fill in scores and limitations in the CONFIG section below
# # 4. Run the entire script to generate CREED scores
# # 5. Results saved to CSV files

# library(targets)
# library(dplyr)
# library(tibble)
# library(glue)
# library(STOPeData) # Assuming your package functions are here
# library(eDataDRF)

# i_am("README.md")

# campaign_name <- "Vm_2010_2025"
# # 1. LOAD DATA ----
# tar_load(
#   c(
#     vm_edata_campaign,
#     vm_edata_reference,
#     vm_edata_parameters,
#     vm_edata_sites,
#     vm_edata_samples,
#     vm_edata_biota,
#     vm_edata_measurements,
#     vm_edata_methods
#   )
# )

# # 2. MOCK SESSION DATA STRUCTURE ----
# sessiondata <- list(
#   campaignData = vm_edata_campaign,
#   referenceData = vm_edata_reference,
#   parametersData = vm_edata_parameters,
#   sitesData = vm_edata_sites,
#   samplesData = vm_edata_samples,
#   biotaData = vm_edata_biota,
#   measurementsData = vm_edata_measurements,
#   methodsData = vm_edata_methods,
#   compartmentsData = vm_edata_biota # Assuming biota contains compartment info
# )

# # 3. AUTO-POPULATE FIELDS FROM DATA ----
# cat("\n========== AUTO-POPULATED RELEVANCE FIELDS ==========\n\n")
# relevance_autopop <- summarise_CREED_relevance(sessiondata) |>
#   mutate(value = paste(value))
# print(relevance_autopop)

# cat("\n\n========== AUTO-POPULATED RELIABILITY FIELDS ==========\n\n")
# reliability_autopop <- summarise_CREED_reliability(sessiondata)
# print(reliability_autopop)

# # 4. PURPOSE STATEMENT ----
# # Fill this in for copper assessment

# purpose_data <- yaml::read_yaml("data/clean/CREED_Copper_Purpose.yml")

# # 5. CONFIG: YOUR MANUAL ASSESSMENTS ----
# #
# # Fill in scores and limitations for each criterion below.
# # SCORES: Use one of:
# # "Not Met" = 4,
# # "Fully Met" = 1,
# # "Partly Met" = 2,
# # "Not Reported" = 3,
# # "Not Relevant" = 1

# # Auto-populated data is shown in the tibbles above - reference those when
# # writing your limitations/justifications.

# # RELEVANCE CRITERIA ----
# relevance_config <- list(
#   # RV1: Sample Medium/Matrix (Required) ----
#   # "RV1: Was the sampling medium/matrix appropriate for  the given purpose?"
#   RV1 = list(
#     score = "1", # <-- FILL IN: "1", "2", "3", "4", or ""
#     limitations = "Dataset includes many relevant matrices/media." # <-- FILL IN: Your assessment notes
#   ),

#   # RV2: Collection Method/Sample Type (Recommended) ----
#   # "RV2: Was the sample collection method adequate for the given purpose?"
#   RV2 = list(
#     score = "3",
#     limitations = "On the whole, sampling techniques are appropriate and follow (international) standarded.
#     However, the number of techiques marked as UKJENT mean it's not possible to have full confidence in the dataset."
#   ),

#   # RV3: Study Area (Required) ----
#   # "RV3: Were the study area and number of locations sampled  suitable for the given purpose?"
#   RV3 = list(
#     score = "1",
#     limitations = "Some sites have been reported with erroneous coordinates (e.g. 0,0), but the vast majority are well-characterised
#     in terms of location and name."
#   ),

#   # RV4: Site Type (Recommended) ----
#   # "RV4: Was the rationale for selection of sampling locations  provided and is it suitable for the given purpose?"
#   RV4 = list(
#     score = "1",
#     limitations = "Although rationale is not directly addressed for sites, most sites are marked as part of a monitoring campaign (e.g.
#     Rivers contaminated by mines, airports, reference sites, etc.). Full context is not necessarily available (although it may be in reports),
#     but for a dataset of this size and diversity this level of detail is better than might be expected."
#   ),

#   # RV5: Sampling Timespan (Required) ----
#   # "RV5: Were the samples collected over a time scale that was appropriate for the given purpose?"
#   RV5 = list(
#     score = "1",
#     limitations = "Since this dataset covers and extended time period, score has been marked as 1. A more nuanced exploration of whether
#      the same (or reasonably comparable) sites are resampled every year would permit much greater insight (the average site has a sampling
#      range of ~3 days, but without a more detailed exploration of site use and reuse it's difficult to be more precise). Overall, as the
#      sampling period and frequency are far greater than any of the manuscripts reviewed in this assessment, it seems appropriate to give it
#      the highest score."
#   ),

#   # RV6: Sampling Frequency (Required) ----
#   # "RV6: Over the timespan, was the sampling frequency appropriate for the given purpose?"
#   RV6 = list(
#     score = "1",
#     limitations = "See above."
#   ),

#   # RV7: Temporal Conditions (Recommended) ----
#   # "RV7: Were conditions during sampling events documented and relevant for the given purpose?"
#   RV7 = list(
#     score = "4",
#     limitations = "Conditions were likely documented in relevant reported, but without an exhaustive review of 100+ documents this
#     is impossible to determine."
#   ),

#   # RV8: Analyte (Required) ----
#   # "RV8: Was/were the analyte(s) reported appropriate for the given purpose?"
#   RV8 = list(
#     score = "2",
#     limitations = "Yes; copper. However, full data on fractionation not available in data."
#   ),

#   # RV9: Sensitivity/LOD/LOQ (Required) ----
#   # "RV9: The method was sensitive enough for the given purpose"
#   RV9 = list(
#     score = "2",
#     limitations = "The data report a variety of analytical methods including AAS and ICP-MS. Given this variability, Partially met has been selected."
#   ),

#   # RV10: Summary Statistics Type (Recommended) ----
#   # "RV10: The summary statistics provided were appropriate for the given purpose"
#   RV10 = list(
#     score = "4",
#     limitations = "The dataset does not report any use of summary statistics. However, some rows indicate a potential sample size > 1 (Ant_verdier);
#     It is presumed that in this case means were calculated, but a lack of clarity makes it difficult to know."
#   ),

#   # RV11: Supporting Parameters (Recommended) ----
#   # "RV11: All supporting parameters that were needed to achieve the given purpose were provided"
#   RV11 = list(
#     score = "2",
#     limitations = "Again, determining the exact fractionation/bioavailability reported for such a large dataset is difficult. It is assumed that the
#     majority of samples were of total copper."
#   )
# )

# # RELIABILITY CRITERIA ----
# reliability_config <- list(
#   # RB1: Sample Medium/Matrix (Required) ----
#   # Was the sampling medium/matrix reported in detail/appropriate?
#   RB1 = list(
#     score = "4", # <-- FILL IN
#     limitations = "Vannmiljø's MediumID field records some information on matrix, but not to the level of detail specified in CREED.
#     Biota age and sex are rarely reported."
#   ),

#   # RB2: Collection Method/Sample Type (Recommended) ----
#   # Was the sample collection method reported?
#   RB2 = list(
#     score = "2",
#     limitations = "As with RV2, those collection methods that are reported are accredited techniques, but many methods are left as UKJENT."
#   ),

#   # RB3: Sample Handling (Recommended) ----
#   # Was information reported on sample handling?
#   RB3 = list(
#     score = "4",
#     limitations = "No relevant information."
#   ),

#   # RB4: Site Location (Required) ----
#   # Were the site locations reported?
#   RB4 = list(
#     score = "1",
#     limitations = "Site locations are well-characterised by the standards of this assessment."
#   ),

#   # RB5: Date and Time (Required) ----
#   # Were the date and time of sample collection reported?
#   RB5 = list(
#     score = "1",
#     limitations = "Sampling dates are well-characterised, although times are not. Still, this is not anticipated to be a relevant factor;
#     we would expect the vast majority of studies assessed to sample during the day."
#   ),

#   # RB6: Analyte(s) Measured (Required) ----
#   # Was/were the analyte(s) of interest suitably and definitively identified?
#   RB6 = list(
#     score = "2",
#     limitations = "As with sampling techniques, each row is either based on an accredited or an unknown analytical technique."
#   ),

#   # RB7: Limit of Detection and/or Limit of Quantification (Required) ----
#   # Were limits of detection and/or quantification provided?
#   RB7 = list(
#     score = "2",
#     limitations = "Censored data are reported piecemeal, with specific LODs/LOQs reported for 60-90% of rows."
#   ),

#   # RB8: Accreditation/Quality Management System (Required) ----
#   # Were the laboratory and method accredited for all or almost all samples?
#   RB8 = list(
#     score = "4",
#     justification = "Although many datapoints were sampled/analysed using accredited methods, it is not possible
#     to quickly determine if all labs were accreddited." # Note: RB8 uses 'justification' not 'limitations'
#   ),

#   # RB9: Method (Required) ----
#   # Was the method sufficiently described or referenced, such that it can be reproduced if necessary? Was method validation included?
#   RB9 = list(
#     score = "4",
#     limitations = "As with RB8, it is likely that many of the samples used well-described and reproducible methods, but it is
#     not possible to determine this to an acceptable level of detail without reviewing all relevant reports."
#   ),

#   # RB10: Lab Blank Contamination (Recommended) ----
#   # Was method blank contamination assessed with laboratory blanks?
#   RB10 = list(
#     score = "4",
#     limitations = "Question skipped (see RB9)"
#   ),

#   # RB11: Recovery/Accuracy (Recommended) ----
#   # Were method recovery/accuracy and/or uncertainty assessed by recovery of standard reference material (SRM) and/or were lab spike samples assessed?
#   RB11 = list(
#     score = "4",
#     limitations = "Question skipped (see RB9)"
#   ),

#   # RB12: Reproducibility/Precision (Recommended) ----
#   #  Were method reproducibility and/or uncertainty assessed with lab replicates and long-term control recoveries?
#   RB12 = list(
#     score = "4",
#     limitations = "Question skipped (see RB9)"
#   ),

#   # RB13: Field QC (Recommended) ----
#   # Were quality control (QC) samples collected during field sampling (such as field blanks, spikes, replicates) to demonstrate the method performance for a given field study?
#   RB13 = list(
#     score = "4",
#     limitations = "Question skipped (see RB9)"
#   ),

#   # RB14: Calculations (Recommended) ----
#   #  If chemical concentrations were normalised or adjusted (e.g., to represent bioavailability or toxicity), then were the calculations explained and were they appropriate?
#   RB14 = list(
#     score = "1",
#     limitations = "No normalisation/adjustment of calculations is reported in dataset."
#   ),

#   # RB15: Significant Figures (Recommended) ----
#   # During calculations, were data reported to the appropriate number of significant figures or decimal places?
#   RB15 = list(
#     score = "4",
#     limitations = "As significant figure reporting varied considerably across the dataset, we elected to use the worse score (most conservative approach)."
#   ),

#   # RB16: Outliers (Recommended) ----
#   # For any outliers deleted from the data set, was evidence provided that these outliers were due to an error in measurement or contamination?
#   RB16 = list(
#     score = "1",
#     limitations = "No outliers were reported as removed."
#   ),

#   # RB17: Censored Data (Required) ----
#   # Were censored data reported correctly (e.g., as a numerical value plus a less-than sign or another indicator of a nondetect)? If a substitution method was used for nondetects (e.g., censored data were replaced by zero, or by 1/2 or another fraction of the LOD/LOQ),
#   # then can the original censored data be restored by back-calculation using the reported LOD/LOQ?
#   RB17 = list(
#     score = "2",
#     limitations = "Censored data were reported following the method above. Most (60-90%) rows had an LOD/LOQ reported."
#   ),

#   # RB18: Summary Statistics Procedures (Recommended) ----
#   # Were summary statistics calculated appropriately? If the dataset contained censored data,
#   # then were censored data included and were appropriate procedures used to determine summary statistics?
#   RB18 = list(
#     score = "2",
#     limitations = "Some values may have been reported as means (see RV10), but as the vast majority represent single samples we have treated
#     this as negligible. Under the circumstances, we assume the data is relatively unaffected by such issues, but our Partially Met score reflects
#     our"
#   ),

#   # RB19: Supporting Data Quality (Recommended) ----
#   # If any supporting parameters are required for the assessment purpose, then were the supporting parameter data provided, and were their methods and data quality addressed?
#   RB19 = list(
#     score = "1",
#     limitations = "Not relevant."
#   )
# )

# # 6. CREATE MOCK INPUT LIST ----
# # This simulates the Shiny input object that collect_CREED_data() expects

# mock_input_list <- list()

# # Add relevance scores and data ----
# for (criterion_id in names(relevance_config)) {
#   # Get auto-populated data
#   autopop_value <- relevance_autopop |>
#     filter(field == criterion_id) |>
#     pull(value)

#   if (length(autopop_value) == 0) {
#     autopop_value <- ""
#   }

#   # Add to mock input
#   mock_input_list[[paste0(criterion_id, "_score")]] <-
#     relevance_config[[criterion_id]]$score
#   mock_input_list[[paste0(criterion_id, "_relevant_data")]] <-
#     autopop_value
#   mock_input_list[[paste0(criterion_id, "_limitations")]] <-
#     relevance_config[[criterion_id]]$limitations
# }

# # Add reliability scores and data ----
# for (criterion_id in names(reliability_config)) {
#   # Get auto-populated data
#   autopop_value <- reliability_autopop |>
#     filter(field == criterion_id) |>
#     pull(value)

#   if (length(autopop_value) == 0) {
#     autopop_value <- ""
#   }

#   # Add to mock input
#   mock_input_list[[paste0(criterion_id, "_score")]] <-
#     reliability_config[[criterion_id]]$score
#   mock_input_list[[paste0(criterion_id, "_relevant_data")]] <-
#     autopop_value

#   # Handle RB8 special case (justification vs limitations)
#   if (criterion_id == "RB8") {
#     mock_input_list[[paste0(criterion_id, "_justification")]] <-
#       reliability_config[[criterion_id]]$justification
#   } else {
#     mock_input_list[[paste0(criterion_id, "_limitations")]] <-
#       reliability_config[[criterion_id]]$limitations
#   }
# }

# # 7. COLLECT CREED DATA ----

# # Define criteria configurations (from module files) ----
# relevance_criteria_config <- list(
#   RV1 = list(title = "Sample Medium/Matrix", type = "Required"),
#   RV2 = list(title = "Collection Method/Sample Type", type = "Recommended"),
#   RV3 = list(title = "Study Area", type = "Required"),
#   RV4 = list(title = "Site Type", type = "Recommended"),
#   RV5 = list(title = "Sampling Timespan", type = "Required"),
#   RV6 = list(title = "Sampling Frequency", type = "Required"),
#   RV7 = list(title = "Temporal Conditions", type = "Recommended"),
#   RV8 = list(title = "Analyte", type = "Required"),
#   RV9 = list(title = "Sensitivity/LOD/LOQ", type = "Required"),
#   RV10 = list(title = "Summary Statistics Type", type = "Recommended"),
#   RV11 = list(title = "Supporting Parameters", type = "Recommended")
# )

# reliability_criteria_config <- list(
#   RB1 = list(title = "Sample Medium/Matrix", type = "Required"),
#   RB2 = list(title = "Collection Method/Sample Type", type = "Recommended"),
#   RB3 = list(title = "Sample Handling", type = "Recommended"),
#   RB4 = list(title = "Site Location", type = "Required"),
#   RB5 = list(title = "Date and Time", type = "Required"),
#   RB6 = list(title = "Analyte(s) Measured", type = "Required"),
#   RB7 = list(
#     title = "Limit of Detection and/or Limit of Quantification",
#     type = "Required"
#   ),
#   RB8 = list(
#     title = "Accreditation/Quality Management System",
#     type = "Required"
#   ),
#   RB9 = list(title = "Method", type = "Required"),
#   RB10 = list(title = "Lab Blank Contamination", type = "Recommended"),
#   RB11 = list(title = "Recovery/Accuracy", type = "Recommended"),
#   RB12 = list(title = "Reproducibility/Precision", type = "Recommended"),
#   RB13 = list(title = "Field QC", type = "Recommended"),
#   RB14 = list(title = "Calculations", type = "Recommended"),
#   RB15 = list(title = "Significant Figures", type = "Recommended"),
#   RB16 = list(title = "Outliers", type = "Recommended"),
#   RB17 = list(title = "Censored Data", type = "Required"),
#   RB18 = list(title = "Summary Statistics Procedures", type = "Recommended"),
#   RB19 = list(title = "Supporting Data Quality", type = "Recommended")
# )

# # Collect relevance data ----
# creed_relevance <- collect_CREED_data(
#   criteria_config = relevance_criteria_config,
#   input = mock_input_list
# )

# # Collect reliability data ----
# creed_reliability <- collect_CREED_data(
#   criteria_config = reliability_criteria_config,
#   input = mock_input_list
# )

# # 8. CALCULATE SCORES ----
# # This mirrors the logic from mod_CREED_server lines 282-396

# # if (nrow(creed_reliability) > 1 && nrow(creed_relevance) > 1) {
# #   # Add numeric score column ----
# #   reliability_data <- creed_reliability |>
# #     mutate(numeric_score = as.integer(score))

# #   relevance_data <- creed_relevance |>
# #     mutate(numeric_score = as.integer(score))

# #   # Calculate Silver levels (Required criteria) ----
# #   reliability_silver <- reliability_data |>
# #     filter(required_recommended == "Required") |>
# #     pull(numeric_score) |>
# #     max(na.rm = TRUE)

# #   relevance_silver <- relevance_data |>
# #     filter(required_recommended == "Required") |>
# #     pull(numeric_score) |>
# #     max(na.rm = TRUE)

# #   # Calculate Gold levels (Recommended criteria) ----
# #   reliability_gold <- reliability_data |>
# #     filter(required_recommended == "Recommended") |>
# #     pull(numeric_score) |>
# #     max(na.rm = TRUE)

# #   relevance_gold <- relevance_data |>
# #     filter(required_recommended == "Recommended") |>
# #     pull(numeric_score) |>
# #     max(na.rm = TRUE)

# #   # Map scores back to categories ----
# #   reliability_categories <- c(
# #     "1" = "Reliable without restrictions",
# #     "2" = "Reliable with restrictions",
# #     "3" = "Not assignable",
# #     "4" = "Not usable"
# #   )

# #   relevance_categories <- c(
# #     "1" = "Relevant without restrictions",
# #     "2" = "Relevant with restrictions",
# #     "3" = "Not assignable",
# #     "4" = "Not usable"
# #   )

# #   # Create results tibble (pretty version) ----
# #   creed_scores_pretty <- tibble(
# #     level = c("Silver", "Gold"),
# #     reliability_score = c(reliability_silver, reliability_gold),
# #     reliability_category = reliability_categories[as.character(c(
# #       reliability_silver,
# #       reliability_gold
# #     ))],
# #     relevance_score = c(relevance_silver, relevance_gold),
# #     relevance_category = relevance_categories[as.character(c(
# #       relevance_silver,
# #       relevance_gold
# #     ))]
# #   )

# #   # Get reference ID ----
# #   ref_id <- if (nrow(sessiondata$referenceData) > 0) {
# #     sessiondata$referenceData$REFERENCE_ID
# #   } else {
# #     "Vannmiljo_Database"
# #   }

# #   # Create backend version ----
# #   creed_scores_backend <- initialise_CREED_scores_tibble() |>
# #     add_row(
# #       REFERENCE_ID = ref_id,
# #       SILVER_RELIABILITY = reliability_categories[as.character(
# #         reliability_silver
# #       )],
# #       SILVER_RELEVANCE = relevance_categories[as.character(relevance_silver)],
# #       GOLD_RELIABILITY = reliability_categories[as.character(reliability_gold)],
# #       GOLD_RELEVANCE = relevance_categories[as.character(relevance_gold)]
# #     )
# #   # Display Results ----
# #   cat("\n\n========== CREED ASSESSMENT RESULTS ==========\n\n")
# #   cat("Silver Level:\n")
# #   cat("  Reliability:", reliability_categories[as.character(reliability_silver)], "\n")
# #   cat("  Relevance:  ", relevance_categories[as.character(relevance_silver)], "\n\n")
# #   cat("Gold Level:\n")
# #   cat("  Reliability:", reliability_categories[as.character(reliability_gold)], "\n")
# #   cat("  Relevance:  ", relevance_categories[as.character(relevance_gold)], "\n")

# #   # Export to CSV ----

# #   ## Generate timestamp ----
# #   timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# #   ## Define output directory ----
# #   output_dir <- here("data", "clean", "unzipped")

# #   ## Create filenames ----
# #   filename_rb <- glue("{campaign_name}_CREED_RB_{timestamp}.csv")
# #   filename_rv <- glue("{campaign_name}_CREED_RV_{timestamp}.csv")
# #   filename_score <- glue("{campaign_name}_CREED_Score_{timestamp}.csv")

# #   ## Write CSV files ----
# #   write.csv(
# #     creed_reliability,
# #     here(output_dir, filename_rb),
# #     row.names = FALSE
# #   )

# #   write.csv(
# #     creed_relevance,
# #     here(output_dir, filename_rv),
# #     row.names = FALSE
# #   )

# #   write.csv(
# #     creed_scores_backend,
# #     here(output_dir, filename_score),
# #     row.names = FALSE
# #   )

# #   ## Report saved files ----
# #   cat("\n\n========== FILES SAVED ==========\n")
# #   cat("✓", filename_rb, "\n")
