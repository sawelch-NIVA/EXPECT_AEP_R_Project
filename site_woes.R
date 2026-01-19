# Warning messages:
# 1: Exceedance of failed test units where values in `SITE_CODE` should have been in the set of `NorSeal1988_Area I`, `NorSeal1988_Area II`, `NorSeal1988_Area III` (and 24847 more).
# The `col_vals_in_set()` validation failed beyond the absolute threshold level (1).
# * failure level (19) >= failure threshold (1)
# 2: Site Codes not found in reference set: SorfjordStarfish2000_S1, SorfjordStarfish2000_S2, SorfjordStarfish2000_S3, SorfjordStarfish2000_S4, BarentsSeaMetals2000_Area4, ArcticSeals1999_Godhavn, SeafoodProcessing2014_GREENLAND_W, SeafoodProcessing2014_ILULISSAT, SeafoodProcessing2014_RORVIK, CAGE22-1_Core2, CAGE22-1_Core3, CAGE22-1_Core5, CAGE22-1_Core7, BarentsEchinoderms2015_St4, BarentsEchinoderms2015_St10, BarentsEchinoderms2015_St11, WalrusHeavyMetals2019-2021_Komsomolsky, WalrusHeavyMetals2019-2021_Apolonov, WalrusHeavyMetals2019-2021_ThreeRay

# Why are out sites not found in measurement_data? and actually, is this a pb?


    name = sites_data,
    command = {
      fread_all_module_files(sites_files, initialise_sites_tibble) |>
        # some dates are still messed up
        mutate(
          ENTERED_DATE = parse_date_time(ENTERED_DATE, orders = c("ymd", "dmy"))
        ) |>
        standardise_IDate_all() |>
        add_row(vm_edata_sites) |>
        pb_validate_sites(agent = FALSE, actions = pb_action_levels) |>
        # do we have 1+ measurement corresponding to every site
        col_vals_in_set_verbose(
          columns = SITE_CODE,
          set = unique(measurements_data$SITE_CODE),
          actions = pb_action_levels,
          value_name = "Site Codes"
        )
    }
  ),


  