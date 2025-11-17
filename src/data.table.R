ices_biota_copper <- data.table::fread(
  file = "data/raw/ICES DOME/biota/ICES_DOME_Copper_Biota_20251008_raw.csv",
  nThread = 4
) |>
  filter(PARAM == "CU") # internal code for copper

ices_seawater_copper <- data.table::fread(
  file = "data/raw/ICES DOME/seawater/ContaminantsSeawater_2025100810310726.csv",
  nThread = 4
) |>
  filter(PARAM == "CU") # internal code for copper

ices_sediment_copper <- data.table::fread(
  file = "data/raw/ICES DOME/sediment/ContaminantsSediment_2025100810135211.csv",
  nThread = 4
) |>
  filter(PARAM == "CU") # internal code for copper

# .9 mil total rows.
ices_copper <- bind_rows(
  ices_biota_copper,
  ices_seawater_copper,
  ices_sediment_copper
)

# group by station - 17k sampling points
# lots unfortunately lack names...
ices_copper_stations <- ices_copper |>
  group_by(Latitude, Longitude) |>
  reframe(
    Station = STATN,
    Country,
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    num_samples = n()
  ) |>
  unique() |>
  st_drop_geometry() |> # Remove incorrect geometry
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # Corr # almost certainly WGS84, but needs checking


# Create the leaflet map
leaflet(ices_copper_stations_fixed) |>
  addTiles() |> # Default OpenStreetMap tiles
  addCircleMarkers(
    radius = 3,
    fillOpacity = 0.7,
    stroke = FALSE,
    clusterOptions = markerClusterOptions() # Cluster nearby points
  ) |>

  setView(lng = 0, lat = 65, zoom = 4) # Center on Europe/Arctic region


#m Well, if we don't want to be a total idiot:
1.
Focus
the
data
down
to
the
bare
min.
2.
Do
some
conversions.
3.
Take
another
look.


ices_copper_subset <- ices_copper |>
  # Create readable column names from ICES codes ----
  dplyr::mutate(
    Monitoring_Programme = MPROG,
    Purpose_Of_Monitoring = PURPM,
    Country = Country,
    Reporting_Laboratory = RLABO,
    Station_Name = STATN,
    Monitoring_Year = MYEAR,
    Date = DATE,
    Latitude = Latitude,
    Longitude = Longitude,
    Species = Species,
    Sex_Code = SEXCO,
    Number_Of_Individuals_In_Pool = NOINP,
    Matrix = MATRX,
    Number_Of_Diseases = NODIS,
    Parameter_Group = PARGROUP,
    Parameter = PARAM,
    Basis = BASIS,
    Quality_Flag = QFLAG,
    Value = Value,
    Measurement_Unit = MUNIT,
    Value_Flag = VFLAG,
    Detection_Limit = DETLI,
    Limit_Of_Quantification = LMQNT,
    Uncertainty = UNCRT,
    Method_Uncertainty = METCU,
    Analytical_Laboratory = ALABO,
    Reference_Sample_Key = REFSK,
    Method_Storage = METST,
    Method_Pre_Treatment = METPT,
    Method_Prep_And_Separation = METPS,
    Method_Final_Prep = METFP,
    Method_Chemical_Extraction = METCX,
    Method_Of_Analysis = METOA,
    Formula = FORML,
    Test_Organism = `Test Organism`,
    Sample_Type = SMTYP,
    Subsample_Number = SUBNO,
    Bulk_ID = BULKID,
    Final_Flag = FINFL,
    Data_Check_Flags = DCFLGs,
    Table_Analysis_ID = tblAnalysisID,
    Table_Parameter_ID = tblParamID,
    Table_Bio_ID = tblBioID,
    Table_Sample_ID = tblSampleID,
    Depth_Upper = DEPHU,
    Depth_Lower = DEPHL,
    .keep = "unused"
  ) |>
  filter(Monitoring_Year >= 2010)

# gimme some
