library(data.table)
library(readxl)
library(summarytools)
library(datapasta)
library(glue)

# Downloaded 2025.12.05, data for 2025.01.01 - 2025.12.05, all kommune, all media, all campaigns, etc.
# Search for Kobber & Kobberpyrition
# 138615 hits
if (!exists("vm_copper_2025")) {
  # let's not run this more often than we have to, it's very slow
  vm_copper_2025 <- readxl::read_excel(
    path = "data/raw/vannmiljo/Vm_Copper_2025.12.05.xlsx",
    sheet = 1,
    guess_max = 138615 # avoid warnings (because read_excel will automatically convert columns to logical if they have a lot of NAs)
  )
}

# little bit of data inspection
dfSummary(vm_copper_2025) |> view() # where have you been my whole life.

# What do we do with the columns?
colnames <- names(vm_copper_2025) |> as_tibble_col("Vm_Variable")
# This works - returns the tribble code as a string
tribble_code <- tribble_construct(colnames)

## Helper Function

# Function to convert tibble to tribble code ----
to_tribble <- function(df) {
  library(glue)

  # Header line with column names
  header <- glue("  ~{names(df)}", .sep = ", ")

  # Format each value (add quotes for characters)
  format_val <- function(x) {
    if (is.character(x)) {
      glue('"{x}"')
    } else if (is.na(x)) {
      "NA"
    } else {
      as.character(x)
    }
  }

  # Build each row
  rows <- apply(df, 1, function(row) {
    vals <- sapply(row, format_val)
    glue("  {vals}", .sep = ", ")
  })

  # Combine everything
  glue(
    "tribble(
{header},
{paste(rows, collapse = ',\n')}
)"
  )
}

## ------------------------

to_tribble(colnames)

tribble(
  ~Vm_Variable            , ~eData_Variable , Transformation                              ,
  "Registrerings_id"      , # just keep in comments?
  "Vannlok_kode"          , "SITE_CODE"     , str_glue("Vm_{Vannlokalitet_kode")          , # important. I don't see any site coords here so I guess that's a different table
  "Aktivitet_id"          , "CAMPAIGN"      , str_glue("Vm_{YEAR}_{Aktivitet_id}")        , # important
  "Oppdragsgiver"         , # not very important, send to comments
  "Oppdragstaker"         , # not very important, send to comments
  "Parameter_id"          , # very important but currenly only going to be 1 of two substances. requires lookup
  "Medium_id"             , "Many"          , # complicated, important, must be mapped against multiple lookups in multiple tables
  "LatinskNavn_id"        , "Not retained"  , NA                                          ,
  "VitenskapligNavn"      , "SPECIES_NAME"  , NA                                          , # biig lookup but easy to do
  "Provetakmetode_id"     , # important, lookup + translation
  "Analysemetode_id"      , # important, lookup + translation
  "Tid_provetak"          , "SAMPLING_DATE" , ymd_hms(Tid_provetak) |> format("%Y.%m.%d") , # imoprtant, but date conversions are easy
  "Ovre_dyp"              , # moderate important, needs to be averaged w/ nedre deep
  "Nedre_dyp"             , # see above
  "DybdeEnhet"            , # see above
  "Filtrert_Prove"        , # important, needs to go into FRACTIONATION_PROTOCOL, possibly with expert intepretation
  "UnntasKlassifisering"  , # probasbly not important?
  "Operator"              , # important, needs to be lookuped into our own stuff for and spread across value/LOQ/LOD columns
  "Verdi"                 , # important.
  "Enhet_id"              , # important, will need to lookup units and standardise
  "Provenr"               , # retain in comments or even just add a new column
  "Deteksjonsgrense"      , # important, units must be standardised
  "Kvantifiseringsgrense" , "LOQ_VALUE"     , "Unit Standardisation"                      , # important, units must be standardised
  "Opprinnelse"           , # probably worth keeping?
  "Ant_verdier"           , # becomes sample_n?
  "Kommentar"             , # retain as comment?
  "Arkiv_id"              , # retain as comment?
  "ID_lokal"              , # no ideas
  "Produktbeskrivelse" # I think this is empty always?
)
