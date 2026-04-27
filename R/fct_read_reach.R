# read_reach_produktregisteret.R
# Reads REACH/Produktregisteret copper data from the two data sheets.
# Source: Produktregisteret export, net quantities in tonnes.
# Note: covers declared chemicals only (deklarasjonspliktige kjemikalier).
#       "Netto mengde" = imported + produced - exported per declaration.

# ==============================================================================
# 1. CAS reference table (from Forklaring sheet) ----
# ==============================================================================

reach_cas <- tribble(
  ~cas_nr      , ~kjemisk_navn             ,
  "1111-67-7"  , "kobberthiocyanat"        ,
  "1317-38-0"  , "Kobber(II)oksid"         ,
  "1317-39-1"  , "Kobber(I)oksid"          ,
  "1338-02-9"  , "Kobbernaftenat"          ,
  "7440-50-8"  , "kobber pulver"           ,
  "7447-39-4"  , "Kobberklorid"            ,
  "7758-89-6"  , "Kobber(I)klorid"         ,
  "7758-98-7"  , "Kobber(II)sulfat"        ,
  "12069-69-1" , "Kobberhydroksidkarbonat" ,
  "14915-37-8" , "kobberpyrition"          ,
  "54253-62-2" , "Kobber(II)metansulfonat"
)


# ==============================================================================
# 2. Read data sheets ----
# ==============================================================================

path_reach <- "inst/extdata/emissions/REACH_copper_prtd.xlsx"

# -- By sector (hovednæring) --------------------------------------------------
# TODO: A mess - blank columns and negative numbers - but that's fine(?) -
# imported quantity + produced quantity - exported quantity, so probably fine to just use them
# will assume no category = "Other"
reach_by_sector <- read_excel(
  path_reach,
  sheet = "Sum HovedgruppeAndvendelse"
) |>
  rename(
    year = AmountYear,
    netto_tonn = `Netto Mengde (tonn)`,
    sector = Beskrivelse
  ) |>
  mutate(
    year = as.integer(year),
    netto_tonn = as.numeric(netto_tonn),
    sector = na_if(trimws(sector), "Other") # TODO: blank sector -> "Other"
  )

# -- By product type ----------------------------------------------------------
reach_by_product <- read_excel(
  path_reach,
  sheet = "kobber sum på produkttype"
) |>
  rename(
    year = AmountYear,
    netto_tonn = `Netto mengde i tonn`,
    product_type = Tekst
  ) |>
  mutate(
    year = as.integer(year),
    netto_tonn = as.numeric(netto_tonn),
    product_type = trimws(product_type)
  )

reach_by_sector |> select(sector) |> distinct()

# Based on https://ec.europa.eu/eurostat/documents/3859598/5902521/KS-RA-07-015-EN.PDF.pdf/dd5443f5-b886-40e4-920d-9df03590ff91?t=1414781457000
# p143
nace_sectors <- tribble(
  ~sector_no                                                  , ~sector_no_en                                          , ~isic_nace_section , ~isic_nace_description                                                                              ,
  NA                                                          , "Unclassified"                                         , NA                 , NA                                                                                                  ,
  "Jordbruk, skogbruk og fiske"                               , "Agriculture, forestry and fishing"                    , "A"                , "Agriculture, forestry and fishing"                                                                 ,
  "Bergverksdrift og utvinning"                               , "Mining and quarrying"                                 , "B"                , "Manufacturing, mining and quarrying and other industry"                                            ,
  "Industri"                                                  , "Manufacturing"                                        , "C"                , "Manufacturing, mining and quarrying and other industry"                                            ,
  "Elektrisitets-, gass-, damp- og varmtvannsforsyning"       , "Electricity, gas, steam and air conditioning supply"  , "D"                , "Manufacturing, mining and quarrying and other industry"                                            ,
  "Vannforsyning, avløps- og renovasjonsvirksomhet"           , "Water supply, sewerage and waste management"          , "E"                , "Manufacturing, mining and quarrying and other industry"                                            ,
  "Bygge- og anleggsvirksomhet"                               , "Construction"                                         , "F"                , "Construction"                                                                                      ,
  "Varehandel, reparasjon av motorvogner"                     , "Wholesale and retail trade, repair of motor vehicles" , "G"                , "Wholesale and retail trade, transportation and storage, accommodation and food service activities" ,
  "Transport og lagring"                                      , "Transportation and storage"                           , "H"                , "Wholesale and retail trade, transportation and storage, accommodation and food service activities" ,
  "Omsetning og drift av fast eiendom"                        , "Real estate activities"                               , "L"                , "Real estate activities"                                                                            ,
  "Faglig, vitenskapelig og teknisk tjenesteyting"            , "Professional, scientific and technical activities"    , "M"                , "Professional, scientific, technical, administration and support service activities"                ,
  "Forretningsmessig tjenesteyting"                           , "Administrative and support service activities"        , "N"                , "Professional, scientific, technical, administration and support service activities"                ,
  "Kulturell virksomhet, underholdning og fritidsaktiviteter" , "Arts, entertainment and recreation"                   , "R"                , "Other services"                                                                                    ,
  "Annen tjenesteyting"                                       , "Other service activities"                             , "S"                , "Other services"
) |>
  rename(sector_no = sector_no, sector_en = sector_no_en)

nace_sectors


# ! now what?

reach_by_sector_en_summarised <- reach_by_sector_en |>
  # mutate(z_score = scale(net_kg)[, 1], .by = sector_en) |>
  # ungroup() |>
  # filter(abs(z_score) < 2) |> # use z = 2 as a cutoff
  reframe(
    mean_net_kg = mean(net_kg),
    sd_net_kg = sd(net_kg),
    n_years_reported = n(),
    nace_section = range(isic_nace_section),
    .by = "sector_en"
  ) |>
  distinct() |>
  arrange(desc(mean_net_kg))

# TODO: Product category translations via LLM. Could find a better source
product_category_translations <- tibble::tribble(
  ~category_no                                                          , ~category_en                                       , ~confidence ,
  "BRENSELTILSETNINGER"                                                 , "Fuel additives"                                   , "high"      ,
  "Absorpsjons/ og adsorpsjonsmaterialer"                               , "Absorption and adsorption materials"              , "high"      ,
  "ANDRE SMØREMIDLER"                                                   , "Other lubricants"                                 , "high"      ,
  "ARMERINGSMIDLER"                                                     , "Reinforcing agents"                               , "medium"    , # could be "armoring agents" in some contexts
  "Bindemidler"                                                         , "Binders"                                          , "high"      ,
  "Biocider"                                                            , "Biocides"                                         , "high"      ,
  "Borekjemikalier inkl råolje/gass"                                    , "Drilling chemicals incl. crude oil/gas"           , "high"      ,
  "BOREOLJER"                                                           , "Drilling oils"                                    , "high"      ,
  "BUNNFELLINGSHINDRENDE MIDLER, GENERELT"                              , "Anti-precipitation agents, general"               , "medium"    , # could be "anti-settling" or "anti-sedimentation"
  "Fyllingsmidler"                                                      , "Fillers"                                          , "high"      ,
  "GJØDNING, GENERELT"                                                  , "Fertilisers, general"                             , "high"      ,
  "Impregnering"                                                        , "Impregnation agents"                              , "high"      ,
  "INSEKTSMIDDEL, INSEKTMIDLER OG ANDRE MIDLER MOT SKADEDYR PÅ PLANTER" , "Insecticides and other plant pest control agents" , "high"      ,
  "Konstruksjonsmaterialer"                                             , "Construction materials"                           , "high"      ,
  "Lim"                                                                 , "Adhesives"                                        , "high"      ,
  "Maling"                                                              , "Paint"                                            , "high"      ,
  "PH-REGULERENDE MIDLER, GENERELT"                                     , "pH-regulating agents, general"                    , "high"      ,
  "Prosessregulerendemidler"                                            , "Process control agents"                           , "medium"    , # or "process regulating agents"
  "Rengjøring"                                                          , "Cleaning agents"                                  , "high"      ,
  "Rustbeskyttelse"                                                     , "Corrosion protection"                             , "high"      ,
  "SALT TIL GALVANISKE BAD"                                             , "Salts for electroplating baths"                   , "high"      ,
  "Sprengstoff"                                                         , "Explosives"                                       , "high"      ,
  "STØPEMASSER, GENERELT"                                               , "Casting compounds, general"                       , "medium"    , # could be "moulding compounds"
  "SYNTESERÅVARER OG MELLOMPRODUKTER"                                   , "Synthesis raw materials and intermediates"        , "high"      ,
  "Trykkfarger"                                                         , "Printing inks"                                    , "high"      ,
  "BLEKERE TIL FOTOGRAFISK FILM"                                        , "Bleaching agents for photographic film"           , "high"      ,
  "BRUNERINGSSALTER"                                                    , "Browning salts"                                   , "medium"    , # technical term for metal darkening/bluing salts
  "Glasur, emalje"                                                      , "Glaze, enamel"                                    , "high"      ,
  "Herdere"                                                             , "Hardeners/curing agents"                          , "high"      ,
  "Loddemidler"                                                         , "Soldering agents/fluxes"                          , "medium"    , # loddemidler is broad; could be just "solders"
  "BILPLEIEMIDLER, GENERELT"                                            , "Car care products, general"                       , "high"      ,
  "FLUSSMIDLER (SVEISING)"                                              , "Fluxes (welding)"                                 , "high"      ,
  "PIGMENT TIL GLASURER, EMALJER OG GLASS"                              , "Pigments for glazes, enamels and glass"           , "high"      ,
  "Metalloverflatebehandlingsmidler"                                    , "Metal surface treatment agents"                   , "high"      ,
  "ANTIOKSIDANTER (ANTIOZONANTER)"                                      , "Antioxidants (antiozonants)"                      , "high"      ,
  "Poler og pleieMIDLER"                                                , "Polishes and care products"                       , "medium" # odd capitalisation in source; "poler" = polishes
)

reach_by_product_translated <- reach_by_product |>
  left_join(
    product_category_translations,
    by = join_by(product_type == category_no)
  )

reach_by_product_summarised <- reach_by_product_translated |>
  reframe(
    mean_net_kg = mean(netto_tonn * 1000),
    sd_net_kg = sd(netto_tonn * 1000),
    n_years_reported = n(),
    .by = "category_en"
  ) |>
  distinct() |>
  arrange(desc(mean_net_kg))
