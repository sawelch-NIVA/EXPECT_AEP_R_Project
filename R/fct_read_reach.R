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

reach_by_sector_en <- reach_by_sector |>
  left_join(nace_sectors, by = join_by("sector" == "sector_no")) |>
  mutate(net_kg = netto_tonn * 1000)

ggplot(
  data = reach_by_sector_en,
  mapping = aes(
    x = year,
    y = net_kg,
    colour = sector_en
  )
) +
  geom_point() +
  geom_line() +
  scale_y_log10()
# ! now what?

reach_by_sector_en_summarised <- reach_by_sector_en |>
  mutate(z_score = scale(net_kg)[, 1], .by = sector_en) |>
  ungroup() |>
  filter(abs(z_score) < 2) |> # use z = 2 as a cutoff
  reframe(
    mean_net_kg = mean(net_kg),
    sd_net_kg = sd(net_kg),
    n_years_reported = n(),
    nace_section = range(isic_nace_section),
    .by = "sector_en"
  ) |>
  distinct() |>
  arrange(desc(mean_net_kg))

reach_by_sector_en_summarised
# ok, this gives us something to work with
#    sector_en                                              mean_net_kg sd_net_kg n_years_reported nace_section
#    <chr>                                                        <dbl>     <dbl>            <int> <chr>
#  1 Manufacturing                                        40620233.       2.48e+7                6 C
#  2 Agriculture, forestry and fishing                     1114054.       9.70e+5                6 A
#  3 Mining and quarrying                                   682269.       1.26e+5                6 B
#  4 Unclassified                                            37212.       5.78e+4                5 NA
#  5 Wholesale and retail trade, repair of motor vehicles     7962.       6.98e+3                6 G
#  6 Construction                                             1243.       4.18e+2                5 F
#  7 Transportation and storage                                715.       4.83e+2                6 H
#  8 Water supply, sewerage and waste management                98.6      4.27e+1                3 E
#  9 Arts, entertainment and recreation                         18.8      9.14e+0                4 R
# 10 Real estate activities                                      3.06     1.60e+0                6 L
# 11 Professional, scientific and technical activities           0.0562   7.07e-3                2 M
# 12 Other service activities                                    0.0117   1.46e-2                2 S
