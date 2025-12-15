# Should this be code? Absolutely not!
# Result of
# get_categorical_summary(vm_copper_2025, max_unique = 50)

## Aktivitet_id (50 unique values)

library(tibble)
library(dplyr)
library(readr)

tribble(
  ~value , ~n    ,
  "GRUV" , 22617 ,
  "TILT" , 16027 ,
  "MOMC" , 14057 ,
  "MYFO" , 10405 ,
  "KAVE" ,  9244 ,
  "FOSJ" ,  8729 ,
  "INDU" ,  8501 ,
  "ANNE" ,  6295 ,
  "RIDD" ,  4480 ,
  "MILK" ,  4043 ,
  "PROB" ,  3533 ,
  "FORS" ,  3518 ,
  "AREA" ,  2644 ,
  "FLYP" ,  2479 ,
  "DEPO" ,  2202 ,
  "SKYT" ,  1796 ,
  "CEMP" ,  1743 ,
  "LTAM" ,  1503 ,
  "MIFE" ,  1333 ,
  "BARE" ,  1218 ,
  "RELV" ,  1193 ,
  "JRBN" ,  1071 ,
  "GRVN" ,  1015 ,
  "JOVA" ,   993 ,
  "ELVE" ,   948 ,
  "KALL" ,   873 ,
  "BAPO" ,   761 ,
  "KALK" ,   679 ,
  "MIUR" ,   668 ,
  "PASV" ,   590 ,
  "DRIK" ,   587 ,
  "KOMM" ,   451 ,
  "MITE" ,   422 ,
  "EMUD" ,   340 ,
  "KART" ,   283 ,
  "MARE" ,   264 ,
  "TILF" ,   256 ,
  "MGKK" ,   193 ,
  "SCRE" ,   190 ,
  "LANG" ,   144 ,
  "VASS" ,    92 ,
  "SOFP" ,    67 ,
  "GEOS" ,    53 ,
  "OEKF" ,    42 ,
  "MINN" ,    29 ,
  "BIOM" ,    23 ,
  "MONS" ,    12 ,
  "ANLA" ,     4 ,
  "SPFO" ,     3 ,
  "KAKA" ,     2
)

## Parameter_id (1 unique values)

tribble(
  ~value , ~n     ,
  "CU"   , 138615
)

## Medium_id (26 unique values)

vm_medium_id_values <- tribble(
  ~value , ~n    ,
  "VF"   , 81921 ,
  "SS"   , 29681 ,
  "VS"   ,  5434 ,
  "BB"   ,  4597 ,
  "BL"   ,  4280 ,
  "GV"   ,  4074 ,
  "SF"   ,  3009 ,
  "LUF"  ,  1123 ,
  "BM"   ,   777 ,
  "BPV"  ,   573 ,
  "BH"   ,   488 ,
  "PA"   ,   439 ,
  "VRG"  ,   388 ,
  "VOV"  ,   363 ,
  "BE"   ,   338 ,
  "BG"   ,   244 ,
  "BO"   ,   225 ,
  "BK"   ,   216 ,
  "JOR"  ,   178 ,
  "VAR"  ,   137 ,
  "VAU"  ,    56 ,
  "PSS"  ,    50 ,
  "BF"   ,    17 ,
  "HS"   ,     5 ,
  "BA"   ,     1 ,
  "KLS"  ,     1
)

vm_medium_id_vocab <- read_excel(
  "data/raw/vannmiljo/Vannmiljø_Medium_2025-12-15.xlsx"
) |>
  left_join(vm_medium_id_lookup, c("MediumID" = "value")) |>
  mutate(
    ENVIRON_COMPARTMENT = NA_character_,
    ENVIRON_COMPARTMENT_SUB = NA_character_,
    SPECIES_GROUP = NA_character_,
    SAMPLE_SPECIES = NA_character_,
    SAMPLE_TISSUE = NA_character_,
    SITE_GEOGRAPH_FEATURE = NA_character_,
    SITE_GEOGRAPH_FEATURE_SUB = NA_character_
  ) |>
  write_excel_csv("data/clean/Vm_medium_lookup_matrix.csv")


## Provetakmetode_id (29 unique values)

tribble(
  ~value                  , ~n    ,
  "UKJENT"                , 69109 ,
  "NS-EN ISO 5667-19B"    , 21129 ,
  "NS-ISO 5667-6:2014-1"  , 15332 ,
  "NS-ISO 5667-6A"        , 13067 ,
  "NS-EN ISO 5667-19C"    ,  4635 ,
  "NS-ISO 5667-9A"        ,  3529 ,
  "NS-ISO 5667-4A"        ,  3103 ,
  "NS-ISO 5667-4:2016A"   ,  2848 ,
  "NS-EN ISO 5667-6:2016" ,  2054 ,
  "PASSIV-DGT"            ,   639 ,
  "NS-ISO 5667-11:2009"   ,   609 ,
  "NS-EN ISO 5667-19A"    ,   434 ,
  "NS 9434:2017-UP"       ,   381 ,
  "NS 9434:2017-SE"       ,   292 ,
  "FRONTASYEVA_2014"      ,   265 ,
  "NS-ISO 5667-12C"       ,   249 ,
  "LEVER_TV"              ,   242 ,
  "NS-ISO 5667-17A"       ,   177 ,
  "NS-ISO 5667-12:2017B"  ,   157 ,
  "NS-ISO 5667-12B"       ,   121 ,
  "NS-ISO 5667-17:2008A"  ,    60 ,
  "NS-ISO 5667-12:2017C"  ,    49 ,
  "NS 9410:2016"          ,    39 ,
  "NS-ISO 5667-17:2008D"  ,    38 ,
  "NS-ISO 5667-12:2017A"  ,    28 ,
  "PASSIV-SORBICELL"      ,    14 ,
  "NS 9410:2007"          ,     6 ,
  "HAANDCORER"            ,     5 ,
  "NS-ISO 5667-12A"       ,     4
)

## Analysemetode_id (9 unique values)

tribble(
  ~value                   , ~n    ,
  "UKJENT"                 , 68651 ,
  "NS-EN ISO 17294-2:2016" , 30509 ,
  "NS-EN ISO 17294-2"      , 26823 ,
  "NS-EN ISO 11885"        ,  7593 ,
  "NS-EN ISO 11885:2009"   ,  2980 ,
  "NS 4773"                ,   634 ,
  "NS-EN ISO 15586"        ,   617 ,
  "NS 4781"                ,   507 ,
  "NS-EN ISO 17294-2:2023" ,   301
)

## DybdeEnhet (3 unique values)

tribble(
  ~value , ~n    ,
  "m"    , 87171 ,
  "cm"   , 32707 ,
  NA     , 18737
)

## Filtrert_Prove (2 unique values)

tribble(
  ~value      , ~n     ,
  "Ufiltrert" , 126655 ,
  "Filtrert"  ,  11960
)

## UnntasKlassifisering (2 unique values)

tribble(
  ~value    , ~n     ,
  NA        , 138564 ,
  "Unntatt" ,     51
)

## Operator (4 unique values)

tribble(
  ~value , ~n     ,
  "="    , 129618 ,
  "<"    ,   8982 ,
  "ND"   ,      9 ,
  ">"    ,      6
)

## Enhet_id (4 unique values)

tribble(
  ~value , ~n    ,
       8 , 92457 ,
      41 , 34257 ,
      42 , 10778 ,
      84 ,  1123
)

## Deteksjonsgrense (29 unique values)

tribble(
  ~value      , ~n     ,
  NA          , 116471 ,
      0.5     ,  14553 ,
      0       ,   2492 ,
      0.01    ,   1643 ,
      1       ,   1033 ,
      0.03    ,    667 ,
      0.05    ,    596 ,
      0.012   ,    260 ,
      0.1     ,    220 ,
      0.2     ,    167 ,
      0.3     ,    157 ,
      1.7     ,    154 ,
      0.67    ,     34 ,
      3       ,     29 ,
      0.03333 ,     23 ,
      0.033   ,     20 ,
      0.004   ,     18 ,
      3.3     ,     15 ,
      1.5     ,     14 ,
      0.4     ,     12 ,
      7       ,     11 ,
     10       ,      6 ,
      1.6     ,      6 ,
  5e-04       ,      6 ,
      0.17    ,      3 ,
      0.002   ,      2 ,
      0.8     ,      1 ,
      1.4     ,      1 ,
      1.8     ,      1
)

## Ant_verdier (9 unique values)

tribble(
  ~value , ~n     ,
       1 , 138445 ,
       3 ,     79 ,
       4 ,     29 ,
       5 ,     17 ,
       8 ,     16 ,
       2 ,     12 ,
       7 ,      8 ,
      10 ,      5 ,
      37 ,      4
)

## Arkiv_id (2 unique values)

tribble(
  ~value , ~n     ,
  "N"    , 137129 ,
  "J"    ,   1486
)
