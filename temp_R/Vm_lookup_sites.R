
# > get_categorical_summary(vm_copper_sites_2025)

library(tibble)
library(dplyr)
library(readr)


## Objekttype (2 unique values)

tribble(
  ~value, ~n,
  "point", 30521,
  "polygon", 20
)

## Midlertidig (1 unique values)

tribble(
  ~value, ~n,
  "Nei", 30541
)

## Vannkategori (9 unique values)

tribble(
  ~value, ~n,
  "C", 20927,
  "R", 4756,
  "L", 3025,
  "J", 707,
  "G", 550,
  "O", 256,
  "U", 250,
  "S", 63,
  "A", 7
)

## Klassifiseres (2 unique values)

tribble(
  ~value, ~n,
  "Ja", 21619,
  "Nei", 8922
)

## Knytt til påvirkning (5 unique values)

tribble(
  ~value, ~n,
  NA, 25429,
  "Akvakultur", 2574,
  "AKVAKULTUR", 1360,
  "Industri", 1108,
  "INDUSTRI", 70
)

## Referansestasjon (4 unique values)

tribble(
  ~value, ~n,
  NA, 29696,
  "NÆRSTASJON", 538,
  "REFERANSESTASJON", 165,
  "Referansestasjon", 142
)

## Produksjonsområde (12 unique values)

tribble(
  ~value, ~n,
  NA, 30436,
  "4", 53,
  "6", 13,
  "3", 11,
  "5", 7,
  "8", 6,
  "13", 5,
  "9 – Vestfjorden og Vesterålen", 4,
  "2", 3,
  "12", 1,
  "Halvardøy", 1,
  "Våtvika", 1
)
> get_categorical_summary(vm_copper_sites_2025) |> writeClipboard()

## Objekttype (2 unique values)

tribble(
  ~value, ~n,
  "point", 30521,
  "polygon", 20
)

## Midlertidig (1 unique values)

tribble(
  ~value, ~n,
  "Nei", 30541
)



