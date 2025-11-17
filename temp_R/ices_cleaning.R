library(data.table)
library(tidyverse)
library(dtplyr)
library(purrr)

ices_biota_copper <- read_csv(
  file = "data/raw/ICES DOME/biota/ICES_DOME_Copper_Biota_20251008_raw.csv"
) |>
  filter(PARAM == "CU") # internal code for copper

str(ices_biota_copper)

ggplot(data = ices_biota_copper) +
  geom_bar(aes(x = MYEAR, fill = Country))

ggplot(data = ices_biota_copper) +
  geom_map()

# Ugh. codelists/vocab
ices_dome_vocab <- data.table::fread(
  file = "data/raw/ICES DOME/vocab/RECO_Export_08-13-2025-12-13-11.csv",
  nThread = 4
) |>
  # Removing non-copper parameters brings us down to 2172 rows, but I don't know we can really filter futher
  dplyr::filter(CodeType != "PARAM" | (CodeType == "PARAM" & Code == "CU")) |>
  # group by code type
  dplyr::group_by(CodeType) |>
  # translate letter codes into something easier to read

  # 46 lists! cool!
  ices_dome_vocab_lists <- dplyr::group_split(ices_dome_vocab)

imap(.x = ices_dome_vocab_lists, .f = function(x, idx) .x[[idx]])

map(ices_dome_vocab_lists)

testlist <- list(list(1, 2), list(1, 2))
names(testlist) <- c("List1", "List2")

get_CodeType <- function(x, e) {
  CodeType <- x$CodeType |> unique()
  set_names(list(x), CodeType)
}

ices_dome_vocab_lists <- map(
  ices_dome_vocab_lists,
  get_CodeType
)
