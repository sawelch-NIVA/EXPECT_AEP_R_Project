# our sites are rubbish, metadata-wise. what do we do?

library(tidyverse)
library(targets)
library(hexbin)
library(ggrepel)
library(viridis)
library(patchwork)
library(ggnewscale)
library(igraph)
library(sf)
library(gridExtra)
library(grid)
library(knitr)

pkgload::load_all(quiet = TRUE)

# == Shared map data ----
world_map <- map_data("world")

# == Study area bbox ----
# xmin/xmax: longitude; ymin/ymax: latitude
STUDY_BBOX <- c(xmin = -65, xmax = 40, ymin = 55, ymax = 75)

# Helper: append coord_sf with fixed study area to a ggplot object.
# ratio = 2 approximates equal-area appearance at these latitudes.
study_coords <- function(p, ratio = 2) {
  coord_sf(
    xlim = c(STUDY_BBOX[["xmin"]], STUDY_BBOX[["xmax"]]),
    ylim = c(STUDY_BBOX[["ymin"]], STUDY_BBOX[["ymax"]]),
    expand = FALSE
  )
}

# == Outlier filter ----
# Z-score threshold for excluding extreme values.
# Applied locally per matrix using MEASURED_VALUE_STANDARD.
# MEASURED_VALUE is retained in the data for traceability only; never used for analysis.
Z_THRESHOLD <- 3

is_outlier <- function(x, threshold = Z_THRESHOLD) {
  abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)) > threshold
}

# == Site comment cleaning patterns ----
# NOTE: \p{Lu} requires Unicode-aware regex; stringr::regex() handles this correctly.
remove_patterns <- c(
  # URLs / emails
  "(?i)https?://\\S+|www\\.\\S+|\\S+@\\S+",
  # Parenthetical / bracket content
  "\\s*\\([^)]*\\)|\\s*\\[[^]]*\\]",
  # Report codes e.g. 01.01-385, 2021/09
  "\\b\\d{1,4}(?:[\\.\\-/]\\d{1,4})+\\b",
  # Station / sample codes e.g. L1, L2, Stasjon 3
  "\\bL\\d+\\b|\\bStasjon\\s*\\d+\\b|\\bStation\\s*\\d+\\b",
  # Short numeric tokens (excludes 4-digit years)
  "\\b(?!\\d{4}\\b)\\d{1,3}\\b",
  # Report words (Norwegian / English)
  "(?i)rapport(?:\\s+nr\\.?|nr\\.?|nummer|:)?.*?\\b|(?i)report(?:\\s+no\\.?|\\s+nr\\.?|\\s+nr)?",
  "(?i)rapporter?\\b|(?i)reference(?:\\s+site)?\\b|\\bref\\.?\\b",
  # Vannmiljø / resipient noise
  "(?i)vannmilj[oø]\\b|\\bvm\\b|(?i)resipient(?:unders[kq]økelse|unde)?",
  "(?i)vm\\s*emission\\s*|(?i)miljøgifter\\b|(?i)miljøgifter\\s+i",
  # Contractor / org names
  "(?i)akvaplan-?niva|niva\\b|nina\\b|ramb[ao]ll\\b|norconsult\\b|asplan\\s+viak\\b|ngi\\b|ng\\&i\\b|APN|AS",
  "\\b\\p{Lu}[\\p{L}''\\-]+(?:\\s+\\p{Lu}[\\p{L}''\\-]+)*\\s+AS\\b",
  # Page / folio refs
  "\\bpg?\\.?\\s*\\d+\\b|\\bp\\.\\s*\\d+\\b",
  # Unit / measurement fragments
  "\\bmg/kg\\b|\\bmg/kg\\s*dry\\b|\\bmg/kg\\s*wet\\b|\\bSD\\b|\\bStandard\\s+Deviat(?:ion|ions)\\b",
  # Stray punctuation
  "[,;:\\|]{2,}|--+|\\.\\.+",
  # Literal noise strings
  "Original Comment:",
  "Emission :",
  "rsøkelse",
  "Vannmiljø Station",
  "Station"
)

clean_regex <- str_c(remove_patterns, collapse = "|")

sites <- tar_read(load_literature_pqt) |>
  select(SITE_NAME, SITE_CODE, LATITUDE, LONGITUDE) |>
  distinct()

sites

# QUESTIONS
# What do we need to know?
# A pretty name?
# Fylke/Commune
# Geofeature name
# Address?
# WFD ID, if avaiable?

# In fact, VM has a lot of this available already. How to exploit it?
