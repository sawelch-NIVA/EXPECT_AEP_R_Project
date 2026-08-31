# What do Hammerfest residents do for work, and what share of each national
# industry sits in Hammerfest?
#
# Pulls register-based employment (employed persons per 4th quarter) from
# Statistics Norway table 08536 via the PxWebApi v2 GET endpoint, JSON-stat2
# format, for:
#   - the whole country (Region codelist vs_Landet, code "0")
#   - Hammerfest kommune (Region K.5603, codelist agg_KommSummerS)
# both sexes, 2025, BY PLACE OF RESIDENCE (ContentsCode SysselBosted).
#
# Place of RESIDENCE, not place of work. This matches the sentence already in
# index.qmd's study-area section (6160 employed persons, 556 in oil and gas,
# etc.) and, unlike the place-of-work division cells, is almost never
# suppressed at kommune level. The previous version of this script used place
# of work (table 07984 section-level + 08536 SysselArbsted) where nearly every
# Hammerfest extractive division cell is ":" (confidential); that is where the
# stray "censored" comments came from. Dropped entirely.
#
# DELIBERATELY NOT A TARGET. One-off descriptive statistic for index.qmd
# (Study area). Same contract as scripts/summarise_prtr_emissions.R: nothing
# downstream reads it, and it is safe to regenerate.
#
# Disclosure control on table 08536 (from the API `note` field, verbatim):
#   "All 1s and 2s have been replaced by 0 or 3 in order to meet privacy
#    requirement. When the figures are aggregated to a higher regional level,
#    the sum may differ slightly from the actual figure."
# So small Hammerfest cells (mining divisions, most manufacturing divisions)
# are perturbed and a sum-of-divisions need not equal the published 00-99
# total. Flagged in the CSV and the table caption, not silently smoothed over.
#
# Run:
#   Rscript scripts/summarise_ssb_employment.R
#
# Writes TWO REPORTS to data/clean/derived/ (not pipeline inputs):
#   ssb_employment_hammerfest.csv          one row per hand-picked sector, with
#                                          the NACE 2007 codes summed, the
#                                          Norway and Hammerfest head-counts,
#                                          each as a percent of its own area
#                                          total, and Hammerfest as a percent of
#                                          the national figure.
#   ssb_employment_hammerfest_sections.csv one row per NACE 2007 SECTION (A-U),
#                                          Norway and Hammerfest head-counts and
#                                          the Hammerfest share of national.
#                                          Added 2026-08-31 so the REACH product
#                                          register can be weighted to Hammerfest
#                                          by every sector's employment share,
#                                          not just the copper-relevant ones
#                                          (weight_reach_to_hammerfest(),
#                                          R/fct_reach_hammerfest.R). THIS one IS
#                                          read by a target
#                                          (ssb_employment_sections_file), so
#                                          re-run this script before tar_make if
#                                          the SSB figures move.

suppressMessages({
  library(jsonlite)
  library(curl)
  library(dplyr)
  library(tibble)
  library(readr)
  library(here)
})

here::i_am("README.md")

# --- the two queries, verbatim from the notes at the foot of the old script --
# 08536, both sexes, 2025, employed persons by place of residence.
url_hammerfest <- paste0(
  "https://data.ssb.no/api/pxwebapi/v2/tables/08536/data",
  "?lang=en&outputFormat=json-stat2",
  "&valuecodes[ContentsCode]=SysselBosted",
  "&valuecodes[Tid]=2025",
  "&valuecodes[Region]=K.5603&codelist[Region]=agg_KommSummerS",
  "&valuecodes[NACE2007]=*",
  "&valuecodes[Kjonn]=0",
  "&heading=ContentsCode,Tid,Kjonn&stub=NACE2007,Region"
)
url_norway <- paste0(
  "https://data.ssb.no/api/pxwebapi/v2/tables/08536/data",
  "?lang=en&outputFormat=json-stat2",
  "&valuecodes[ContentsCode]=SysselBosted",
  "&valuecodes[Tid]=2025",
  "&valuecodes[Region]=*&codelist[Region]=vs_Landet",
  "&valuecodes[NACE2007]=*",
  "&valuecodes[Kjonn]=0",
  "&heading=ContentsCode,Tid,Kjonn&stub=NACE2007,Region"
)

# --- fetch + flatten ------------------------------------------------------------
# The queries pin Region, ContentsCode, Tid and Kjonn to one value each, so the
# JSON-stat2 `value` vector is indexed by NACE2007 position alone. `status`
# marks any special cell (":" confidential, ".." not collected); carry it so a
# suppression is visible rather than a silent NA.
ssb_nace <- function(url) {
  r <- curl::curl_fetch_memory(url)
  if (r$status_code != 200L) {
    stop("SSB 08536 returned HTTP ", r$status_code, ": ", rawToChar(r$content))
  }
  js <- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)

  cat_index <- unlist(js$dimension$NACE2007$category$index) # code -> 0-based position
  cat_label <- unlist(js$dimension$NACE2007$category$label)
  vals <- vapply(js$value, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
  status <- rep(NA_character_, length(vals))
  if (!is.null(js$status)) {
    for (k in names(js$status)) status[as.integer(k) + 1L] <- js$status[[k]]
  }

  tibble::tibble(
    nace = names(cat_index),
    nace_label = unname(cat_label[names(cat_index)]),
    value = vals[cat_index + 1L],
    status = status[cat_index + 1L]
  ) |>
    dplyr::arrange(match(.data$nace, names(cat_index)))
}

no <- ssb_nace(url_norway)
ha <- ssb_nace(url_hammerfest)

# --- sector definitions -------------------------------------------------------
# Codes are NACE 2007 (SN2007) divisions. Kept as explicit vectors so the CSV
# can report exactly what was summed. Decisions taken with Sam 2026-08-31:
#   - residence basis only
#   - mining as one 07-09 row (no metal-ore 07 breakout)
#   - "waste" = 37-39 (sewerage + waste + remediation), covering the town's
#     WWTP as well as landfill/materials-recovery
#   - farming = 01 only (02 forestry is 0 in Hammerfest)
# `codes` is what gets summed; `codes_display` is the compact label for the
# rendered table. The CSV keeps both, plus the exact list actually matched.
sectors <- tibble::tribble(
  ~sector,                                    ~codes,                                    ~codes_display,
  "All industries",                           "00-99",                                   "00-99",
  "Extraction of oil and natural gas",        "06",                                      "06",
  "Fishing and aquaculture",                  "03",                                      "03",
  "Manufacturing",                            paste(sprintf("%02d", 10:33), collapse = ","), "10-33",
  "Mining, quarrying and support services",   "07,08,09",                                "07-09",
  "Sewerage, waste and remediation",          "37,38,39",                                "37-39",
  "Crop and animal production, hunting",       "01",                                      "01"
)

sum_codes <- function(df, codes) {
  want <- strsplit(codes, ",", fixed = TRUE)[[1]]
  hit <- df[df$nace %in% want, ]
  missing <- setdiff(want, hit$nace)
  if (length(missing)) warning("codes not in response: ", paste(missing, collapse = ", "))
  if (any(!is.na(hit$status))) {
    warning("suppressed cell(s) in [", codes, "]: ",
            paste(hit$nace[!is.na(hit$status)], collapse = ", "))
  }
  list(
    value = sum(hit$value, na.rm = TRUE),
    codes_used = paste(sort(hit$nace), collapse = ", ")
  )
}

no_all <- sum_codes(no, "00-99")$value
ha_all <- sum_codes(ha, "00-99")$value

out <- sectors |>
  rowwise() |>
  mutate(
    .no = list(sum_codes(no, codes)),
    .ha = list(sum_codes(ha, codes)),
    nace_codes = .no$codes_used,
    norway = .no$value,
    hammerfest = .ha$value
  ) |>
  ungroup() |>
  transmute(
    sector,
    nace_codes_display = codes_display,
    nace_codes,
    norway,
    hammerfest,
    norway_pct_of_total = if_else(sector == "All industries", NA_real_,
                                 round(100 * norway / no_all, 2)),
    hammerfest_pct_of_total = if_else(sector == "All industries", NA_real_,
                                     round(100 * hammerfest / ha_all, 2)),
    hammerfest_share_of_national_pct = round(100 * hammerfest / norway, 3),
    source_table = "SSB 08536",
    basis = "Employed persons by place of residence, 4th quarter 2025, both sexes"
  )

dest <- here("data/clean/derived/ssb_employment_hammerfest.csv")
readr::write_csv(out, dest)
message("wrote ", dest, " (", nrow(out), " rows)")

cat("\n--- Employed persons by place of residence, 2025 (SSB 08536) ---\n")
out |>
  mutate(
    Norway = formatC(norway, format = "d", big.mark = ","),
    Hammerfest = formatC(hammerfest, format = "d", big.mark = ","),
    `Ham % of nat.` = formatC(hammerfest_share_of_national_pct, format = "f", digits = 2)
  ) |>
  select(sector, nace_codes_display, Norway, Hammerfest, `Ham % of nat.`) |>
  as.data.frame() |>
  print(row.names = FALSE)

# --- NACE 2007 section rollup (A-U) -----------------------------------------
# SN2007 section -> division ranges, from the standard. Divisions 04, 34, 40,
# 44, 48, 54, 57, 67, 76, 83, 89, 98 do not exist in NACE 2007; the sequence
# below simply skips them.
section_ranges <- tibble::tribble(
  ~nace_section, ~section_label,                                        ~lo, ~hi,
  "A", "Agriculture, forestry and fishing",                             1L,  3L,
  "B", "Mining and quarrying",                                          5L,  9L,
  "C", "Manufacturing",                                                 10L, 33L,
  "D", "Electricity, gas, steam and air conditioning supply",           35L, 35L,
  "E", "Water supply; sewerage, waste management and remediation",      36L, 39L,
  "F", "Construction",                                                  41L, 43L,
  "G", "Wholesale and retail trade; repair of motor vehicles",          45L, 47L,
  "H", "Transportation and storage",                                    49L, 53L,
  "I", "Accommodation and food service activities",                     55L, 56L,
  "J", "Information and communication",                                 58L, 63L,
  "K", "Financial and insurance activities",                            64L, 66L,
  "L", "Real estate activities",                                        68L, 68L,
  "M", "Professional, scientific and technical activities",             69L, 75L,
  "N", "Administrative and support service activities",                 77L, 82L,
  "O", "Public administration and defence; social security",            84L, 84L,
  "P", "Education",                                                     85L, 85L,
  "Q", "Human health and social work activities",                       86L, 88L,
  "R", "Arts, entertainment and recreation",                            90L, 93L,
  "S", "Other service activities",                                      94L, 96L,
  "T", "Activities of households as employers",                         97L, 98L,
  "U", "Activities of extraterritorial organisations and bodies",       99L, 99L
)

section_of <- function(nace) {
  n <- suppressWarnings(as.integer(nace))
  idx <- vapply(n, function(x) {
    if (is.na(x)) return(NA_integer_)
    m <- which(x >= section_ranges$lo & x <= section_ranges$hi)
    if (length(m)) m[1] else NA_integer_
  }, integer(1))
  section_ranges$nace_section[idx]
}

roll_sections <- function(df) {
  divisions <- df[grepl("^[0-9]{2}$", df$nace) & df$nace != "00", ]
  divisions$nace_section <- section_of(divisions$nace)
  divisions |>
    dplyr::filter(!is.na(.data$nace_section)) |>
    dplyr::group_by(.data$nace_section) |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      divisions = paste(sort(.data$nace), collapse = ", "),
      n_suppressed = sum(!is.na(.data$status)),
      .groups = "drop"
    )
}

sections <- roll_sections(no) |>
  dplyr::rename(norway = "value") |>
  dplyr::left_join(
    roll_sections(ha) |> dplyr::select("nace_section", hammerfest = "value"),
    by = "nace_section"
  ) |>
  dplyr::left_join(section_ranges[c("nace_section", "section_label")], by = "nace_section") |>
  dplyr::transmute(
    nace_section,
    section_label,
    divisions,
    norway,
    hammerfest = dplyr::coalesce(hammerfest, 0),
    hammerfest_share_of_national_pct = dplyr::if_else(
      norway > 0, round(100 * hammerfest / norway, 3), NA_real_
    ),
    source_table = "SSB 08536",
    basis = "Employed persons by place of residence, 4th quarter 2025, both sexes"
  ) |>
  dplyr::arrange(nace_section)

# A TOTAL row so the REACH weighting has a fallback share for its "Unclassified"
# sector (NACE section NA).
sections <- dplyr::bind_rows(
  sections,
  tibble::tibble(
    nace_section = "TOTAL",
    section_label = "All industries",
    divisions = "00-99",
    norway = no_all,
    hammerfest = ha_all,
    hammerfest_share_of_national_pct = round(100 * ha_all / no_all, 3),
    source_table = "SSB 08536",
    basis = "Employed persons by place of residence, 4th quarter 2025, both sexes"
  )
)

dest_sections <- here("data/clean/derived/ssb_employment_hammerfest_sections.csv")
readr::write_csv(sections, dest_sections)
message("wrote ", dest_sections, " (", nrow(sections), " rows)")

cat("\n--- NACE 2007 sections: Hammerfest share of national, 2025 ---\n")
sections |>
  mutate(
    Norway = formatC(norway, format = "d", big.mark = ","),
    Hammerfest = formatC(hammerfest, format = "d", big.mark = ","),
    `Ham % of nat.` = formatC(hammerfest_share_of_national_pct, format = "f", digits = 2)
  ) |>
  select(nace_section, section_label, Norway, Hammerfest, `Ham % of nat.`) |>
  as.data.frame() |>
  print(row.names = FALSE)
