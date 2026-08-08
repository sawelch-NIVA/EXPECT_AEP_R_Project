# Stable identifiers for sample groups (2026-07-30).
#
# A group is defined by an eight-column key, which is unusable as a reference: you
# cannot write it in a note, a commit message, or a `lump_into` cell. This gives
# every group a short accession number instead.
#
# THE PROPERTY THAT MATTERS IS STABILITY. These IDs end up in hand-written notes
# and, eventually, in the manuscript. An ID whose value depends on what else is in
# the set silently re-points every existing reference when the data change. That
# rules out anything derived from rank or row order: rank moves whenever `n`
# moves, so a rank-derived `G001` means Freshwater today and something else after
# the next Vannmiljø pull, with nothing erroring.
#
# So IDs are ALLOCATED ONCE AND STORED, never recomputed. The ledger is the
# authority; the code only appends to it. Same cache-versus-curation split as the
# decisions file: `data/clean/decisions/group_ids.csv` is append-only identity, and
# `group_decisions.csv` is judgement.
#
# Deliberately a separate file from the decisions. Identity and judgement have
# different lifecycles, and keeping them apart means the summary table and the
# triage notebook can carry IDs without depending on whether anyone has made a
# decision yet.
#
# UPDATE 2026-08-08: `group_id` is no longer just the bare `G001` accession
# number -- see format_composite_group_id() below for the compartment/
# geography/species/tissue/unit code appended to it, and
# scripts/migrate_group_ids_to_composite.R for the one-off rewrite that
# folded the composite form INTO the ledger itself (Sam: "I want to replace
# the old ones with them"). THE PROPERTY THAT MATTERS is unchanged: whatever
# string sits in `group_id` for a given group is frozen the moment it is
# written, and only ever rewritten by that one-off script, never by the
# ordinary pipeline. A group's compartment or species getting hand-corrected
# later does NOT retroactively update its id -- the composite form is a
# snapshot taken once, not a live view.

#' Format a Group ID
#'
#' `G` prefix so IDs are greppable and cannot be mistaken for `n` or `rank`.
#' Zero-padded to three digits so they sort lexically; 999 is ample headroom
#' against the current 245 groups.
#'
#' @param i Integer vector.
#' @return A character vector.
#' @export
format_group_id <- function(i) {
  sprintf("G%03d", as.integer(i))
}

#' Read the Group ID Ledger
#'
#' @param path Path to the ledger CSV.
#' @return A tibble of the group key plus `group_id`, or a zero-row tibble.
#' @export
read_group_ids <- function(
  path = here::here("data/clean/decisions/group_ids.csv")
) {
  key <- triage_group_cols()
  if (!file.exists(path)) {
    empty <- as.data.frame(
      stats::setNames(rep(list(character(0)), length(key)), key)
    )
    return(tibble::as_tibble(cbind(empty, group_id = character(0))))
  }
  ids <- readr::read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(c(key, "group_id"), names(ids))
  if (length(missing_cols) > 0) {
    stop(
      "ID ledger at ", path, " is missing column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (anyDuplicated(ids$group_id) > 0) {
    stop("ID ledger contains duplicate group_id values.")
  }
  ids
}

#' Allocate IDs to Any Groups That Lack One
#'
#' Append-only. Existing IDs are never changed and **retired IDs are never
#' reused**: the next ID is always one past the highest ever issued, not one past
#' the highest currently in use. Reuse is how a note written in March ends up
#' pointing at a different group in September.
#'
#' Allocation order for a fresh ledger is by `n` descending, which is arbitrary
#' but harmless because it is frozen immediately.
#'
#' @param summary_data The `summarise_literature_data` target.
#' @param path Path to the ledger CSV.
#' @param verbose Report what changed?
#' @return The full ledger, invisibly.
#' @export
allocate_group_ids <- function(
  summary_data,
  path = here::here("data/clean/decisions/group_ids.csv"),
  verbose = TRUE
) {
  key <- triage_group_cols()
  ledger <- read_group_ids(path)

  wanted <- summary_data |>
    dplyr::arrange(dplyr::desc(.data$n)) |>
    dplyr::select(dplyr::all_of(key)) |>
    dplyr::distinct()

  have <- do.call(paste, ledger[key])
  need <- wanted[!do.call(paste, wanted[key]) %in% have, , drop = FALSE]

  if (nrow(need) > 0) {
    highest <- if (nrow(ledger) == 0) {
      0L
    } else {
      # Not just sub("^G", ""): once group_id carries a composite suffix
      # (Sam 2026-08-08: "G014-Bf-Cnr-Gmor-Liv-Mw" rather than bare "G014"),
      # stripping only the "G" leaves "014-Bf-Cnr-..." and as.integer() of
      # that is NA. Capture the leading digit run and discard the rest.
      max(as.integer(sub("^G(\\d+).*$", "\\1", ledger$group_id)))
    }
    need$group_id <- format_group_id(highest + seq_len(nrow(need)))
    ledger <- dplyr::bind_rows(ledger, need)
    readr::write_csv(ledger, path, na = "")
  }

  retired <- sum(!have %in% do.call(paste, wanted[key]))
  if (verbose) {
    message(
      "group_ids.csv: ", nrow(ledger), " IDs (",
      nrow(need), " newly allocated, ", retired,
      " retired but kept)"
    )
  }
  invisible(ledger)
}

#' Read a Group Code Lookup
#'
#' Shared reader for the three hand-edited lookups behind
#' [format_composite_group_id()]. Kept separate from [read_group_ids()]
#' because these map *vocabulary values* (compartment names, species groups)
#' to short codes, not group keys to IDs, and have no `n`-driven allocation
#' step -- Sam edits them directly when a new value appears in the data.
#'
#' @param path Path to the lookup CSV.
#' @param key_col Name of the value column (e.g. `"ENVIRON_COMPARTMENT"`).
#' @return A tibble with `key_col` and `code`.
#' @keywords internal
read_group_code_lookup <- function(path, key_col) {
  lookup <- readr::read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(c(key_col, "code"), names(lookup))
  if (length(missing_cols) > 0) {
    stop(
      "Code lookup at ", path, " is missing column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  lookup
}

#' One Capital-Plus-Lowercase Code Block
#'
#' A single axis of [format_composite_group_id()]: a parent value's capital
#' letter directly followed by a child value's lowercase code, e.g. `"Wfw"`
#' (Aquatic + Freshwater) or `"Bl"` (Biota + Molluscs). No hyphen inside the
#' block -- the capital/lowercase split *is* the parent/child marker (Sam
#' 2026-08-07: "take out the hyphen between B and L... make the second letter
#' small so that the hierarchy is represented"), so hyphens are reserved for
#' separating axes from each other, not levels within one axis.
#'
#' @param parent,child Character vectors, same length, e.g.
#'   `data$ENVIRON_COMPARTMENT` and the row-wise sub-compartment/species-group
#'   value already selected by the caller.
#' @param parent_codes,child_codes Lookups from [read_group_code_lookup()],
#'   keyed on `parent_name`/`child_name` respectively.
#' @param parent_name,child_name The lookups' key columns.
#' @return A character vector of blocks, `NA` where either lookup misses.
#' @keywords internal
group_code_block <- function(
  parent, child, parent_codes, child_codes, parent_name, child_name
) {
  parent_code <- parent_codes$code[match(parent, parent_codes[[parent_name]])]
  child_code <- tolower(child_codes$code[match(child, child_codes[[child_name]])])
  ifelse(is.na(parent_code) | is.na(child_code), NA_character_, paste0(parent_code, child_code))
}

#' Abbreviate a Scientific Name
#'
#' Deterministic, not looked up: `"Gadus morhua"` -> `"G-mor"`, capital genus
#' initial, hyphen, first 3 letters of the epithet in lowercase (Sam
#' 2026-08-07: "an abbreviated form of the abbreviated scientific name...
#' this costs more in characters but I think will be worth it"). Originally a
#' dot rather than a hyphen; changed 2026-08-08 once it was clear the id
#' would be embedded in Quarto heading anchors and `@fig-` cross-reference
#' ids -- a dot means something in a CSS id selector, and anything doing
#' `querySelector('#' + id)` unescaped would break on it, so the separator
#' had to match the hyphens used everywhere else in the composite id. A
#' single-word name -- a genus, family, order or other higher taxon on its
#' own, e.g. `"Chironomidae"`, `"Littorina"` -- has no epithet to abbreviate,
#' so it instead takes its own first 4 letters, capitalised: `"Chir"`.
#'
#' The scheme is NOT collision-free: two different genera sharing an epithet
#' (`"Hymenodora glacialis"` / `"Heliometra glacialis"`, both -> `"H-gla"`)
#' or a trinomial's subspecies (`"Odobenus rosmarus divergens"` /
#' `"...rosmarus"`, both -> `"O-ros"`) collide by construction. `overrides`
#' exists for exactly this: a hand-edited lookup of the specific names that
#' need a different code, `data/clean/lookups/group_species_code_overrides.csv`.
#' [format_composite_group_id()] does not error or warn on a collision here --
#' the code is a readability aid layered on the stable G-number, which is
#' already unique on its own, so a collision degrades legibility rather than
#' identity.
#'
#' Running this over the committed ledger (2026-08-07) turned up two
#' collisions that looked like genuine taxonomic duplicates rather than
#' unrelated species that happen to collide, and Sam fixed both at the
#' source (2026-08-08): the misspelled/superseded name was corrected to
#' match its counterpart, so the two pairs are now genuinely the same
#' species rather than a code collision -- see item 14 in `misc-todo.md`.
#'
#' @param species Character vector of `SAMPLE_SPECIES` values. `NA`/blank
#'   pass through as `NA`, since the whole species+tissue block is omitted
#'   for a group with no species (Sam: "this is an optional block, we don't
#'   need to include it in stuff without a species").
#' @param overrides A tibble with `SAMPLE_SPECIES` and `code`, or `NULL` to
#'   read the default lookup (an empty table if it doesn't exist yet).
#' @return A character vector, same length as `species`.
#' @export
format_species_code <- function(species, overrides = NULL) {
  if (is.null(overrides)) {
    path <- here::here("data/clean/lookups/group_species_code_overrides.csv")
    overrides <- if (file.exists(path)) {
      read_group_code_lookup(path, "SAMPLE_SPECIES")
    } else {
      data.frame(SAMPLE_SPECIES = character(0), code = character(0))
    }
  }

  derived <- vapply(species, function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(NA_character_)
    }
    parts <- strsplit(x, "\\s+")[[1]]
    if (length(parts) >= 2) {
      paste0(toupper(substr(parts[1], 1, 1)), "-", tolower(substr(parts[2], 1, 3)))
    } else {
      paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 4)))
    }
  }, character(1), USE.NAMES = FALSE)

  override_code <- overrides$code[match(species, overrides$SAMPLE_SPECIES)]
  ifelse(!is.na(override_code), override_code, derived)
}

#' Composite Group Codes
#'
#' Builds the human-referenceable form of a group id -- e.g.
#' `"G014-Wfw-Cwb-G-mor-Liv-Mw"` from the bare accession number `"G014"` plus
#' its compartment, geography, species/tissue and unit.
#'
#' UPDATE 2026-08-08: this used to be a pure display-layer function, called
#' fresh on every render so `group_id` in the ledger stayed the bare number.
#' It is now instead the engine behind a ONE-OFF migration
#' (`scripts/migrate_group_ids_to_composite.R`) that wrote its output
#' directly into `group_id` in the ledger -- the composite form IS the id
#' everywhere now, not a label recomputed on top of it. Call this function
#' again only to regenerate that migration (e.g. after extending the
#' lookups to cover more of the grouping variables), not as part of any
#' normal read path -- calling it on a table whose `group_id` is already
#' composite would double-append the blocks.
#'
#' Scheme (Sam 2026-08-07): `G<num>-<compartment block>-<geography block>
#' [-<species code>-<tissue code>]-<unit code>`. The compartment and
#' geography blocks are each a [group_code_block()]: one capital parent
#' letter directly followed by a lowercase child code, no internal hyphen, so
#' case alone marks the parent/child split within that block. The
#' compartment block's capital is Water/Earth/Air/Biota; its lowercase part
#' is a 2-letter sub-compartment code, or a 1-letter species-group code when
#' the compartment is Biota (Biota's own `ENVIRON_COMPARTMENT_SUB` is always
#' just "Biota, Aquatic" / "Biota, Terrestrial" and carries no information
#' the species group doesn't already give more usefully, hence the swap
#' rather than a fourth lookup). The geography block's capital is
#' `SITE_GEOGRAPHIC_FEATURE`'s first letter and its lowercase part is a
#' 2-letter `SITE_GEOGRAPHIC_FEATURE_SUB` code.
#'
#' `SITE_GEOGRAPHIC_FEATURE`'s own letters deliberately avoid W/E/A/B, the
#' compartment letters: the geography block sits immediately after the
#' compartment block, so reusing one of those (WWTP -> "W" would have been
#' the obvious pick) would put two same-lettered-but-different-meaning
#' capitals back to back, e.g. an aquifer sample would misleadingly read as
#' "GW" repeated. `data/clean/lookups/group_geography_codes.csv` documents
#' the choice per feature.
#'
#' The species/tissue segment is OPTIONAL and omitted entirely for a group
#' with no `SAMPLE_SPECIES` (Sam: "we don't need to include it in stuff
#' without a species") -- unlike the compartment/geography blocks, its
#' absence is not a gap and does not warn. It is two hyphen-joined pieces
#' rather than one [group_code_block()], because species and tissue are
#' peers describing the same sample rather than a parent/child pair, and the
#' species code already uses its own internal `.` (see
#' [format_species_code()]).
#'
#' The unit code is always present: `"C"` (concentration, `mg/L`), `"Md"`
#' (mass, dry weight), `"Mw"` (mass, wet weight), or `"X"` for anything else
#' -- a real category, not a gap, so an unmapped unit falls back to `"X"`
#' silently rather than warning.
#'
#' @param data A table carrying `group_id` plus at least
#'   `ENVIRON_COMPARTMENT`, `ENVIRON_COMPARTMENT_SUB`, `SPECIES_GROUP`,
#'   `SITE_GEOGRAPHIC_FEATURE`, `SITE_GEOGRAPHIC_FEATURE_SUB`,
#'   `SAMPLE_SPECIES`, `SAMPLE_TISSUE`, `MEASURED_UNIT_STANDARD`.
#' @param compartment_codes,subcompartment_codes,species_group_codes,
#'   geography_codes,geography_sub_codes,tissue_codes,unit_codes The seven
#'   lookups, from [read_group_code_lookup()]. `NULL` reads them from their
#'   default paths in `data/clean/lookups/`.
#' @param species_overrides Passed to [format_species_code()].
#' @return A character vector the same length as `nrow(data)`. A row with no
#'   compartment or geography code yet warns once and falls back to the bare
#'   `group_id`, so a lookup that has fallen behind the data fails loud
#'   rather than emitting a silently wrong code. A row with a species but no
#'   tissue code yet warns separately and drops just the species/tissue
#'   segment, keeping the rest of the ID.
#' @export
format_composite_group_id <- function(
  data,
  compartment_codes = NULL,
  subcompartment_codes = NULL,
  species_group_codes = NULL,
  geography_codes = NULL,
  geography_sub_codes = NULL,
  tissue_codes = NULL,
  unit_codes = NULL,
  species_overrides = NULL
) {
  if (is.null(compartment_codes)) {
    compartment_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_compartment_codes.csv"),
      "ENVIRON_COMPARTMENT"
    )
  }
  if (is.null(subcompartment_codes)) {
    subcompartment_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_subcompartment_codes.csv"),
      "ENVIRON_COMPARTMENT_SUB"
    )
  }
  if (is.null(species_group_codes)) {
    species_group_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_species_group_codes.csv"),
      "SPECIES_GROUP"
    )
  }
  if (is.null(geography_codes)) {
    geography_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_geography_codes.csv"),
      "SITE_GEOGRAPHIC_FEATURE"
    )
  }
  if (is.null(geography_sub_codes)) {
    geography_sub_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_geography_sub_codes.csv"),
      "SITE_GEOGRAPHIC_FEATURE_SUB"
    )
  }
  if (is.null(tissue_codes)) {
    tissue_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_tissue_codes.csv"),
      "SAMPLE_TISSUE"
    )
  }
  if (is.null(unit_codes)) {
    unit_codes <- read_group_code_lookup(
      here::here("data/clean/lookups/group_unit_codes.csv"),
      "MEASURED_UNIT_STANDARD"
    )
  }

  # ENVIRON_COMPARTMENT_SUB and SPECIES_GROUP are two different lookups keyed
  # on two different columns, so this can't be a single group_code_block()
  # call; resolve each row against its own lookup by ENVIRON_COMPARTMENT.
  is_biota <- data$ENVIRON_COMPARTMENT == "Biota"
  compartment_child_code <- ifelse(
    is_biota,
    tolower(species_group_codes$code[match(data$SPECIES_GROUP, species_group_codes$SPECIES_GROUP)]),
    tolower(subcompartment_codes$code[
      match(data$ENVIRON_COMPARTMENT_SUB, subcompartment_codes$ENVIRON_COMPARTMENT_SUB)
    ])
  )
  compartment_parent_code <- compartment_codes$code[
    match(data$ENVIRON_COMPARTMENT, compartment_codes$ENVIRON_COMPARTMENT)
  ]
  compartment_block <- ifelse(
    is.na(compartment_parent_code) | is.na(compartment_child_code),
    NA_character_,
    paste0(compartment_parent_code, compartment_child_code)
  )

  geography_block <- group_code_block(
    data$SITE_GEOGRAPHIC_FEATURE, data$SITE_GEOGRAPHIC_FEATURE_SUB,
    geography_codes, geography_sub_codes,
    "SITE_GEOGRAPHIC_FEATURE", "SITE_GEOGRAPHIC_FEATURE_SUB"
  )

  composite <- paste(data$group_id, compartment_block, geography_block, sep = "-")
  missing <- is.na(compartment_block) | is.na(geography_block)
  if (any(missing)) {
    cli::cli_warn(c(
      "{sum(missing)} group(s) have no compartment/geography code yet.",
      "i" = "Add the missing value(s) to the lookup CSVs in data/clean/lookups/."
    ))
    composite[missing] <- data$group_id[missing]
  }

  has_species <- !is.na(data$SAMPLE_SPECIES) & nzchar(data$SAMPLE_SPECIES)
  species_code <- format_species_code(data$SAMPLE_SPECIES, species_overrides)
  tissue_code <- tissue_codes$code[match(data$SAMPLE_TISSUE, tissue_codes$SAMPLE_TISSUE)]
  needs_tissue <- has_species & !missing & is.na(tissue_code)
  if (any(needs_tissue)) {
    cli::cli_warn(c(
      "{sum(needs_tissue)} group(s) have a species but no tissue code yet.",
      "i" = "Add the missing SAMPLE_TISSUE value(s) to group_tissue_codes.csv."
    ))
  }
  add_species_tissue <- has_species & !missing & !is.na(tissue_code)
  composite[add_species_tissue] <- paste0(
    composite[add_species_tissue], "-",
    species_code[add_species_tissue], "-", tissue_code[add_species_tissue]
  )

  unit_code <- unit_codes$code[match(data$MEASURED_UNIT_STANDARD, unit_codes$MEASURED_UNIT_STANDARD)]
  unit_code[is.na(unit_code)] <- "X"
  composite[!missing] <- paste0(composite[!missing], "-", unit_code[!missing])

  composite
}

#' Attach Group IDs to a Table
#'
#' Left join on the full group key, with an assertion that the row count did not
#' change: a ledger with duplicate keys would otherwise multiply the data
#' silently.
#'
#' @param data Any table carrying the group-key columns.
#' @param ids The ledger, from [read_group_ids()].
#' @param warn_missing Warn about groups with no ID yet?
#' @return `data` with a `group_id` column.
#' @export
attach_group_ids <- function(data, ids, warn_missing = TRUE) {
  key <- triage_group_cols()
  n_before <- nrow(data)

  out <- data |>
    dplyr::left_join(
      ids |> dplyr::select(dplyr::all_of(key), "group_id"),
      by = key
    )

  if (nrow(out) != n_before) {
    stop(
      "attach_group_ids() changed the row count from ", n_before, " to ",
      nrow(out), ": the ID ledger has duplicate group keys."
    )
  }
  if (warn_missing && any(is.na(out$group_id))) {
    cli::cli_warn(c(
      "{sum(is.na(out$group_id))} group(s) have no ID yet.",
      "i" = "Run scripts/allocate_group_ids.R to append them."
    ))
  }
  out
}
