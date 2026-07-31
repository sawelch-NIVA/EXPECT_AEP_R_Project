# Common names for scientific species names.
#
# Rewritten 2026-07-30. Scientific names give no intuition for whether a value is
# plausible for an organism, so the triage tables and plots need English names
# wherever one exists.
#
# WoRMS first, NCBI second. NCBI only carries common names for taxa it happens to
# have annotated, which left 55 of 126 species unnamed, including plenty that do
# have well-known English names (Fucus vesiculosus is bladder wrack, Littorina
# littorea the common periwinkle, Lamna nasus the porbeagle). WoRMS is the
# authoritative register for marine species and this dataset is overwhelmingly
# marine.
#
# Species with genuinely no English vernacular (most copepods and amphipods) stay
# NA. That is the expected outcome, not a failure, and it is cached so they are
# not re-queried on every run.

#' Pick One English Vernacular Name
#'
#' WoRMS returns every vernacular in every language, and often a dozen English
#' ones: *Lamna nasus* has 14, led by "(common) Atlantic mackerel shark".
#'
#' Takes the **first** unparenthesised name in the register's own order.
#' Parenthesised forms are dropped because they read badly in a table heading.
#'
#' A shortest-name rule was tried first and rejected on inspection: it returned
#' "Popweed" for *Fucus vesiculosus* rather than bladder wrack, "Rockweed" for
#' *Ascophyllum nodosum* rather than knotted wrack, "Steamer" for *Mya arenaria*,
#' and "Blue dog" for *Lamna nasus*, which is a porbeagle shark. Shortness has no
#' relationship to how well known a name is, and the whole point of this column
#' is recognisability.
#'
#' The register's order is not authoritative either, so a wrong-looking name here
#' is a cache edit away: `data/clean/species_common_names_cache.csv` is
#' hand-editable and is never overwritten for a species already in it.
#'
#' @param x A character vector of candidate names, in source order.
#' @return A single sentence-cased string, or `NA_character_`.
#' @export
pick_common_name <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  # NOT "[()\\[\\]]". In a POSIX bracket expression the `]` closes the class
  # early, so that pattern only matched a paren *followed by* a literal `]` and
  # let "(Common) Atlantic mackerel shark" straight through.
  clean <- x[!grepl("[()]|\\[|\\]", x)]
  if (length(clean) > 0) {
    x <- clean
  }
  # Capitalise the first letter only. str_to_sentence() lowercases everything
  # else, which turns "Common Northern European opossum shrimp" into "... european
  # ..." and "Atlantic" into "atlantic".
  out <- x[1]
  paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
}

#' Fetch an English Common Name from WoRMS
#'
#' Returns `NA_character_` for anything that fails or has no English vernacular.
#' WoRMS answers `204 No Content` for a taxon it holds but has no vernaculars
#' for, which `worrms` surfaces as an error, so the `tryCatch` here is the normal
#' path rather than an exceptional one.
#'
#' @param sp A single scientific name.
#' @return A single string, or `NA_character_`.
#' @export
fetch_common_name_worms <- function(sp) {
  tryCatch(
    {
      id <- worrms::wm_name2id(sp)
      vern <- worrms::wm_common_id(id)
      pick_common_name(vern$vernacular[vern$language %in% "English"])
    },
    error = function(e) NA_character_
  )
}

#' Fetch a Common Name from NCBI via taxize
#'
#' @param sp A single scientific name.
#' @return A single string, or `NA_character_`.
#' @export
fetch_common_name_ncbi <- function(sp) {
  tryCatch(
    {
      res <- suppressMessages(
        taxize::sci2comm(sp, db = "ncbi", simplify = TRUE)
      )
      pick_common_name(unlist(res, use.names = FALSE))
    },
    error = function(e) NA_character_
  )
}

#' Get Common Names for Scientific Species Names
#'
#' Looks up an English common name per species, caching results to CSV so the
#' APIs are hit once per species per database. Databases are tried in the order
#' given and the first hit wins.
#'
#' Fixes carried out 2026-07-30, all of which were live faults:
#'
#' * **`NA` species were queried.** `unique()` on the input column includes `NA`,
#'   which was sent to the API and cached as a row that could never match, so it
#'   was re-queried on every run.
#' * **The join could silently duplicate rows.** Nothing guaranteed one cache row
#'   per species, and a left join against a duplicated cache multiplies the
#'   measurement data. Now de-duplicated before joining, and the row count is
#'   asserted afterwards.
#' * **One API failure lost the whole batch.** `sci2comm()` was called on the
#'   full vector, so a single error returned nothing and nothing was cached.
#'   Queries are now per species, and a failure caches an `NA` rather than
#'   aborting.
#' * **A short or reordered API response mismatched the input.** Building a
#'   tibble from `new_species` alongside `sapply()` over the response assumed
#'   both were the same length and order; `sci2comm()` does not guarantee that.
#' * **An existing `output_col` was silently overwritten**, so re-running on
#'   already-named data destroyed the names.
#'
#' @param biota_data A data frame containing scientific species names.
#' @param input_col Column holding scientific names.
#' @param output_col Column to write common names into.
#' @param cache_path Path to the cache CSV.
#' @param dbs Databases to try, in preference order. WoRMS first: see the note at
#'   the top of this file.
#' @param verbose Print progress?
#' @return `biota_data` with `output_col` added.
#' @export
get_common_names <- function(
  biota_data,
  input_col = "SAMPLE_SPECIES",
  output_col = "SPECIES_COMMON_NAME",
  cache_path = here_rel("data/clean/species_common_names_cache.csv"),
  dbs = c("worms", "ncbi"),
  verbose = TRUE
) {
  cache_cols <- c("scientific_name", "common_name", "db", "date_retrieved")

  cache <- if (file.exists(cache_path)) {
    readr::read_csv(cache_path, show_col_types = FALSE)
  } else {
    tibble::tibble(
      scientific_name = character(),
      common_name = character(),
      db = character(),
      date_retrieved = as.Date(character())
    )
  }
  # Guard the schema rather than assuming it: this cache is hand-editable.
  missing_cols <- setdiff(cache_cols, names(cache))
  if (length(missing_cols) > 0) {
    stop(
      "Cache at ",
      cache_path,
      " is missing column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # NA is not a species. Querying it wastes a call and caches a row that can
  # never match, so it gets re-queried forever.
  species <- unique(biota_data[[input_col]])
  species <- species[!is.na(species) & nzchar(species)]

  fetchers <- list(worms = fetch_common_name_worms, ncbi = fetch_common_name_ncbi)

  for (db_name in dbs) {
    fetcher <- fetchers[[db_name]]
    if (is.null(fetcher)) {
      cli::cli_warn("No fetcher for db {.val {db_name}}; skipping.")
      next
    }
    # Skip anything already resolved by an earlier database, and anything already
    # attempted against this one (including known misses).
    resolved <- cache$scientific_name[!is.na(cache$common_name)]
    attempted <- cache$scientific_name[cache$db == db_name]
    todo <- setdiff(species, union(resolved, attempted))

    if (length(todo) == 0) {
      if (verbose) message("No new species to look up in ", db_name)
      next
    }
    if (verbose) {
      message("Looking up ", length(todo), " species in ", db_name, "...")
    }

    # Per species, so one failure costs one name rather than the whole batch.
    found <- vapply(todo, fetcher, character(1), USE.NAMES = FALSE)

    cache <- dplyr::bind_rows(
      cache,
      tibble::tibble(
        scientific_name = todo,
        common_name = found,
        db = db_name,
        date_retrieved = Sys.Date()
      )
    )
    readr::write_csv(cache, cache_path)
    if (verbose) {
      message(
        "  ",
        sum(!is.na(found)),
        " of ",
        length(todo),
        " resolved from ",
        db_name
      )
    }
  }

  # One row per species, preferring the earliest database in `dbs` that found a
  # name. Without the de-duplication the left join below multiplies rows.
  lookup <- cache |>
    dplyr::filter(!is.na(.data$common_name), !is.na(.data$scientific_name)) |>
    dplyr::mutate(.rank = match(.data$db, dbs)) |>
    dplyr::arrange(.data$.rank) |>
    dplyr::distinct(.data$scientific_name, .keep_all = TRUE) |>
    dplyr::select("scientific_name", "common_name")

  if (verbose) {
    hits <- sum(species %in% lookup$scientific_name)
    message(
      "Common names available for ",
      hits,
      " of ",
      length(species),
      " species (",
      round(100 * hits / max(1, length(species))),
      "%)"
    )
  }

  # Refuse to clobber an existing column: re-running on already-named data used
  # to overwrite it.
  if (output_col %in% names(biota_data)) {
    biota_data[[output_col]] <- NULL
  }

  n_before <- nrow(biota_data)
  out <- biota_data |>
    dplyr::left_join(
      lookup |> dplyr::rename(!!output_col := "common_name"),
      by = stats::setNames("scientific_name", input_col)
    )
  if (nrow(out) != n_before) {
    stop(
      "get_common_names() changed the row count from ",
      n_before,
      " to ",
      nrow(out),
      ": the cache has duplicate scientific_name entries."
    )
  }
  out
}
