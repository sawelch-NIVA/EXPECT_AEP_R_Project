#' The Project Bibliography, Cached
#'
#' `RefManageR::ReadBib()` over `references.bib`, read once per session (it is
#' a 4.6 MB, ~2400-entry file and slow to reparse). Chosen over `bib2df`:
#' spot-checked against four real keys 2026-09-12, `bib2df` left `YEAR` blank
#' for entries `RefManageR` parsed correctly, and produced duplicate rows for
#' at least one key -- this file is exactly the kind of large, slightly messy
#' real-world `.bib` `bib2df`'s own warning ("entries may have been dropped")
#' is about.
#'
#' @return A `RefManageR::BibEntry` object.
project_bibliography <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- suppressWarnings(
        RefManageR::ReadBib(here::here("references.bib"), check = FALSE)
      )
    }
    cached
  }
})

#' The Project Bibliography as a Data Frame, Cached
#'
#' `as.data.frame()` on a ~2400-entry `BibEntry` is itself expensive (it is
#' RefManageR formatting every field of every entry), not just the initial
#' `ReadBib()` parse -- caching only [project_bibliography()] and redoing this
#' conversion inside [match_reference_bib_keys()] on every call is what turned
#' `summarise_literature_data` into a 16-minute target (2026-09-12): with
#' ~245 groups each calling into it, the conversion ran ~245 times instead of
#' once. This cache is the fix; `match_reference_bib_keys()` must call this,
#' never `as.data.frame(project_bibliography())` directly.
#'
#' @return A data frame with the bibliography's fields plus `BIBTEXKEY` and
#'   `YEAR_NUM` (integer, `NA` where `year` does not parse).
project_bibliography_df <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      bib <- project_bibliography()
      df <- as.data.frame(bib)
      df$BIBTEXKEY <- names(bib)
      df$YEAR_NUM <- suppressWarnings(as.integer(df$year))
      cached <<- df
    }
    cached
  }
})

#' Author-Year Citation Text for One or More BibTeX Keys
#'
#' Formats "Surname (Year)" / "Surname1 & Surname2 (Year)" / "Surname1 et al.
#' (Year)", looked up directly from `references.bib` via
#' [project_bibliography()]. Built for the AEP node tables' external-node
#' `references` cell (Sam, 2026-09-12): a flextable cell is raw OOXML inside a
#' docx and is never seen by pandoc's citeproc, so typing a literal `[@key]`
#' there would render as literal bracket text and never resolve to a real
#' citation. This builds the same author-year text citeproc would, from the
#' same source file, so the cell reads correctly regardless.
#'
#' A key that fails to resolve in the bibliography is shown as-is (the raw key
#' text) rather than dropped, so a typo or a reference not yet added to
#' `references.bib` stays visible instead of silently vanishing.
#'
#' **This alone does not put the reference into the manuscript's bibliography
#' section** -- pandoc's citeproc only collects entries it saw cited as real
#' `[@key]` syntax somewhere it parses (prose, headings, table captions), never
#' inside a flextable cell. The AEP node-table chunks in `_03-results.qmd`
#' additionally fold these same keys into their `tbl-cap`, which IS parsed
#' text (this file already has real citations inside `fig-cap`/`tbl-cap`
#' strings elsewhere, e.g. `_06-SI01.qmd`'s biocide-use figure), so the
#' reference both displays in the table row and earns its place in the
#' reference list.
#'
#' @param keys A single string of one or more BibTeX keys, `;`-separated
#'   (matching `aep_nodes.csv`'s `external_ref_keys` column), or `NA`.
#' @return A single string, `"; "`-joined if `keys` held more than one, or
#'   `NA` if `keys` was `NA`, empty, or blank.
#' @export
format_bib_citations <- function(keys) {
  if (length(keys) == 0 || is.na(keys) || !nzchar(trimws(keys))) {
    return(NA_character_)
  }
  key_vec <- trimws(strsplit(keys, ";")[[1]])
  key_vec <- key_vec[nzchar(key_vec)]
  paste(vapply(key_vec, format_one_bib_citation, character(1)), collapse = "; ")
}

#' Author-Year Citation Text for One BibTeX Key, Cached
#'
#' The single-key worker behind [format_bib_citations()], split out so it can
#' be memoized per key (an entry's author/year never changes within a
#' session): called once per group/node across the whole pipeline, so without
#' this cache the same handful of keys gets re-looked-up and re-formatted
#' hundreds of times. See [match_reference_bib_keys()] for the sibling
#' problem this pattern also fixes, and why it matters (16-minute target,
#' 2026-09-12).
#'
#' @param k A single BibTeX key.
#' @return A single string: "Surname (Year)" etc., or `k` itself if it does
#'   not resolve.
format_one_bib_citation <- local({
  cache <- new.env(parent = emptyenv())
  function(k) {
    if (exists(k, envir = cache, inherits = FALSE)) {
      return(get(k, envir = cache))
    }
    bib <- project_bibliography()
    entry <- tryCatch(bib[k], error = function(e) NULL)
    result <- if (is.null(entry) || length(entry) == 0) {
      k
    } else {
      authors <- tryCatch(entry$author, error = function(e) NULL)
      year <- tryCatch(entry$year, error = function(e) NA_character_)
      if (is.null(authors) || length(authors) == 0 || is.na(year)) {
        k
      } else {
        surnames <- format(authors, include = "family")
        who <- if (length(surnames) == 1) {
          surnames[1]
        } else if (length(surnames) == 2) {
          paste(surnames[1], "&", surnames[2])
        } else {
          paste(surnames[1], "et al.")
        }
        paste0(who, " (", year, ")")
      }
    }
    assign(k, result, envir = cache)
    result
  }
})

#' Hardcoded eData REFERENCE_ID -> Bibliography-Key Overrides
#'
#' Exceptions [match_reference_bib_keys()] cannot reach by title matching,
#' because the eData `TITLE` genuinely is not the source's own citable title.
#'
#' `"VannmiljøCopper2010-2025"` is this project's own description of the
#' Vannmiljø database extract ("Vannmiljø Database - Copper and Copper
#' Pyrithione Data"), not the database's own title ("Vannmiljø") -- the two
#' will never token-match no matter the threshold, verified 2026-09-12.
#'
#' @return A named character vector, REFERENCE_ID -> bibtex key.
reference_id_overrides <- function() {
  c("VannmiljøCopper2010-2025" = "norwegianenvironmentagencyVannmiljo2026")
}

#' Match eData Literature References to Their Bibliography Entries
#'
#' `REFERENCE_ID` (e.g. `"2017SternalTheImpactOf"`, generated at extraction
#' time from author+year+title) and a `references.bib` autokey (e.g.
#' `"sternalImpactSubmarineCopper2017"`, generated by Zotero from the same
#' three facts under a different slugging scheme) cannot be string-matched
#' directly, even though they describe the same paper. This matches on the
#' underlying facts instead: `YEAR` (+/- 1, for online-first vs print) gates
#' the candidate set, then token-Jaccard overlap of normalised `TITLE` against
#' the bibliography's own `title` field picks the best candidate.
#'
#' **Verified 2026-09-12 against all 32 distinct literature `REFERENCE_ID`s in
#' `literature_analysis_ready`**: 30 of 32 matched at a score of exactly 1.0
#' (the normalised title's whole token set is shared), the remaining two
#' scored under 0.2 -- a wide, clean gap either side of the `min_score`
#' default. Do not lower `min_score` to force a marginal case through; add it
#' to [reference_id_overrides()] instead if it is a genuine special case, or
#' accept that the source is not yet in `references.bib`.
#'
#' @param reference_id,title,year Vectors, same length, from a distinct set of
#'   REFERENCE_ID/TITLE/YEAR triples (e.g. `literature_analysis_ready`).
#' @param min_score Minimum token-Jaccard score to accept a match. **Cached
#'   per `reference_id`, so a call site that needs a different `min_score`
#'   for the same `reference_id` within one process will get the first
#'   value's cached result.** Not a concern today: every call site in this
#'   project uses the default. Would matter if that ever changes.
#' @return A character vector the same length as `reference_id`: a bibtex key
#'   where matched (or overridden), `NA` otherwise -- a study not yet added to
#'   `references.bib` is exactly this case, e.g. `"2026KogelUsingAtlanticHaddock"`
#'   as of 2026-09-12, which has no candidate scoring above 0.06.
#' @export
match_reference_bib_keys <- local({
  cache <- new.env(parent = emptyenv())
  function(reference_id, title, year, min_score = 0.9) {
    norm_title <- function(x) {
      x <- tolower(x)
      x <- gsub("[^a-z0-9 ]", " ", x)
      trimws(gsub("\\s+", " ", x))
    }
    token_jaccard <- function(a, b) {
      ta <- unique(strsplit(a, " ")[[1]])
      tb <- unique(strsplit(b, " ")[[1]])
      ta <- ta[nchar(ta) > 2]
      tb <- tb[nchar(tb) > 2]
      if (length(ta) == 0 || length(tb) == 0) {
        return(0)
      }
      length(intersect(ta, tb)) / length(union(ta, tb))
    }

    overrides <- reference_id_overrides()
    out <- character(length(reference_id))
    # Only ~32 distinct literature REFERENCE_IDs exist project-wide, but this
    # is called once per GROUP/NODE (hundreds of times): without this cache,
    # summarise_literature_data recomputed the same 32 matches ~245 times and
    # took 16 minutes instead of the ~1.5 it should (2026-09-12).
    uncached <- !vapply(reference_id, exists, logical(1), envir = cache, inherits = FALSE)

    if (any(uncached)) {
      bib_df <- project_bibliography_df()
      for (i in which(uncached)) {
        rid <- reference_id[i]
        if (!is.na(overrides[rid])) {
          assign(rid, unname(overrides[rid]), envir = cache)
          next
        }
        yr <- suppressWarnings(as.integer(year[i]))
        ttl <- norm_title(title[i])
        cand <- bib_df[
          !is.na(bib_df$YEAR_NUM) & abs(bib_df$YEAR_NUM - yr) <= 1 &
            !is.na(bib_df$title),
        ]
        result <- NA_character_
        if (nrow(cand) > 0) {
          scores <- vapply(cand$title, function(t) token_jaccard(ttl, norm_title(t)), numeric(1))
          best <- which.max(scores)
          if (scores[best] >= min_score) {
            result <- cand$BIBTEXKEY[best]
          }
        }
        assign(rid, result, envir = cache)
      }
    }

    vapply(reference_id, function(rid) get(rid, envir = cache), character(1))
  }
})

#' Author-Year Citation Text for a Set of Literature References
#'
#' Companion to [format_bib_citations()] for the empirical (measured) side:
#' resolves each distinct `REFERENCE_ID` to a bibliography entry via
#' [match_reference_bib_keys()] and formats it "Author (Year)". A reference
#' that does not resolve (not yet added to `references.bib`) shows as its raw
#' `REFERENCE_ID` instead -- visible-but-unresolved, not a wrong or invented
#' citation and not silently dropped.
#'
#' @param reference_id,title,year As [match_reference_bib_keys()], one row per
#'   measurement (this function takes the distinct combinations itself).
#' @return A single string: the sorted, comma-separated citations/ids.
#' @export
reference_citation_summary <- function(reference_id, title, year) {
  d <- unique(data.frame(
    reference_id = reference_id, title = title, year = year,
    stringsAsFactors = FALSE
  ))
  keys <- match_reference_bib_keys(d$reference_id, d$title, d$year)
  display <- ifelse(is.na(keys), d$reference_id, vapply(keys, function(k) {
    if (is.na(k)) NA_character_ else format_bib_citations(k)
  }, character(1)))
  paste(sort(unique(display)), collapse = ", ")
}

#' Citation Suffix for a Table Caption, Listing External-Node Sources
#'
#' Real pandoc citation syntax (`[@key]`), meant to be appended to a table's
#' `tbl-cap`. A caption **is** pandoc-parsed text, unlike a flextable body
#' cell (see [format_bib_citations()]): this repo already has proof of that
#' in `_06-SI01.qmd`'s biocide-use `fig-cap`, which contains a working
#' `[@key]` citation. Putting the same syntax here means a table's sources
#' both display (via [format_bib_citations()] / [reference_citation_summary()]
#' in the table itself) and earn their place in the manuscript's reference
#' list, without a fragile dynamic-`nocite` mechanism.
#'
#' The shared helper behind [external_node_cite_suffix()] and
#' [literature_cite_suffix()].
#'
#' @param keys Character vector of bibtex keys, possibly containing `NA` or
#'   blank entries (dropped).
#' @param label Text introducing the citation list, e.g.
#'   `"External-node sources"`.
#' @return A single string: `""` where `keys` has nothing usable, else
#'   `" <label>: [@k1]; [@k2]."`.
cite_suffix <- function(keys, label) {
  keys <- unique(keys[!is.na(keys) & nzchar(keys)])
  if (length(keys) == 0) {
    return("")
  }
  paste0(" ", label, ": ", paste0("[@", keys, "]", collapse = "; "), ".")
}

#' Citation Suffix for External-Node Sources
#'
#' @param scoped One element of [aep_scoped_nodes()] (a single AEP's nodes),
#'   or the full `aep_nodes` tibble.
#' @return As [cite_suffix()].
#' @export
external_node_cite_suffix <- function(scoped) {
  raw <- scoped$external_ref_keys[!is.na(scoped$external_ref_keys)]
  keys <- unique(trimws(unlist(strsplit(raw, ";"))))
  cite_suffix(keys, "External-node sources")
}

#' Citation Suffix for a Table's Literature References
#'
#' Companion to [external_node_cite_suffix()] for the empirical/literature
#' side: resolves REFERENCE_ID/TITLE/YEAR via [match_reference_bib_keys()] and
#' appends whichever resolve to a real key. A reference that fails to resolve
#' is silently omitted here -- it already shows as a raw REFERENCE_ID in the
#' table itself, per [reference_citation_summary()], and there is no key to
#' cite until the source is added to `references.bib`.
#'
#' @param reference_id,title,year As [match_reference_bib_keys()].
#' @return As [cite_suffix()].
#' @export
literature_cite_suffix <- function(reference_id, title, year) {
  d <- unique(data.frame(
    reference_id = reference_id, title = title, year = year,
    stringsAsFactors = FALSE
  ))
  keys <- match_reference_bib_keys(d$reference_id, d$title, d$year)
  cite_suffix(keys, "Literature sources")
}

# The AEP node layer (PLAN.md P3.1-P3.4). Added 2026-08-05.
#
# WHY THIS IS NOT JUST group_decisions.csv WITH MORE COLUMNS.
#
# A sampling group is defined by triage_group_cols(): compartment, species,
# tissue, site type, unit. An AEP node is whatever Sam decides to assess as one
# thing, and the two are not the same. His own prototype proves it. In
# docs/NBXX-algae.qmd the marine node is:
#
#     filter(ENVIRON_COMPARTMENT_SUB == "Freshwater",
#            LATITUDE >= 66.5,
#            SITE_GEOGRAPHIC_FEATURE == "River, stream, canal")
#
# `LATITUDE >= 66.5` is not in the group key at all, and a few lines later the
# same node drops outliers. So a node can be one group, several groups, or a
# restricted slice of either.
#
# The design answer is a MEMBERSHIP FILE plus a FIXED SET OF RESTRICTION COLUMNS,
# not a filter expression in a CSV cell. Arbitrary R in a spreadsheet cannot be
# validated, fails at pipeline runtime rather than at read time, and cannot be
# diffed meaningfully in review. The restrictions here cover every case the
# prototype notebooks actually use; anything genuinely beyond them should become
# a new column with a name, not an escape hatch.
#
# SPLIT OF AUTHORITY, same as the decisions layer:
#   * the pipeline READS these files and never writes them;
#   * scripts/scaffold_aep_nodes.R appends, and never overwrites a judgement.

#' Latitude of the Arctic Circle
#'
#' @return A single numeric.
#' @export
arctic_circle_lat <- function() {
  66.5
}

#' Permitted Node Levels
#'
#' The stage of a node in the aggregate exposure pathway, and the reason `y` is
#' hand-placed rather than laid out automatically: vertical position carries
#' source-to-exposure meaning, so an automatic graph layout is actively wrong
#' here (PLAN.md P5.1).
#'
#' Split from the original four (`source` / `medium` / `organism` / `tse`) into
#' the five canonical AEP stages on 2026-09-03, so an example pathway has one
#' node per stage (Sam). The old `medium` maps to `exposure_medium` and the old
#' `organism` to `internal_exposure`; `external_exposure` and
#' `target_site_exposure` are new and are empty in the real AEPs, which hold no
#' boundary-contact or site-of-action measurements.
#'
#' * `source` -- a release: emissions, tonnage, an industrial sector.
#' * `exposure_medium` -- an environmental compartment carrying copper
#'   (water, sediment, air, food).
#' * `external_exposure` -- copper at the organism boundary: the concentration
#'   contacted or the intake rate, before uptake.
#' * `internal_exposure` -- copper measured inside the organism: a tissue or
#'   whole-body burden.
#' * `target_site_exposure` -- copper at the site of toxic action, the end of
#'   the pathway.
#'
#' @return A character vector, in pathway order.
#' @export
aep_node_levels <- function() {
  c(
    "source",
    "exposure_medium",
    "external_exposure",
    "internal_exposure",
    "target_site_exposure"
  )
}

#' Permitted Node Types
#'
#' * `empirical` -- resolved from sampling groups in the data. Must have members.
#' * `external` -- carried from an assessment made outside this dataset, with the
#'   magnitude typed in. The emissions and REACH nodes are these: PLAN.md P3.6
#'   makes the point that those WoE assessments are already written as prose in
#'   `docs/NBXX-norske-utslipp.qmd` and need transcribing, not re-deriving.
#'   Must NOT have members, and carries `value` / `value_unit` instead.
#'
#' @return A character vector.
#' @export
aep_node_types <- function() {
  c("empirical", "external")
}

#' Permitted `trend` Values on a Node
#'
#' The direction the node's reported quantity is judged to be moving over the
#' study period. Hand-entered on `aep_nodes.csv`, one value per node, and shown
#' on the card as a small grey glyph after the headline figure
#' ([trend_icon_path()]). The assessment itself is made holistically in the
#' methods section, not row by row; a blank cell means "not yet assessed" and
#' draws no glyph, which is deliberately distinct from `unknown` ("assessed, no
#' firm direction").
#'
#' @return A character vector.
#' @export
node_trend_levels <- function() {
  c("up", "flat", "down", "unknown")
}

#' The Four EPEQ Score Columns and Their Justifications
#'
#' Adapted from Peng et al. 2022, and scored 1-3 exactly as in
#' `docs/NBXX-algae.qmd`, which is the reference implementation and is Sam's own
#' wording. Every score carries a written justification in the adjacent column,
#' because a bare number is not a weight of evidence assessment.
#'
#' This is the full set of four, as they appear on an **edge** and on a
#' **scoped** node (after [aep_scope_nodes()] has merged the per-AEP evidence /
#' quantification in). The `aep_nodes.csv` file itself carries only
#' [aep_node_epeq_cols()]; evidence and quantification live on the per-AEP
#' membership file. See the header of `R/fct_aep_manifest.R`.
#'
#' @return A character vector of column names, scores and justifications
#'   interleaved.
#' @export
epeq_cols <- function() {
  c(
    "essentiality_score", "essentiality_justification",
    "plausibility_score", "plausibility_justification",
    "evidence_score", "evidence_justification",
    "quantification_score", "quantification_justification"
  )
}

#' The EPEQ Columns That Live on `aep_nodes.csv`
#'
#' Essentiality and plausibility only: claims about the world, written once per
#' node. Evidence and quantification are claims about the dataset an AEP scope
#' selects, so they live on `aep_membership_<id>.csv` ([aep_scoped_epeq_cols()])
#' and were removed from `aep_nodes.csv` on 2026-09-08. The two together are
#' [epeq_cols()].
#'
#' @return A character vector, scores and justifications interleaved.
#' @export
aep_node_epeq_cols <- function() {
  c(
    "essentiality_score", "essentiality_justification",
    "plausibility_score", "plausibility_justification"
  )
}

#' Magnitude Columns for External Nodes Only
#'
#' **These are never read for an `empirical` node**, whose mean, sd, geometric
#' mean, GSD, median, n and source count are all computed from its constituent
#' groups by [node_report_card()]. They exist only for `external` nodes, where
#' there is no data in this dataset to compute from: a national emissions total,
#' a REACH tonnage, a crustal abundance.
#'
#' **Renamed from `value*` to `external_*` on 2026-08-05**, when Sam asked the
#' obvious question: "why are we specifying these manually rather than
#' calculating from constituent groups?" The answer was "we do calculate them,
#' just not for these nodes", which is a sign the columns were misnamed rather
#' than a sign the question was wrong. A column called `value` on a table of
#' nodes reads as *the* value of every node.
#'
#' [read_aep_nodes()] now **stops** if one of these is filled on an `empirical`
#' node, rather than ignoring it. A number typed into a column that is never
#' read is the same failure class as the untracked decisions file and the
#' unhashed package namespace: work that appears done and silently is not.
#'
#' `external_refs` added 2026-08-12. An empirical node counts its own sources
#' with `n_distinct(REFERENCE_ID)`; an external one has no rows to count, so
#' its card read "refs = -" and looked like missing information rather than a
#' known quantity. Sam: the REACH cards "all have 1 ref, this should be marked
#' on them". It is a hand-entered column rather than an inference from having
#' a REACH series, because the external nodes do not share a provenance: N004
#' to N011 come from one REACH extract, N003 mine tailings wants a figure from
#' Sternal or Pedersen, and N001 and N029 have no source yet at all.
#'
#' @return A character vector of column names.
#' @export
external_value_cols <- function() {
  c("external_value", "external_sd", "external_n", "external_unit",
    "external_refs", "external_ref_keys")
}

#' Columns Owned by the Human
#'
#' Never overwritten by [scaffold_aep_nodes()].
#'
#' @return A character vector of column names.
#' @export
aep_node_human_cols <- function() {
  c(
    "label", "level", "node_type", "x", "y",
    "lat_min", "lat_max", "date_min", "date_max",
    "exclude_references", "exclude_campaigns", "drop_outliers",
    external_value_cols(),
    aep_node_epeq_cols(),
    "notes"
  )
}

#' Slugify a Node Label for a Composite Node Id
#'
#' Lowercase, hyphen-separated, punctuation collapsed. Separate from
#' [slugify_name()] (underscore-separated, `targets`-name-safe) because this
#' slug is for humans reading `aep_edges.csv`'s `from`/`to` columns, not for a
#' generated target name.
#'
#' @param label A character vector.
#' @return A character vector of slugs.
#' @export
node_label_slug <- function(label) {
  label |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-") |>
    stringr::str_replace_all("^-+|-+$", "")
}

#' Mint the Next Composite Node Id
#'
#' `N<number>-<slug>`. The number is the real key (unique, never reused,
#' driven off the highest one already in use, same reasoning as
#' `scaffold_aep_edges.R`'s edge ids); the slug exists so `from`/`to` and
#' membership rows are legible without a lookup into `aep_nodes.csv`.
#'
#' **The slug freezes at creation.** If a node is later relabelled, its id is
#' NOT regenerated: doing so would require rewriting every file that names it
#' (`aep_edges.csv`, `aep_node_members.csv`, `aep_node_groups.csv`, and every
#' `aep_membership_<aep_id>.csv`), turning a one-cell edit into a many-file one.
#' A slightly stale slug is a smaller cost than that cascade.
#'
#' @param nodes The existing nodes table (for the highest number in use).
#' @param label The new node's label, to derive the slug from.
#' @return A single composite node id string.
#' @export
next_node_id <- function(nodes, label) {
  nums <- suppressWarnings(as.integer(sub("^N([0-9]+).*$", "\\1", nodes$node_id)))
  next_num <- if (length(nums) == 0 || all(is.na(nums))) 1L else max(nums, na.rm = TRUE) + 1L
  sprintf("N%03d-%s", next_num, node_label_slug(label))
}

#' An Empty Nodes Table
#'
#' The schema in one place, so the scaffold, the reader and the tests cannot
#' drift apart.
#'
#' @return A zero-row tibble.
#' @export
empty_aep_nodes <- function() {
  tibble::tibble(
    node_id = character(0),
    label = character(0),
    level = character(0),
    node_type = character(0),
    x = numeric(0),
    y = numeric(0),
    lat_min = numeric(0),
    lat_max = numeric(0),
    date_min = as.Date(character(0)),
    date_max = as.Date(character(0)),
    exclude_references = character(0),
    # Added 2026-08-06. A whole campaign can be defective in a way that is not a
    # unit error and so cannot be repaired by unit_corrections.csv: 20 of the 44
    # G. morhua muscle rows carry liver-like concentrations, isolated to two
    # campaigns, with no intermediate values and with liver flat over the same
    # period. That is a tissue-labelling fault, and there is no factor that
    # fixes it. Excluding the affected rows and scoring what remains is honest;
    # averaging over rows believed to be mislabelled and calling the result
    # low-quality evidence is not.
    #
    # NOT covered by drop_outliers: 20 of 44 rows is far too large a fraction
    # for Tukey fences to reach, and a mode that size is not an outlier in any
    # statistical sense. This is a provenance judgement, not a statistical one.
    exclude_campaigns = character(0),
    drop_outliers = logical(0),
    # External nodes only; see external_value_cols().
    external_value = numeric(0),
    external_sd = numeric(0),
    external_n = numeric(0),
    external_unit = character(0),
    external_refs = numeric(0),
    # BibTeX keys behind external_refs' count, `;`-separated. Added 2026-09-12
    # so external nodes can carry real, pandoc-resolvable citations rather
    # than just a count; see ?format_bib_citations.
    external_ref_keys = character(0),
    # Essentiality and plausibility only; evidence and quantification are
    # per-AEP and live on aep_membership_<id>.csv (removed here 2026-09-08).
    # See aep_node_epeq_cols() and the header of R/fct_aep_manifest.R.
    essentiality_score = numeric(0),
    essentiality_justification = character(0),
    plausibility_score = numeric(0),
    plausibility_justification = character(0),
    notes = character(0),
    # Added 2026-09-04. Node-level direction of travel for the headline figure,
    # one of node_trend_levels() or blank. `trend` drives the card glyph;
    # `trend_basis` is free text for the reasoning, surfaced in the methods
    # table (tbl-node-trends) rather than on the card. Both blank until the
    # holistic assessment is done.
    trend = character(0),
    trend_basis = character(0)
  )
}

#' Parse a Date Bound, Accepting a Bare Year
#'
#' `date_min` and `date_max` accept either a full `YYYY-MM-DD` or a bare year.
#' A bare year expands to the **inclusive** end of its interval: `2010` as a
#' lower bound is `2010-01-01`, and as an upper bound `2010-12-31`. So
#' `date_min = 2010, date_max = 2020` means the eleven whole years you would
#' expect it to mean.
#'
#' **This exists because the alternative silently emptied every node.** Sam's
#' first pass entered `date_min = 1900, date_max = 2100`, which is the obvious
#' thing to type. `readr` parsed them as numbers, and comparing a `Date` to
#' `2100` coerces the date to days-since-1970, so the bound meant "before
#' mid-1975" and every node resolved to zero rows with no error. Refusing years
#' outright would be safe but obtuse; accepting them under a stated convention is
#' both safe and what the typist meant.
#'
#' Anything that is neither is an error rather than an `NA`, because an
#' unparseable restriction that quietly becomes "no restriction" is how a node
#' silently changes meaning.
#'
#' @param x A character, numeric or Date vector.
#' @param bound `"min"` or `"max"`, deciding which end of a bare year is taken.
#' @return A Date vector.
#' @export
parse_node_date <- function(x, bound = c("min", "max")) {
  bound <- match.arg(bound)
  if (length(x) == 0) {
    return(as.Date(character(0)))
  }
  if (inherits(x, "Date")) {
    return(x)
  }

  chr <- trimws(as.character(x))
  out <- as.Date(rep(NA, length(chr)))

  blank <- is.na(chr) | !nzchar(chr)
  year <- !blank & grepl("^[0-9]{4}$", chr)
  full <- !blank & !year

  if (any(year)) {
    out[year] <- as.Date(paste0(
      chr[year],
      if (bound == "min") "-01-01" else "-12-31"
    ))
  }
  if (any(full)) {
    parsed <- suppressWarnings(as.Date(chr[full], format = "%Y-%m-%d"))
    if (any(is.na(parsed))) {
      stop(
        "Unparseable date_", bound, " value(s): ",
        paste(sQuote(utils::head(chr[full][is.na(parsed)], 5)), collapse = ", "),
        ". Use YYYY-MM-DD, or a bare year."
      )
    }
    out[full] <- parsed
  }

  out
}

#' Read and Validate the AEP Nodes File
#'
#' Validates rather than trusts, for the same reason as
#' [read_group_decisions()]: this file is hand-edited, and a typo must fail here
#' rather than produce an empty or wrong node in a manuscript figure.
#'
#' @param path Where the CSV lives.
#' @return A tibble of nodes.
#' @export
read_aep_nodes <- function(path = here_rel("data/clean/aep/aep_nodes.csv")) {
  if (!file.exists(path)) {
    stop(
      "No nodes file at ", path,
      ". Run scripts/scaffold_aep_nodes.R first."
    )
  }
  nodes <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      node_id = readr::col_character(),
      label = readr::col_character(),
      level = readr::col_character(),
      node_type = readr::col_character(),
      exclude_references = readr::col_character(),
      exclude_campaigns = readr::col_character(),
      external_unit = readr::col_character(),
      # Explicit, not guessed: every row is currently blank (column added
      # 2026-09-12, not yet filled in), and col_guess() reads an all-blank
      # column as logical, not character.
      external_ref_keys = readr::col_character(),
      notes = readr::col_character(),
      trend = readr::col_character(),
      trend_basis = readr::col_character(),
      # Read as text, then parsed by parse_node_date(). Letting readr guess is
      # what allowed a bare year through as a number, which then compared
      # against a Date as days-since-1970.
      date_min = readr::col_character(),
      date_max = readr::col_character()
    )
  )

  nodes$date_min <- parse_node_date(nodes$date_min, "min")
  nodes$date_max <- parse_node_date(nodes$date_max, "max")

  inverted <- !is.na(nodes$date_min) & !is.na(nodes$date_max) &
    nodes$date_min > nodes$date_max
  if (any(inverted)) {
    stop(
      sum(inverted), " node(s) have date_min after date_max: ",
      paste(sQuote(nodes$node_id[inverted]), collapse = ", ")
    )
  }
  inverted_lat <- !is.na(nodes$lat_min) & !is.na(nodes$lat_max) &
    nodes$lat_min > nodes$lat_max
  if (any(inverted_lat)) {
    stop(
      sum(inverted_lat), " node(s) have lat_min above lat_max: ",
      paste(sQuote(nodes$node_id[inverted_lat]), collapse = ", ")
    )
  }

  missing <- setdiff(names(empty_aep_nodes()), names(nodes))
  if (length(missing) > 0) {
    stop("Nodes file is missing column(s): ", paste(missing, collapse = ", "))
  }

  dup <- nodes$node_id[duplicated(nodes$node_id)]
  if (length(dup) > 0) {
    stop("Duplicate node_id(s): ", paste(unique(dup), collapse = ", "))
  }

  bad_level <- setdiff(stats::na.omit(unique(nodes$level)), aep_node_levels())
  if (length(bad_level) > 0) {
    stop(
      "Unrecognised level(s): ", paste(sQuote(bad_level), collapse = ", "),
      ". Permitted: ", paste(aep_node_levels(), collapse = ", ")
    )
  }

  bad_type <- setdiff(stats::na.omit(unique(nodes$node_type)), aep_node_types())
  if (length(bad_type) > 0) {
    stop(
      "Unrecognised node_type(s): ", paste(sQuote(bad_type), collapse = ", "),
      ". Permitted: ", paste(aep_node_types(), collapse = ", ")
    )
  }

  # Same contract as level and node_type: a typo in a controlled-vocabulary
  # column fails here rather than drawing a wrong (or no) glyph on a card. Blank
  # is allowed and means "not assessed"; only non-blank values are checked.
  trend_seen <- trimws(stats::na.omit(unique(nodes$trend)))
  bad_trend <- setdiff(trend_seen[nzchar(trend_seen)], node_trend_levels())
  if (length(bad_trend) > 0) {
    stop(
      "Unrecognised trend value(s): ", paste(sQuote(bad_trend), collapse = ", "),
      ". Permitted: ", paste(node_trend_levels(), collapse = ", "), ", or blank."
    )
  }

  # STOPS rather than warns, and rather than ignoring. An empirical node's
  # magnitude is computed from its member groups, so a number typed into these
  # columns is never read: the node would report a value the file does not
  # contain, and the file would show a value the node does not use. Silently
  # discarding hand-entered numbers is the failure this project has now hit three
  # times (untracked decisions file, unhashed package namespace, this).
  filled <- vapply(
    external_value_cols(),
    function(col) !is.na(nodes[[col]]),
    logical(nrow(nodes))
  )
  if (nrow(nodes) == 1) {
    filled <- matrix(filled, nrow = 1, dimnames = list(NULL, external_value_cols()))
  }
  offenders <- nodes$node_id[
    nodes$node_type %in% "empirical" & apply(filled, 1, any)
  ]
  if (length(offenders) > 0) {
    stop(
      length(offenders), " empirical node(s) have external_* values set: ",
      paste(sQuote(offenders), collapse = ", "),
      ". These columns are only read for node_type = 'external'; an empirical ",
      "node's magnitude is computed from its member groups. Either clear them ",
      "or change node_type."
    )
  }

  # Scores are 1-3 or blank. A 0 or a 4 is a typo, and a typo that survives into
  # a figure is indistinguishable from a judgement. Only essentiality and
  # plausibility live here now; evidence and quantification are range-checked
  # per-AEP in read_aep_membership().
  for (col in aep_node_epeq_cols()[c(TRUE, FALSE)]) {
    v <- nodes[[col]]
    bad <- !is.na(v) & !(v %in% 1:3)
    if (any(bad)) {
      stop(
        sum(bad), " row(s) have an out-of-range ", col,
        ": scores are 1, 2 or 3, or blank if unscored."
      )
    }
  }

  nodes
}

#' Read and Validate the Node Membership File
#'
#' @param path Where the CSV lives.
#' @param nodes Optional nodes table, to check every `node_id` exists.
#' @param ids Optional group id ledger, to check every `group_id` exists.
#' @return A tibble of `node_id`, `group_id`, `notes`.
#' @export
read_aep_node_members <- function(
  path = here_rel("data/clean/aep/aep_node_members.csv"),
  nodes = NULL,
  ids = NULL
) {
  if (!file.exists(path)) {
    stop(
      "No membership file at ", path,
      ". Run scripts/scaffold_aep_nodes.R first."
    )
  }
  members <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  missing <- setdiff(c("node_id", "group_id"), names(members))
  if (length(missing) > 0) {
    stop("Membership file is missing column(s): ", paste(missing, collapse = ", "))
  }

  dup <- members |>
    dplyr::count(.data$node_id, .data$group_id) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dup) > 0) {
    stop(
      "Duplicate membership row(s): ",
      paste(dup$node_id, dup$group_id, collapse = ", ")
    )
  }

  if (!is.null(nodes)) {
    unknown <- setdiff(members$node_id, nodes$node_id)
    if (length(unknown) > 0) {
      stop(
        "Membership names ", length(unknown), " unknown node_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", ")
      )
    }
  }
  if (!is.null(ids)) {
    unknown <- setdiff(members$group_id, ids$group_id)
    if (length(unknown) > 0) {
      stop(
        "Membership names ", length(unknown), " unknown group_id(s): ",
        paste(sQuote(utils::head(unknown, 5)), collapse = ", "),
        ". Run scripts/allocate_group_ids.R if these are new groups."
      )
    }
  }

  members
}

#' Apply One Semicolon-Separated Exclusion Column
#'
#' Shared by `exclude_references` and `exclude_campaigns`, so the two cannot
#' drift apart in how they split, trim, or handle a blank cell.
#'
#' **Warns when a listed value matches nothing.** A stale exclusion silently
#' doing nothing is the same failure that has now bitten this project three
#' times (the missing `imports`, the untracked decisions file, and a correction
#' whose selector no longer matched). Here it is quieter and worse: the node
#' still resolves, still produces a mean, and the rows you believed you had
#' removed are back in it. A typo in a campaign name is easy and invisible
#' otherwise, since these strings carry spaces and parentheses.
#'
#' It warns rather than aborts, unlike the corrections layer, because a node
#' exclusion narrows an estimate rather than rewriting a measurement, and
#' because a legitimately empty match happens while a node is being built up.
#'
#' @section Typo, not scope (fixed 2026-08-13):
#'
#' The warning used to be raised against `data`, i.e. the node's rows **after**
#' the AEP scope and the node's own restrictions had already narrowed them. So
#' it fired on two completely different situations with one message:
#'
#' * the name is wrong and the rows are still in the node, which is the fault
#'   worth shouting about; and
#' * the name is right but those rows are not in *this* AEP, which is ordinary
#'   and expected, and happens for every scoped AEP whose bounding box excludes
#'   the campaign.
#'
#' It cost an afternoon. `N016-g-morhua-muscle` warned "2 values in
#' exclude_campaigns matched no rows" on every build, purely because A002 and
#' A003 leave that node with no rows at all inside their boxes. Read as a typo,
#' it led to the cell being "corrected" from `Vm_2010_2025 (...)` to the long
#' `Vannmiljø Copper Monitoring 2010-2025 (...)`, which matches `CAMPAIGN_NAME`
#' but **not** `CAMPAIGN_NAME_SHORT`, the column this is actually pointed at.
#' That silently put the node back on all 44 rows including the 18 mislabelled
#' Urban Fjord ones, undoing PLAN.md 9e.
#'
#' So the typo check now runs against `vocabulary`, the **unrestricted** pool,
#' where "is this a real campaign name" is a question that has one answer
#' regardless of which AEP is being drawn. A name that exists but matches
#' nothing here is silent: it is not an error, and saying so on every build
#' trains you to ignore the message that matters.
#'
#' @param data The node's rows so far.
#' @param node A one-row nodes tibble.
#' @param col Name of the exclusion column on `node`.
#' @param target Name of the column in `data` to match against.
#' @param vocabulary The unrestricted data (or a bare vector of valid values)
#'   to check the listed names against. Defaults to `data`, which reproduces
#'   the pre-2026-08-13 behaviour for any direct caller that does not have the
#'   full pool to hand.
#' @return `data` with excluded rows removed.
#' @export
apply_node_exclusion <- function(data, node, col, target, vocabulary = data) {
  if (!col %in% names(node) || is.na(node[[col]][1]) ||
    !nzchar(node[[col]][1])) {
    return(data)
  }
  drop <- trimws(strsplit(node[[col]][1], ";", fixed = TRUE)[[1]])
  drop <- drop[nzchar(drop)]
  if (length(drop) == 0) {
    return(data)
  }
  if (!target %in% names(data)) {
    cli::cli_warn(
      "Node {.val {node$node_id[1]}} sets {.field {col}} but the data has no \\
       {.field {target}} column; the exclusion did nothing."
    )
    return(data)
  }

  # Checked against the WHOLE pool, not against `data`. See the "Typo, not
  # scope" section above: `data` has already been narrowed by the AEP's
  # bounding box and the node's own restrictions, so an absence there says
  # nothing about whether the name is real.
  known <- if (is.data.frame(vocabulary)) {
    if (target %in% names(vocabulary)) vocabulary[[target]] else data[[target]]
  } else {
    vocabulary
  }

  unknown <- drop[!drop %in% known]
  if (length(unknown) > 0) {
    cli::cli_warn(c(
      "Node {.val {node$node_id[1]}}: {length(unknown)} value{?s} in \\
       {.field {col}} {?is/are} not {?a/} known {.field {target}}.",
      "*" = "{.val {unknown}}",
      "i" = "Check for a typo. The rows you meant to exclude are still in \\
             the node.",
      "i" = "Note {.field {target}} is matched exactly, and is the SHORT \\
             campaign name where that column is in use."
    ))
  }

  # Deliberately silent where a name is real but matches nothing in `data`.
  # That is what a scoped AEP looks like, not a fault.
  data[!data[[target]] %in% drop, , drop = FALSE]
}

#' Resolve One Node to its Rows of Data
#'
#' Membership first, then the restriction columns, in that order.
#'
#' **Mixed units are refused, not averaged.** A node pooling `mg/kg (dry)` with
#' `mg/kg (wet)` would produce a mean that means nothing, and the difference is
#' routinely a factor of four or five in biota. This is the same reasoning that
#' makes the unit part of the group key in the first place, and the same reason
#' [parse_measured_unit()] refuses a bare `mg/kg`.
#'
#' `external` nodes resolve to zero rows by design and are not an error: their
#' magnitude is typed into `value`, having been assessed elsewhere.
#'
#' @param node A one-row nodes tibble.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger, to map `group_id` back to group-key columns.
#' @return A tibble of the node's rows, possibly zero-row.
#' @export
resolve_node_data <- function(node, members, data, ids) {
  stopifnot(nrow(node) == 1)

  if (identical(node$node_type[1], "external")) {
    return(data[0, , drop = FALSE])
  }

  my_groups <- members$group_id[members$node_id == node$node_id[1]]
  if (length(my_groups) == 0) {
    return(data[0, , drop = FALSE])
  }

  key <- triage_group_cols()
  keys <- ids |>
    dplyr::filter(.data$group_id %in% my_groups) |>
    dplyr::select(dplyr::all_of(key))

  out <- data |> dplyr::semi_join(keys, by = key)

  # --- restrictions, each skipped when blank -----------------------------
  if (!is.na(node$lat_min[1])) {
    out <- out[!is.na(out$LATITUDE) & out$LATITUDE >= node$lat_min[1], ]
  }
  if (!is.na(node$lat_max[1])) {
    out <- out[!is.na(out$LATITUDE) & out$LATITUDE <= node$lat_max[1], ]
  }
  # Longitude arrives only from an AEP's bounding box (see aep_scope_nodes()),
  # so the columns are absent on a bare nodes table and their absence means "no
  # restriction" rather than an error.
  # `%in% names()`, not `$`: a tibble warns on `$` for a column it does not
  # have, and this runs once per node per AEP.
  if ("lon_min" %in% names(node) && !is.na(node$lon_min[1])) {
    out <- out[!is.na(out$LONGITUDE) & out$LONGITUDE >= node$lon_min[1], ]
  }
  if ("lon_max" %in% names(node) && !is.na(node$lon_max[1])) {
    out <- out[!is.na(out$LONGITUDE) & out$LONGITUDE <= node$lon_max[1], ]
  }
  # Dates, not numbers. Comparing a Date against a bare year silently reads the
  # year as days-since-1970 and empties the node; read_aep_nodes() converts, and
  # this catches any caller that bypassed it.
  for (col in c("date_min", "date_max")) {
    if (!is.na(node[[col]][1]) && !inherits(node[[col]], "Date")) {
      stop(
        col, " on node ", node$node_id[1], " is ", class(node[[col]])[1],
        ", not a Date. Read the file with read_aep_nodes(), which accepts a ",
        "bare year and converts it."
      )
    }
  }
  if (!is.na(node$date_min[1])) {
    out <- out[!is.na(out$SAMPLING_DATE) &
      out$SAMPLING_DATE >= node$date_min[1], ]
  }
  if (!is.na(node$date_max[1])) {
    out <- out[!is.na(out$SAMPLING_DATE) &
      out$SAMPLING_DATE <= node$date_max[1], ]
  }
  # Semicolon-separated, because a comma cannot survive a CSV cell unquoted and
  # reference ids are already long enough to be mistyped.
  # `vocabulary = data` is the UNRESTRICTED pool, deliberately, so the typo
  # check asks "is this a real name" rather than "is it present in whatever is
  # left after this AEP's bounding box". See apply_node_exclusion().
  out <- apply_node_exclusion(
    out, node, "exclude_references", "REFERENCE_ID",
    vocabulary = data
  )
  # Campaign names contain commas and parentheses ("Vm_2010_2025 (Urban Fjord
  # Contaminants)"), so the semicolon separator matters more here still.
  #
  # CAMPAIGN_NAME_SHORT, not CAMPAIGN_NAME. The long form
  # ("Vannmiljø Copper Monitoring 2010-2025 (...)") will not match, and on
  # 2026-08-13 that cost a day when a cell was "corrected" into it.
  out <- apply_node_exclusion(
    out, node, "exclude_campaigns", "CAMPAIGN_NAME_SHORT",
    vocabulary = data
  )
  if (isTRUE(node$drop_outliers[1]) && nrow(out) > 0) {
    # Computed WITHIN the resolved node, not inherited from the sampling group.
    # A value that is an outlier against its own small group may be unremarkable
    # against the pooled node, and the node is the thing being assessed.
    flags <- flag_outliers(out$MEASURED_VALUE_STANDARD)
    out <- out[!(flags$outlier_RMZ %in% TRUE & flags$outlier_IQR %in% TRUE), ]
  }

  units <- unique(stats::na.omit(out$MEASURED_UNIT_STANDARD))
  if (length(units) > 1) {
    stop(
      "Node ", node$node_id[1], " (", node$label[1], ") pools ",
      length(units), " units: ", paste(units, collapse = ", "),
      ". Split it, or restrict its membership to one unit."
    )
  }

  out
}

#' Weighted Median
#'
#' No dependency for four lines. Ties and zero weights behave as you would
#' expect; an even split takes the lower of the two straddling values rather
#' than interpolating, which keeps the result a value that was actually
#' observed.
#'
#' @param x Numeric values. @param w Weights, same length.
#' @return A single number, or `NA_real_` where nothing is usable.
#' @export
weighted_median <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

#' Why the Centre is Weighted and the Spread is Not
#'
#' Recorded here because it is the one methodological choice in the node layer,
#' and Sam asked for it to be explained rather than asserted (2026-08-05).
#'
#' A row in this project is one of two things. A Vannmiljø row is a single
#' measurement, `MEASURED_N = 1`. A literature row is a **summary**: `MEASURED_N
#' = 50` means the authors measured fifty samples and reported one number for
#' them. There are 368 such rows, carrying 6,056 of 95,816 measurements.
#'
#' **The centre is weighted.** If fifty mussels averaged 2.4 mg/kg, that fact
#' should carry the weight of fifty mussels rather than of one. An unweighted
#' mean over rows lets a single Vannmiljø observation outvote a fifty-sample
#' study, and makes the reported `n` describe a different population from the
#' reported mean. That was the inconsistency in the first version: node N003
#' reported `n = 5,498` beside a geometric mean computed over 3,093 rows, 45% of
#' the claimed n coming from 1.5% of the rows.
#'
#' **The spread is not weighted, and cannot honestly be.** We hold the study
#' *means*, not the study *values*. Weighting the spread would treat those fifty
#' mussels as fifty copies of one number, erasing the within-study variation and
#' reporting a dataset far tighter than it is. Reconstructing the real variance
#' would need a within-study spread for every aggregated row, and this dataset
#' has one for 202 of 368 rows in five non-interconvertible forms (standard
#' deviation, 95% confidence interval, geometric SD, interquartile range,
#' min-max). Converting between those needs distributional assumptions per row.
#'
#' CLAUDE.md's standing rule settles it: a spread statistic that cannot be
#' justified in the methods section is worse than none. So `sd` and `gsd` are
#' **per row**, `n_rows` sits beside them in the card, and the difference is
#' documented rather than papered over.
#'
#' @name node_statistic_weighting
NULL

#' Report Card for One Node
#'
#' The compact summary PLAN.md section 4.3 asks a node to carry, as one row.
#'
#' **Arctic coverage is reported, never filtered.** Sam's decision 2026-08-05,
#' chosen over a global `LATITUDE >= 66.5` cut that would have dropped 81% of
#' measurements and left the marine node on 258. So the AEP is Norwegian and
#' Arctic representativeness is a stated property of each node, in the same
#' spirit as `n_sources`: a visible weakness rather than a silent one.
#'
#' Geometric mean and GSD are still computed alongside the arithmetic pair,
#' matching `summarise_literature_data`, but are no longer the headline
#' statistic anywhere a reader sees this card (Sam, 2026-09-12: GM/GSD are not
#' how pollution concentrations are conventionally reported, and reporting
#' them in only some tables was itself the inconsistency). `fractionation`
#' (see [fractionation_summary()]) is carried for the same reason `sd` is:
#' something a reader needs before trusting the mean, not decoration.
#'
#' @param node A one-row nodes tibble.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger.
#' @return A one-row tibble.
#' @export
node_report_card <- function(node, members, data, ids) {
  d <- resolve_node_data(node, members, data, ids)

  if (nrow(d) == 0) {
    # An external node reports the magnitude that was typed in; an empirical one
    # with no rows reports nothing and is caught by validate_aep_nodes().
    return(tibble::tibble(
      node_id = node$node_id[1],
      label = node$label[1],
      level = node$level[1],
      node_type = node$node_type[1],
      n = node$external_n[1],
      n_rows = 0L,
      n_groups = 0L,
      # From the hand-entered column, not NA. An external node has no rows to
      # count REFERENCE_IDs over, but that does not make its provenance
      # unknown: the REACH sector nodes are one extract, so "refs = 1" is a
      # fact about them and "refs = -" was reading as missing data.
      n_sources = as.integer(node$external_refs[1]),
      # From aep_nodes.csv's own external_ref_keys (BibTeX keys, `;`-separated),
      # formatted as author-year text. See ?format_bib_citations for why this
      # cannot be a bare pandoc [@key] typed into the cell. NA (a dash in the
      # rendered table) where the column is blank, i.e. not filled in yet.
      references = format_bib_citations(node$external_ref_keys[1]),
      unit = node$external_unit[1],
      mean = node$external_value[1],
      sd = node$external_sd[1],
      geo_mean = NA_real_,
      gsd = NA_real_,
      median = NA_real_,
      # No rows, so no fractionation protocol to report either.
      fractionation = NA_character_,
      n_arctic = NA_real_,
      pct_arctic = NA_real_,
      lat_min = NA_real_,
      lat_max = NA_real_,
      date_min = as.Date(NA),
      date_max = as.Date(NA)
    ))
  }

  v <- d$MEASURED_VALUE_STANDARD
  w <- d$MEASURED_N
  lat <- d$LATITUDE
  arctic <- !is.na(lat) & lat >= arctic_circle_lat()

  tibble::tibble(
    node_id = node$node_id[1],
    label = node$label[1],
    level = node$level[1],
    node_type = node$node_type[1],
    # Every level of aggregation a node spans, per Sam 2026-08-05: "each node
    # [represents] 1+ group covering 1+ MEASURED_N and 1+ different references.
    # we need to report each of these levels".
    n = sum(w, na.rm = TRUE),
    n_rows = nrow(d),
    n_groups = length(unique(members$group_id[members$node_id == node$node_id[1]])),
    n_sources = dplyr::n_distinct(d$REFERENCE_ID),
    # Author-Year text where the reference resolves in references.bib, the raw
    # REFERENCE_ID otherwise (see ?reference_citation_summary): sorted and
    # comma separated so the cell is stable between rebuilds. Displayed by
    # node_report_flextable().
    references = reference_citation_summary(d$REFERENCE_ID, d$TITLE, d$YEAR),
    unit = unique(d$MEASURED_UNIT_STANDARD)[1],
    # CENTRE: weighted by MEASURED_N, so it describes the same population as the
    # `n` reported beside it. SPREAD: per row, because we hold study means and
    # not study values. See ?node_statistic_weighting.
    mean = stats::weighted.mean(v, w = w, na.rm = TRUE),
    sd = stats::sd(v, na.rm = TRUE),
    geo_mean = 10^stats::weighted.mean(log10(v), w = w, na.rm = TRUE),
    gsd = 10^stats::sd(log10(v), na.rm = TRUE),
    median = weighted_median(v, w),
    # Distinct fractionation protocols behind the node, so lumping Total and
    # Filtered rows together shows up here rather than staying invisible in
    # the mean. See ?fractionation_summary.
    fractionation = fractionation_summary(
      d$FRACTIONATION_PROTOCOL_CLASS, d$FRACTIONATION_PROTOCOL
    ),
    n_arctic = sum(w[arctic], na.rm = TRUE),
    pct_arctic = 100 * sum(w[arctic], na.rm = TRUE) / sum(w, na.rm = TRUE),
    lat_min = suppressWarnings(min(lat, na.rm = TRUE)),
    lat_max = suppressWarnings(max(lat, na.rm = TRUE)),
    # as.Date(), not the bare min(). SAMPLING_DATE is an IDate (data.table),
    # courtesy of standardise_IDate_all(), while the zero-row branch above
    # returns as.Date(NA). vctrs refuses to combine IDate with Date, so a node
    # set containing both an empirical and an external node failed to bind at
    # all: "Can't combine ..1$date_min <IDate> and ..6$date_min <date>".
    # Caught by the pipeline, not by the unit tests, whose fixtures use plain
    # Dates throughout.
    date_min = as.Date(suppressWarnings(min(d$SAMPLING_DATE, na.rm = TRUE))),
    date_max = as.Date(suppressWarnings(max(d$SAMPLING_DATE, na.rm = TRUE)))
  )
}

#' Report Cards for Every Node
#'
#' @param nodes The nodes table.
#' @param members The membership table.
#' @param data The `literature_analysis_ready` target.
#' @param ids The group id ledger.
#' @return A tibble, one row per node.
#' @export
aep_node_report_cards <- function(nodes, members, data, ids) {
  if (nrow(nodes) == 0) {
    return(node_report_card(
      dplyr::bind_rows(empty_aep_nodes(), tibble::tibble(node_id = NA_character_)),
      members, data, ids
    )[0, ])
  }
  purrr::list_rbind(purrr::map(
    seq_len(nrow(nodes)),
    function(i) node_report_card(nodes[i, , drop = FALSE], members, data, ids)
  ))
}

#' Validate the Node Layer as a Whole
#'
#' Cross-file checks that neither reader can make alone. Warnings rather than
#' errors throughout, because a half-built node layer is the normal state while
#' the assessment is in progress and the pipeline must still run.
#'
#' @param nodes The nodes table.
#' @param members The membership table.
#' @param cards Output of [aep_node_report_cards()].
#' @return `nodes`, invisibly.
#' @export
validate_aep_nodes <- function(nodes, members, cards) {
  problems <- character(0)

  empirical <- nodes$node_id[nodes$node_type %in% "empirical"]
  no_members <- setdiff(empirical, members$node_id)
  if (length(no_members) > 0) {
    problems <- c(problems, paste0(
      length(no_members), " empirical node(s) have no members: ",
      paste(no_members, collapse = ", ")
    ))
  }

  external_with_members <- intersect(
    nodes$node_id[nodes$node_type %in% "external"],
    members$node_id
  )
  if (length(external_with_members) > 0) {
    problems <- c(problems, paste0(
      length(external_with_members),
      " external node(s) have members, which are ignored: ",
      paste(external_with_members, collapse = ", ")
    ))
  }

  empty <- cards$node_id[cards$n_rows == 0 & cards$node_type %in% "empirical"]
  if (length(empty) > 0) {
    problems <- c(problems, paste0(
      length(empty), " empirical node(s) resolve to no data: ",
      paste(empty, collapse = ", "),
      " (check the restriction columns)"
    ))
  }

  # The converse of the check in read_aep_nodes(). An external node with no
  # magnitude is the other half-finished state: it has no member groups to
  # compute from AND nothing typed in, so its card reports NA and says so
  # nowhere else.
  no_value <- nodes$node_id[
    nodes$node_type %in% "external" & is.na(nodes$external_value)
  ]
  if (length(no_value) > 0) {
    problems <- c(problems, paste0(
      length(no_value), " external node(s) have no external_value: ",
      paste(no_value, collapse = ", "),
      " (nothing to compute from and nothing entered)"
    ))
  }

  # Essentiality and plausibility are always present (aep_nodes.csv). Evidence
  # and quantification are only present once aep_scope_nodes() has merged them
  # from the membership file, so check whichever score columns this table
  # actually carries.
  score_cols <- intersect(
    c("essentiality_score", "plausibility_score",
      "evidence_score", "quantification_score"),
    names(nodes)
  )
  any_na <- Reduce(`|`, lapply(score_cols, function(col) is.na(nodes[[col]])))
  unscored <- nodes$node_id[any_na]
  if (length(unscored) > 0) {
    problems <- c(problems, paste0(
      length(unscored), " node(s) are not fully EPEQ scored: ",
      paste(utils::head(unscored, 8), collapse = ", ")
    ))
  }

  unplaced <- nodes$node_id[is.na(nodes$x) | is.na(nodes$y)]
  if (length(unplaced) > 0) {
    problems <- c(problems, paste0(
      length(unplaced), " node(s) have no x/y placement: ",
      paste(utils::head(unplaced, 8), collapse = ", ")
    ))
  }

  if (length(problems) > 0) {
    cli::cli_warn(c(
      "AEP node layer is incomplete:",
      stats::setNames(problems, rep("*", length(problems)))
    ))
  }

  invisible(nodes)
}

#' What Has Not Been Claimed by Any Node
#'
#' **The backlog view, and the reason it exists.** Sam abandoned sequential review
#' of all 245 groups on 2026-08-05 in favour of picking groups of interest and
#' expanding outwards. That is the right call, but it needs the complement: a
#' ranked list of what has *not* been picked, so stopping is an informed choice
#' rather than an omission nobody noticed.
#'
#' Ranked by measurements descending, so the largest unclaimed group is always
#' the first thing on screen.
#'
#' @param members The membership table.
#' @param summary_data The `summarise_literature_data` target.
#' @param ids The group id ledger.
#' @param decisions Optional decisions table, to carry `decision` through so a
#'   group deliberately dropped is distinguishable from one never looked at.
#' @return A tibble, one row per group, with `node_id` (or `NA`) and `claimed`.
#' @export
node_coverage <- function(members, summary_data, ids, decisions = NULL) {
  key <- triage_group_cols()

  claimed <- members |>
    dplyr::group_by(.data$group_id) |>
    dplyr::summarise(
      node_id = paste(sort(unique(.data$node_id)), collapse = "; "),
      .groups = "drop"
    )

  out <- summary_data |>
    add_coverage_columns() |>
    attach_group_ids(ids) |>
    dplyr::left_join(claimed, by = "group_id") |>
    dplyr::mutate(claimed = !is.na(.data$node_id))

  if (!is.null(decisions) && "decision" %in% names(decisions)) {
    out <- out |>
      dplyr::left_join(
        decisions |> dplyr::select("group_id", "decision"),
        by = "group_id"
      )
  }

  out |>
    dplyr::select(
      dplyr::any_of(c(
        "group_id", "rank", "n", "n_sources", "cum_pct", "tier",
        "node_id", "claimed", "decision"
      )),
      dplyr::all_of(key)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

#' One-Line Summary of Node Coverage
#'
#' What share of the data the current node set accounts for. The number to watch
#' when deciding whether to add another node or stop.
#'
#' @param coverage Output of [node_coverage()].
#' @return A one-row tibble.
#' @export
node_coverage_summary <- function(coverage) {
  tibble::tibble(
    groups = nrow(coverage),
    groups_claimed = sum(coverage$claimed),
    measurements = sum(coverage$n),
    measurements_claimed = sum(coverage$n[coverage$claimed]),
    pct_measurements_claimed = round(
      100 * sum(coverage$n[coverage$claimed]) / sum(coverage$n),
      1
    )
  )
}
