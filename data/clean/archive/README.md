# data/clean/archive

**Nothing in this repository reads any file in this directory.** That was
verified by grep across `_targets.R`, `R/`, `scripts/`, `docs/` and
`tests/` on 2026-08-06, when `data/clean/` was split into subdirectories.

Archived rather than deleted, because "no code path reads it" is not the same as
"it is not needed": several of these are raw exports or reference data that may
be wanted again, and one is the input to a step that was never wired up. Deleting
them is a decision for Sam, not a side effect of tidying.

| File | Date | Why it is here |
|---|---|---|
| `Vm_2025_20251216_100409.zip` | 2025-12-16 | Vannmiljø bulk export. No code unzips or reads it; the pipeline reads `data/raw/`. Keep as provenance for the current extract. |
| `Vm_Unzipped/` | 2026-03-10 | Unpacked contents of the above. Same. |
| `Vm_analysis_method_lookup.csv` | 2026-01-05 | Superseded by `lookups/vm_methods_lookup_filled.csv`. |
| `Vm_lookup_references.csv` | 2024-09-25 | No consumer. Predates the current eData reference handling. |
| `Vm_medium_lookup_matrix.csv` | 2025-12-15 | Unfilled version. The pipeline reads `lookups/Vm_medium_lookup_matrix_filled.csv`. |
| `lit_data.csv` | 2026-01-16 | No consumer. Appears to be an early flat export of the literature table. |
| `ecotox_2025_06_12_species.parquet` | 2025-09-29 | ECOTOX species list. No consumer; species names now resolve through `lookups/species_common_names_cache.csv`. |

## Not archived, but also not wired up

`derived/literature_data.parquet` stayed in `derived/` rather than moving here.
Its reader and writer both exist (`load_literature_parquet()`,
`save_literature_parquet()`), and neither is called from `_targets.R` -- the
`save_literature_pqt` dependency mentioned in the comment at `_targets.R:1000`
is not a real target reference. See CLAUDE.md 3.7. It is a derived artefact of a
step that is currently dormant, not residue, so it is filed by what it is.

## Before deleting anything here

Check `git log --follow` on the file first. Several predate the current pipeline
and may be the only record of an earlier extract.
