# docs/archive

Parked notebooks and appendices, moved here 2026-09-11 so the working
`docs/` directory only holds what's actually live. None of this is deleted:
git history is intact (`git mv`, not `git rm`), and the files themselves
still sit here on disk rather than only in `git log`.

**Nothing in `_targets.R` or `_quarto.yml` builds anything in this
directory.** Verified by grep: only five documents render at all --
`index.qmd`, `docs/NBXX-Sample-Groups.qmd`, `docs/AP04-unit-corrections.qmd`,
`docs/AP05-aep.qmd`, `docs/NBXX-rfjord-2.qmd` -- and none of those reference
a file below. Anything here can still be rendered by hand with
`quarto render docs/archive/<file>.qmd`.

Archived rather than deleted for the same reason as `data/clean/archive/`:
"nothing reads it" is not "nothing in it matters". Several of these are
cited by name in code comments as provenance for logic that now lives in
`R/`, and a couple may hold hand-transcribed source data that was never
moved anywhere else. Deleting any of them is a decision for Sam, not a side
effect of tidying.

| File | Last touched | Why it is here |
|---|---|---|
| `AP03-creed-criteria.qmd` | 2026-07-29 | One of two files both numbered AP03 (CLAUDE.md 3.4); parked. |
| `AP03-project.qmd` | 2026-07-29 | The other AP03; parked. |
| `NB01-pipeline.qmd` | 2026-07-29 | Parked pipeline-overview notebook. |
| `NB02-vannmiljo-qc.qmd` | 2026-07-29 | Parked; opens with a FIXME about a broken validation function (CLAUDE.md 3.7). |
| `NB02-vannmiljo.qmd` | 2026-08-06 | Parked Vannmiljø exploration notebook. |
| `NB03-qc.qmd` | 2026-07-29 | Parked; titled "Notebook 01" internally (CLAUDE.md 3.4 numbering drift). |
| `NB03-visualisation.qmd` | 2026-07-29 | Parked. |
| `NB04-map.qmd` | 2026-07-29 | Parked. |
| `NB05-network.qmd` | 2026-07-29 | Parked. |
| `NB06-WoE.qmd` | 2026-08-31 | Parked; links `peng_aep_diagram.png`, which was deleted in the 2026-08-31 figures split, so it already dangled before this move. |
| `NB07-aep-review.qmd` | 2026-08-06 | One of two files numbered NB07 (CLAUDE.md 3.4); parked. |
| `NB07-emissions.qmd` | 2026-07-29 | The other NB07, titled "Notebook 08" internally; parked. |
| `NB08-ecology.qmd` | 2026-07-30 | Parked. |
| `NBXX-Outliers.qmd` | 2026-07-29 | The hand-authored *G. morhua* liver case study the (now-deleted) outlier factory was generalised from. Conceptually important, but parked and superseded as a workflow by the AEP node/edge CSVs. |
| `NBXX-REACH.qmd` | 2026-09-03 | Logic pulled out into `R/fct_reach.R` / `R/fct_reach_products.R`, which still cite this file in comments for provenance. Parked. |
| `NBXX-algae.qmd` | 2026-08-31 | The end-to-end worked example for scoring an AEP node (CLAUDE.md 4.1), cited by `R/fct_aep_nodes.R`. Archived at Sam's request 2026-09-11 rather than left live. |
| `NBXX-aquaculture.qmd` | 2026-09-08 | Parked exploratory notebook. |
| `NBXX-data-processing.qmd` | 2026-08-31 | Parked. |
| `NBXX-emissions-prtr.qmd` | 2026-08-06 | Parked; overlaps with `scripts/summarise_prtr_emissions.R`, which notes both need the same PRTR figures and warns against keeping two copies. |
| `NBXX-fish.qmd` | 2026-07-29 | Parked. |
| `NBXX-norske-utslipp.qmd` | 2026-07-29 | Parked. `R/fct_prtr_emissions.R` notes some numbers here are "written as prose... and need TRANSCRIBING, not re-deriving" -- check before deleting that the transcription actually landed elsewhere. |
| `NBXX-repparfjorden.qmd` | 2026-08-26 | Superseded by the live `docs/NBXX-rfjord-2.qmd`. |
| `NBXX-soerfjorden.qmd` | 2026-08-31 | Parked. |
| `NBXX-spin-use-categories.qmd` | 2026-09-03 | Parked. |
| `_journals.qmd` | 2026-07-29 | Parked planning journal, superseded by `PLAN.md` at the repo root. |
| `dev-node-card-style.qmd` | 2026-08-31 | Scratch notebook for node-card styling iteration, same spirit as `images/dev/`. |

## Before deleting anything here

Run `git log --follow` on the file first, and for `NBXX-norske-utslipp.qmd`
and `NBXX-emissions-prtr.qmd` specifically, confirm the data they describe
made it into `data/clean/derived/` or a script output before it goes --
git history keeps the text, but not the certainty that it was ever acted on.
