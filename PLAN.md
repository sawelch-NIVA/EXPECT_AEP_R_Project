# Plan: from 300 messy groups to a submitted AEP paper

Written 2026-07-29. Supersedes `docs/_planning.qmd`, which is stale (April, plans a late-July submission).

**Submission target: Monday 2026-09-14.**

### Actual capacity (recount, 2026-07-29)

You estimated ~3 working weeks. The calendar says otherwise:

| Block | Dates | Working days |
|---|---|---|
| This week | Wed 29 – Fri 31 Jul | 3 |
| Week 2 | Mon 3 – Fri 7 Aug | 5 |
| Week 3 | Mon 10 – Fri 14 Aug | 5 |
| **Holiday** | **Mon 17 – Tue 25 Aug** | **0 planned** |
| Week 4 | Wed 26 – Fri 28 Aug | 3 |
| Week 5 | Mon 31 Aug – Fri 4 Sep | 5 |
| Week 6 | Mon 7 – Fri 11 Sep | 5 |
| Submit | Mon 14 Sep | — |
| | | **26 days** |

**You have roughly 26 working days, not 15.** The holiday is 7 working days, not half the remaining period. Laptop availability over Aug 17-25 is deliberately costed at **zero**: anything achieved there is a bonus, and nothing critical is scheduled into it.

Given the stated tendency to overrun, the recommendation is to **bank the slack rather than expand scope.** Week 6 is left as buffer.

**Immediate goal: meaningful progress on the data analysis by Friday 2026-07-31.** Concretely, that means grouping decisions made for the groups that carry most of the data. It does not mean touching the AEP.

------------------------------------------------------------------------

## 1. Post-mortem: why attempt one failed

Recorded so we design against it rather than rediscovering it.

| \# | What happened | Root cause | Design response |
|------------------|------------------|------------------|------------------|
| 1 | Some plots meaningless (n too small), some crashed the render (n too large) | One plot spec applied across a 4-order-of-magnitude range of n | Summarising geoms only; switch presentation on n at a single chokepoint |
| 2 | Plots took forever to resolve through targets | Non-summarising aesthetics, and plot objects rebuilt/redrawn per render | Write PNGs from `format = "file"` targets; notebooks embed, never draw |
| 3 | *G. morhua* liver structure did not generalise | Depth-first on one group, then assumed reuse | Breadth-first triage first; depth only for groups that earn it |

The deeper pattern: attempt one tried to produce **comprehensive coverage** and then read it. Attempt two produces a **ranked shortlist**, reads that, and spends the remaining effort only where the reading says it is worth it.

------------------------------------------------------------------------

## 2. Architecture

Four thin layers. Each is small, and each hands off to the next through a hand-edited CSV, so human judgement is always the interface.

```         
summarise_literature_data  (exists, needs a triage score)
        │
        v
  drop_nonpositive_values (new: strips NA and 0 measured values, counts losses)
        │
        v
  triage panels           one PNG *per plot* per group, n >= 100, format = "file"
        │
        v
  docs/NBXX-Triage.qmd    contact sheet, ranked. YOU READ THIS.
        │
        v
  data/clean/group_decisions.csv     <-- hand-edited. THE decision layer.
        │
        v
  report cards / nodes  ──> data/clean/aep_nodes.csv   <-- hand-edited EPEQ + x/y
        │
        v
  data/clean/aep_edges.csv                             <-- hand-edited EPEQ + status
        │
        v
  plot_aep()              ggraph, manual layout, node cards as images
```

Nothing downstream of `group_decisions.csv` runs until the decisions exist. That is deliberate: it stops the pipeline from generating work you have not asked for yet.

------------------------------------------------------------------------

## 3. Phase 0 — Unblock ✅ DONE 2026-07-29

These are cheap and they are blocking. Do not extend this phase.

- [x] **P0.1** Fix [NBXX-Sample-Groups.qmd:81](docs/NBXX-Sample-Groups.qmd#L81): column built as `` `multimodal (p)` ``, referenced as `"Multimodal (p)"`. flextable will error. One-character fix.
- [x] **P0.2** **DECIDED 2026-07-29: delete.** Remove the outlier `tar_map` factory (`_targets.R:78-185`, and the commented list entries at `:1158-1160` and `:1262`), the 14 generated `docs/NBXX-Distributions-*.qmd` files, and `scripts/generate_distribution_notebooks.R`. The triage layer replaces them. **Keep:** `docs/NBXX-Outliers.qmd` (hand-authored *G. morhua* case study), `R/fct_outlier_detection.R` and `R/fct_statistics.R` (statistics reused by the triage panel), and `R/fct_outlier_groups.R` (`slugify_name()`, `get_compartment_groups()`, `get_biota_groups()` are all still needed to enumerate groups). **Salvage first:** the hand-written prose in `NBXX-Distributions-Aquatic-Sediment.qmd` is the reference implementation for P1.1 and the zero-value note at its lines 53-57 is a real data finding. Lift both out before deleting anything. Also drop `~200` orphaned `outlier_*` objects from the store via `tar_prune()` once the factory is gone.
- [x] **P0.3** Delete `docs/NBXX-Distributions-Aquatic copy.qmd` (byte-identical duplicate). Stage the already-deleted `.rmarkdown` file.
- [x] **P0.4** Confirm whether deleting `docs/.nojekyll` was deliberate.

**Outcome:**

- `multimodal` replaces `bimodal` throughout (`R/fct_outlier_detection.R`, `R/fct_outlier_report.R`, `_targets.R`, `docs/NBXX-Outliers.qmd`). Hartigan's dip test rejects unimodality; it does not count modes, so the old name asserted more than the test supports.
- `docs/NBXX-Sample-Groups.qmd` renders. Verified in the output: red text on exactly 15 cells (the 15 multimodal groups), yellow background on 147 cells (20 flagged rows). Also made both flags NA-safe, so untested groups (n below `min_n`) no longer leak into the highlight sets.
- 16 `.qmd` files deleted (14 distributions + duplicate + `.rmarkdown`), plus `scripts/generate_distribution_notebooks.R` and 108 lines of `tar_map` factory from `_targets.R`. Plot specs salvaged to `scripts/reference_triage_plots.R`, which is never rendered.
- Render targets cut from **16 to 2** (`render_index`, `render_nbxx_sample_groups`). `_quarto.yml` gained an explicit `project: render:` list, so `quarto render` no longer walks every `.qmd` in the tree. Sidebar trimmed to match, avoiding dead links.
- All 22 parked notebooks got `execute: freeze: true`; all 22 frontmatter blocks verified as valid YAML afterwards.
- `index.qmd` now emits **html + docx** (docx via `custom-reference-doc.docx`, png/8x6/300dpi). Confirmed via `quarto inspect` that project-level html settings still merge through: svg, 12x8, code-fold, embed-resources all survive.
- Deployment cruft removed: `rsconnect/`, `.posit/`, `manifest.json`, stale `docs/search.json`, and the commented Posit Connect target. ~2.9 MB.
- `tar_prune()` removed **206 orphaned `outlier_*` objects**; verified beforehand that the prune list contained nothing else. 53 real objects remain.
- Pipeline validates: 65 targets defined.

------------------------------------------------------------------------

## 4. Phase 1 — Triage infrastructure (Wed PM – Thu AM)

The goal is a contact sheet you can read in one sitting.

- [x] **P1.0** ✅ **Built 2026-07-29, untested.** `R/fct_analysis_ready.R` provides `drop_nonpositive_measurements()` (filters `MEASURED_VALUE_STANDARD` for `NA` / zero / negative, on that column only) and `report_dropped_measurements()` (per-group loss counts: `n_na`, `n_zero`, `n_negative`, `n_dropped`, `prop_dropped`, worst first). Two targets added: `literature_analysis_ready` and `literature_dropped_report`. `summarise_literature_data` now reads from the former, and its redundant internal `filter(!is.na(...))` is removed.
      **Not run.** `tar_make()` is yours. Expect `summarise_literature_data` and everything downstream to invalidate; group stats may shift slightly where zeros were previously included in `mean` / `sd` / `median`.
- [~] **P1.1** 🔧 **Pilot built 2026-07-29, untested.** `R/fct_group_triage.R`, one function per plot: `triage_plot_density()`, `triage_plot_by_date()`, `triage_plot_by_category()` (shared by the campaign and site-type views), `triage_plot_spatial()`. No patchwork. Each writes its own PNG at a fixed 8x5in / 150dpi canvas.
      `triage_use_points()` owns the single n-switch: under 30 observations a view draws points, over it draws 2D bins or a density. `triage_empty_plot()` returns a labelled blank where a view cannot apply (no coordinates, no category clearing `min_facet_n`), so one awkward group cannot kill a whole batch and the gap stays visible on the contact sheet.
      **Pilot scope:** targets `triage_pilot_groups` (5 groups sampled from the 27 with `n >= 100`, `seed = 20260729` so the selection is stable) and `triage_pilot_plots` (`format = "file"`, writes to `_triage/`). Widen by raising `n_sample`, or set it to `Inf` for all 27.
      **Not run.** Aesthetics are expected not to fit every group yet; that is what the pilot is for.
- [x] **P1.1a** ✅ **Tested 2026-07-29.** 73 tests across `tests/testthat/test-fct_analysis_ready.R`, `test-fct_group_triage.R`, `test-fct_sample_groups_table.R`, all on synthetic fixtures so they do not depend on the target store. Plus a smoke run over all 5 pilot groups with real data, forcing both `ggplot_build()` and `ggsave()`.
      Bugs found and fixed: (1) `map_data()` is exported by **ggplot2**, not `maps` — passing a `maps::map()` object to `geom_polygon()` routes through the deprecated `fortify.map()` and dies in `names[df$group, 1]`; (2) `.keep = "none"` left `n` and `median` in their original input positions, so the table rendered as N, Median, Group, Location instead of Group first — now ordered explicitly.
      **Measured cost: 0.35s per plot**, not the 2-3s assumed. All 27 groups with `n >= 100` would take under a minute, so widening the pilot is cheap.
- [x] **P1.1b** ✅ **Pilot feedback applied 2026-07-29.** (1) `docs/NBXX-Sample-Groups.qmd` gained a Triage plots section: one `##` heading per group, its five views in a `layout-ncol=5` row, lightbox enabled, generated `asis` from the targets so it scales past the pilot. (2) Files renamed `{group}_{a-e}_{view}.png` so a file browser sorts them into reading order. (3) Plot (a) is now unit-agnostic: it receives a subset built with `filter_to_group(..., exclude_cols = "MEASURED_UNIT_STANDARD")` and colours by unit, so dry vs wet divergence is visible (G. morhua liver goes from 218 rows/1 unit to 3,014 rows/2 units, 2 density curves). (4) `prettify_campaign_name()` extracted as its own function and wired in via a new `label_fn` argument. (5) The points-vs-bins switch is now documented in a callout in the notebook.
      **Watch out:** the `case_when()` in `docs/NBXX-Outliers.qmd` and `scripts/reference_triage_plots.R` has no `.default`, so it turns every non-Vannmiljø campaign into `NA` — 28 of 72 campaigns. `prettify_campaign_name()` fixes this; the notebook copies still have the bug.
      87 tests passing. Notebook renders: 25 base64-embedded images, 5 layout rows, no broken paths.
- [x] **P1.1c** ✅ **2026-07-29.** Facet minimum removed from panels (c) and (d): they answer a coverage question, not a statistical one. Safe because cardinality is bounded (max 31 campaigns, 5 site types per group). This restores **62 of 204** campaign facets that the `n >= 10` filter had been hiding.
      **Panel (d) was degenerate and is now fixed.** `SITE_GEOGRAPHIC_FEATURE(_SUB)` are part of the group key, so within a strict group there was always exactly one site type and the panel was a single row. It now uses a geography-relaxed subset (same trick as panel (a) with units), showing 3-5 site types where the data supports it.
      `case_when()` bug fixed at both remaining sites (`docs/NBXX-Outliers.qmd`, `scripts/reference_triage_plots.R`), both now calling `prettify_campaign_name()`. In the Outliers notebook the `.default =` was additionally being passed to `mutate()` rather than `case_when()`, silently creating a spurious `.default` column.
      90 tests passing.
- [x] **P1.1d** ✅ **2026-07-29.** Shared scales via a new `triage_scale_limits` target (`compute_triage_scale_limits()`, grouped by `ENVIRON_COMPARTMENT`, `by =` parameterised). Every panel in a group and every group in a compartment now share a value axis. Panel (e) colour scale is binned on **both** branches with shared limits (it was continuous for points, binned for hex). Group headings report `n_sources`. Embedding disabled and `_triage/` renamed `triage/`: page went **3.3 MB to 0.24 MB** with 25 linked PNGs.
      **Measured, so you can judge the squeeze:** Aquatic spans **12.3 orders of magnitude**, which is the entire global range, so per-compartment limits are effectively global for ~90% of rows. Adding unit to `by` only narrows Aquatic/mg-L to 9.8. The spread is genuinely within-unit.
      **Data flag:** max Aquatic sediment value is 270,000 mg/kg dry = 27% Cu by mass. Ore grade, not sediment. Worth chasing.
      99 tests passing.

- [x] **P1.1e** ⚠️ **Root cause found for the long-standing invalidation bug.** `tar_option_set(imports = "STOPAEP")` added. Functions live in the package namespace via `pkgload::load_all()`, and without `imports` targets does not hash them, so **editing any `R/fct_*.R` function invalidated nothing**. Confirmed directly: after changing `sample_triage_groups()`, `tar_outdated()` reported `(none)` and `tar_make()` reused the stale target. This is almost certainly the cause of the `load_literature_pqt` "doesn't properly update" note (CLAUDE.md 3.7).
      Enabling it invalidated 60 targets once, as expected. Full rebuild run; store now clean (`tar_outdated()` = 0).
- [x] **P1.1f** ✅ **Widened to 25 groups 2026-07-29.** Date axis is now global and never grouped (`compute_triage_scale_limits()` carries `date_min`/`date_max` spanning 1988-07-01 to 2025-11-18); rug lines thickened. 125 PNGs, full pipeline 1.7 min, page 0.33 MB with linked images.
      **Two bugs found by widening, both invisible at 5 groups:**
      1. `slugify_name()` calls `make.unique()`, so groups sharing a label got `_1`/`_2` suffixes. `triage_group_label()` omitted `SITE_GEOGRAPHIC_FEATURE_SUB` even though it is part of the group key, so distinct groups collided. That produced **duplicate notebook headings** and left unsuffixed slugs as string prefixes of suffixed ones, breaking filename matching. Label now includes the sub-site; 25/25 slugs unique, no suffixes.
      2. Constructing filenames by hand removed the chunk's `tar_read(triage_pilot_plots)` call, which was the **only thing giving the render target its dependency on the plots** — so Quarto rendered before the PNGs existed and every image came out "not available". The chunk now validates against the target's own output, keeping the dependency live.
      112 tests passing.

- [x] **P1.1g** ✅ **2026-07-30. Thresholds, headings, table links, layout.**
      `R/fct_threshold_match.R` (new) matches `copper_toxicity_thresholds` to a
      group by compartment, unit and taxon. Read its header before interpreting
      any line: the comparators are borrowed across compartments, species and
      tissues, deliberately, and are a sanity check rather than an assessment.
      Mapping: all non-marine water to the freshwater classes, marine and
      brackish to coastal, sediment to (marine) sediment, terrestrial blank;
      vertebrates to *G. morhua*, invertebrates to *M. edulis*, algae and the
      catch-all groups blank. Units are **matched, never converted**, so
      wet-weight biota get PROREF, dry-weight invertebrates fall through to the
      ICES BAC, and dry-weight vertebrates get nothing.
      **Corrected the threshold data itself.** Freshwater and sediment boundaries
      were misaligned by one row against their own comments (sediment had two
      classes sharing an upper boundary of 20). Copper has no Class III in
      either: M-608 runs Class II into Class IV. Now
      `c(0.3, 7.8, 15.6, NA)` and `c(20, 84, 147, NA)` over four classes.
      **Presentation, second attempt.** The first put rotated labels beside each
      line; with the shared axis spanning up to 12.3 orders and the boundaries
      inside about one, three labels landed within 7% of the panel width and
      stacked illegibly. Class names now sit on a **secondary axis** and the
      panels carry no text at all. Colour and linetype are keyed on the class
      *number* (muted MD blue-green-ochre-orange-red, dotted through to solid),
      so Poor stays orange despite Class III being absent, and a dataset that
      does have a Class III picks up the third style with nothing reordered.
      **Two bugs worth remembering:** (1) `stat_bin2d()` takes its binning range
      from the *shared scale*, not its own layer, so a label at `y = Inf` pushed
      that range to infinity, the stat asked for over a million bins and failed
      outright, drawing no heatmap. Panels (c) and (d) now count explicitly and
      draw `geom_tile(height = 1)`, which also fixed the band-height gaps (0.179
      of a row pitch of 1.0). (2) A class-number regex `(I{1,3}|IV|V)` matched the
      bare `I` inside `(IV)`, styling Poor as Background. Ordered alternation is
      load-bearing. Caught by eye on a prototype, not by any numeric check.
      Also: hierarchical headings (six levels, unit not a level, unit variants
      share a heading and an anchor), siblings ordered by summed `MEASURED_N`,
      summary-table hyperlinks into the 24 sections (opt-in per document, so the
      manuscript does not carry dead anchors into its docx), `theme_minimal()` via
      a shared `triage_theme()`, dotted grey60 trendline over a solid white halo,
      and `docs/_metadata.yml` for notebook-wide `page-layout: full` /
      `toc-depth: 6`. 237 tests passing, up from 112.
      **Come back to:** panel (b) draws threshold lines but omits the class axis,
      because on a vertical axis the numerals collide (II and IV are 0.24 orders
      apart on a 10.7-order axis) and the rotated title crowds the legend. No
      better idea yet, and these are triage plots.

- [x] **P1.2** ✅ **2026-07-30.** Delivered as `write_triage_plots_for_group()`: fixed 8x5in / 150dpi canvas, PNG via `ragg::agg_png`, returning the written paths.
- [x] **P1.3** ✅ **2026-07-30, by a different design.** One `triage_pilot_plots` target with `format = "file"` writing all 125 PNGs to `triage/`, rather than a `tar_map()` over groups. Ticked off as-is on Sam's call.
      **Known cost, so it is not a surprise later:** invalidation is all-or-nothing. Editing any plot function redraws all 125 PNGs, measured at 1m 20s to 2m 20s. A `tar_map()` would redraw only the affected group's five. Revisit only if that wait starts to bite.
      Note the directory is `triage/`, not `_triage/` as this item originally said: Quarto skips underscore-prefixed directories as project resources, which broke every linked image.
- [x] **P1.4** ✅ **2026-07-30.** Ranked by `n` descending, no composite score.
      `R/fct_triage_flags.R` holds `add_triage_flags()`, which the
      `summarise_literature_data` target now ends with, plus `group_flag_text()`
      and `group_summary_line()`. The summary table and the per-group text under
      each triage heading read the **same** function, and a test walks every
      group asserting the highlighting and the prose agree.
      **Scope pulled back from what this item originally listed**, on Sam's call
      2026-07-30, and the reasoning is worth keeping because it will recur:
      - `n_references == 1` **dropped as a flag.** It fired on 234 of 245 groups.
        Vannmiljø is one `REFERENCE_ID` covering monitoring for the whole of
        Norway, so a single source is the normal state of this dataset, not an
        exception. Flagging the baseline left 236 of 245 groups flagged and
        buried the signals that discriminate. `n_sources` is still reported.
      - **CV removed entirely, and no replacement added.** Measured on this data,
        CV correlated **0.96** (Spearman) with max/median across the 51 groups
        with `n >= 20`: it tracked the single largest value, not the spread.
        Dropping one row of 4,969 from Marine/Salt Water moved CV from 40.7 to
        4.7, while a log-scale spread measure moved 2.9 to 2.8. It was also
        redundant, being exactly `sd / mean` with both already reported.
        Geometric SD and the interquartile ratio were offered as defensible
        log-scale alternatives and **declined**: a spread statistic that cannot be
        justified in the methods section is worse than none.
      - Drop proportion and unit count are **columns, not flags**, which is what
        this item asked for. A 90% drop rate is reported and not warned about.
      **The flag set is now exactly two**, both predating this work: outlier
      fraction > 5% and the dip test. `triage_flag_limits()` carries a note that
      adding a third is Sam's call, and a test asserts the set has not grown.
      287 tests passing.
- [x] **P1.5** ✅ **2026-07-30. Weighted by `MEASURED_N`**, per Sam's decision.
      `n_double_outliers` is now `sum((outlier_RMZ & outlier_IQR) * MEASURED_N)`,
      so numerator and denominator are both measurement counts. The old row count
      is retained as `n_outlier_rows` so the change stays auditable.
      **Measured effect:** 20 groups flagged before, **22** after; nothing
      un-flagged. Small in count because most groups have `MEASURED_N == 1`
      throughout, but large where it bites: an Aquatic Sediment group went 3.7% to
      **15.9%** (6 outlier rows carrying 26 measurements) and a *G. morhua* group
      3.9% to **11.8%**. `na.rm = TRUE` because `robust_modified_z_score()`
      returns `NA` where the MAD is zero, so untested rows count as
      non-outliers, which is the conservative direction.

- [ ] ~~**P1.5** **Fix the outlier-fraction denominator.**~~ Superseded above. In `summarise_literature_data`, `n = sum(MEASURED_N)` (total measurements) but `n_double_outliers = sum(outlier_RMZ & outlier_IQR)` (count of **rows**). The ratio `n_double_outliers / n` therefore divides a row count by a measurement count, so the flag systematically under-fires wherever `MEASURED_N > 1`. It currently trips on 20 of 245 groups; the true figure is higher. **Not fixed in Phase 0 because it changes a reported statistic, which is your call, not mine.** Options: divide by row count instead, or weight the outlier count by `MEASURED_N`. Cheap either way, but decide before the Phase 2 decisions rest on it.

**Budget check:** at `n >= 100`, expect roughly **30-50 groups** rather than the 60-100 at `n >= 30`. At 2-3s x 5 plots that is 8-13 min single-threaded, a few minutes across crew workers. Runs once, then targets caches each PNG independently, so editing one plot function only invalidates that plot across groups.

**Deferred by choice:** groups in the 30-100 range get panels only if Week 6 buffer survives. Note in `group_decisions.csv` that they were decided from the table alone, so it is visible later which decisions had thinner support.

**Cut line (3pm Thursday):** if the `tar_map` file-target factory is fighting you, abandon it and generate the panels from a plain loop in a script writing to `_triage/`. Losing the dependency tracking costs you one manual re-run. Losing Friday costs you the week.

------------------------------------------------------------------------

## 5. Phase 2 — Make the decisions (Thu PM – Fri)

**This is the Friday deliverable, and it is the only thing that matters this week.**

- [x] **P2.1** ✅ **2026-07-30, satisfied by `docs/NBXX-Sample-Groups.qmd`** rather than by a new file. It already embeds the panels as linked images, one row of five views per group, under a hierarchical heading tree, above a summary table that links into each section. A second near-identical notebook would be duplication, and the 14 distributions notebooks it was meant to replace are already gone (P0.2).
      Also widened from a 25-group sample to **all 27 groups with `n >= 100`** (`n_sample = Inf`). At 25 there were two eligible groups with no panels and nothing on the page to say they were missing rather than absent.
      **Difference from this item as written:** ordering is hierarchical with siblings ranked by `sum(MEASURED_N)`, not flat rank order. Nesting is what makes 27 groups navigable, and the summary table above is already strictly rank-ordered, so rank order is available where it is useful.
- [x] **P2.2** ✅ **2026-07-30.** `R/fct_group_decisions.R` plus `scripts/scaffold_group_decisions.R`, which writes `data/clean/group_decisions.csv`: 245 rows, the full group key, `species_common_name`, `rank`, `n`, `n_sources`, `cum_pct`, `tier`, both flags, and empty `decision` / `lump_into` / `notes`.
      **Scaffolding is a hand-run script, not a target.** The pipeline reads the file (`group_decisions` target, which also warns when groups in the data are absent from the file) and never writes it. Writing a hand-edited file from a target is how an afternoon of judgement gets silently overwritten by a rebuild.
      **The scaffold is an idempotent merge**, so it is safe to re-run whenever new data arrives: machine context (`n`, coverage, flags) refreshes, `decision` / `lump_into` / `notes` are never touched, new groups append as undecided, and a *decided* group that has vanished warns rather than disappearing quietly. Same cache-versus-curation split as section 10. `read_group_decisions()` validates the vocabulary and warns on a `lump` with no `lump_into`. 24 tests.
- [ ] **P2.3** **You read the contact sheet and fill in the decisions.** No automation. Work down from largest n.

**Definition of done, revised 2026-07-30 after measuring it.** The original wording was "every group covering the top 90% of total measurements", guessed at "probably the top 30-50 rows". The real distribution is far more skewed than that:

| Coverage | Groups needed |
|---|---|
| 50% | **2** |
| 75% | 3 |
| 90% | **7** |
| 95% | 17 |
| 99% | 85 |

183 of 245 groups have `n < 30`. So the literal Friday criterion is **7 decisions**, which is an hour, not two days. All 7 already have panels, as do all 17 of the top 95%.

**The criterion is also aimed at the wrong thing, and this matters more than the count.** Measurement volume is dominated by Vannmiljø water and sediment monitoring, so the top 90% is 4 abiotic groups plus 3 biota groups (two of them the same *Mytilus edulis* / soft tissue group in different site types). That gives almost no biota diversity. The biota groups an AEP needs as nodes are nearly all small: **62 of the 68 `top99` groups are Biota**, sitting between `n = 20` and `n = 218`. Algae, one of the systems P3.5 names, does not appear until well below the 95% line (*Ascophyllum nodosum* 75 rows, *Fucus vesiculosus* 71).

**Recommended replacement:** decide all **17** groups down to 95% coverage, *plus* every group needed for AEP node coverage regardless of `n`, driven by the systems chosen in P3.5. Ranking by `n` is the right way to order the work; it is the wrong way to decide where to stop.

**Explicitly not this week:** report cards, EPEQ scoring, edges, any AEP rendering, any repo cleanup beyond Phase 0.

------------------------------------------------------------------------

## 6. Phase 3 — Report cards and nodes (Mon 3 – Fri 7 Aug)

- [ ] **P3.1** `node_report_card(group)` returning a one-row tibble: mean, median, n, unit, n references, geographic range, temporal range.
- [ ] **P3.2** Mini distribution PNG per node. Reuses P1.2 at a smaller canvas.
- [ ] **P3.3** Scaffold `data/clean/aep_nodes.csv` from the groups marked `own_notebook`, with empty EPEQ score and justification columns.
- [ ] **P3.4** **You score the nodes.** Copy the reasoning style already in `NBXX-algae.qmd:tbl-epeq-algae`; it is good and it is yours.
- [ ] **P3.5** Pick the systems to model. Target 3-5, likely fish, algae, shellfish, sediment, water. Driven by the Phase 2 decisions, not chosen in advance.

------------------------------------------------------------------------

## 7. Phase 4 — Edges (Mon 10 – Fri 14 Aug, hard time-box)

**Your stated highest risk. The mitigation is scope, not effort.**

- [ ] **P4.1** Scaffold `data/clean/aep_edges.csv` for every plausible flow between chosen nodes, all defaulting to `status = "putative"`.
- [ ] **P4.2** **Time-box gap-filling to 3 working days.** For each edge, spend at most \~30 minutes looking for empirical support. Found: score it, mark `empirical`, cite. Not found: leave `putative`, write one sentence on what evidence *would* settle it, move on.
- [ ] **P4.3** Make `putative` visually distinct in the diagram (dashed, greyed, unlabelled magnitude).

The unfilled edges are a result. A regional-scale AEP that honestly marks its own gaps is a more defensible contribution than one that quietly implies completeness. This is also the argument that lets you stop on schedule.

------------------------------------------------------------------------

## 8. Phase 5 — Assemble and render (Wed 26 – Fri 28 Aug)

- [ ] **P5.1** `plot_aep(nodes, edges)`: ggraph with **manual** coordinates from `aep_nodes.csv`. Automatic layouts are wrong here because vertical position carries source-to-TSE meaning.
- [ ] **P5.2** Node cards placed with `ggimage::geom_image`.
- [ ] **P5.4** **Composed multi-panel figures.** This is where patchwork comes back, deliberately and only for figures actually going into the manuscript. Triage plots stay as individual PNGs forever.
- [ ] **P5.3** 3-5 focused system AEPs plus one holistic low-detail AEP (matching the `docs/_planning.qmd` Materials/Methods items 1 and 2, which are still the right targets even though its schedule is dead).

------------------------------------------------------------------------

## 9. Phase 6 — Writing (Mon 31 Aug – Fri 11 Sep, buffer included)

You write. Noted as scheduled work so it is not treated as free.

- [ ] **P6.1** Rewrite `AP01-review-protocol.qmd` to present the review as targeted evidence-gathering complementing Vannmiljø, not as a systematic review. Cheap, and removes a reviewer attack surface.
- [ ] **P6.2** Move introduction material to discussion, as anticipated in `_planning.qmd` on the basis of the eData paper experience.
- [ ] **P6.3** Resolve the `**Comment:**` and `**KET:**` co-author prompts left inline in `index.qmd`.
- [ ] **P6.4** Results and discussion from the AEPs.

------------------------------------------------------------------------

## 10. Deferred, deliberately

Real problems (see `CLAUDE.md` section 3) that must not eat working days before 2026-09-14. Revisit after submission.

- renv disabled; `renv.lock` decorative.
- Absolute Windows paths in both `_targets.yaml` files.
- Notebook numbering collapse (three-way disagreement between filename, title, and sidebar); duplicate `AP03` and `NB07` prefixes.
- `_quarto.yml` sidebar \~20 files behind the tree.
- Half-migrated frontmatter between commits `9134762` and `8ad2d1f`.
- `DESCRIPTION` placeholder license, missing imports, one test file.
- 4.6 MB `references.bib`, 770 KB `manifest.json`, \~190 orphaned `.quarto/quarto-session-temp*` directories.

### Split the species-name cache from the curation layer

Noted 2026-07-30 while the reasoning was fresh. **An important pattern, not an
important task right now.** Roughly 30-45 minutes including tests, and the
current state works (94 of 128 species named, 99% of biota rows by volume).

`data/clean/species_common_names_cache.csv` currently does two jobs with
different lifecycles: it stores what the API said (reproducible, disposable,
regenerable) *and* it is the only place a hand correction could live
(irreplaceable). Because they share one file, the cache cannot safely be deleted
to force a refresh, which is the opposite of what a cache is for. Hand edits
survive today only because `get_common_names()` never re-queries a species
already present, i.e. by accident rather than by design.

The pattern, if and when it is worth doing:

1. **Machine cache**, append-only, never hand-edited, safe to delete. One row per
   (query, source) **including negative results**, so a species with genuinely no
   English name is not re-queried forever and "never asked" stays distinguishable
   from "asked, no answer".
2. **Override file**, hand-written, never machine-written, committed, with a
   `reason` column so a decision is reviewable later and defensible in the
   methods.
3. **One resolver** owning the precedence `override > source[1] > source[2] > NA`,
   emitting a `name_source` column.

Plus the piece that actually closes the loop: a **curation-todo target** listing
unresolved species by data volume, *with the candidate names the APIs returned*.
New data repopulates it automatically; the negative cache stops it re-proposing
what has already been rejected. Same shape as `group_decisions.csv`: pipeline
proposes, human decides in a CSV, pipeline reads and never writes.

Two traps worth recording:

- **Name resolution and attribute lookup are different problems.** `Eukronia
  hamata` in the data is a misspelling of `Eukrohnia hamata`, which resolves
  fine. Fuzzy name resolution belongs upstream, with its own overrides;
  otherwise every typo looks like a missing common name and the same organism
  gets curated twice.
- **Not everything unnamed is a species.** `Bunndyr` (Norwegian for benthic
  fauna) and `Zooplankton epilimnion` should fail validation on the way in, not
  sit in a curation queue. Wants a `not_a_species` bucket.

Rejected alternatives: a database (loses reviewable diffs, which is the whole
point at this scale); `memoise`/`cachem` (opaque binary, cannot be a curation
surface); the `targets` store (disposable by design, so human decisions must not
live there).

**One exception worth doing if the pipeline is genuinely costing you time:** cut the number of rendered notebooks. Roughly 34 `.qmd` files currently render on every full build, of which \~20 are unreachable from the sidebar anyway. Phase 0.2 plus Phase 2.1 removes 14 of them on their own. Render cost is the main pipeline slowness; repo size is mostly not.

Also worth an hour at some point, but not before Friday: `load_literature_pqt` is a pass-through whose comment claims a dependency on a `save_literature_pqt` target that is not actually referenced, which is very likely the cause of the "doesn't properly update" note at `_targets.R:1038`.

------------------------------------------------------------------------

## 11. Open questions

1.  ~~**P0.2**: delete or re-enable the outlier factory?~~ **Answered 2026-07-29: delete.** See Phase 0.
2.  ~~**When exactly is the holiday?**~~ **Answered: Mon 17 – Tue 25 Aug**, laptop available but costed at zero. Real dates now pinned throughout.
3.  ~~**Is `n >= 30` the right triage cutoff?**~~ **Answered: start at `n >= 100`.** Revisit the 30-100 band only if Week 6 buffer survives.
4.  ~~`docs/_planning.qmd` contract-end vs deadline?~~ **Answered: 2026-09-14 governs.** `docs/_planning.qmd` is hopelessly out of date and should be either deleted or replaced with a pointer to this file (it is still linked from the `_quarto.yml` sidebar, so it renders into the site as-is).
5.  **P1.0 scope:** confirm that dropping `NA`/`0` means the measured value column only, not a whole-row `drop_na()`. Proceeding on the narrow reading.