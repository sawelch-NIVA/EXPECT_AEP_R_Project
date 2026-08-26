# Misc TODO notes. Putting here so that I don't accidentally fuzt with plan while it's being written to

2. Spplit the diff. between compact and regular nodes
4. compact nodes need more info on threshholds?
5. each AEP I guess comes with a table of node info
6. Colour nodes by unit type? 
7. better grouping aesthetics, esp. for mussels
8. I think we are going to have to accept loads of missing data at this stage
11. Add a node type for outflow to AOPs
1. Fix Coteur parsing issue.
12. Swap aep_nodes.csv/aep_node_members.csv for one JSON file with a list column
    for group membership -- CSV can't do array columns without inviting trouble.
    Rough estimate 2026-08-07: half a day to a day, mostly in switching every
    fct_aep_*.R consumer of the members join over to unnesting a list column
    instead. Readers/writers themselves are cheap to convert.
13. Groups with missing key data, found while building composite group IDs
    (2026-08-07). Each falls back to its bare G-number since there's nothing
    to compose a code from:
    - G127: no SPECIES_GROUP/SAMPLE_SPECIES/SAMPLE_TISSUE at all.
    - G087, G094, G131: no SITE_GEOGRAPHIC_FEATURE.
    Worth checking whether these are recoverable from the source references
    or genuinely unreported.
14. RESOLVED 2026-08-08: possible duplicate species under different names,
    found while abbreviating SAMPLE_SPECIES for composite group IDs
    (2026-08-07). "Eukronia hamata" -> "Eukrohnia hamata" and "Phoca
    groenlandica" -> "Pagophilus groenlandicus" corrected at the source in
    group_ids.csv. The Pagophilus/Phoca pair are still separate groups (same
    species, different tissue) -- fine, the point was the name. The Eukrohnia
    pair turned out to be a genuine duplicate: see item 16.
16. G130 retired 2026-08-08, folded into G068. Both were "Eukrohnia
    hamata / Whole body / Ocean, sea, territorial waters / Water column,
    pelagic zone / mg/kg (dry)" -- identical on every group-key column --
    once item 14's rename fixed G130's SAMPLE_SPECIES text, which is exactly
    what surfaced the collision (attach_group_ids() errors loudly on a
    duplicated ledger key, by design). Checked against the freshly computed
    `summarise_literature_data`: the merged group's n = 34 exactly equals
    G068's old n (26) plus G130's old n (8), confirming the underlying
    literature data already treated these as one population and only the
    ledger had them split. Removed G130's rows from group_ids.csv and
    group_decisions.csv, and its section from
    docs/groups/crustaceans-and-invertebrates.qmd (machine-generated only,
    verdict was unwritten, no triage panels existed for it). G068's own
    decision/notes are unreviewed since this merge -- worth a look now that
    its n has grown from 26 to 34.
15. Orphaned triage PNGs, expected from the group_id/group_slug migration
    (2026-08-08, see R/fct_group_ids.R header and
    scripts/migrate_group_ids_to_composite.R). `group_slug` used to be an
    independent slugify_name(label) derivation and is now an alias for the
    composite group_id, so every triage/*.png filename changes on the next
    render. `targets` will write the new files but will not delete the old
    ones under the previous slugs -- `triage/` needs a manual sweep at some
    point to remove whatever is left unreferenced. Not urgent.17. **RESOLVED, and it was never broken.** N016-g-morhua-muscle's
    `exclude_campaigns` works, PLAN.md 9f's short form is correct, and the
    node resolves to the 21 rows 9e concluded with (GM 0.182 mg/kg wet,
    0.10-0.50, dip p = 0.099, i.e. unimodal).

    Recorded because the false alarm cost an afternoon and would cost another.
    On 2026-08-12 `tar_meta()` showed `apply_node_exclusion()` warning "2
    values in exclude_campaigns matched no rows" for this node, which was read
    as a typo. It is not. **The warning came from a spatially scoped AEP**
    (A002/A003), where the bounding box leaves this node with no rows at all,
    so the exclusion legitimately matches nothing *within that scope*. The
    same build's warning block says so one line earlier: "N016-g-morhua-muscle
    ... resolve to no data".

    Acting on it made things worse: the cell was rewritten to the long
    `Vannmiljø Copper Monitoring 2010-2025 (...)` form, which matches
    `CAMPAIGN_NAME` but **not `CAMPAIGN_NAME_SHORT`, which is the column
    `resolve_node_data()` actually passes** (fct_aep_nodes.R:622). That put the
    node back on all 44 rows including the 18 mislabelled Urban Fjord ones,
    which is what "extremely suspect" looked like. Reverted 2026-08-13.

    **The real defect is the warning, not the data.** `apply_node_exclusion()`
    cannot tell a typo from "these rows are outside this AEP's scope", and it
    fires the same message for both. Worth making it say which, or suppressing
    it where the scoped subset is empty. Until then, check an exclusion against
    A001 before believing a warning from a scoped AEP.
