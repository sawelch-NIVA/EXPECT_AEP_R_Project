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
14. Possible duplicate species under different names, found while abbreviating
    SAMPLE_SPECIES for composite group IDs (2026-08-07). Both pairs abbreviate
    to the same code by coincidence, which is what surfaced them, but they
    look like genuine taxonomic duplicates rather than unrelated species that
    happen to collide:
    - "Eukrohnia hamata" and "Eukronia hamata" -- looks like a misspelling of
      the same chaetognath, not two species.
    - "Phoca groenlandica" and "Pagophilus groenlandicus" -- old vs. current
      name for harp seal.
    If they are the same species, those groups should probably be lumped
    rather than each given a distinguishing code. Left un-overridden in
    group_species_code_overrides.csv on purpose for now, so the composite ID
    keeps surfacing the collision (both -> "E.ham" / "P.gro") until it's
    resolved one way or the other.