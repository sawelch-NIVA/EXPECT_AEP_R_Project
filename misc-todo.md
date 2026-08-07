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