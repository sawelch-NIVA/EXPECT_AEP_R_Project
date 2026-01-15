# oh shit, look at all these new Methods

missing_s <- vm_edata_intermediate |>
  group_by(Provetakmetode_id) |>
  filter(Provetakmetode_id %notin% vm_lookup_methods$ISO_ID) |>
  reframe(n = n()) |>
  arrange(desc(n))

# 18 missing sampling methods :(

missing_a <- vm_edata_intermediate |>
  group_by(Analysemetode_id) |>
  filter(Analysemetode_id %notin% vm_lookup_methods$ISO_ID) |>
  reframe(n = n()) |>
  arrange(desc(n))
# 1 missing analysis method

tar_read(vm_lookup_sampling) |>
  filter(SamplingMethodID %in% missing_s$Provetakmetode_id) |>
  View()

tar_load(vm_edata_intermediate)

# joins ----
command = join_all_literature_modules(
  measurements_data = measurements_data,
  sites_data = sites_data,
  reference_data = reference_data,
  biota_data = API_biota_common_names, # FIXME: problems
  campaign_data = campaign_data,
  parameters_data = parameters_data,
  methods_data = methods_data # FIXME: problems with Vm numbered protocols - why are they still there?
)

print("sites_data")
join1 <- left_join(measurements_data, sites_data)
nrow(join1)
print("reference_data")
join2 <- left_join(join1, reference_data)
nrow(join2)
join3 <- left_join(join2, API_biota_common_names)
nrow(join3)
join4 <- left_join(join3, campaign_data, by = "CAMPAIGN_NAME_SHORT")
nrow(join4)
join5 <- left_join(join4, parameters_data)
nrow(join5)
join6 <- left_join(join5, methods_data)
nrow(join6)


## Campaign
campaign_data |>
  group_by(CAMPAIGN_NAME_SHORT) |>
  reframe(n = n()) |>
  arrange(desc(n))
# vm is in here twice... why?

tar_read(vm_edata_campaign) |>
  group_by(CAMPAIGN_NAME_SHORT) |>
  reframe(n = n()) |>
  arrange(desc(n))

tar_read(campaign_data) |> filter(str_detect(CAMPAIGN_NAME_SHORT, "Vm"))
