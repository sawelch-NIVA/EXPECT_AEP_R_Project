if (!exists("requirements_loaded")) {
  source("temp_R/scratchpad.r")
  load_all_requirements()
}

### Units ---

test_units <- main_table |>
  select(MEASURED_UNIT) |>
  distinct()


# ok, we can't do mixed units in one table
test_units |>
  mutate(
    MEASURED_VALUE_UNIT = set_units(MEASURED_VALUE, !!unit_map[[MEASURED_UNIT]])
  )

# what do our units actually look like?
main_table |> select(MEASURED_UNIT) |> distinct()

# and post-standardisation
test <- standardise_measured_units(main_table) |>
  slice_sample(n = 10) |>
  select(
    SITE_CODE,
    MEASURED_VALUE,
    MEASURED_UNIT,
    MEASURED_VALUE_STANDARD,
    MEASURED_UNIT_STANDARD
  )

# from a sample of 10 it looks fine, although we have lost some precision in places

main_table |>
  group_by(MEASURED_UNIT) |>
  reframe(MEASURED_UNIT, count = count())
