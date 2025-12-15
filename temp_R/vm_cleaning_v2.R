library(data.table)
library(readxl)
library(summarytools)
library(datapasta)
library(glue)

# Downloaded 2025.12.05, data for 2025.01.01 - 2025.12.05, all kommune, all media, all campaigns, etc.
# Search for Kobber & Kobberpyrition
# 138615 hits
if (!exists("vm_copper_2025")) {
  # let's not run this more often than we have to, it's very slow
  vm_copper_2025 <- readxl::read_excel(
    path = "data/raw/vannmiljo/Vm_Copper_2025.12.05.xlsx",
    sheet = 1,
    guess_max = 138615 # avoid warnings (because read_excel will automatically convert columns to logical if they have a lot of NAs)
  )
}

# little bit of data inspection
dfSummary(vm_copper_2025) |> view() # where have you been my whole life.

# What do we do with the columns?
colnames <- names(vm_copper_2025) |> as_tibble_col("Vm_Variable")
# This works - returns the tribble code as a string
tribble_code <- tribble_construct(colnames)

## Helper Function

# Function to convert tibble to tribble code ----
to_tribble <- function(df) {
  # Header line with column names
  header <- glue("  ~{names(df)}", .sep = ", ")

  # Format each value (add quotes for characters)
  format_val <- function(x) {
    if (is.character(x)) {
      glue('"{x}"')
    } else if (is.na(x)) {
      "NA"
    } else {
      as.character(x)
    }
  }

  # Build each row
  rows <- apply(df, 1, function(row) {
    vals <- sapply(row, format_val)
    glue("  {vals}", .sep = ", ")
  })

  # Combine everything
  glue(
    "tribble(
{header},
{paste(rows, collapse = ',\n')}
)"
  )
}

## ------------------------

to_tribble(colnames)


# ---
# Medium ID
# We can guess lots - but not all - columns from here

vm_copper_mediums <- vm_copper_2025 |>
  left_join(vm_medium_id_lookup, by = c(Medium_id = "MediumID"))
