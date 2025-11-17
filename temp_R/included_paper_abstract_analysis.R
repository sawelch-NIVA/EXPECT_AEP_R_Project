library(janitor)
library(tidyverse)
library(stringr)
library(lubridate)

data <- read_csv(
  "data/raw/covidence/2025.07.28 - included_paper_subsample.csv"
) |>
  clean_names() |>
  mutate(
    is_review = case_when(
      str_detect(tags, "non-primary source") ~ "yes",
      TRUE ~ "no"
    )
  )
head(data)

data |>
  ggplot(mapping = aes(x = published_year, fill = is_review)) +
  geom_bar() +
  scale_x_continuous(
    breaks = seq(1985, 2025, by = 5),
    minor_breaks = seq(1985, 2025, by = 1),
    name = "Published Year",
    limits = c(1985, 2026)
  ) +
  scale_y_continuous(
    minor_breaks = seq(0, 15, by = 1),
    limits = c(0, 15),
    name = "Number of Papers"
  ) +
  scale_fill_manual(
    labels = c("Original Data", "Review"),
    name = "",
    values = c("skyblue", "tomato4")
  ) +
  geom_vline(xintercept = 2009.5, linetype = "dashed") +
  theme_classic() +
  coord_cartesian(expand = FALSE)

data_boolean <- data |>
  mutate(tags_split = str_split(string = tags, pattern = "; ")) |>
  unnest_wider(col = tags_split, names_sep = "") |>
  pivot_wider(cols = contains("tags_split"), names_from)

data_boolean <- data |>
  mutate((!!!str_split_1(string = "tags", pattern = ",")) = "")
