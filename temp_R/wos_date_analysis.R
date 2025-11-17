wos <- read_excel("data/raw/2025.06.05 - copper search - WoS.xls")

wos_count <- wos |> count(`Publication Year`, sort = FALSE)

wos_count |>
  ggplot(mapping = aes(x = `Publication Year`, y = n)) +
  geom_col()

wos_before_after <- wos |> count(`Publication Year` > 2015)
# 171/315 (54%) of papers were published after 2015
