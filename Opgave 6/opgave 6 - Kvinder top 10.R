library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)

table(competitions$competition_name)


competitions <- fromJSON("data/competitions.json")

all_matches <- map_df(seq_len(nrow(competitions)), function(i) {
  comp <- competitions[i, ]
  path <- file.path("data/matches", comp$competition_id, comp$season_id)
  file <- paste0(path, ".json")
  fromJSON(file)
})

all_events <- map_df(all_matches$match_id, function(id) {
  file <- file.path("data/events", paste0(id, ".json"))
  fromJSON(file, flatten = TRUE)
})


denmark_events <- all_events %>% 
  filter(team.name == "Denmark Women's")


top10_shots <- denmark_events %>% 
  filter(type.name == "Shot") %>% 
  count(player.name, sort = TRUE) %>% 
  mutate(
    pct = n / sum(n) * 100
  ) %>% 
  slice_head(n = 10)

ggplot(top10_shots, aes(x = reorder(player.name, n), y = n)) +
  geom_col(fill = "firebrick") +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Pernille Harder skyder klart mest på landsholdet",
    x = "Spiller",
    y = "Afslutninger"
  ) +
  theme_minimal(base_size = 14) +
  expand_limits(y = max(top10_shots$n) * 1.15)



