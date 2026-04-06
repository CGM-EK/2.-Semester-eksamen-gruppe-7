library(tidyverse)
library(ggsoccer)
library(deldir)
library(jsonlite)
#load vbobfiler
data <- fromJSON("~/vbob-meta.json", flatten = T)
#hiver navne ud fra spillere
players_list <- list(
  home = data[["homePlayers"]],
  away = data[["awayPlayers"]]
)
#binder de to sammen
players_df <- bind_rows(
  data$homePlayers %>% mutate(team = "home"),
  data$awayPlayers %>% mutate(team = "away")
)
#definerer tidspunkt for vores dataframe ca 24min 30 sek
sek10df <- vbob[36430:36730,]
sek10dfp <- sek10df[,3:70]
#laver vektor for navne til vores kolonner
names <- c("player1_id",
           "player2_id",
           "player3_id",
           "player4_id",
           "player5_id",
           "player6_id",
           "player7_id",
           "player8_id",
           "player9_id",
           "player10_id",
           "player11_id",
            "player1_x","player1_y",
            "player2_x","player2_y",
            "player3_x","player3_y",
            "player4_x","player4_y",
            "player5_x","player5_y",
            "player6_x","player6_y",
            "player7_x","player7_y",
            "player8_x","player8_y",
            "player9_x","player9_y",
            "player10_x","player10_y",
            "player11_x","player11_y",
            "player12_id",
            "player13_id",
            "player14_id",
            "player15_id",
            "player16_id",
            "player17_id",
            "player18_id",
            "player19_id",
            "player20_id",
            "player21_id",
            "player22_id",
            "player12_x","player12_y",
            "player13_x","player13_y",
            "player14_x","player14_y",
            "player15_x","player15_y",
            "player16_x","player16_y",
            "player17_x","player17_y",
            "player18_x","player18_y",
            "player19_x","player19_y",
            "player20_x","player20_y",
            "player21_x","player21_y",
            "player22_x","player22_y",
            "ball_x", "ball_y"
)
colnames(sek10dfp) <- names
colnames(sek10dfp) <- gsub("([0-9]+)([xy])", "\\1_\\2", colnames(sek10dfp))
colnames(sek10dfp)
id_to_name <- setNames(players_df$name, players_df$ssiId)
df_ids_named <- sek10dfp

for(i in 1:22){
  col <- paste0("player", i, "_id")
  df_ids_named[[col]] <- id_to_name[sek10dfp[[col]]]
}

df_long <- df_ids_named %>%
  pivot_longer(
    cols = everything(),
    names_to = c("player", ".value"),
    names_pattern = "(player\\d+|ball)_(id|x|y)"
  )

dfsubset <- df_long[1:23,]
dfsubset$x_scaled <- (dfsubset$x+50)/100*105
dfsubset$y_scaled <- (dfsubset$y+50)/100*68
ggplot(dfsubset, aes(x = x_scaled, y = y_scaled)) +
  annotate_pitch()+
  geom_text(aes(label = player))+
  geom_point(size = 3, color = "red", position = position_jitter(width = 0.5, height = 0.5)) +
  theme_pitch()

max(df_long$y)
# Definerer banestørrelse (meter)

df_long <- df_long %>%
  mutate(
    team = case_when(
      player == "ball" ~ "Ball",
      as.numeric(gsub("player", "", player)) <= 11 ~ "Odense Boldklub",
      TRUE ~ "Vejle Boldklub"
    )
  )
#scaler vores data til at passe til en 105x68 fodboldbane
df_long$x_scaled <- (df_long$x+50)/100*105+2
df_long$y_scaled <- (df_long$y+50)/100*68+11

tri <- deldir(df_long$x_scaled[1:22], df_long$y_scaled[1:22])
edges <- tri$dirsgs

#
ggplot() +
  annotate_pitch(fill = "darkgreen", colour = "white") +
  geom_segment(
    data = edges,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = "black",
    size = 0.5
  ) +
  
  # spillere
  geom_point(
    data = df_long[1:23,],
    aes(x = x_scaled, y = y_scaled, colour = team),
    size = 3
  ) +
  theme_pitch()

df_long$frame <- rep(1:(nrow(df_long)/23), each = 23)
#deluaney triangluering
edges_all <- df_long %>%
  filter(player != "ball") %>%  # bold skal ikke med i triangulation
  group_by(frame) %>%
  group_modify(~{
    tri <- deldir(.x$x_scaled, .x$y_scaled)
    as_tibble(tri$dirsgs)
  }) %>%
  ungroup()

voronoi_area <- df_long %>%
  filter(player != "ball") %>%
  group_by(frame) %>%
  group_modify(~{
    
    tri <- deldir(.x$x_scaled, .x$y_scaled)
    
    tiles <- tile.list(tri)
    
    tibble(
      player = .x$player,
      team = .x$team,
      area = sapply(tiles, function(tile){
        # polygon area (shoelace formula)
        x <- tile$x
        y <- tile$y
        abs(sum(x * dplyr::lead(y, default = y[1]) -
                  y * dplyr::lead(x, default = x[1]))) / 2
      })
    )
    
  }) %>%
  ungroup()

avg_space <- voronoi_area %>%
  group_by(player, team) %>%
  summarise(mean_area = mean(area, na.rm = TRUE)) %>%
  arrange(desc(mean_area))
#
df_long_filtered <- df_long %>%
  filter(
    player != "ball",
    (team == "Odense Boldklub" & x_scaled > 52.5) |
      (team == "Vejle Boldklub" & x_scaled < 52.5)
  )

#udregner område omkring spillerne
voronoi_area <- df_long_filtered %>%
  filter(player != "ball") %>%
  group_by(frame) %>%
  group_modify(~{
    
    tri <- deldir(.x$x_scaled, .x$y_scaled, rw = c(0,105,0,68))
    tiles <- tile.list(tri)
    
    tibble(
      player = .x$id[seq_along(tiles)],
      team   = .x$team[seq_along(tiles)],
      x      = .x$x_scaled[seq_along(tiles)],
      area = sapply(tiles, function(tile){
        x <- tile$x
        y <- tile$y
        abs(sum(x * dplyr::lead(y, default = y[1]) -
                  y * dplyr::lead(x, default = x[1]))) / 2
      })
    )
    
  }) %>%
  ungroup()
avg_space <- voronoi_area %>%
  group_by(player, team) %>%
  summarise(mean_area = mean(area, na.rm = TRUE)) %>%
  arrange(desc(mean_area))

avg_space %>%
  arrange(mean_area) %>%
  ggplot(aes(x = reorder(player, mean_area), y = mean_area, fill = team)) +
  geom_col() +
  geom_text(aes(label = round(mean_area, 0)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    x = "Spiller",
    y = "Gennemsnitlig areal i m²",
    title = "Der ses tydeligt at i de frames op til målet for Vejle Boldklub, at de skete var
    størst frit areal på modstanders banehalvdel for spillere fra Odense Boldklub"
  ) +
  scale_fill_manual(values = c(
    "Odense Boldklub" = "blue",  # OB blå
    "Vejle Boldklub" = "red"    # Vejle rød
  )) +
  theme_minimal()
#
library(gganimate)
p <- ggplot() +
  annotate_pitch(fill = "darkgreen", colour = "white") +
  
  # triangulation
  geom_segment(
    data = edges_all,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = "white",
    linewidth = 0.3
  ) +
  
  # spillere + bold
  geom_point(
    data = df_long,
    aes(x = x_scaled, y = y_scaled, color = team),
    size = 2
  ) +
  geom_text(
    data = df_long,
    aes(x = x_scaled, y = y_scaled, label = id),
    color = "white",
    size = 3,
    vjust = -1
  ) +
  scale_color_manual(values = c(
    "Odense Boldklub" = "blue",
    "Vejle Boldklub" = "red",
    "Ball"   = "green"
  )) +
  theme_pitch() +
  
  transition_time(frame) +
  ease_aes("linear")
triang <- animate(p, fps = 10, width = 800, height = 500)
anim_save("triangulation6.gif", triang)

