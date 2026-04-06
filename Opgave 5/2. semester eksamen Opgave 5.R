library(tidyverse)
#load vedhæftede filer "maleevents.rds" og "femaleevents.rds"
#laver x og y start og endlokationer for events
femaleevents <- femaleevents %>%
  mutate(
    x = as.numeric(str_extract(location, "(?<=c\\()[^,]+")),
    y = as.numeric(str_extract(location, "(?<=,\\s)[^\\)]+")),
    xend = as.numeric(str_extract(pass.end_location, "(?<=c\\()[^,]+")),
    yend = as.numeric(str_extract(pass.end_location, "(?<=,\\s)[^\\)]+"))
  )

maleevents <- maleevents %>%
  mutate(
    x = as.numeric(str_extract(location, "(?<=c\\()[^,]+")),
    y = as.numeric(str_extract(location, "(?<=,\\s)[^\\)]+")),
    xend = as.numeric(str_extract(pass.end_location, "(?<=c\\()[^,]+")),
    yend = as.numeric(str_extract(pass.end_location, "(?<=,\\s)[^\\)]+"))
  )
#laver skud og afleveringer i seperate dataframes
maleshots <- maleevents %>% filter(type.name=="Shot")

femaleshots <- femaleevents %>% filter(type.name=="Shot")

malepasses <- maleevents %>% filter(type.name=="Pass")

femalepasses <- femaleevents %>% filter(type.name=="Pass")

table(malepasses$pass.height.name)
table(femalepasses$pass.height.name)

#laver en dataframe med procentmæssig fordeling af heightname
male_prop <- prop.table(table(malepasses$pass.height.name))
female_prop <- prop.table(table(femalepasses$pass.height.name))

df_plot <- data.frame(
  type = rep(names(male_prop), 2),
  proportion = c(male_prop, female_prop),
  gender = rep(c("Male", "Female"), each = length(male_prop))
)

ggplot(df_plot, aes(x = type, y = proportion, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Størstedelen af alle passes sker som groundpasses, dog kan vi se at mænd laver procentmæssigt flere groundpasses
    kvinder foretager procentmæssigt flere high passes og lowpasses end mænd",
    x = "Pass type",
    y = "Andel"
  ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1, by=0.1)) +
  theme_minimal()

#udregner distance
maleshots <- maleshots %>%
  mutate(
    x_meter = x * 105 / 100,
    y_meter = y * 68 / 100,
    distance_to_goal = sqrt((x_meter - 105)^2 + (y_meter - 34)^2)
  )
femaleshots <- femaleshots %>%
  mutate(
    x_meter = x * 105 / 100,
    y_meter = y * 68 / 100,
    distance_to_goal = sqrt((x_meter - 105)^2 + (y_meter - 34)^2)
  )

#distanceplot
df_plotdist <- bind_rows(
  maleshots %>% mutate(gender = "Male"),
  femaleshots %>% mutate(gender = "Female")
)

ggplot(df_plotdist, aes(x = gender, y = distance_to_goal, fill = gender)) +
  geom_boxplot() +
  labs(title = "Vi kan se at mænd og kvinder gennemsnitsmæssigt tager skud på mål fra samme afstand, 
      mænd har dog nogle højere outliers")+
  theme_minimal()

table(maleshots$shot.outcome.name)
table(femaleshots$shot.outcome.name)

#laver en dataframe med procentmæssig fordeling af shotoutcome
male_propshot <- prop.table(table(maleshots$shot.outcome.name))
female_propshot <- prop.table(table(femaleshots$shot.outcome.name))

df_plotshot <- data.frame(
  type = rep(names(male_propshot), 2),
  proportion = c(male_propshot, female_propshot),
  gender = rep(c("Male", "Female"), each = length(male_propshot))
)
df_plotshot <- df_plotshot %>%
  mutate(type = ifelse(type == "Off T", "Off Target", type))

ggplot(df_plotshot, aes(reorder(x = type,proportion), y = proportion, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Den procentmæssige fordeling af skudresultater ligner hinanden meget.
    Vi kan dog se at kvinder skyder lidt flere skud forbi målet og mænd scorer flere mål",
    x = "Shot type",
    y = "Andel"
  ) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1, by=0.05)) +
  theme_minimal()
#passesheatmap
library(ggsoccer)
ggplot(malepasses, aes(x = x, y = y)) +
  annotate_pitch() +
  geom_density_2d_filled(alpha = 0.7) +
  labs(title = "Mænd")+
  theme_pitch()

ggplot(femalepasses, aes(x = x, y = y)) +
  annotate_pitch() +
  geom_density_2d_filled(alpha = 0.7) +
  labs(title = "Kvinder")+
  theme_pitch()

#shotheatmap
ggplot(maleshots, aes(x = x, y = y)) +
  annotate_pitch() +
  geom_density_2d_filled(alpha = 0.7) +
  labs(title = "Mænd")+
  theme_pitch()

ggplot(femaleshots, aes(x = x, y = y)) +
  annotate_pitch() +
  geom_density_2d_filled(alpha = 0.7) +
  labs(title = "Kvinder")+
  theme_pitch()

mean(malepasses$duration, na.rm = TRUE)
mean(femalepasses$duration, na.rm = TRUE)
mean(maleshots$duration, na.rm = TRUE)
mean(femaleshots$duration, na.rm = TRUE)
mean(maleevents$duration, na.rm = TRUE)
mean(femaleevents$duration, na.rm = TRUE)
df_plotduration <- data.frame(
  category = c("Passes", "Passes", "Shots", "Shots", "All events", "All events"),
  gender = c("Male", "Female", "Male", "Female", "Male", "Female"),
  mean_duration = c(
    1.519547, 1.626475,
    0.6526269, 0.77337,
    1.266812, 1.218796
  )
)
ggplot(df_plotduration, aes(x = category, y = mean_duration, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(mean_duration,2)),vjust = -0.5), position = position_dodge(width = 0.9))+
  labs(
    title = "Når det kommer til skud og afleveringer, så er mænd lidt hurtigere til at tage beslutningen,
    men på tværs af alle events det meget lige, med kvinderne der er en smule hurtigere",
    x = "Handlingstype",
    y = "Gennemsnitlig varighed af event (sekunder)"
  ) +
  theme_minimal()
mean(maleevents$minute)
mean(femaleevents$minute)


malepasses <- malepasses %>%
  mutate(pass_length = sqrt((xend - x)^2 + (yend - y)^2))
femalepasses <- femalepasses %>%
  mutate(pass_length = sqrt((xend - x)^2 + (yend - y)^2))
mean(femalepasses$pass.length)
mean(malepasses$pass.length)

table(femaleevents$type.name)
male_prop <- sum(maleevents$type.name == "Foul Committed", na.rm = TRUE) / nrow(maleevents)
female_prop <- sum(femaleevents$type.name == "Foul Committed", na.rm = TRUE) / nrow(femaleevents)
df_plotfoul <- data.frame(
  gender = c("Male", "Female"),
  fouls = c(male_prop, female_prop),
  n = c(sum(maleevents$type.name == "Foul Committed", na.rm = TRUE), sum(femaleevents$type.name == "Foul Committed", na.rm = TRUE))
)

ggplot(df_plotfoul, aes(x = gender, y = fouls, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5)+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Mænd foretager flere fouls end kvinder både totalt og også som andel af totale events",
    x = "Køn",
    y = "Andel"
  ) +
  theme_minimal()

table(femaleevents$type.name)
dftype <- bind_rows(
  maleevents %>% mutate(gender = "Male"),
  femaleevents %>% mutate(gender = "Female")
)
dftype <- dftype %>%
  filter(type.name %in% c("Duel", "Pass", "Shot"))

ggplot(dftype%>% filter(type.name=="Duel"), aes(x = type.name, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Der sker flere dueller hos kvinder end hos mænd",
    x = "Event type",
    y = "Antal"
  ) +
  theme_minimal()

ggplot(dftype%>% filter(type.name=="Pass"), aes(x = type.name, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Mænd foretager flere afleveringer end kvinder",
    x = "Event type",
    y = "Antal"
  ) +
  theme_minimal()

ggplot(dftype%>% filter(type.name=="Shot"), aes(x = type.name, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Kvinder foretager flere skud end mænd",
    x = "Event type",
    y = "Antal"
  ) +
  theme_minimal()

df_duration <- dftype %>%
  filter(!is.na(duration)) %>%
  group_by(gender, type.name) %>%
  summarise(mean_duration = mean(duration), .groups = "drop")

ggplot(df_duration %>% filter(type.name=="Shot"), aes(x = type.name, y = mean_duration, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(mean_duration,2)),vjust = -0.5), position = position_dodge(width = 0.9))+
  labs(
    title = "Den gennemsnitlige tid det tager fra spilleren modtager bolden
    til at de skyder til bolden er længere hos kvinder",
    x = "Event type",
    y = "Sekunder"
  ) +
  theme_minimal()
table(maleevents$type.name)
table(femaleevents$type.name)

dfpass <- bind_rows(
  malepasses %>% mutate(gender = "Male"),
  femalepasses %>% mutate(gender = "Female")
)
df_summary <- dfpass %>% filter(!is.na(pass.length)) %>% group_by(gender) %>% summarise(mean_length = mean(pass.length), .groups = "drop")
            
ggplot(df_summary, aes(x = gender, y = mean_length, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(mean_length)),vjust = -0.5))+
  labs(
    title = "Gennemsnitslængden på skud blandt mænd og kvinder ligger begge på 21",
    x = "Køn",
    y = "Længde"
  ) +
  theme_minimal()                                                                                
