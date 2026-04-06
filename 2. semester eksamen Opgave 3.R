# ---- Library ----
library(tidyverse)
library(RMariaDB)
library(scales)
library(rpart)
library(rpart.plot)
library(gbm)
library(purrr)
library(pROC)

# ---- Queries ----
con <- dbConnect(MariaDB(),
                 host = "www.talmedos.com",
                 user = "dalremote",
                 password = "OttoRehagel123456789Long2026!",
                 dbname = "superliga2",
                 port = 3306
)
secondaryquery <- "select st.EVENT_WYID,
		st.SECONDARYTYPE1,
        st.SECONDARYTYPE2,
        st.SECONDARYTYPE3,
        st.SECONDARYTYPE4,
        st.SECONDARYTYPE5,
        st.SECONDARYTYPE6,
        st.SECONDARYTYPE7,
        st.SECONDARYTYPE8
 from wyscout_matchevents_secondarytype st"
secondary <- dbGetQuery(con,secondaryquery)

secondaryqueryshot <- "select st.EVENT_WYID,
                      st.PRIMARYTYPE
from wyscout_matchevents_secondarytype st"

secondaryshot <- dbGetQuery(con,secondaryqueryshot)

querycom <- "select c.SEASON_WYID,
                    c.EVENT_WYID,
                    c.PLAYER_WYID,
                    c.RELATEDEVENT_WYID,
                    c.TEAM_WYID,
                    c.MINUTE,
                    c.MATCHPERIOD,
                    c.LOCATIONX,
                    c.LOCATIONY,
                    c.MATCH_WYID,
                    c.PRIMARYTYPE
from wyscout_matchevents_common c where SEASON_WYID in (191611,189918)
"
common <- dbGetQuery(con, querycom)

querypas <- "select pa.MATCH_WYID,
                    pa.EVENT_WYID,
                    pa.PRIMARYTYPE,
                    pa.ACCURATE,
                    pa.ANGLE,
                    pa.HEIGHT,
                    pa.LENGTH,
                    pa.RECIPIENT_WYID,
                    pa.ENDLOCATIONX,
                    pa.ENDLOCATIONY
from wyscout_matchevents_passes pa 
"
passes <- dbGetQuery(con,querypas)

queryshots <- "Select * from wyscout_matchevents_shots s
join wyscout_matches e on e.MATCH_WYID = s.MATCH_WYID
where e.SEASON_WYID in (189918,191611)
"
shots <- dbGetQuery(con,queryshots)

queryteams <- "select t.TEAMNAME,
                      t.TEAM_WYID
from wyscout_teams t
"
teams <- dbGetQuery(con,queryteams)

queryplayers <- "select concat(p.firstname, ' ', p.lastname) playername,
                        p.PLAYER_WYID,
                        p.ROLENAME,
                        p.CURRENTTEAM_WYID
from wyscout_players p
"
players <- dbGetQuery(con,queryplayers)

queryform <- "select * from wyscout_matchformations
"
formation <- dbGetQuery(con,queryform)

querymatches <- "select matches.MATCH_WYID,
matches.MATCHLABEL
from wyscout_matches matches
"
matches <- dbGetQuery(con,querymatches)

advancedmatchstatsquery <- "select mas.MATCH_WYID,
mas.TEAM_WYID,
mas.REDCARDS,
mas.YELLOWCARDS,
mas.FOULS,
mas.GOALS
from WYSCOUT_MATCHADVANCEDSTATS_GENERAL mas" 

advancedmatchstats <- dbGetQuery(con, advancedmatchstatsquery)
 # ---- Joins ----
xgdf <- readRDS("C:/Users/caspe/Downloads/xgdf.rds")
# ----- pass -----
databasepasstest <- passes %>%
  inner_join(common, by = "EVENT_WYID")

databasepasstest <- databasepasstest %>% 
  inner_join(teams, by = c("TEAM_WYID" = "TEAM_WYID"), relationship = "many-to-many")

databasepasstest <- databasepasstest %>% 
  inner_join(players, by = c("PLAYER_WYID" = "PLAYER_WYID"), relationship = "many-to-many")

databasepasstest <- databasepasstest %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

databasepasstest <- databasepasstest %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

databasepasstest <- databasepasstest %>% 
  inner_join(secondary, by = c("EVENT_WYID" = "EVENT_WYID"), relationship = "many-to-many")
# ---- shots -----
tester1 <- common %>% 
  inner_join(secondaryshot, by = c("EVENT_WYID" = "EVENT_WYID"))

tester1 <-  tester1 %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

tester1 <- tester1 %>%
  rename(Relatedevent = PRIMARYTYPE.x)
shots <- shots[, !duplicated(names(shots))]

tester2 <- shots %>% 
  left_join(common, by = c("EVENT_WYID","SEASON_WYID"), relationship = "many-to-many")
tester2 <- tester2 %>% filter(PRIMARYTYPE.x == "shot")


tester1 <- tester2 %>% 
  inner_join(tester1, by = c("EVENT_WYID" = "RELATEDEVENT_WYID"), relationship = "many-to-many")


advancedmatchstats_unique <- advancedmatchstats %>%
  distinct(MATCH_WYID, .keep_all = TRUE)

inner_join(advancedmatchstats_unique, advancedmatchstats_unique, by = "MATCH_WYID")

secondary <- secondary %>%
  mutate(yellow_card = ifelse(
    grepl("yellow_card", SECONDARYTYPE1) |
      grepl("yellow_card", SECONDARYTYPE2) |
      grepl("yellow_card", SECONDARYTYPE3),
    1,
    0
  ))

secondary <- secondary %>%
  mutate(red_card = ifelse(
    grepl("red_card", SECONDARYTYPE1) |
      grepl("red_card", SECONDARYTYPE2) |
      grepl("red_card", SECONDARYTYPE3),
    1,
    0
  ))

kort <- common %>%
  left_join(secondary, by ="EVENT_WYID")

kort <- kort %>% filter(SEASON_WYID == "191611")

teams <- teams %>% 
  distinct(TEAM_WYID, .keep_all = TRUE)

kort <- kort %>% 
  left_join(teams, by = "TEAM_WYID")

kort <- kort %>%
  select(-ends_with(".y"))
names(kort) <- make.unique(sub("\\.x$", "", names(kort)))

kort <-  kort %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

tester1 <- tester1 %>%
  select(-ends_with(".y"))

names(tester1) <- make.unique(sub("\\.x$", "", names(tester1)))

databasepasstest <- databasepasstest %>%
  select(-ends_with(".y"))
names(databasepasstest) <- make.unique(sub("\\.x$", "", names(databasepasstest)))


holdnavn <- teams %>%
  distinct(TEAM_WYID, .keep_all = TRUE)

holdnavn <- common %>%
  left_join(holdnavn, by ="TEAM_WYID")

holdnavn <- holdnavn %>% filter(SEASON_WYID == "191611")

tester1 <- tester1 %>% left_join(xgdf, by = "EVENT_WYID")

tester1 <- tester1 %>%
  mutate(predXG.x = replace_na(predXG, 0))

# ---- cleaning ----
table(tester1$Relatedevent)
tester1 <- tester1 %>%
  filter(!(Relatedevent == "duel" & TEAM_WYID != TEAM_WYID))
tester1 <- tester1 %>% 
  filter(!(Relatedevent == "shot_against"))
tester1 <- tester1 %>% 
  filter(!(Relatedevent == "goalkeeper_exit"))
tester1 <- tester1 %>% 
  filter(!(Relatedevent == "goalkeeper_kick"))
tester1 <- tester1 %>% 
  filter(!(Relatedevent == "clearance"))
tester1 <- tester1 %>% 
  filter(!(Relatedevent == "goal_kick"))
tester1 <- tester1 %>% 
  filter(!(Relatedevent == "touch"))
tester1 <-  tester1 %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

tester1 %>%
  count(EVENT_WYID) %>%
  filter(n > 1)

recplayers <- players
colnames(recplayers) <- c("playername", "RECIPIENT_WYID","ROLENAME", "RECTEAM_WYID")

databasepasstest <- databasepasstest %>% 
  left_join(recplayers %>% select(RECIPIENT_WYID, RECTEAM_WYID), by = "RECIPIENT_WYID")

databasepasstest <- databasepasstest %>% 
  distinct(EVENT_WYID, .keep_all = TRUE)
databasepasstest <- databasepasstest %>%
  select(-ends_with(".y"))
names(databasepasstest) <- make.unique(sub("\\.x$", "", names(databasepasstest)))

databasepasstest <- databasepasstest %>% filter(PRIMARYTYPE == "pass")
databasepasstest$ACCURATE <- ifelse(
  !is.na(databasepasstest$TEAM_WYID) & 
    !is.na(databasepasstest$RECTEAM_WYID) &
    databasepasstest$TEAM_WYID == databasepasstest$RECTEAM_WYID,
  1, 
  0
)

tester1 <- tester1 %>% filter(SEASON_WYID == "191611")

tester1 <- tester1 %>% 
  inner_join(teams, by = c("TEAM_WYID" = "TEAM_WYID"), relationship = "many-to-many")

tester1 <-  tester1 %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

tester1 <- tester1 %>% 
  inner_join(players,  by = c("PLAYER_WYID" = "PLAYER_WYID"), relationship = "many-to-many")

tester1 <-  tester1 %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

tester1 <- tester1 %>% 
  left_join(matches, by = c("MATCH_WYID" = "MATCH_WYID"))
tester1 <- tester1 %>% filter(SEASON_WYID == 191611)

tester1 <- tester1 %>%
  mutate(goal_flag = ifelse(
    grepl("goal", SHOTBODYPART, ignore.case = TRUE) | SHOTISGOAL == 1,
    1,
    0
  ))


names(tester1) <- make.unique(sub("\\.x$", "", names(tester1)))

tester1 <- tester1 %>%
  select(-ends_with(".y")) %>%

  rename_with(~ gsub("\\.x$", "", .x), ends_with(".x"))

# ---- Clustering afleveringer ----
passesclustertest1 <- databasepasstest %>% 
  filter(SEASON_WYID == "191611", PRIMARYTYPE == "pass") %>% 
  mutate(
    progressive = ifelse(ANGLE > 0, 1, 0),
    key_pass = ifelse(if_any(starts_with("SECONDARYTYPE"), ~ . == "key_pass"), 1, 0),
    into_box = ifelse(ENDLOCATIONX > 83 & ENDLOCATIONY > 21 & ENDLOCATIONY < 79, 1, 0),
    high_pass = ifelse(HEIGHT == "high", 1, 0),
    accurate = ifelse(TEAM_WYID == RECTEAM_WYID, 1, 0)
  ) %>% 
  select(
    MATCH_WYID,
    TEAM_WYID,
    TEAMNAME, 
    playername,
    ROLENAME, 
    LENGTH,
    ANGLE,
    LOCATIONX,
    LOCATIONY,
    ENDLOCATIONX,
    ENDLOCATIONY,
    progressive,
    key_pass,
    into_box,
    high_pass,
    accurate
  ) %>% 
  drop_na()

passesclustertest1 <- passesclustertest1 %>% 
  drop_na()

afleveringer_cluster_data <- passesclustertest1 %>% 
  select(-MATCH_WYID, -TEAM_WYID, -TEAMNAME, - playername, -ROLENAME)

#scaler
afleveringer_cluster_scaled <- scale(afleveringer_cluster_data)

#ELBOW
afleveringer_wss <- sapply(1:10, function(k){
  kmeans(afleveringer_cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

#Kontrol plot
plot(1:10, afleveringer_wss, type = "b")

#KMEANS
set.seed(123)
kmeans_pass <- kmeans(afleveringer_cluster_scaled, centers = 4, nstart = 25)

#tilføjer clusters til df
passesclustertest1$cluster <- kmeans_pass$cluster

# FORTOLKNING AF CLUSTERS
passesclustertest1 %>% 
  group_by(cluster) %>% 
  summarise(across(c(LENGTH, progressive, key_pass, into_box, high_pass, accurate), mean))

#PCA OG PLOTTING

afleveringer_pca <- prcomp(afleveringer_cluster_scaled)

afleveringer_pca_df <- as.data.frame(afleveringer_pca$x)
afleveringer_pca_df$cluster <- factor(kmeans_pass$cluster)

passesclustertest1 <- passesclustertest1 %>% 
  mutate(cluster_name = case_when(
    cluster == 1 ~ "Etablernede spil",
    cluster == 2 ~ "Chanceskabende afleveringer",
    cluster == 3 ~ "Høje og lange afleveringer",
    cluster == 4 ~ "Bagudgående kontrol afleveringer"
  ))
afleveringer_pca_df$cluster_name <- passesclustertest1$cluster_name
afleveringer_pca_df$TEAMNAME <- passesclustertest1$TEAMNAME


ggplot(afleveringer_pca_df, aes(x = PC1, y = PC2, color = cluster_name)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Defensiv zone → Offensiv zone", 
    y = "Retningsbaseret progression",
    color = "Afleveringstype",
    title = "Clustering viser en klar opdeling mellem defensive, progressive og offensive afleveringer"
  )

# Vægtning
afleveringer_pca$rotation

# ---- Clustering kampe ----
#laver variabler
tester1 <- tester1 %>%
  mutate(SHOTONTARGET = ifelse(grepl("^tru", SHOTGOALZONE), 1, 0))

Kamplængde <- databasepasstest %>%
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>%
  filter(SEASON_WYID == "191611") %>% 
  summarise(max_minute = max(MINUTE, na.rm = TRUE), .groups = "drop")


Skudprkamp <- shots_per_match <- tester1 %>%
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>%
  summarise(
    shots = n(),.groups = "drop"
  )

Skudpåmålkamp <- tester1 %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  filter(SEASON_WYID == "191611") %>% 
  summarise(SHOTONTARGET =
              sum(SHOTONTARGET))

xGprkamp <- tester1 %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(xG = sum(predXG, na.rm = TRUE))

xGdiff <- tester1 %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(xG = sum(predXG, na.rm = TRUE), .groups = "drop") %>% 
  group_by(MATCH_WYID) %>% 
  mutate(xGdiff = max(xG) - min(xG)) %>% 
  ungroup()


possession_diff <- databasepasstest %>% 
  filter(PRIMARYTYPE == "pass", SEASON_WYID == "191611") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(passes = n(), .groups = "drop") %>% 
  group_by(MATCH_WYID) %>% 
  mutate(
    possession_diff = (max(passes) - min(passes)) / sum(passes) * 100
  ) %>% 
  ungroup()

skuddiff <- tester1 %>% 
  filter(SEASON_WYID == "191611") %>%
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>%
  summarise(shots = n(), .groups = "drop") %>% 
  group_by(MATCH_WYID) %>% 
  mutate(
    skuddiff = (max(shots) - min(shots)) / sum(shots) * 100
  ) %>% 
  ungroup()

progressiveafleveringer <- databasepasstest %>%
  filter(SEASON_WYID == "191611") %>% 
  filter(ANGLE > 0) %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>%
  summarise(progressive = n(), .groups = "drop")

langeafleveringer <- databasepasstest %>%
  filter(SEASON_WYID == "191611") %>% 
  filter(if_any(starts_with("secondarytype"), ~ . == "long_pass")) %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(long_passes = n(), .groups = "drop") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(long_passes = sum(long_passes), .groups = "drop")

indlæg <- databasepasstest %>%
  filter(SEASON_WYID == "191611") %>% 
  filter(if_any(starts_with("secondarytype"), ~ . == "cross")) %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(crosspass = n(), .groups = "drop") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(crosspass = sum(crosspass), .groups = "drop")

succesfuldeafleveringer <- databasepasstest %>% 
  filter(SEASON_WYID == "191611") %>% 
  filter(PRIMARYTYPE == "pass") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(
    total_passes = n(),
    successful_passes = sum(ACCURATE, na.rm = TRUE),
    pass_accuracy = successful_passes / total_passes * 100,
    .groups = "drop"
  )
eventsprkamp <- holdnavn %>% 
  filter(SEASON_WYID == "191611") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(events = n(), .groups = "drop")

afleveringerprkamp <- databasepasstest %>% 
  filter(SEASON_WYID == "191611") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(afleveringer = n(), .groups = "drop")

infractionsprkamp <- holdnavn %>% 
  filter(SEASON_WYID == "191611") %>% 
  filter(PRIMARYTYPE == "infraction") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(infraction = n(), .groups = "drop")

duellerprkamp <- holdnavn %>% 
  filter(SEASON_WYID == "191611") %>% 
  filter(PRIMARYTYPE == "duel") %>% 
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>% 
  summarise(duels = n(), .groups = "drop")

gulekort <- kort %>%
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>%
  filter(yellow_card >= 0) %>% 
  summarise(sum(yellow_card), .groups = "drop")

rødekort <- kort %>%
  group_by(MATCH_WYID, TEAM_WYID, TEAMNAME) %>%
  filter(red_card >= 0) %>% 
  summarise(sum(red_card), .groups = "drop")
# Putter variabler i liste
kampclusterliste <- list(
  Kamplængde,
  Skudprkamp,
  Skudpåmålkamp,
  xGprkamp,
  xGdiff,
  possession_diff,
  skuddiff,
  progressiveafleveringer,
  langeafleveringer,
  indlæg,
  succesfuldeafleveringer,
  eventsprkamp,
  afleveringerprkamp,
  infractionsprkamp,
  duellerprkamp,
  gulekort,
  rødekort
)
#fjerner ekstra variabler som bruges til shiny
  kampcluster <- reduce(kampclusterliste, left_join, 
                        by = c("MATCH_WYID", "TEAM_WYID", "TEAMNAME"))
#laver alt om til pr. 90 min  
kampcluster <- kampcluster %>%
    mutate(across(c("shots.x", "SHOTONTARGET", "xG.x", "xGdiff", 
                    "possession_diff", "skuddiff","progressive",
                    "long_passes", "crosspass",
                    "successful_passes", "pass_accuracy", "events",
                    "afleveringer", "infraction", "duels",
                    "sum(yellow_card)", "sum(red_card)"),
                  ~ .x * 90 / max_minute))

cluster_data <- kampcluster %>% 
  select(-MATCH_WYID, -TEAM_WYID, -TEAMNAME, -xG.y, -passes, -shots.y, -total_passes)

cluster_scaled <- scale(cluster_data)

# Elbow
wss <- sapply(1:10, function(k){
  kmeans(cluster_scaled, centers = k, nstart = 25)$tot.withinss
})

# Kotrol plot
plot(1:10, wss, type = "b")

set.seed(123)
kmeans_result <- kmeans(cluster_scaled, centers = 3, nstart = 25)

kampcluster$cluster <- kmeans_result$cluster

pca <- prcomp(cluster_scaled)

pca_df <- as.data.frame(pca$x)
pca_df$cluster <- factor(kmeans_result$cluster)

pca_df <- pca_df %>% 
  mutate(Kamptype = case_when(
    cluster == 1 ~ "Effektive angrebskampe",
    cluster == 2 ~ "Højintense kampe",
    cluster == 3 ~ "Kaotiske kampe med lav effektivitet",
    cluster == 4 ~ "Lavintense og chancefattige kampe"
  ))

pca_df$TEAMNAME <- kampcluster$TEAMNAME
pca_df$MATCH_WYID <- kampcluster$MATCH_WYID

#Plot
ggplot(pca_df, aes(x = PC1, y = PC2, color = Kamptype)) +
  geom_point(size = 3) +
  theme_minimal()+
  labs(x = "Aktivitet / tempo / volumen", y = "Kvalitet / intensitet",
       title = "Kampe i Superligaen spænder fra lavintense og chancefattige til højintense og effektive")

# Vægtning
pca$rotation