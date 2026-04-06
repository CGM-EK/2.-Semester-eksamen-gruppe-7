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
                    st.PRIMARYTYPE,
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

querycom <- "select c.SEASON_WYID,
                    c.Match_WYID,
                    c.EVENT_WYID,
                    c.PLAYER_WYID,
                    c.RELATEDEVENT_WYID,
                    c.TEAM_WYID,
                    c.MINUTE,
                    c.MATCHPERIOD,
                    c.LOCATIONX,
                    c.LOCATIONY 
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
                    where e.SEASON_WYID in (189918, 191611)
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
players <- players %>% rename(TEAM_WYID=CURRENTTEAM_WYID)

queryform <- "select * from wyscout_matchformations
"
formation <- dbGetQuery(con,queryform)

querymatches <- "select matches.MATCH_WYID,
                    matches.MATCHLABEL
                    from wyscout_matches matches
"
matches <- dbGetQuery(con,querymatches)

# ---- Joins / Udvælg variabler til xGdataframe ----
xgdataframe <- shots[,c(2,3,4,5,19)]
xgdataframe <- xgdataframe %>% 
  left_join(secondary %>% 
              select(EVENT_WYID, 
                     SECONDARYTYPE1,
                     SECONDARYTYPE2,
                     SECONDARYTYPE3), 
            by = "EVENT_WYID") %>% 
  filter(PRIMARYTYPE == "shot"|SEASON_WYID == 189918)

secondarycommon <- common %>% 
  left_join(secondary, by = "EVENT_WYID")

sec.comdf <- secondarycommon %>% 
  filter(PRIMARYTYPE=="corner"|
           PRIMARYTYPE=="free_kick"|
           PRIMARYTYPE=="interception"|
           PRIMARYTYPE=="duel"|
           PRIMARYTYPE=="acceleration"|
           PRIMARYTYPE=="pass")

sec.comdf <- sec.comdf %>%
  rename(Relatedevent = PRIMARYTYPE)

xgdataframe <- xgdataframe %>% 
  left_join(sec.comdf %>% 
              select(RELATEDEVENT_WYID,Relatedevent), 
            by = c("EVENT_WYID"="RELATEDEVENT_WYID"))

xgdataframe <- xgdataframe %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

xgdataframe <- xgdataframe %>%
  filter(!is.na(SECONDARYTYPE1))

xgdataframe <- xgdataframe %>% 
  filter(SECONDARYTYPE1!="touch_in_box")

xgdataframe <- xgdataframe %>%
  mutate(goal_flag = ifelse(
    grepl("goal", SECONDARYTYPE1, ignore.case = TRUE),
    1,
    0
  ))

xgdataframe <- xgdataframe %>% 
  left_join(common %>% 
              select(EVENT_WYID,LOCATIONX,LOCATIONY), 
            by = "EVENT_WYID")

xgdataframe <- xgdataframe %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

xgdataframe <- xgdataframe %>%
  mutate(
    in_box = ifelse(
      LOCATIONX >= 84 &
        LOCATIONY >= 19 &
        LOCATIONY <= 81,
      1, 0
    )
  )

# ---- Udregninger af distance til mål og vinkel på skud ----
#udregner distance til mål
xgdataframe <- xgdataframe %>%
  mutate(
    x_meter = LOCATIONX * 105 / 100,
    y_meter = LOCATIONY * 68 / 100,
    distance_to_goal = sqrt((x_meter - 105)^2 + (y_meter - 34)^2)
  )
#udregner vinkel af skud
goal_x  <- 105
goal_y1 <- 34 - 7.32/2 
goal_y2 <- 34 + 7.32/2 

xgdataframe <- xgdataframe %>%
  mutate(
    x_meter = LOCATIONX * 105 / 100,
    y_meter = LOCATIONY * 68 / 100,
    
    goal_x  = 105,
    goal_y1 = 34 - 7.32/2,
    goal_y2 = 34 + 7.32/2,
    
    a = sqrt((goal_x - x_meter)^2 + (goal_y1 - y_meter)^2),
    b = sqrt((goal_x - x_meter)^2 + (goal_y2 - y_meter)^2),
    c = 7.32,
    
    shot_angle = acos((a^2 + b^2 - c^2) / (2 * a * b)),
    
    shot_angle_deg = shot_angle * 180 / pi
  )
# ---- Laver variabler på spil før skud ----
xgdataframe <- xgdataframe %>% 
  mutate(
    pass = ifelse(Relatedevent=="pass", 1,0)
  )
xgdataframe <- xgdataframe %>% 
  mutate(
    duel = ifelse(Relatedevent=="duel", 1,0)
  )
xgdataframe <- xgdataframe %>% 
  mutate(
    interception = ifelse(Relatedevent=="interception", 1,0)
  )
xgdataframe <- xgdataframe %>% 
  mutate(
    corner = ifelse(Relatedevent=="corner", 1,0)
  )
xgdataframe <- xgdataframe %>% 
  mutate(
    freekick = ifelse(Relatedevent=="free_kick", 1,0)
  )
xgdataframe <- xgdataframe %>% 
  mutate(
    acceleration = ifelse(Relatedevent=="acceleration", 1,0)
  )
xgdataframe <- xgdataframe %>% 
  mutate(
    head = ifelse(SHOTBODYPART=="head_or_other"|SHOTBODYPART=="head_sho", 1,0)
  )

#---- Plots for forskellige variabler----
#andel af skud
andeldf <- xgdataframe %>%
  summarise(across(c(in_box, pass, duel, interception, corner, freekick, acceleration, head),
                   ~ mean(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "variabel",
               values_to = "andel")

ggplot(andeldf, aes(x = reorder(variabel, andel), y = andel, fill = variabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(andel*100,1),"%")), vjust = -0.3)+
  labs(title = "Langt de fleste skud inde i boxen og efter en duel eller et pass",
       x = "Variabler til brug i XG",
       y = "Procentmæssig andel af skud hvor variablen gælder")+
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0,0.7, by = 0.1)
  )+
  theme_minimal()
saveRDS(andeldf, file = "andeldf.rds")
#andel mål
andelmåldf <- xgdataframe[xgdataframe$goal_flag==1,] %>%
  summarise(across(c(in_box, pass, duel, interception, corner, freekick, acceleration, head),
                   ~ mean(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "variabel",
               values_to = "andel")
ggplot(andelmåldf, aes(x = reorder(variabel, andel), y = andel, fill = variabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(andel*100,1),"%")), vjust = -0.3)+
  labs(title = "Langt de fleste skud, som bliver til mål foregår i boksen, efter en duel eller et pass",
       x = "Variabler til brug i XG",
       y = "Procentmæssig andel af mål hvor variablen gælder")+
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0,1, by = 0.1)
  )+
  theme_minimal()

#skud gns, distance fra mål og skud vinkel
numdf <- xgdataframe %>%
  summarise(across(c(shot_angle_deg, distance_to_goal),
                   ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "variabel",
               values_to = "andel")
ggplot(numdf, aes(x = reorder(variabel, andel), y = andel, fill = variabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(andel,1))), vjust = -0.3)+
  labs(title = "Gennemsnittet for vinklen på skud er 27 imens afstanden gennemsnitligt er 16.4",
       x = "Variabler til brug i XG",
       y = "gennemsnitsværdi i grader og meter")+
  scale_y_continuous(
    breaks = seq(0,36, by = 3)
  )+
  theme_minimal()
saveRDS(numdf, file = "numdf.rds")
#mål gns, distance fra mål og skud vinkel
nummåldf <- xgdataframe[xgdataframe$goal_flag==1,] %>%
  summarise(across(c(shot_angle_deg, distance_to_goal),
                   ~ mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "variabel",
               values_to = "andel")
ggplot(nummåldf, aes(x = reorder(variabel, andel), y = andel, fill = variabel)) +
  geom_col() +
  geom_text(aes(label = paste0(round(andel,1))), vjust = -0.3)+
  labs(title = "Gennemsnittet for vinklen på skud der bliver til mål er 38.5 imens afstanden gennemsnitligt er 12.2",
       x = "Variabler til brug i XG",
       y = "gennemsnitsværdi i grader og meter")+
  scale_y_continuous(
    breaks = seq(0,40, by = 3)
  )+
  theme_minimal()

#---- BESLUTNINGSTRÆ KLASSIFIKATION ----
tree_model <- rpart(
  factor(goal_flag) ~ distance_to_goal + 
    shot_angle_deg +
    in_box + 
    head + 
    pass + 
    duel +
    interception +
    corner+
    freekick+
    acceleration,
  data = xgdataframe,
  method = "class",
  control = rpart.control(
    cp = 0.007,
    maxdepth = 5,
    minsplit = 20
  )
)
rpart.plot(tree_model)
printcp(tree_model)
plotcp(tree_model)
best_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree_model, cp = best_cp)
print(tree_model)
summary(tree_model)
rpart.plot(pruned_tree)

#---- Foretager en GLM for at vurdere variablers sammenhængskraft med mål ----
glm1 <- glm(goal_flag~distance_to_goal + 
              shot_angle_deg +
              in_box + 
              head + 
              pass +
              interception +
              corner+
              acceleration+
              duel,
            data = xgdataframe, family = "binomial")

#---- GBM- Booting ----
xgdataframe$shot_angle_deg[is.nan(xgdataframe$shot_angle_deg)] <- NA
gbm1 <- gbm(goal_flag~distance_to_goal +
              shot_angle_deg +
              in_box + 
              head + 
              pass + 
              duel +
              interception +
              corner+
              acceleration,
            data = xgdataframe,
            distribution = "bernoulli",
            n.trees = 499,
            interaction.depth = 3,
            shrinkage = 0.01,
            n.minobsinnode = 20,
            cv.folds = 5)
summary(gbm1)

best_iter <- gbm.perf(gbm1, method = "cv")

model_bundle <- list(
  model = gbm1,
  best_iter = best_iter
)

#----Testdata----
#UDVÆLGER VARIABLER FRA SHOTS TIL DF
testxgdataframe <- shots[,c(2,3,4,5,19)]
testxgdataframe <- testxgdataframe %>% 
  left_join(secondary %>% 
              select(EVENT_WYID, 
                     SECONDARYTYPE1,
                     SECONDARYTYPE2,
                     SECONDARYTYPE3), 
            by = "EVENT_WYID")

testxgdataframe <- testxgdataframe %>% 
  filter(SEASON_WYID==191611) %>% 
  filter(PRIMARYTYPE=="shot")

testsecondarycommon <- common %>% 
  left_join(secondary, by = "EVENT_WYID")

testsec.comdf <- testsecondarycommon %>% 
  filter(PRIMARYTYPE=="corner"|
           PRIMARYTYPE=="free_kick"|
           PRIMARYTYPE=="interception"|
           PRIMARYTYPE=="duel"|
           PRIMARYTYPE=="acceleration"|
           PRIMARYTYPE=="pass") %>% 
  rename(Relatedevent = PRIMARYTYPE)

testxgdataframe <- testxgdataframe %>% 
  left_join(testsec.comdf %>% 
              select(RELATEDEVENT_WYID,Relatedevent), 
            by = c("EVENT_WYID"="RELATEDEVENT_WYID")) %>% 
  distinct(EVENT_WYID, .keep_all = TRUE) %>% 
  filter(!is.na(SECONDARYTYPE1)|SECONDARYTYPE1!="touch_in_box")

testxgdataframe <- testxgdataframe %>%
  mutate(goal_flag = ifelse(
    grepl("goal", SECONDARYTYPE1, ignore.case = TRUE),
    1,
    0
  ))

testxgdataframe <- testxgdataframe %>% 
  left_join(common %>% 
              select(EVENT_WYID,LOCATIONX,LOCATIONY), 
            by = "EVENT_WYID") %>%
  distinct(EVENT_WYID, .keep_all = TRUE)

#udregner om skudet er i boxen
testxgdataframe <- testxgdataframe %>%
  mutate(
    in_box = ifelse(
      LOCATIONX >= 84 &
        LOCATIONY >= 19 &
        LOCATIONY <= 81,
      1, 0
    )
  )
#udregner afstand til målet fra skudet
testxgdataframe <- testxgdataframe %>%
  mutate(
    x_meter = LOCATIONX * 105 / 100,
    y_meter = LOCATIONY * 68 / 100,
    distance_to_goal = sqrt((x_meter - 105)^2 + (y_meter - 34)^2)
  )
#udregner vinkel for skud
goal_x  <- 105
goal_y1 <- 34 - 7.32/2
goal_y2 <- 34 + 7.32/2

testxgdataframe <- testxgdataframe %>%
  mutate(
    x_meter = LOCATIONX * 105 / 100,
    y_meter = LOCATIONY * 68 / 100,
    
    goal_x  = 105,
    goal_y1 = 34 - 7.32/2,
    goal_y2 = 34 + 7.32/2,
    
    a = sqrt((goal_x - x_meter)^2 + (goal_y1 - y_meter)^2),
    b = sqrt((goal_x - x_meter)^2 + (goal_y2 - y_meter)^2),
    c = 7.32,  
    
    shot_angle = acos((a^2 + b^2 - c^2) / (2 * a * b)),
    
    shot_angle_deg = shot_angle * 180 / pi
  )
#Laver variabler for spil før skudet
testxgdataframe$Relatedevent[is.na(testxgdataframe$Relatedevent)] <- 0
testxgdataframe <- testxgdataframe %>% 
  mutate(
    pass = ifelse(Relatedevent=="pass", 1,0)
  )
testxgdataframe <- testxgdataframe %>% 
  mutate(
    duel = ifelse(Relatedevent=="duel", 1,0)
  )
testxgdataframe <- testxgdataframe %>% 
  mutate(
    interception = ifelse(Relatedevent=="interception", 1,0)
  )
testxgdataframe <- testxgdataframe %>% 
  mutate(
    corner = ifelse(Relatedevent=="corner", 1,0)
  )
testxgdataframe <- testxgdataframe %>% 
  mutate(
    freekick = ifelse(Relatedevent=="free_kick", 1,0)
  )
testxgdataframe <- testxgdataframe %>% 
  mutate(
    acceleration = ifelse(Relatedevent=="acceleration", 1,0)
  )
testxgdataframe <- testxgdataframe %>% 
  mutate(
    head = ifelse(SHOTBODYPART=="head_or_other"|SHOTBODYPART=="head_sho", 1,0)
  )

# ---- PREDICTION ----
predglm <- predict(
  glm1,
  newdata = testxgdataframe,
  type = "response"
)
view(predglm)

length(unique(testxgdataframe$MATCH_WYID))
install.packages("pROC")
library(pROC)

roc_objglm <- roc(testxgdataframe$goal_flag, predglm)
auc(roc_objglm)
auc_value <- auc(roc_objglm)
plot(roc_objglm, col = "darkolivegreen4", lwd = 2)

saveRDS(roc_objglm, file = "roc_objglm.rds")

unique(testxgdataframe$SEASON_WYID)

testxgdataframe$predic_xg_glm <- predglm
testxgdataframe$pred_goal_glm <- ifelse(testxgdataframe$predic_xg_glm > 0.28, 1, 0)

table(
  Predicted = testxgdataframe$pred_goal_glm,
  Actual = testxgdataframe$goal_flag
)

# ---- predglm ----
pred <- predict(
  gbm1,
  newdata = testxgdataframe,
  n.trees = best_iter,
  type = "response"
)

roc_obj <- roc(testxgdataframe$goal_flag, pred)
auc(roc_obj)
auc_value <- auc(roc_obj)
plot(roc_obj, col = "darkolivegreen4", lwd = 2)

testxgdataframe$predic_xg <- pred
testxgdataframe$pred_goal <- ifelse(testxgdataframe$predic_xg > 0.28, 1, 0)

table(
  Predicted = testxgdataframe$pred_goal,
  Actual = testxgdataframe$goal_flag
)
conf_mat_2 <- table(
  Predicted = testxgdataframe$pred_goal_glm,
  Actual = testxgdataframe$goal_flag
)

conf_mat_df_2 <- as.data.frame(conf_mat)

pred <- predict(
  gbm1,
  newdata = xgdataframe,
  n.trees = best_iter,
  type = "response"
)
xpdataframe <- xgdataframe
xpdataframe$XG <- pred

xpdataframe <- xpdataframe %>% 
  left_join(common %>% select(EVENT_WYID, PLAYER_WYID, TEAM_WYID),
            by = "EVENT_WYID")

match_xg <- xpdataframe %>%
  group_by(MATCH_WYID, TEAM_WYID) %>%
  summarise(xg = sum(XG, na.rm = TRUE),
            goals = sum(goal_flag, na.rm = TRUE),.groups = "drop")


match_xg2 <- match_xg %>%
  group_by(MATCH_WYID) %>%
  mutate(opp_xg = rev(xg),
         opp_goals = rev(goals)) %>%
  ungroup()

match_data <- match_xg2 %>%
  group_by(MATCH_WYID) %>%
  mutate(
    xg_diff = xg - opp_xg,
    xg_total = xg + opp_xg
  ) %>%
  ungroup()

match_data <- match_xg2 %>%
  mutate(
    result = case_when(
      goals > opp_goals ~ 3,
      goals == opp_goals ~ 1,
      TRUE ~ 0
    )
  )
match_data$result_cat <- factor(match_data$result,
                                levels = c(0, 1, 3),
                                labels = c("loss", "draw", "win"))

match_data <- match_data %>% mutate(xg_diff = xg - opp_xg,
                                    xg_total = xg + opp_xg)

gbm_model <- gbm(
  result_cat ~ xg_diff + xg_total,
  data = match_data,
  distribution = "multinomial",
  n.trees = 125,
  interaction.depth = 3,
  shrinkage = 0.05,
  cv.folds = 1
)
best_iter1 <- gbm.perf(gbm_model, method = "cv")
# ---- testxP ----
predxp <- predict(
  gbm1,
  newdata = testxgdataframe,
  n.trees = best_iter,
  type = "response"
)
testxpdataframe <- testxgdataframe
testxpdataframe$XG <- predxp

testxpdataframe <- testxpdataframe %>% left_join(common %>% select(EVENT_WYID, PLAYER_WYID, TEAM_WYID),
                                                 by = "EVENT_WYID")

testmatch_xg <- testxpdataframe %>%
  group_by(MATCH_WYID, TEAM_WYID) %>%
  summarise(xg = sum(XG, na.rm = TRUE),
            goals = sum(goal_flag, na.rm = TRUE),.groups = "drop")


testmatch_xg2 <- testmatch_xg %>%
  group_by(MATCH_WYID) %>%
  mutate(opp_xg = rev(xg),
         opp_goals = rev(goals),
         opp_WYID = rev(TEAM_WYID)) %>%
  ungroup()

testmatch_data <- testmatch_xg2 %>%
  group_by(MATCH_WYID) %>%
  mutate(
    xg_diff = xg - opp_xg,
    xg_total = xg + opp_xg
  ) %>%
  ungroup()

testmatch_data <- testmatch_xg2 %>%
  mutate(
    result = case_when(
      goals > opp_goals ~ 3,
      goals == opp_goals ~ 1,
      TRUE ~ 0
    )
  )
testmatch_data$result_cat <- factor(testmatch_data$result,
                                    levels = c(0, 1, 3),
                                    labels = c("loss", "draw", "win"))

testmatch_data <- testmatch_data %>% mutate(xg_diff = xg - opp_xg,
                                            xg_total = xg + opp_xg)

testprobs <- predict(gbm_model,
                     testmatch_data,
                     n.trees = 125,
                     type = "response")
testprobsdf <- data.frame(testprobs)
names(testprobsdf) <- c("p_loss", "p_draw", "p_win")
testmatch_data <- bind_cols(testmatch_data, testprobsdf)
testmatch_data$pred_class <- apply(
  testmatch_data[, c("p_loss", "p_draw", "p_win")],
  1,
  function(x) c("loss", "draw", "win")[which.max(x)]
)
colnames(testmatch_data)
testmatch_data$predresult <- factor(testmatch_data$pred_class,
                                    levels = c("loss", "draw", "win"),
                                    labels = c(0, 1, 3))

table(testmatch_data$predresult, testmatch_data$result)

testmatch_data <- testmatch_data %>%
  mutate(xp = 3 * p_win + 1 * p_draw)

xphold <- testmatch_data %>%
  group_by(TEAM_WYID) %>%
  summarise(xP = sum(xp, na.rm = TRUE),
            P = sum(result, na.rm = TRUE))
xphold <- xphold %>%
  left_join(teams, by = "TEAM_WYID") %>% distinct(TEAM_WYID, .keep_all = T)


