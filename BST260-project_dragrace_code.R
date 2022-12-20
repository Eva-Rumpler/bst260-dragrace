
### BST260 Final project
### RuPaul Drag race
### Eva RUMPLER 

### Load libraries
#install.packages("dragracer")
#devtools::install_github("svmiller/dragracer")
library(dragracer)
library(tidyverse)
library(knitr)
#install.packages("jsonlite")
require("jsonlite") # or require better ? 
#install.packages("gsheet")
library(gsheet)
library(lubridate)
library(stringr)
library(ggalluvial)
library(caret)


### Load datasets 

## Data source 1: package dragracer
#data(package = "dragracer")
data(rpdr_contep)
data(rpdr_contestants)
data(rpdr_ep)
saveRDS(rpdr_contep, './data/rpdr_contep.RDS')
saveRDS(rpdr_contestants, './data/rpdr_contestants.RDS')

#View(rpdr_contep)
#View(rpdr_contestants)
#View(rpdr_ep)
# save(rpdr_contep, file = "rpdr_contep.RData")
# save(rpdr_contep, file = "rpdr_contestants.RData")
# save(rpdr_contep, file = "rpdr_ep.RData")
# load("rpdr_contep.RData")
# load("rpdr_contestants.RData")
# load("rpdr_ep.RData")

## Data source 2: 
fromJSON("http://www.nokeynoshade.party/api/seasons/1/episodes")
queens <- fromJSON("http://www.nokeynoshade.party/api/queens/all")
episodes <- fromJSON("http://www.nokeynoshade.party/api/episodes")
seasons <- fromJSON("http://www.nokeynoshade.party/api/seasons")
lipsyncs <- fromJSON("http://www.nokeynoshade.party/api/lipsyncs")
View(queens)
nrow(queens) # missing 3 seasons or so. 
View(episodes)
View(seasons)
View(lipsyncs)

## Data source 3: 
all_contestant <- gsheet2tbl('docs.google.com/spreadsheets/d/1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q/edit#gid=1613421713')
all_contestant
all_social_media <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q/edit#gid=1915800778')
all_social_media
saveRDS(all_contestant, './data/all_contestant.RDS')
saveRDS(all_social_media, './data/all_social_media.RDS')




### Idea: to delete. 
#https://github.com/RuPaulsDataRace/Rupository
# Rupository w idea of analyses
# Prediction from one episode to the next
# Do a regression to try to predict winner from: Age, winner in the quote, etc etc

#https://drag-race-api.readme.io/docs/getting-started
# API all data ? 
# Idea: Does having "winner in quoteé helps win ? 

# http://svmiller.com/blog/2019/02/dragracer-rupauls-drag-race-analysis/

# https://towardsdatascience.com/drag-race-analytics-using-plotly-and-dash-838f7d71b3e1

# https://shiraamitchell.github.io/rpdr
# https://docs.google.com/spreadsheets/d/1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q/edit#gid=228427306

#knitr::dep_prev()
# gs_auth()
# rpdr_data <- "1Sotvl3o7J_ckKUg5sRiZTqNQn3hPqhepBSeOpMTK15Q" %>%  gs_key
# all_episodes <- rpdr_data %>% gs_read("all_episodes")
# all_contestants <- rpdr_data %>% gs_read("all_contestants")
# all_rankings <- rpdr_data %>% gs_read("all_rankings")

# Predict whether winner based on: hometown, misscongeniality, number of challenges or mini challenges win, age, whether win or crown is in the quote?, snatch game win, first episode win, 
# 1st or last to arrive in werkroom ? 
# also type of challenge win( snatch game, sewing, etc).
# Or predict Winner or Misc congeniality separately or Making it to the finale ? 

# Predict next week win based on previous week performance ? 

# Surviving after being in btm, once, twice etc ? 

# Limitation: restricted analysis to RPDR US, could have included other francises ? 

# Justify not AS because rules are different ! (and no data :) )

# TODO: deal w sherry pie. 

# TODO: is it a pb that after you're eleminated you no longer have bots and highs ? Maybe its ok bc it correlates w having been kicked out earlier. 

# TODO: idea: doing % of tops/bots etc instead of raw number



### Data cleaning and wrangling 

# Extract list of finale participants, snatch game and miss congeniality 
## Which queens are present 2 different seasons ? 
test <- rpdr_contep %>% group_by(contestant) %>%
  summarise(season = season) %>% distinct() #%>% View()
test
list_twosseasons <- as.data.frame(table(test$contestant)) %>% filter(Freq != 1)
list_twosseasons
#Vanessa Vanjie Mateo
#Shangela
#Cynthia Lee Fontaine
#Eureka O'Hara
## Miss congeniality CAN DELETE 
# list_missc <- rpdr_contep %>% filter(missc == 1) %>% pull(contestant) %>% unique()
# list_missc 
## Finale 
list_finale <- rpdr_contep %>% filter(finale == 1 & !is.na(outcome) & !outcome %in% c("Guest", "RTRN", "MISSCON")) %>% pull(contestant) %>% unique()
list_finale
## Snatch game 
#grepl("Snatch", rpdr_ep$nickname)
#rpdr_ep[grepl("Snatch", rpdr_ep$nickname),] %>% View()
sntchgame_ep <- rpdr_ep[grepl("Snatch", rpdr_ep$nickname),] %>% select(season, episode, nickname)
sntchgame_ep
list_snatchgame <- left_join(sntchgame_ep, rpdr_contep) %>% filter(outcome == "WIN") %>% pull(contestant) %>% unique()
list_snatchgame

rpdr_contestants2 <- rpdr_contestants %>%
  mutate(#missc = ifelse(contestant %in% list_missc, T, F), 
         finale = ifelse(contestant %in% list_finale, T, F), 
         finale = ifelse(contestant == "Eureka O'Hara" & season == "S09", F, finale), 
         snatchgame = ifelse(contestant %in% list_snatchgame, T, F), 
         # homestate2 = stri_extract_last_words(rpdr_contestants$hometown), 
         # homestate2 = ifelse(homestate == "Rico", "Puerto Rico", homestate),
         # homestate2 = ifelse(homestate == "York", "New York", homestate),
         # homestate2 = ifelse(homestate == "Mexico", "New Mexico", homestate),
         # homestate2 = ifelse(homestate == "Jersey", "New Jersey", homestate),
         # homestate2 = ifelse(homestate == "Carolina", "North Carolina", homestate), # wrong actually one queen from south carolina
         homestate = str_trim(sub("^[^,]*,", "", str_trim(sub("^[^,]*,", "", hometown)))))
rpdr_contestants2
rpdr_contestants2 %>% filter(contestant %in% c(list_twosseasons$Var1)) %>% View()

# Extract data from rpdr_contep and put clean in rpdr_contestants
# rank, number tops, number wins, number bot, number minichallenge wins, 
ep_not_finale_or_reunion_list <- rpdr_ep %>%
  filter(special != 1 & finale != 1) %>%
  select(c(season, episode))
ep_not_finale_or_reunion_list

test2 <- inner_join(rpdr_contep, ep_not_finale_or_reunion_list) %>% 
  group_by(contestant, season) %>% 
  summarise(rank = rank,
            minichalw = sum(as.numeric(minichalw), na.rm = T),
            missc = missc,
            nwin = sum(as.numeric(outcome %in% c("WIN","TOP 4","TOP2")), na.rm = T),
            nhighwin = sum(as.numeric(outcome %in% c("WIN","HIGH","TOP 4","TOP2")), na.rm = T),
            nbot = sum(as.numeric(outcome %in% c("BTM","SAVE")), na.rm = T),
            nlowbot = sum(as.numeric(outcome %in% c("LOW","BTM","SAVE")), na.rm = T),
            nsafe = sum(as.numeric(outcome %in% c("SAFE","SAFE+DEPT")), na.rm = T)) %>%
  distinct() %>%
  mutate(winner = ifelse(rank == 1, T, F))

clean_data <- inner_join(rpdr_contestants2, test2, by = c("contestant", "season")) %>%
  filter(contestant != "Sherry Pie")
clean_data
saveRDS(clean_data, './data/clean_data.RDS')
#

## Twitter data
# two different datasets have names coded slightly differently ! 
contestant_names <- all_contestant %>% select(c(contestant_id, contestant_name, season_number, contestant_entrance))
contestant_names
all_social_media
all_social_media2 <- merge(contestant_names, all_social_media, by = "contestant_id") %>%
  mutate(date = as.Date(datetime))
head(all_social_media2)
season_date <- rpdr_ep %>%
  filter(episode == 1) %>% select(c(season, airdate)) %>%
  mutate(season_number = as.numeric(str_replace_all(season, c("S|S0"), ""))) %>% select(-c(season))
season_date
all_social_media3 <- merge(season_date, all_social_media2, by = "season_number", all = T) %>%
  filter(format(airdate, "%Y-%m") == format(date, "%Y-%m")) %>%
  group_by(contestant_name) %>% slice(n=1) %>%
  mutate(contestant = case_when(contestant_name == "Serena ChaCha" ~ "Serena Cha Cha",
                                     contestant_name == "Eureka" ~ "Eureka O'Hara",
                                     contestant_name == "Ra'jah D. O'Hara" ~ "Ra'Jah O'Hara",
                                     contestant_name == "A'keria Chanel Davenport" ~ "A'Keria C. Davenport",
                                     T ~ contestant_name))
nrow(all_social_media3) # only 87 w twitter data available !!! 
head(all_social_media3)
clean_data
clean_data_twitter <- full_join(clean_data, all_social_media3, by = "contestant")
saveRDS(clean_data_twitter %>% rename(datefollower = date) %>% select(-c(season_number, airdate, contestant_id, contestant_name, datetime)) %>% filter(is.na(followers_twitter) == F), './data/clean_data_twitter.RDS')


## Plots showing data
clean_data
clean_data$nhighwin
# clean_data %>% select(contestant, season, missc, snatchgame, winner) %>%
#   filter(missc == 1 | snatchgame == T | winner == T) 

colnames(clean_data)
clean_data %>%
  select(-c(season, age, dob, hometown, finale, snatchgame, homestate, rank, missc, winner)) %>%
  pivot_longer(cols = c("minichalw", "nwin", "nhighwin",  "nbot", "nlowbot", "nsafe"),
                names_to='Outcome',
                values_to='Number') %>%
  mutate(Number = as.character(Number),
         Outcome = factor(Outcome,
                            levels = c('nwin',
                                        'nhighwin',
                                        'nsafe',
                                        'nlowbot', 
                                        'nbot',
                                        'minichalw'))) %>%
  ggplot() + 
  geom_bar(aes(x = Number, fill = Outcome), position = "dodge") +
  scale_fill_discrete(labels=c('Win',
                               'High or Win',
                               'Safe',
                               'Low or Bottom',
                               'Bottom',
                               'Mini challenge')) +
  labs(title="Distribution of performance in the mini and maxi challenges",
       x ="Number of times ranked in each category",
       y = "Count") +
  theme_minimal() 
ggsave("./plots/Figure_1.png", width = 7, height = 4)

clean_data %>%
  ggplot() +
  geom_boxplot(aes(x = season, y = age)) +
  labs(title="Distribution of age of contestant across seasons",
       x ="Seasons of RuPaul's Drag Race US",
       y = "Age") +
  theme_minimal() 
ggsave("./plots/Figure_1B.png", width = 7, height = 4)

clean_data %>%
  ggplot(aes(x = season, fill = homestate)) +
  geom_bar(position="fill") + # stat="identity"
  labs(title="Distribution of state of origin across seasons",
       x ="Seasons of RuPaul's Drag Race US",
       y = "State",
       fill = "Homestate") +
  theme_minimal() 
ggsave("./plots/Figure_1C.png", width = 7, height = 5)


#

## Run glm
clean_data
?lm
colnames(clean_data)

"age"    "homestate"

"missc" missc outcome:  "snatchgame"   "minichalw" "rank" 
"winner"winner outcome : "snatchgame"   "minichalw" 

     "nwin"  "nhighwin"   "nbot"       "nlowbot"    "nsafe"      


model_missc_A <- glm(data = clean_data,
                factor(missc) ~ age + homestate,
                family=binomial(link="logit"))
summary(model_missc_A)

model_missc_B <- glm(data = clean_data,
                     factor(missc) ~ age + homestate + snatchgame + minichalw,
                     family=binomial(link="logit"))
summary(model_missc_B)

model_missc_C <- glm(data = clean_data,
                     factor(missc) ~ age + homestate + snatchgame + minichalw + rank,
                     family=binomial(link="logit"))
summary(model_missc_C)

model_missc_D <- glm(data = clean_data,
                     factor(missc) ~ age + homestate + snatchgame + minichalw + nwin + nhighwin,
                     family=binomial(link="logit"))
summary(model_missc_D)

model_missc_E <- glm(data = clean_data,
                     factor(missc) ~ age + homestate + snatchgame + minichalw + nwin + nhighwin + nbot + nlowbot + nsafe,
                     family=binomial(link="logit"))
summary(model_missc_E)


prediction_model_missc_D <- ifelse(predict(model_missc_D,
                                     newdata = clean_data,
                                     type="response") > 0.5, 1, 0) |> factor(levels = c(0, 1))
CM_1 <- confusionMatrix(prediction_model_missc_D, factor(clean_data$missc))
CM_1

clean_data$contestant[prediction_model_missc_D == 1]

##

model_win_A <- glm(data = clean_data,
                     winner ~ age + homestate,
                     family=binomial(link="logit"))
summary(model_win_A)

model_win_A <- glm(data = clean_data,
                   winner ~ age + homestate ,
                   family=binomial(link="logit"))
summary(model_win_A)

model_win_B <- glm(data = clean_data,
                   winner ~ age + homestate  + snatchgame + minichalw,
                   family=binomial(link="logit"))
summary(model_win_B)

model_win_C <- glm(data = clean_data,
                   winner ~ age + homestate  + snatchgame + minichalw + nwin + nhighwin + nbot + nlowbot + nsafe,
                   family=binomial(link="logit"))
summary(model_win_C)

prediction_model_win_C <- ifelse(predict(model_win_C,
                                           newdata = clean_data,
                                           type="response") > 0.5, 1, 0) |> factor(levels = c(0, 1))
CM_1 <- confusionMatrix(prediction_model_win_C, factor(as.numeric(clean_data$winner)))
CM_1

table(prediction_model_win_C)

clean_data$contestant[prediction_model_win_C == 1]

clean_data$contestant[prediction_model_win_C == 1 & clean_data$winner == FALSE]

#
# model1 <- lm(data = clean_data, formula = missc ~ age + snatchgame + homestate + rank + minichalw + nwin + nhighwin + nlowbot + nbot + finale)
# summary(model1)
# model2 <- lm(data = clean_data, formula = missc ~ age + snatchgame  + rank + minichalw + nwin + nhighwin + nlowbot + nbot + finale)
# summary(model2)
# model4 <- lm(data = clean_data, formula = missc ~ age + snatchgame  + rank + minichalw + nwin  + nlowbot + nbot + finale)
# summary(model4)
# model3 <- lm(data = clean_data, formula = missc ~ age + snatchgame  + rank + minichalw + nwin + nhighwin + nlowbot + nbot)
# summary(model3)
# 
# model5 <- lm(data = clean_data, formula = winner ~ missc + age + snatchgame + minichalw + nwin + nhighwin + nlowbot + nbot)
# summary(model5)
# model6 <- lm(data = clean_data, formula = winner ~ missc + age + snatchgame  + minichalw + nwin + nbot)
# summary(model6)
# model7 <- lm(data = clean_data, formula = winner ~ age + snatchgame  + minichalw + nwin + nbot + homestate)
# summary(model7)
# model8 <- lm(data = clean_data, formula = winner ~ age + snatchgame  + minichalw + nwin + nbot)
# summary(model8)
# 
# model9 <- glm(data = clean_data,
#               formula = winner ~ age + snatchgame  + minichalw + nwin + nbot + homestate)
# summary(model9)
# model10 <- glm(data = clean_data,
#               # family = "binomial",
#               formula = winner ~ age + snatchgame  + minichalw + nwin + nbot + homestate)
# summary(model10)



### Making cool alluvial graph 
View(rpdr_contep)

rpdr_contep_clean <- rpdr_contep %>%
  filter( !(season == "S11" & episode == "13")) %>%
  filter( !(season == "S12" & episode == "13")) %>%
  filter( !(season == "S13" & episode == "15")) %>%
  filter( !(season == "S13" & episode == "14")) %>%
  filter( !(season == "S14" & episode == "15")) %>%
  mutate(episode = case_when(season == "S12" & episode %in% c("1","2") ~ 2,
                             season == "S11" & episode == "14" ~ 13,
                             season == "S12" & episode == "14" ~ 13,
                             season == "S13" & episode %in% c("2","3") ~ 3,
                             season == "S06" & episode %in% c("1","2") ~ 2,
                             season == "S14" & episode %in% c("1","2") ~ 2,
                             T ~ episode)) %>%
  mutate(outcome = ifelse( (season == "S13" & episode == 1 & outcome == "WIN"), "HIGH", outcome)) %>% 
  filter( !(season == "S13" & episode == "3" & participant == 0)) %>%
  filter( !(season == "S06" & episode == "2" & participant == 0)) %>% 
  filter( !(season == "S12" & episode == "2" & participant == 0)) %>%
  filter( !(season == "S14" & episode == "2" & participant == 0))
# rpdr_contep_clean %>%
#   filter(season == "S13" & episode == 3) %>% View() 
# rpdr_contep_clean %>%
#   filter(season == "S14" & episode == 16) %>% View()

alluvial_data <- rpdr_contep_clean %>%
  filter((outcome != "TEST" | is.na(outcome))) %>% # finale != 1  & contestant != "Sherry Pie"
  select(-c(missc, rank, minichalw, finale, penultimate)) %>%
  mutate(outcome2 = case_when(participant == 0 ~ "ELIM",
                              #is.na(outcome) == T ~ "ELIM",
                              outcome == "Guest" ~ "ELIM", 
                              outcome == "OUT" ~ "ELIM", 
                              outcome == "Miss C" ~ "ELIM",
                              outcome == "WDR" ~ "ELIM",
                              outcome %in% c("MISSCON", "RTRN") ~ "ELIM",
                              outcome == "SAFE" ~ "SAFE",
                              outcome == "SAFE+DEPT" ~ "SAFE",
                              outcome == "STAY" ~ "SAFE",
                              outcome == "WIN" ~ "WIN", 
                              outcome == "Winner" ~ "WIN", 
                              outcome == "WIN+RTRN" ~ "WIN",
                              outcome == "TOP 4" ~ "WIN",
                              outcome == "LOW" ~ "LOW",
                              outcome == "LOSS" ~ "LOW",
                              outcome == "HIGH" ~ "HIGH",
                              outcome %in% c("Runner-up", "Eliminated") ~ "HIGH",
                              outcome == "TOP2" ~ "HIGH",
                              outcome %in% c("LOST3RD ROUND", "LOST2ND ROUND", "LOST1ST ROUND") ~ "HIGH",
                              outcome == "BTM" ~ "BTM",
                              outcome == "SAVE" ~ "BTM",
                              T ~ "99"),
         outcome2 = factor(outcome2, levels = c("WIN", "HIGH", "SAFE", "LOW", "BTM", "ELIM", "RTRN", "DISQ"))) %>%
  select(-c(outcome, eliminated, participant))
table(alluvial_data$outcome2)
alluvial_data
#View(alluvial_data)

# All seasons 
# alluvial_data_wide <- alluvial_data %>%
#   group_by(season) %>%
#   pivot_wider(names_from = episode, values_from = outcome2)
# alluvial_data_wide

# Season 01
alluvial_data_S01 <- alluvial_data %>% filter(season == "S01")
alluvial_data_S01
alluvial_data_S01_wide <- alluvial_data_S01 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Finale = `8`)
alluvial_data_S01_wide
queen_order_S01 <- alluvial_data_S01_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S01
alluvial_data_S01_wide <- alluvial_data_S01_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S01)$Queens))
alluvial_data_S01_wide #alluvial_data_S01_wide$Queens
ggplot(data = alluvial_data_S01_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 1") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S01_wide)))) 
ggsave("./plots/Figure_S1.png", width = 8, height = 4)

# Season 02
alluvial_data_S02 <- alluvial_data %>% filter(season == "S02")
alluvial_data_S02
alluvial_data_S02_wide <- alluvial_data_S02 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Finale = `11`)
alluvial_data_S02_wide
queen_order_S02 <- alluvial_data_S02_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S02
alluvial_data_S02_wide <- alluvial_data_S02_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S02)$Queens))
alluvial_data_S02_wide #alluvial_data_S02_wide$Queens
ggplot(data = alluvial_data_S02_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 2") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S02_wide)))) 
ggsave("./plots/Figure_S2.png", width = 10, height = 4)

# Season 03
alluvial_data_S03 <- alluvial_data %>% filter(season == "S03")
alluvial_data_S03
alluvial_data_S03_wide <- alluvial_data_S03 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `2`,
         Ep2 = `3`,
         Ep3 = `4`,
         Ep4 = `5`,
         Ep5 = `6`,
         Ep6 = `7`,
         Ep7 = `8`,
         Ep8 = `9`,
         Ep9 = `10`,
         Ep10 = `11`,
         Ep11 = `12`,
         Ep12 = `13`,
         Finale = `15`)
alluvial_data_S03_wide
queen_order_S03 <- alluvial_data_S03_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S03
alluvial_data_S03_wide <- alluvial_data_S03_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S03)$Queens))
alluvial_data_S03_wide #alluvial_data_S03_wide$Queens
ggplot(data = alluvial_data_S03_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Ep12, axis13 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Ep12", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 3") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S03_wide)))) 
ggsave("./plots/Figure_S3.png", width = 11, height = 4)

# Season 04
alluvial_data_S04 <- alluvial_data %>% filter(season == "S04")
alluvial_data_S04
alluvial_data_S04_wide <- alluvial_data_S04 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Ep10 = `10`,
         Ep11 = `11`,
         Finale = `14`)
alluvial_data_S04_wide
queen_order_S04 <- alluvial_data_S04_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S04
alluvial_data_S04_wide <- alluvial_data_S04_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S04)$Queens))
alluvial_data_S04_wide #alluvial_data_S04_wide$Queens
ggplot(data = alluvial_data_S04_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 4") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S04_wide)))) 
ggsave("./plots/Figure_S4.png", width = 10, height = 4)

# Season 05
alluvial_data_S05 <- alluvial_data %>% filter(season == "S05")
alluvial_data_S05
alluvial_data_S05_wide <- alluvial_data_S05 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Ep10 = `10`,
         Ep11 = `11`,
         Finale = `14`)
alluvial_data_S05_wide
queen_order_S05 <- alluvial_data_S05_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S05
alluvial_data_S05_wide <- alluvial_data_S05_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S05)$Queens))
alluvial_data_S05_wide #alluvial_data_S05_wide$Queens
ggplot(data = alluvial_data_S05_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 5") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S05_wide)))) 
ggsave("./plots/Figure_S5.png", width = 10, height = 4)

# Season 06
alluvial_data_S06 <- alluvial_data %>% filter(season == "S06")
alluvial_data_S06
alluvial_data_S06_wide <- alluvial_data_S06 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `2`,
         Ep2 = `3`,
         Ep3 = `4`,
         Ep4 = `5`,
         Ep5 = `6`,
         Ep6 = `7`,
         Ep7 = `8`,
         Ep8 = `9`,
         Ep9 = `10`,
         Ep10 = `11`,
         Ep11 = `12`,
         Finale = `14`)
alluvial_data_S06_wide
queen_order_S06 <- alluvial_data_S06_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S06
alluvial_data_S06_wide <- alluvial_data_S06_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S06)$Queens))
alluvial_data_S06_wide #alluvial_data_S06_wide$Queens
ggplot(data = alluvial_data_S06_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 6") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S06_wide)))) 
ggsave("./plots/Figure_S6.png", width = 10, height = 4)

# Season 07
alluvial_data_S07 <- alluvial_data %>% filter(season == "S07")
alluvial_data_S07
alluvial_data_S07_wide <- alluvial_data_S07 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Ep10 = `10`,
         Ep11 = `11`,
         Ep12= `12`,
         Finale = `14`)
alluvial_data_S07_wide
queen_order_S07 <- alluvial_data_S07_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S07
alluvial_data_S07_wide <- alluvial_data_S07_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S07)$Queens))
alluvial_data_S07_wide #alluvial_data_S07_wide$Queens
ggplot(data = alluvial_data_S07_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Ep12, axis13 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Ep12", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 7") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S07_wide)))) 
ggsave("./plots/Figure_S7.png", width = 10, height = 4)

# Season 08
alluvial_data_S08 <- alluvial_data %>% filter(season == "S08")
alluvial_data_S08
alluvial_data_S08_wide <- alluvial_data_S08 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Finale = `10`)
alluvial_data_S08_wide
queen_order_S08 <- alluvial_data_S08_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S08
alluvial_data_S08_wide <- alluvial_data_S08_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S08)$Queens))
alluvial_data_S08_wide #alluvial_data_S08_wide$Queens
ggplot(data = alluvial_data_S08_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 8") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S08_wide)))) 
ggsave("./plots/Figure_S8.png", width = 10, height = 4)

# Season 09
alluvial_data_S09 <- alluvial_data %>% filter(season == "S09")
alluvial_data_S09
alluvial_data_S09_wide <- alluvial_data_S09 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Ep10 = `10`,
         Ep11 = `11`,
         Finale = `14`)
alluvial_data_S09_wide
queen_order_S09 <- alluvial_data_S09_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S09
alluvial_data_S09_wide <- alluvial_data_S09_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S09)$Queens))
alluvial_data_S09_wide #alluvial_data_S09_wide$Queens
alluvial_data_S09_wide[10,2] <- "RTRN"
alluvial_data_S09_wide$Queens
ggplot(data = alluvial_data_S09_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 9") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S09_wide)))) 
ggsave("./plots/Figure_S9.png", width = 10, height = 4)

# Season 10
alluvial_data_S10 <- alluvial_data %>% filter(season == "S10")
alluvial_data_S10
alluvial_data_S10_wide <- alluvial_data_S10 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Ep10 = `10`,
         Ep11 = `11`,
         Finale = `14`)
alluvial_data_S10_wide
queen_order_S10 <- alluvial_data_S10_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S10
alluvial_data_S10_wide <- alluvial_data_S10_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S10)$Queens))
alluvial_data_S10_wide #alluvial_data_S10_wide$Queens
ggplot(data = alluvial_data_S10_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 10") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S10_wide))))
ggsave("./plots/Figure_S10.png", width = 10, height = 4)

# Season 11
alluvial_data_S11 <- alluvial_data %>% filter(season == "S11")
alluvial_data_S11
alluvial_data_S11_wide <- alluvial_data_S11 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `2`,
         Ep3 = `3`,
         Ep4 = `4`,
         Ep5 = `5`,
         Ep6 = `6`,
         Ep7 = `7`,
         Ep8 = `8`,
         Ep9 = `9`,
         Ep10 = `10`,
         Ep11 = `11`,
         Ep12 = `12`,
         Finale = `13`)
alluvial_data_S11_wide
queen_order_S11 <- alluvial_data_S11_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S11
alluvial_data_S11_wide <- alluvial_data_S11_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S11)$Queens))
alluvial_data_S11_wide #alluvial_data_S11_wide$Queens
ggplot(data = alluvial_data_S11_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Ep12, axis13 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Ep12", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 11") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S11_wide))))
ggsave("./plots/Figure_S11.png", width = 10, height = 4)

# Season 12
alluvial_data_S12 <- alluvial_data %>% filter(season == "S12")
alluvial_data_S12
alluvial_data_S12_wide <- alluvial_data_S12 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `2`,
         Ep2 = `3`,
         Ep3 = `4`,
         Ep4 = `5`,
         Ep5 = `6`,
         Ep6 = `7`,
         Ep7 = `8`,
         Ep8 = `9`,
         Ep9 = `10`,
         Ep10 = `11`,
         Ep11 = `12`,
         Finale = `13`)
alluvial_data_S12_wide
queen_order_S12 <- alluvial_data_S12_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S12
alluvial_data_S12_wide <- alluvial_data_S12_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S12)$Queens))
alluvial_data_S12_wide #alluvial_data_S12_wide$Queens
alluvial_data_S12_wide[13, 13] <- "DISQ"
alluvial_data_S12_wide
ggplot(data = alluvial_data_S12_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 12") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S12_wide))))
ggsave("./plots/Figure_S12.png", width = 10, height = 4)

# Season 13
alluvial_data_S13 <- alluvial_data %>% filter(season == "S13")
alluvial_data_S13
alluvial_data_S13_wide <- alluvial_data_S13 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `1`,
         Ep2 = `3`,
         Ep3 = `4`,
         Ep4 = `5`,
         Ep5 = `6`,
         Ep6 = `7`,
         Ep7 = `8`,
         Ep8 = `9`,
         Ep9 = `10`,
         Ep10 = `11`,
         Ep11 = `12`,
         Ep12 = `13`,
         Finale = `16`)
alluvial_data_S13_wide
queen_order_S13 <- alluvial_data_S13_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S13
alluvial_data_S13_wide <- alluvial_data_S13_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S13)$Queens))
alluvial_data_S13_wide #alluvial_data_S13_wide$Queens
ggplot(data = alluvial_data_S13_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Ep12, axis13 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Ep12", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 13") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S13_wide))))
ggsave("./plots/Figure_S13.png", width = 10, height = 4)

# Season 14
alluvial_data_S14 <- alluvial_data %>% filter(season == "S14")
alluvial_data_S14
alluvial_data_S14_wide <- alluvial_data_S14 %>% 
  pivot_wider(names_from = episode, values_from = outcome2) %>%
  select(-c(season)) %>%
  mutate(freq = 1) %>%
  rename(Queens = contestant,
         Ep1 = `2`,
         Ep2 = `3`,
         Ep3 = `4`,
         Ep4 = `5`,
         Ep5 = `6`,
         Ep6 = `7`,
         Ep7 = `8`,
         Ep8 = `9`,
         Ep9 = `10`,
         Ep10 = `11`,
         Ep11 = `12`,
         Ep12 = `13`,
         Ep13 = `14`,
         Finale = `16`)
alluvial_data_S14_wide
queen_order_S14 <- alluvial_data_S14_wide %>% arrange(Ep1) %>% select(Queens) 
queen_order_S14
alluvial_data_S14_wide <- alluvial_data_S14_wide %>% mutate(Queens = factor(Queens, levels = c(queen_order_S14)$Queens))
alluvial_data_S14_wide #alluvial_data_S14_wide$Queens
ggplot(data = alluvial_data_S14_wide,
       aes(axis1 = Ep1, axis2 = Ep2, axis3 = Ep3, axis4 = Ep4, axis5 = Ep5, axis6 = Ep6, axis7 = Ep7, axis8 = Ep8, axis9 = Ep9, axis10 = Ep10, axis11 = Ep11, axis12 = Ep12, axis13 = Ep13, axis14 = Finale,
           y = freq)) +
  scale_x_discrete(limits = c("Ep1", "Ep2", "Ep3", "Ep4", "Ep5", "Ep6", "Ep7", "Ep8", "Ep9", "Ep10", "Ep11", "Ep12", "Ep13", "Finale")) + #, expand = c(.2, .05)
  geom_alluvium(aes(fill = Queens)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Season 14") + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) + 
  scale_fill_manual(values=c(rainbow(nrow(alluvial_data_S14_wide))))
ggsave("./plots/Figure_S14.png", width = 10, height = 4)

#
#




##############
### Try extracting each pair of columns 
# make a matrix that's 1 to 5 and 2 to 6 
# 2 by 5 matrix
# rbind to find all 
# matrix(c(alluvial_data_S01_wide[1,3:7],alluvial_data_S01_wide[1,4:8]), ncol = 2)
# #rbind(
# lapply(1:nrow(alluvial_data_S01_wide), function(n) {
#   matrix(c(alluvial_data_S01_wide[n,3:7],alluvial_data_S01_wide[n,4:8]), ncol = 2)
# })#)
# alluvial_data_wide 



# 

###

