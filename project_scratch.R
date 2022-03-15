library(tidyverse)

#read in basketball statistics for players
Seasons_Stats <- read_csv("C:/Users/Albert/Desktop/stat301/Final Project/Seasons_Stats.csv")
#read in total games played each season
Games <- read_csv("C:/Users/Albert/Desktop/stat301/Final Project/Games.csv")
#read in player info
Players <- read_csv("C:/Users/Albert/Desktop/stat301/Final Project/Players.csv")

View(Seasons_Stats)
View(Games)
  View(Players)
View(Seasons_Stats %>%
       arrange(desc(Year)))


#total fouls each year
#teams played 82 games starting in 1966
Seasons_Stats %>%
  group_by(Year) %>%
  filter(Year > 1966) %>%
  summarize(tot_pf = sum(PF)) %>%
  ggplot(mapping = aes(x = Year, y = tot_pf)) +
  geom_point() +
  geom_smooth()

#total fta each year
Seasons_Stats %>%
  group_by(Year) %>%
  summarize(tot_fta = sum(FTA)) %>%
  ggplot(mapping = aes(x = Year, y = tot_fta)) +
  geom_point() +
  geom_smooth()

#total fta each year
Seasons_Stats %>%
  mutate(FTA_per_min = FTA / MP) %>%
  group_by(Year) %>%
  summarize(FTA_per_min_year = mean(FTA_per_min)) %>%
  ggplot(mapping = aes(x = Year, y = FTA_per_min_year)) +
  geom_point() +
  geom_smooth()

#average free throws attempted per game (total number of games played in a season)
Seasons_Stats %>%
  group_by(Year) %>%
  summarize(tot_FTA = sum(FTA)) %>%
  left_join(Games, by = "Year") %>%
  mutate(FTA_per_game = tot_FTA / tot_games) %>%
  ggplot(mapping = aes(x = Year, y = FTA_per_game)) +
  geom_point() +
  geom_smooth()

#average personal fouls called per game (total number of games played in a season)
Seasons_Stats %>%
  group_by(Year) %>%
  summarize(tot_PF = sum(PF)) %>%
  left_join(Games, by = "Year") %>%
  mutate(PF_per_game = tot_PF / tot_games) %>%
  ggplot(mapping = aes(x = Year, y = PF_per_game)) +
  geom_point() +
  geom_smooth()


#########################

#number of 3 pointers

#How many 3 pointers were attempted for different positions over the year?

#converting character column to numeric column
Seasons_Stats$`3PA` <- as.numeric(as.character(Seasons_Stats$`3PA`))
Seasons_Stats$`3P` <- as.numeric(as.character(Seasons_Stats$`3P`))

#average 3 pointers attempted per game, season by season, by position

#three pointers attempted, for all players
Seasons_Stats %>%
  filter(Year > 1979) %>%
  group_by(Year) %>% 
  summarize(tot_3PA = sum(`3PA`)) %>%
  left_join(Games, by = "Year") %>%
  mutate(`3PA_per_game` = tot_3PA / tot_games) %>%
  ggplot(mapping = aes(x = Year, y = `3PA_per_game`)) +
  geom_point() +
  geom_smooth()

#by position
threepointpos <- function(position) {
  Seasons_Stats %>%
    filter(Year > 1979) %>%
    filter(Pos == position) %>%
    group_by(Year) %>% 
    summarize(tot_3PA = sum(`3PA`)) %>%
    left_join(Games, by = "Year") %>%
    mutate(`3PA_per_game` = tot_3PA / tot_games) %>%
    ggplot(mapping = aes(x = Year, y = `3PA_per_game`)) +
    geom_point() +
    geom_smooth()
}
  
threepointpos("C")
threepointpos("PF")
threepointpos("SF")
threepointpos("SG")
threepointpos("PG")

#change in 3 point percentage each season by position over time

#all players
Seasons_Stats %>%
  filter(Year > 1979) %>%
  group_by(Year) %>% 
  summarize(tot_3PA = sum(`3PA`),
            tot_3PM = sum(`3P`)) %>%
  left_join(Games, by = "Year") %>%
  mutate(`3P%` = tot_3PM / tot_3PA) %>%
  ggplot(mapping = aes(x = Year, y = `3P%`)) +
  geom_point() +
  geom_smooth()

#by position
threepointperc <- function(position) {
  Seasons_Stats %>%
    filter(Year > 1979) %>%
    filter(Pos == position) %>%
    group_by(Year) %>% 
    summarize(tot_3PA = sum(`3PA`),
              tot_3PM = sum(`3P`)) %>%
    left_join(Games, by = "Year") %>%
    mutate(`3P%` = tot_3PM / tot_3PA) %>%
    ggplot(mapping = aes(x = Year, y = `3P%`)) +
    geom_point() +
    geom_smooth()
}

threepointperc("C")
threepointperc("PF")
threepointperc("SF")
threepointperc("SG")
threepointperc("PG")


#########################

#which school produces players with higher PER, win shares?

#PER

#average PER
PER_stats <- Seasons_Stats %>%
  left_join(Players, by = "Player") %>%
  group_by(collage) %>%
  summarize(med_PER = median(PER, na.rm = TRUE)) %>%
  arrange(desc(med_PER))

#how many total player seasons are there
num_seasons <- Seasons_Stats %>%
  left_join(Players, by = "Player") %>%
  group_by(collage) %>%
  count()
sum(num_seasons$n)
#this equals to 24961
count(Seasons_Stats)

#find proportion of seasons from each college
prop_seasons <- Seasons_Stats %>%
  left_join(Players, by = "Player") %>%
  group_by(collage) %>%
  count() %>%
  mutate(prop_seasons = n / 24961)

#join back to average PER
PER_stats %>%
  left_join(prop_seasons, by = "collage") %>%
  mutate(wPER = med_PER * prop_seasons) %>%
  arrange(desc(wPER)) %>% View()


#win shares

WS_stats <- Seasons_Stats %>%
  left_join(Players, by = "Player") %>%
  group_by(collage) %>%
  summarize(med_WS = median(WS, na.rm = TRUE)) %>%
  arrange(desc(med_WS))

WS_stats %>%
  left_join(prop_seasons, by = "collage") %>%
  mutate(wWS = med_WS * prop_seasons) %>%
  arrange(desc(wWS)) %>% View()



