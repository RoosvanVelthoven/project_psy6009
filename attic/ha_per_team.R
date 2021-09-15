library(dplyr)
library(ggplot2)

# 2016
basic_home_box_2016 <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2016.csv"), row.names = "X")

basic_home_box_2016_ha <- basic_home_box_2016 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

ggplot(basic_home_box_2016_ha, aes(team, win_perc, fill = team)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Team",
       y = "Win Percentage",
       title = "HA, per Team, in the 2016/17 Season")

# 2017
basic_home_box_2017 <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2017.csv"), row.names = "X")

basic_home_box_2017_ha <- basic_home_box_2017 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

ggplot(basic_home_box_2017_ha, aes(team, win_perc, fill = team)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Team",
       y = "Win Percentage",
       title = "HA, per Team, in the 2017/18 Season")

# 2018
basic_home_box_2018 <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2018.csv"), row.names = "X")

basic_home_box_2018_ha <- basic_home_box_2018 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

ggplot(basic_home_box_2018_ha, aes(team, win_perc, fill = team)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Team",
       y = "Win Percentage",
       title = "HA, per Team, in the 2018/19 Season")

# 2019
basic_home_box_2019 <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2019.csv"), row.names = "X")

basic_home_box_2019_ha <- basic_home_box_2019 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

ggplot(basic_home_box_2019_ha, aes(team, win_perc, fill = team)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Team",
       y = "Win Percentage",
       title = "HA, per Team, in the 2019/20 Season")

# 2020
basic_home_box_2020 <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2020.csv"), row.names = "X")

basic_home_box_2020_ha <- basic_home_box_2020 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

ggplot(basic_home_box_2020_ha, aes(team, win_perc, fill = team)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Team",
       y = "Win Percentage",
       title = "HA, per Team, in the 2020/21 Season")