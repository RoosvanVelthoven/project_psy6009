# Code to read in games.csv
df_2016_2021 <- read.csv(here("data", "processed", "schedules_and_results_of_2016_2021.csv"))

# Code to specify which variables to keep
df_2016_2021 <- df_2016_2021  %>% 
  select(points_home, points_away, home_win, year) 

# Code to recode home_win values true to won and false to lost
df_2016_2021[df_2016_2021 == "TRUE"] <- "1"
df_2016_2021[df_2016_2021 == "FALSE"] <- "0"

df_2016_2021$home_win <- as.numeric(df_2016_2021$home_win)

# Code to save df_2016_2021 as a csv file (I need this later for my min and max values)
write.csv(df_2016_2021, file = here("data", "processed", "df_2016_2021.csv"))

# Code to create seperate dfs
nba16 <- df_2016_2021[ !(df_2016_2021$year %in% c(2020:2017)), ]
nba17 <- df_2016_2021[ !(df_2016_2021$year %in% c(2020:2018, 2016)), ]
nba18 <- df_2016_2021[ !(df_2016_2021$year %in% c(2020:2019, 2017:2016)), ]
nba19 <- df_2016_2021[ !(df_2016_2021$year %in% c(2020, 2018:2016)), ]
nba20 <- df_2016_2021[ !(df_2016_2021$year %in% c(2019:2016)), ]

# Code to rename columns for easy labelling when data frames are combined
nba16 <- nba16 %>% rename(points_home_16 = "points_home", 
                          points_away_16 = "points_away", 
                          year_16 = "year", 
                          home_win_16 = "home_win")

nba17 <- nba17 %>% rename(points_home_17 = "points_home", 
                          points_away_17 = "points_away", 
                          year_17 = "year", 
                          home_win_17 = "home_win")

nba18 <- nba18 %>% rename(points_home_18 = "points_home", 
                          points_away_18 = "points_away", 
                          year_18 = "year", 
                          home_win_18 = "home_win")

nba19 <- nba19 %>% rename(points_home_19 = "points_home", 
                          points_away_19 = "points_away", 
                          year_19 = "year", 
                          home_win_19 = "home_win")

nba20 <- nba20 %>% rename(points_home_20 = "points_home", 
                          points_away_20 = "points_away", 
                          year_20 = "year", 
                          home_win_20 = "home_win")

# Code to add ID numbers, I need specific keys to merge my data frames
nba16 <- nba16 %>% mutate(id = row_number())
nba17 <- nba17 %>% mutate(id = row_number())
nba18 <- nba18 %>% mutate(id = row_number())
nba19 <- nba19 %>% mutate(id = row_number())
nba20 <- nba20 %>% mutate(id = row_number())

# Each data frame has a different number of games, to merge the data frames without
# losing data, I need to do a full(outer) join. 

# Code to merge dfs, I can only merge two data frames at a time
full16_17 <- merge(nba16, nba17, by = "id", all = TRUE)
full18_19 <- merge(nba18, nba19, by = "id", all = TRUE)
full16_19 <- merge(full16_17, full18_19, by = "id", all = TRUE)
full16_20 <- merge(full16_19, nba20, by = "id", all = TRUE)

# Time to rename my data frame to reduce confusion
nba_final_df <- full16_20 

table(nba_final_df$home_win_16)
home_wins <- 763
away_wins <- 546
total <- home_wins + away_wins
percent_1 <- total/100
home_wins_2016 <- home_wins/percent_1
## [1] 58.28877
away_wins_2016 <- away_wins/percent_1
## [1] 41.71123

table(nba_final_df$home_win_17)
home_wins <- 770
away_wins <- 542
total <- home_wins + away_wins
percent_1 <- total/100
home_wins_2017 <- home_wins/percent_1
## [1] 58.68902
away_wins_2017 <- away_wins/percent_1
## [1] 41.31098

table(nba_final_df$home_win_18)
home_wins <- 775
away_wins <- 537
total <- home_wins + away_wins
percent_1 <- total/100
home_wins_2018 <- home_wins/percent_1
## [1] 59.07012
away_wins_2018 <- away_wins/percent_1
## [1] 40.92988

table(nba_final_df$home_win_19)
home_wins <- 625
away_wins <- 518
total <- home_wins + away_wins
percent_1 <- total/100
home_wins_2019 <- home_wins/percent_1
## [1] 54.68066
away_wins_2019 <- away_wins/percent_1
## [1] 45.31934

table(nba_final_df$home_win_20)
home_wins <- 639
away_wins <- 531
total <- home_wins + away_wins
percent_1 <- total/100
home_wins_2020 <- home_wins/percent_1
## [1] 54.61538
away_wins_2020 <- away_wins/percent_1
## [1] 45.38462


home_wins <- c(home_wins_2016, home_wins_2017, home_wins_2018, home_wins_2019, home_wins_2020)
away_wins <- c(away_wins_2016, away_wins_2017, away_wins_2018, away_wins_2019, away_wins_2020)

home_wins <- as.data.frame(home_wins)
away_wins <- as.data.frame(away_wins)

wins <- cbind(home_wins, away_wins)

season <- c(2016, 2017, 2018, 2019, 2020)
season <- as.data.frame(season)

wins <- cbind(wins, season)

wins_plot <- 
  ggplot(data = wins,
         mapping = aes_string (x = wins$home_wins,
                               # First lists of variables to iterate over
                               y = wins$away_wins,
                               colour = wins$season))

p <- wins_plot + geom_point(size = 5) +
  labs(x = "Home team win percentage", y  = "Away team win percentage")

p
