# The total basic box scores of the 2016-17, 2017-18, 2018-19, 2019-20 and 2020-21 NBA seasons
# are scrapped below 

# Packages
library(here)
library(foreach)
library(rvest)
library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate)

# Note: the total basic box scores for the home and away teams are scraped separately 

# Step one: set up your box_score_urls data frame

# Read in box_score_urls_2016_2020.csv (from the data processed folder)
box_score_urls <-
  read.csv(here("data", "processed", "box_score_urls_2016_2020.csv"), row.names = "X")

# Tell R where to look for the data (i.e., provide total html_nodes information for the home and the away team)
box_score_urls$totals_home <-
  paste0("table#box-", box_score_urls$home, "-game-basic > tfoot > tr > td")
box_score_urls$totals_away <-
  paste0("table#box-", box_score_urls$away, "-game-basic > tfoot > tr > td")

# Code for the home team

# Create an empty df for the total basic box scores of the home teams
basic_home <- data.frame()

# Assign URLs and totals_home to their own objects 
urls <- box_score_urls$urls
totals_home <- box_score_urls$totals_home

# Create a foreach loop

# WARNING THE FIRST FOREACH LOOP MAY TAKE A LONG TIME
# IT TOOK +45 MINUTES
# on a Windows 10 PC 
# processor: Intel(R) Core(TM) i5-7400
# RAM: 8 GB (2 X 4GB) DDR4 2133MHz
# graphic card: NVIDIA GeForce GTX 1060 3GB

foreach(                  # the foreach packages allows us to iterate over multiple lists (urls and totals_home) of the same length simultaneously
  i = urls,               # the foreach packages is needed as each total basic box score table requires a different html_nodes() input (i.e., a different row in the data)
  j = totals_home
) %do% {
  html <- session(i)
  totals_home <- html_nodes(html, j) %>% html_text() # code to tell R we want to extract the text
  
  totals_home <- data.frame(totals_home) # code to create a totals_home df
  totals_home <- as.data.frame(t(totals_home)) # code to convert our totals_home column to a row
  
    basic_home <- rbind(basic_home, totals_home) # code to rbind basic_home and totals_home
}

# Assign new column names
names(basic_home) <- c("mp", "fg", "fga", "fg_pct", "fg3", "fg3a", "fg3_pct", "ft",
                       "fta", "ft_pct", "orb", "drb", "trb", "ast", "stl", "blk", 
                       "tov", "pf", "pts", "plus_minus")

# Save basic_home
write.csv(basic_home, file = here("data", "raw", "total_basic_box_scores_home.csv"))

# Code for the away team

# Create an empty df for the total basic box scores of the away teams
basic_away <- data.frame()

# Assign totals_away to its own objects 
totals_away <- box_score_urls$totals_away

# Create a foreach loop

# WARNING THE SECOND FOREACH LOOP MAY TAKE A LONG TIME
# IT TOOK +15 MINUTES
# on a Windows 10 PC 
# processor: Intel(R) Core(TM) i5-7400
# RAM: 8 GB (2 X 4GB) DDR4 2133MHz
# graphic card: NVIDIA GeForce GTX 1060 3GB

foreach(                  # the foreach packages allows us to iterate over multiple lists (urls and totals_away) of the same length simultaneously
  i = urls,               # the foreach packages is needed as each total basic box score table requires a different html_nodes() input (i.e., a different row in the data)
  k = totals_away
) %do% {
  html <- session(i)
  totals_away <- html_nodes(html, k) %>% html_text() # code to tell R we want to extract the text
  
  totals_away <- data.frame(totals_away) # code to create a totals_away df
  totals_away <- as.data.frame(t(totals_away)) # code to convert our totals_away column to a row
  
  basic_away <- rbind(basic_away, totals_away) # code to rbind basic_away and totals_away
}

# Assign new column names
names(basic_away) <- c("mp", "fg", "fga", "fg_pct", "fg3", "fg3a", "fg3_pct", "ft",
                       "fta", "ft_pct", "orb", "drb", "trb", "ast", "stl", "blk", 
                       "tov", "pf", "pts", "plus_minus")

# Save basic_away
write.csv(basic_away, file = here("data", "raw", "total_basic_box_scores_away.csv"))

##### ----
# The schedule_and_results and total_basic_box_score data is pre-processed below

# Clean the environment
rm(list = ls())

# Read in the processed schedule_and_results_2016_2020.csv 
schedule_and_results <- read.csv(here("data", "processed", "schedule_and_results_2016_2020.csv"), row.names = "X")

# Create two dfs with the columns of schedule_and_results that require stacking
stack_team <- select(schedule_and_results, home_team, away_team)
stack_points <- select(schedule_and_results, points_home, points_away)

# Stack home_team and away_team and points_home and points_away
stack_team <- data.frame(team  = c(stack_team [,"home_team"], stack_team [,"away_team"]))
stack_points <- data.frame(points = c(stack_points [,"points_home"], stack_points [,"points_away"]))

# Remove stacking columns (in schedule_and_results)
schedule_and_results <- select(schedule_and_results, -home_team, -away_team, -points_home, -points_away)

# Read in the raw basic_box_score data
box_home <- read.csv(here("data", "raw", "total_basic_box_scores_home.csv"), row.names = "X")
box_away <- read.csv(here("data", "raw", "total_basic_box_scores_away.csv"), row.names = "X")

# Remove the unneeded columns
box_home <- select(box_home, -mp, -pts, -plus_minus)
box_away <- select(box_away, -mp, -pts, -plus_minus)

# Create a venue column for box_home and box_away
box_home <- cbind(venue = "Home", box_home)
box_away <- cbind(venue = "Away", box_away)

# cbind schedule_and_results and box_home
box_home <-  cbind(schedule_and_results, box_home)

# cbind schedule_and_results and box_away
box_away <-  cbind(schedule_and_results, box_away)

# rbind box_home and box_away
box <- rbind(box_home, box_away)

# cbind box, stack_points and stack_team
box <- cbind(box, stack_points, stack_team)

# Separate the pre-bubble, bubble and post-bubble games

# Create a bubble function
myfunc_bubble <- function(x, y) {
  box[box$date >= x & box$date <= y, ]
}

# create three separate dfs: pre_bubble, bubble, post_bubble
pre_bubble <- myfunc_bubble(as.Date("2016-10-25"), as.Date("2020-03-11"))
bubble <- myfunc_bubble(as.Date("2020-07-30"), as.Date("2020-10-11"))
post_bubble <- myfunc_bubble(as.Date("2020-12-22"), as.Date("2021-07-20"))

# Add bubble columns
pre_bubble <- data.frame(append(pre_bubble, c(bubble = "Pre-bubble"), after = 1))
bubble <- data.frame(append(bubble, c(bubble = "Bubble"), after = 1))
post_bubble <- data.frame(append(post_bubble, c(bubble = "Post-bubble"), after = 1))

# rbind the dfs 
box <- rbind(pre_bubble, bubble, post_bubble)

# Lets visualise the attendance data before we continue

# Retain the season, attendance and venue data and create a new df
box_attendance <- select(box, season, attendance, venue) 

# Filter by home venue
# Note: we could also have used venue == "Away" - this gives the same data (we are only looking at attendance)
box_attendance <- filter(box_attendance, venue == "Home")

# Separate the seasons
at_box_2016 <- filter(box_attendance, season == "2016-17")
at_box_2017 <- filter(box_attendance, season == "2017-18")
at_box_2018 <- filter(box_attendance, season == "2018-19")
at_box_2019 <- filter(box_attendance, season == "2019-20")
at_box_2020 <- filter(box_attendance, season == "2020-21")

# Plot the seasons

# Season 2016-17
p <- ggplot(at_box_2016, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

ggsave(here("figs", "at_2016-17.jpg")) # Code to save figure

# Season 2017-18
p <- ggplot(at_box_2017, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

ggsave(here("figs", "at_2017-18.jpg")) # Code to save figure

# Season 2018-19
p <- ggplot(at_box_2018, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

ggsave(here("figs", "at_2018-19.jpg")) # Code to save figure

# Season 2019-20
p <- ggplot(at_box_2019, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

ggsave(here("figs", "at_2019-20.jpg")) # Code to save figure

# The 2019-20 density plot demonstrates the impact of the COVID-19 pandemic
# (i.e., the NBA bubble)
# We have a peak at 0
# We have a very large peak on the right side - our normal crowd capacity

# Season 2020-21
p <- ggplot(at_box_2020, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

ggsave(here("figs", "at_2020-21.jpg")) # Code to save figure

# The 2020-21 density plot demonstrate the impact of the COVID-19 pandemic
# We have a peak at 0
# We have a small peak between 0 and 5000 - as crowd restrictions are slowly lifted
# We have a very small peak on the right side - our normal crowd capacity

# Attendance across 2016-2021
p <- ggplot(box_attendance, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

ggsave(here("figs", "at_density.jpg")) # Code to save figure

# Lets separate the pre-zero attendance games, the zero attendance games and 
# the post-zero attendance games

# Separate the seasons
box_2016 <- filter(box, season == "2016-17")
box_2017 <- filter(box, season == "2017-18")
box_2018 <- filter(box, season == "2018-19")
box_2019 <- filter(box, season == "2019-20")
box_2020 <- filter(box, season == "2020-21")

# Lets determine the maximum crowd capacity in the 2020-21 season
max(box_2020$attendance)
## [1] 18624

# In the 2020-21 season separate zero attendance data from data that has at
# least 10% of the maximum crowd capacity

# Create a 10% object
ten_perc <- max(box_2020$attendance)/10
## [1] 1862.4

# Retain zero and 10% games
box_2020 <- box_2020 %>% 
  mutate(zero = case_when(attendance == 0 ~ "True",       # 0 = no crowd
                          attendance > ten_perc ~ "Restricted", # 1 = at least 10% maximum crowd capacity
  )
  )

# Create seperate dfs
absent <- filter(box_2020, zero == "True")
spectators <- filter(box_2020, zero == "Restricted")

# rbind the dfs
box_2020 <- rbind(absent, spectators)

# Lets re-examine the density plot

# Filter by home venue
# Note: we could also have used venue == "Away" - this gives the same data (we are only looking at attendance)
box_2020_attendance <- filter(box_2020, venue == "Home")

# Attendance across the reduced 2020-2021 sample
p <- ggplot(box_2020_attendance, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

# The second peak is now more pronounced.  

ggsave(here("figs", "at_2020-21_new.jpg")) # Code to save figure

# Continue with the reduced sample

# Reorder the box_2020 columns
box_2020 <- box_2020[, c(1:2, 29, 3:28)]

# rbind box_2016, box_2017, box_2018 and box_2019
box <- rbind(box_2016, box_2017, box_2018, box_2019)

# Add a zero column
box <- data.frame(append(box, c(zero = "Pre-restrictions"), after = 2))

# rbind box and box_2020
box <- rbind(box, box_2020)

# Visualise the reduced data set as a whole

# Filter by home venue (we could also have chosen away, this gives the same result - we are looking at attendance data)
box_attendance <- filter(box, venue == "Home")

# Attendance across the reduced 2016-2021 sample
p <- ggplot(box_attendance, aes(x=attendance)) 

p + geom_density() +
  labs(x = "Attendance",
       y = "Density") +
  theme_bw() 

# The density plot clearly displays three peaks

ggsave(here("figs", "at_density_new.jpg")) # Code to save figure

# save box df
write.csv(box, file = here("data", "processed", "total_basic_box_scores.csv"))

#### ----
# Data preparation for the linear model and the generalised linear model

# Clean the environment
rm(list = ls())

# read in total_basic_box_scores.csv
box <- read.csv(here("data", "processed", "total_basic_box_scores.csv"), row.names = "X")

# Separate the seasons
box_2016 <- filter(box, season == "2016-17")
box_2017 <- filter(box, season == "2017-18")
box_2018 <- filter(box, season == "2018-19")
box_2019 <- filter(box, season == "2019-20")
box_2020 <- filter(box, season == "2020-21")

# filter the 2019 season by bubble and assign bubble and pre-bubble to different dfs
bubble <- filter(box_2019, bubble == "Bubble")
pre_covid <- filter(box_2019, bubble == "Pre-bubble")

# Change the season value of the bubble df to 2019-21
bubble$season[bubble$season=="2019-20"] <- "2019-21"

# rbind pre_covid and bubble
box_2019 <- rbind(pre_covid, bubble)

# filter the 2020 season by zero and assign true and restricted to different dfs
absent <- filter(box_2020, zero == "True")
spectators <- filter(box_2020, zero == "Restricted")

# Change the season value of the absent df to 2019-21
absent$season[absent$season=="2020-21"] <- "2019-21"

# rbind absent and spectators
box_2020 <- rbind(absent, spectators)

# rbind the seasons
box <- rbind(box_2016, box_2017, box_2018, box_2019, box_2020)

# Check if we have the correct data
table(box$season)
## 2016-17 2017-18 2018-19 2019-20 2019-21 2020-21 
## 2618    2624    2624    1942    1504     956

# save box
write.csv(box, file = here("data", "processed", "total_box_scores_glm_lm.csv"))