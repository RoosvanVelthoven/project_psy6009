# The NBA schedule and results, of basketball-reference.com, are scrapped below
# for the 2016-17, 2017-18, 2018-19, 2019-20 and 2020-21 seasons

# Packages
library(rvest)
library(xml2)
library(data.table)
library(here)
library(dplyr)
library(lubridate)
library(XML)
library(stringr)
library(tidyverse)

# Note:
# The NBA box score URLs are also scrapped 
# (the NBA box score URLs are utilized to obtain total basic box scores 
# (please see total_basic_box_scores.R for further information))

# Step one: Create the NBA schedule and results URLs

# Create a years and a months variable

# Note:
# The years and months variables are utilized for the NBA schedule and results URLs
# See for example https://www.basketball-reference.com/leagues/NBA_2017_games-october.html
# 2017 stands for the 2016-17 season
# October stands for the month

years <- c(
  "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017", "2017",
  "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018",
  "2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019",
  "2020", "2020", "2020", "2020", "2020", "2020", "2020", "2020", "2020", "2020",
  "2021", "2021", "2021", "2021", "2021", "2021", "2021", "2021"
)

# Note: 
# We need to repeat each year multiple times as each month has a separate URL
# 2017 = 9
# 2018 = 9
# 2019 = 9
# 2020 = 10
# 2021 = 8

# Check to see if we have the correct number of years
table(years)
## years
## 2017 2018 2019 2020 2021
## 9    9    9   10    8

# We have the correct number of years, lets continue. 

months <- c(
  "october", "november", "december", "january", "february",
  "march", "april", "may", "june",
  "october", "november", "december", "january", "february",
  "march", "april", "may", "june",
  "october", "november", "december", "january", "february",
  "march", "april", "may", "june",
  "october-2019", "november", "december", "january", "february",
  "march", "july", "august", "september", "october-2020",
  "december", "january", "february", "march", "april", "may", "june",
  "july"
)

# Note: 
# Due to the COVID-19 pandemic two URLs are slightly different 
# We have october-2019 and october-2020
# October = 3
# October-2019 = 1
# October-2020 = 1
# November = 4
# December = 5
# January = 5
# February = 5
# March = 5
# April = 4
# May = 4
# June = 4
# July = 2
# August = 1
# September = 1

# Check to see if we have the correct number of months
table(months)
## months
## april       august     december     february      january         july         june
## 4            1            5            5            5            2            4
## march          may     november      october october-2019 october-2020    september
## 5            4            4            3            1            1            1

# We have the correct number of months, lets continue.

# Create a df of months
df <- as.data.frame(months)

# Add the other URL elements
df$months <- paste0( # paste0 ensures that there are no spaces in our URLs
  "https://www.basketball-reference.com/leagues/NBA_", years, # years adds in our years variable
  "_games-", df$months, ".html" # df$months adds in our months 
)

# Note if you look at the df you can see the NBA schedule and results URLs
# for the 2016-17, 2017-18, 2018-19, 2019-20 and 2020-21 seasons

# Step two: download the NBA schedule and results HTMLs

# Assign df$months to an object (urls)
urls <- df$months

# Download HTMLs
for (url in urls) {   # the for loop function loops through each of our URLs and downloads them
  download_html(url)

  # Create an HTML file list
  htmls <- list.files("...", pattern = ".html") # htmls will create a list of all files in our directory that end in .html
}

# Note this may take some time
# If you wish to see the progress please click on the files tab
# You should be able to see each HTML, once it has been downloaded

# Once our HTMLs are downloaded, we need to ensure that they are in the correct order

# Step three: ensure that the HTMLs remain in the predetermined order

# Create a new df with solely HTML names (utilize df)
df_htmls <- df %>%
  mutate_at("months", str_replace, "https://www.basketball-reference.com/leagues/", "")

# Assign the correctly ordered HTML names to an object
order_htmls <- df_htmls$months

# Obtain the file paths of the order_htmls object
paths <- here(order_htmls)

# create a 45 element vector (the 45 element vector will be used to rename the HTMLs)
a <- c(1:9)
a <- paste0("a0", a)
b <- c(10:45)
b <- paste0("a", b)
a <- as.data.frame(a)
b <- as.data.frame(b)
names(b) <- c("a") # rename the b column in the b data frame to a
numbers <- rbind(a, b) # bind data frame a and b

# Paste .html behind the numbers
numbers$a <- paste0(numbers$a, ".html")

# Assign numbers$a to an object
numbers_htmls <- numbers$a

# Rename the HTMLs (and ensure that they are renamed in the correct order)
for (i in 1:45) {
  file.rename(paths[i], numbers_htmls[i])
}

# Step four: scrape the data

# Create an HTML file list
html_list <- list.files("...", pattern = ".html")

# Use the function readLines to read all text lines of the HTMLs (and assign them to an object)
# Note:
# This is needed for our NBA schedule and results tables
urltxt <- lapply(html_list, function(x) try(readLines(x)))

# Use the function read_html to read all HTMLs (and assign them to an object)
# Note:
# This is needed for our box score URLs
webpages <- lapply(html_list, function(x) try(read_html(x)))

# Parse the urltxt
doc <- htmlParse(urltxt)

# Note this may take some time

# Retrieve all <table> tags
tables <- xpathApply(doc, "//table")

# Read in the tables of interest
schedule_results <- lapply(tables, function(i) readHTMLTable(i))

# Create a df from schedule_results
schedule_and_results_2016_2020 <- data.table::rbindlist(schedule_results)

# Step five: save schedule_and_results_2016_2020 in the raw data folder
write.csv(schedule_and_results_2016_2020, file = here("data", "raw", "schedule_and_results_2016_2020.csv"))

# Step six: obtain box score URLs

# Create an empty box_score_urls df
box_score_urls <- data.frame()

# Create a for loop to loop over the web pages
for (webpage in webpages) {

  # Code to extract box score URLs
  boxscore_links <- webpage %>%
    html_nodes("table#schedule > tbody > tr > td > a") %>% # this line of code tells R where to look for the box score URLs (in each html)
    html_attr("href") %>% # this line of code tells R to retrieve href attributes (each href attribute contains a partial box score URL)
    paste("https://www.basketball-reference.com", ., sep = "") # paste "https://www.basketball-reference.com" in front of the partial box scores to obtain complete box score URLs

  # Create a df using boxscore_links
  urls_df <- as.data.frame(boxscore_links)

  # rbind box_score_urls with urls_df (this is necessary to obtain box score URLs for every season, month and game)
  box_score_urls <- rbind(box_score_urls, urls_df)
  
}

# Step seven: save box_score_urls in the raw data folder
write.csv(box_score_urls, file = here("data", "raw", "box_score_urls_2016_2020.csv"))

# step eight: clean-up!

# Remove HTMLs from our folder
for (i in 1:45) {
  unlink(html_list)
}

##### ----
# The schedule_and_results_2016_2020 data is pre-processed below

# Clean the environment
rm(list = ls())

# Read in schedule_and_results_2016_2020.csv
df_2016_2020 <- read.csv(here("data", "raw", "schedule_and_results_2016_2020.csv"), row.names = "X")

# Rename the columns
names(df_2016_2020) <- c(
  "date", "start_time", "away_team", "points_away",
  "home_team", "points_home", "box_score", "overtime",
  "attendance", "remarks"
)

# Select the columns of interest
df_2016_2020 <- df_2016_2020 %>%
  select(date, away_team, points_away, home_team, points_home, attendance)

# Remove "Playoffs" rows: 1231, 2541 and 3854
df_2016_2020 <- df_2016_2020[-c(1231, 2541, 3854), ]

# Ensure that each column has the correct class
df_2016_2020$date <- mdy(df_2016_2020$date)
df_2016_2020$attendance <- as.numeric(gsub(",", "", df_2016_2020$attendance))

# Code to add a column to indicate a home win
df_2016_2020 <- df_2016_2020 %>%
  add_column(
    home_win = if_else(df_2016_2020$points_away < df_2016_2020$points_home, 1, 0),
    .after = "attendance"
  )

# Change NAs in attendance to zero
df_2016_2020$attendance[is.na(df_2016_2020$attendance)] <- 0

# Most of the NAs where during the 2019 bubble time period
# Seven of the NAs where in the 2020-2021 season
# nba.com was utilized to double check if attendance was zero for each NA attendance game (in the 2020-21 season)

# Create point_difference column
df_2016_2020$point_difference <- (df_2016_2020$points_home - df_2016_2020$points_away)

# Create point difference column based on absolute values
df_2016_2020$abs_point_dif <- abs(df_2016_2020$points_home - df_2016_2020$points_away)

# Create a function to separated the NBA seasons
myfunc_dates <- function(x, y) {
  df_2016_2020[df_2016_2020$date >= x & df_2016_2020$date <= y, ]
}

# Create separated dfs for each season
df_2016_17 <- myfunc_dates(as.Date("2016-10-25"), as.Date("2017-06-12"))
df_2017_18 <- myfunc_dates(as.Date("2017-10-17"), as.Date("2018-06-08"))
df_2018_19 <- myfunc_dates(as.Date("2018-10-16"), as.Date("2019-06-13"))
df_2019_20 <- myfunc_dates(as.Date("2019-10-22"), as.Date("2020-10-11"))
df_2020_21 <- myfunc_dates(as.Date("2020-12-22"), as.Date("2021-07-22"))

# Add in a season column to each df
df_2016_17$season <- "2016-17"
df_2017_18$season <- "2017-18"
df_2018_19$season <- "2018-19"
df_2019_20$season <- "2019-20"
df_2020_21$season <- "2020-21"

# rbind the seperate dfs
df_2016_2020 <- rbind(df_2016_17, df_2017_18, df_2018_19, df_2019_20, df_2020_21)

# Create win percentage column
df_2016_2020 <- df_2016_2020 %>% group_by(home_team, season) %>% mutate(win_percentage = mean(home_win) * 100)

# Save df_2016_2020 in the processed folder
write.csv(df_2016_2020, file = here("data", "processed", "schedule_and_results_2016_2020.csv"))

##### ----
# The box_score_urls_2016_2020 data is pre-processed below

# Clean the environment
rm(list = ls())

# Read in box_score_urls_2016_2020.csv
box_score_urls <- read.csv(here("data", "raw", "box_score_urls_2016_2020.csv"), row.names = "X")

# Step one: separate boxscores from teams URLs

# Create a box scores object
boxscores <- box_score_urls[!grepl(
  "https://www.basketball-reference.com/teams/",
  box_score_urls$boxscore_links
), ]
# Create a teams object
teams <- box_score_urls[!grepl(
  "https://www.basketball-reference.com/boxscores/",
  box_score_urls$boxscore_links
), ]

# Create dfs for box scores and teams
boxscores <- as.data.frame(boxscores)
teams <- as.data.frame(teams)

# Step two: separate home and away team URLs

# odd = away team; even = home team)
row_odd <- seq_len(nrow(teams)) %% 2

# Create a data_row_odd object
data_row_odd <- teams[row_odd == 1, ]

# Create a data_row_even object
data_row_even <- teams[row_odd == 0, ]

# Create dfs for data_row_odd data_row_even
data_row_odd <- as.data.frame(data_row_odd)
data_row_even <- as.data.frame(data_row_even)

# cbind boxscores, data_row_even and data_row_odd
box_scores <- cbind(boxscores, data_row_even, data_row_odd)

# Remove unnecessary details
box_scores <- box_scores %>%
  mutate_at("data_row_even", str_replace, "https://www.basketball-reference.com/teams/", "")

box_scores <- box_scores %>%
  mutate_at("data_row_odd", str_replace, "https://www.basketball-reference.com/teams/", "")

removes <- c("data_row_even", "data_row_odd")

for (remove in removes) {
  box_scores <- box_scores %>%
    mutate_at(remove, str_replace, "/2017.html", "")
  box_scores <- box_scores %>%
    mutate_at(remove, str_replace, "/2018.html", "")
  box_scores <- box_scores %>%
    mutate_at(remove, str_replace, "/2019.html", "")
  box_scores <- box_scores %>%
    mutate_at(remove, str_replace, "/2020.html", "")
  box_scores <- box_scores %>%
    mutate_at(remove, str_replace, "/2021.html", "")
}

# Assign new column names
names(box_scores) <- c("urls", "home", "away")

# Step three: save box_scores
write.csv(box_scores, file = here("data", "processed", "box_score_urls_2016_2020.csv"))