# packages
library(here)
library(foreach)
library(rvest)
library(tidyverse)
library(data.table)
library(dplyr)
library(lubridate)

# The basic box scores of the 2016-17, 2017-18, 2018-19, 2019-20 and 2020-21 NBA seasons
# are scrapped below 
# Note one: the basic box scores of each season are separately scraped

# Step one: set up our box_score_urls data frame

# Read in box_score_urls_2016_2020.csv
box_score_urls <-
  read.csv(here("data", "processed", "box_score_urls_2016_2020.csv"), row.names = "X")

# Add in html_nodes information (html_nodes information tells r where to look for the data)

# Names (titles of the tables)
box_score_urls$names_home <-
  paste0("table#box-", box_score_urls$home, "-game-basic > thead > tr > th")
box_score_urls$names_away <-
  paste0("table#box-", box_score_urls$away, "-game-basic > thead > tr > th")

# Players (names of the players)
box_score_urls$players_home <-
  paste0(
    "table#box-",
    box_score_urls$home,
    "-game-basic > tbody > tr > th > a"
  )
box_score_urls$players_away <-
  paste0(
    "table#box-",
    box_score_urls$away,
    "-game-basic > tbody > tr > th > a"
  )

# Remaining information (all other information in the table (except the totals))
box_score_urls$remainers_home <-
  paste0("table#box-", box_score_urls$home, "-game-basic > tbody > tr > td")
box_score_urls$remainers_away <-
  paste0("table#box-", box_score_urls$away, "-game-basic > tbody > tr > td")

# Totals (the totals of each column)
box_score_urls$totals_home <-
  paste0("table#box-", box_score_urls$home, "-game-basic > tfoot > tr > td")
box_score_urls$totals_away <-
  paste0("table#box-", box_score_urls$away, "-game-basic > tfoot > tr > td")

###               ###
### code 2016/17  ###
###               ###

# note two: for each season separate objects are created to ensure that solely data of that season is utilized

# note three: for each season separate dfs will be created for home and away teams

# note four: the steps to obtain basic box scores for home and away teams are non-dissimilar
# solely the home code of the 2016/17 season will contain commentary  

# home
urls <- box_score_urls$urls[1:1309]
names_home <- box_score_urls$names_home[1:1309]
players_home <- box_score_urls$players_home[1:1309]
remainers_home <- box_score_urls$remainers_home[1:1309]
totals_home <- box_score_urls$totals_home[1:1309]

basic_home <- data.frame() # code to create an empty df for home basic box scores

foreach(                  # the foreach packages allows us to iterate over multiple lists of the same length simultaneously
  i = urls,               # the foreach packages is needed as each basic box score table requires a different html_nodes() input
  j = names_home,
  k = players_home,
  l = remainers_home,
  m = totals_home
) %do% {
  html <- session(i)
  names_home <- html_nodes(html, j) %>% html_attr("data-stat") # code to tell r to retrieve the html attribute named data-stat 
  players_home <- html_nodes(html, k) %>% html_text() # code to tell r to retrieve the raw text
  remainers_home <- html_nodes(html, l) %>% html_text()
  totals_home <- html_nodes(html, m) %>% html_text()

  names_home <- names_home[!is.na(names_home)] # code to remove nas in names_home
  names_home <- names_home[-1:-2] # code to remove the first two values of names_home

  players_home_df <- as.data.frame(players_home) # code to create a players_home df
  players_home_df <-
    tibble::rowid_to_column(players_home_df, "merge_id") # code to add a merge_id column

  remainers_home <-
    remainers_home[remainers_home != "Did Not Play"] # code to remove Did Not Plays in remainers_home
  remainers_home <-
    remainers_home[remainers_home != "Did Not Dress"] 
  remainers_home <-
    remainers_home[remainers_home != "Player Suspended"] 
  remainers_home <-
    remainers_home[remainers_home != "Not With Team"] 
  remainers_home <-
    matrix(remainers_home, ncol = 20, byrow = TRUE) # code to create a matrix from our remaining information
  remainers_home_df <- as.data.frame(remainers_home) 
  remainers_home_df <-
    tibble::rowid_to_column(remainers_home_df, "merge_id")

  totals_home <- data.frame(totals_home)
  totals_home <- as.data.frame(t(totals_home)) # code to convert the totals_home column in a row

  # merge the players_home_df and the remainers_home_df
  jointdataset <-
    merge(players_home_df,
      remainers_home_df,
      by = "merge_id",
      all = TRUE
    )                        
  # remove the merge_id column
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_home_df <-
    tibble::rowid_to_column(totals_home, "merge_id")

  # add "players" to names_home
  names_home <- c("players", names_home)
  
  # code to set the column names of jointdataset to names_home
  names(jointdataset) <- names_home
  
  # code to set the column names of totals_home_df to names_home
  names(totals_home_df) <- names_home

  # rbind jointdataset and totals_home_df
  jointdataset <- rbind(jointdataset, totals_home_df)

  # rbind basic_home and jointdataset
  basic_home <- rbind(basic_home, jointdataset)
}

# create a home df
home_2016 <- basic_home 

# away
names_away <- box_score_urls$names_away[1:1309]
players_away <- box_score_urls$players_away[1:1309]
remainers_away <- box_score_urls$remainers_away[1:1309]
totals_away <- box_score_urls$totals_away[1:1309]

basic_away <- data.frame()

foreach(
  i = urls,
  j = names_away,
  k = players_away,
  l = remainers_away,
  m = totals_away
) %do% {
  html <- session(i)
  names_away <- html_nodes(html, j) %>% html_attr("data-stat")
  players_away <- html_nodes(html, k) %>% html_text()
  remainers_away <- html_nodes(html, l) %>% html_text()
  totals_away <- html_nodes(html, m) %>% html_text()

  names_away <- names_away[!is.na(names_away)]
  names_away <- names_away[-1:-2]

  players_away_df <- as.data.frame(players_away)
  players_away_df <-
    tibble::rowid_to_column(players_away_df, "merge_id")

  remainers_away <-
    remainers_away[remainers_away != "Did Not Play"]
  remainers_away <-
    remainers_away[remainers_away != "Did Not Dress"]
  remainers_away <-
    remainers_away[remainers_away != "Player Suspended"]
  remainers_away <-
    remainers_away[remainers_away != "Not With Team"]
  remainers_away <-
    matrix(remainers_away, ncol = 20, byrow = TRUE)
  remainers_away_df <- as.data.frame(remainers_away)
  remainers_away_df <-
    tibble::rowid_to_column(remainers_away_df, "merge_id")

  totals_away <- data.frame(totals_away)
  totals_away <- as.data.frame(t(totals_away))

  jointdataset <-
    merge(players_away_df,
      remainers_away_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_away_df <-
    tibble::rowid_to_column(totals_away, "merge_id")

  names_away <- c("players", names_away)
  names(jointdataset) <- names_away
  names(totals_away_df) <- names_away

  jointdataset <- rbind(jointdataset, totals_away_df)

  basic_away <- rbind(basic_away, jointdataset)
}

away_2016 <- basic_away

###               ###
### code 2017/18  ###
###               ###

# home
urls <- box_score_urls$urls[1310:2621]
names_home <- box_score_urls$names_home[1310:2621]
players_home <- box_score_urls$players_home[1310:2621]
remainers_home <- box_score_urls$remainers_home[1310:2621]
totals_home <- box_score_urls$totals_home[1310:2621]

basic_home <- data.frame()

foreach(
  i = urls,
  j = names_home,
  k = players_home,
  l = remainers_home,
  m = totals_home
) %do% {
  html <- session(i)
  names_home <- html_nodes(html, j) %>% html_attr("data-stat")
  players_home <- html_nodes(html, k) %>% html_text()
  remainers_home <- html_nodes(html, l) %>% html_text()
  totals_home <- html_nodes(html, m) %>% html_text()

  names_home <- names_home[!is.na(names_home)]
  names_home <- names_home[-1:-2]

  players_home_df <- as.data.frame(players_home)
  players_home_df <-
    tibble::rowid_to_column(players_home_df, "merge_id")

  remainers_home <-
    remainers_home[remainers_home != "Did Not Play"]
  remainers_home <-
    remainers_home[remainers_home != "Did Not Dress"] 
  remainers_home <-
    remainers_home[remainers_home != "Player Suspended"] 
  remainers_home <-
    remainers_home[remainers_home != "Not With Team"] 
  remainers_home <-
    matrix(remainers_home, ncol = 20, byrow = TRUE)
  remainers_home_df <- as.data.frame(remainers_home)
  remainers_home_df <-
    tibble::rowid_to_column(remainers_home_df, "merge_id")

  totals_home <- data.frame(totals_home)
  totals_home <- as.data.frame(t(totals_home))

  jointdataset <-
    merge(players_home_df,
      remainers_home_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_home_df <-
    tibble::rowid_to_column(totals_home, "merge_id")

  names_home <- c("players", names_home)
  names(jointdataset) <- names_home
  names(totals_home_df) <- names_home

  jointdataset <- rbind(jointdataset, totals_home_df)

  basic_home <- rbind(basic_home, jointdataset)
}

home_2017 <- basic_home 

# away
names_away <- box_score_urls$names_away[1310:2621]
players_away <- box_score_urls$players_away[1310:2621]
remainers_away <- box_score_urls$remainers_away[1310:2621]
totals_away <- box_score_urls$totals_away[1310:2621]

basic_away <- data.frame()

foreach(
  i = urls,
  j = names_away,
  k = players_away,
  l = remainers_away,
  m = totals_away
) %do% {
  html <- session(i)
  names_away <- html_nodes(html, j) %>% html_attr("data-stat")
  players_away <- html_nodes(html, k) %>% html_text()
  remainers_away <- html_nodes(html, l) %>% html_text()
  totals_away <- html_nodes(html, m) %>% html_text()

  names_away <- names_away[!is.na(names_away)]
  names_away <- names_away[-1:-2]

  players_away_df <- as.data.frame(players_away)
  players_away_df <-
    tibble::rowid_to_column(players_away_df, "merge_id")

  remainers_away <-
    remainers_away[remainers_away != "Did Not Play"]
  remainers_away <-
    remainers_away[remainers_away != "Did Not Dress"]
  remainers_away <-
    remainers_away[remainers_away != "Player Suspended"]
  remainers_away <-
    remainers_away[remainers_away != "Not With Team"]
  remainers_away <-
    matrix(remainers_away, ncol = 20, byrow = TRUE)
  remainers_away_df <- as.data.frame(remainers_away)
  remainers_away_df <-
    tibble::rowid_to_column(remainers_away_df, "merge_id")

  totals_away <- data.frame(totals_away)
  totals_away <- as.data.frame(t(totals_away))

  jointdataset <-
    merge(players_away_df,
      remainers_away_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_away_df <-
    tibble::rowid_to_column(totals_away, "merge_id")

  names_away <- c("players", names_away)
  names(jointdataset) <- names_away
  names(totals_away_df) <- names_away

  jointdataset <- rbind(jointdataset, totals_away_df)

  basic_away <- rbind(basic_away, jointdataset)
}

away_2017 <- basic_away

###               ###
### code 2018/19  ###
###               ###

# home
urls <- box_score_urls$urls[2622:3933]
names_home <- box_score_urls$names_home[2622:3933]
players_home <- box_score_urls$players_home[2622:3933]
remainers_home <- box_score_urls$remainers_home[2622:3933]
totals_home <- box_score_urls$totals_home[2622:3933]

basic_home <- data.frame()

foreach(
  i = urls,
  j = names_home,
  k = players_home,
  l = remainers_home,
  m = totals_home
) %do% {
  html <- session(i)
  names_home <- html_nodes(html, j) %>% html_attr("data-stat")
  players_home <- html_nodes(html, k) %>% html_text()
  remainers_home <- html_nodes(html, l) %>% html_text()
  totals_home <- html_nodes(html, m) %>% html_text()

  names_home <- names_home[!is.na(names_home)]
  names_home <- names_home[-1:-2]

  players_home_df <- as.data.frame(players_home)
  players_home_df <-
    tibble::rowid_to_column(players_home_df, "merge_id")

  remainers_home <-
    remainers_home[remainers_home != "Did Not Play"]
  remainers_home <-
    remainers_home[remainers_home != "Did Not Dress"] 
  remainers_home <-
    remainers_home[remainers_home != "Player Suspended"] 
  remainers_home <-
    remainers_home[remainers_home != "Not With Team"] 
  remainers_home <-
    matrix(remainers_home, ncol = 20, byrow = TRUE)
  remainers_home_df <- as.data.frame(remainers_home)
  remainers_home_df <-
    tibble::rowid_to_column(remainers_home_df, "merge_id")

  totals_home <- data.frame(totals_home)
  totals_home <- as.data.frame(t(totals_home))

  jointdataset <-
    merge(players_home_df,
      remainers_home_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_home_df <-
    tibble::rowid_to_column(totals_home, "merge_id")

  names_home <- c("players", names_home)
  names(jointdataset) <- names_home
  names(totals_home_df) <- names_home

  jointdataset <- rbind(jointdataset, totals_home_df)

  basic_home <- rbind(basic_home, jointdataset)
}

home_2018 <- basic_home 

# away
names_away <- box_score_urls$names_away[2622:3933]
players_away <- box_score_urls$players_away[2622:3933]
remainers_away <- box_score_urls$remainers_away[2622:3933]
totals_away <- box_score_urls$totals_away[2622:3933]

basic_away <- data.frame()

foreach(
  i = urls,
  j = names_away,
  k = players_away,
  l = remainers_away,
  m = totals_away
) %do% {
  html <- session(i)
  names_away <- html_nodes(html, j) %>% html_attr("data-stat")
  players_away <- html_nodes(html, k) %>% html_text()
  remainers_away <- html_nodes(html, l) %>% html_text()
  totals_away <- html_nodes(html, m) %>% html_text()

  names_away <- names_away[!is.na(names_away)]
  names_away <- names_away[-1:-2]

  players_away_df <- as.data.frame(players_away)
  players_away_df <-
    tibble::rowid_to_column(players_away_df, "merge_id")

  remainers_away <-
    remainers_away[remainers_away != "Did Not Play"]
  remainers_away <-
    remainers_away[remainers_away != "Did Not Dress"]
  remainers_away <-
    remainers_away[remainers_away != "Player Suspended"]
  remainers_away <-
    remainers_away[remainers_away != "Not With Team"]
  remainers_away <-
    matrix(remainers_away, ncol = 20, byrow = TRUE)
  remainers_away_df <- as.data.frame(remainers_away)
  remainers_away_df <-
    tibble::rowid_to_column(remainers_away_df, "merge_id")

  totals_away <- data.frame(totals_away)
  totals_away <- as.data.frame(t(totals_away))

  jointdataset <-
    merge(players_away_df,
      remainers_away_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_away_df <-
    tibble::rowid_to_column(totals_away, "merge_id")

  names_away <- c("players", names_away)
  names(jointdataset) <- names_away
  names(totals_away_df) <- names_away

  jointdataset <- rbind(jointdataset, totals_away_df)

  basic_away <- rbind(basic_away, jointdataset)
}

away_2018 <- basic_away

###               ###
### code 2019/20  ###
###               ###

# home
urls <- box_score_urls$urls[3934:5076]
names_home <- box_score_urls$names_home[3934:5076]
players_home <- box_score_urls$players_home[3934:5076]
remainers_home <- box_score_urls$remainers_home[3934:5076]
totals_home <- box_score_urls$totals_home[3934:5076]

basic_home <- data.frame()

foreach(
  i = urls,
  j = names_home,
  k = players_home,
  l = remainers_home,
  m = totals_home
) %do% {
  html <- session(i)
  names_home <- html_nodes(html, j) %>% html_attr("data-stat")
  players_home <- html_nodes(html, k) %>% html_text()
  remainers_home <- html_nodes(html, l) %>% html_text()
  totals_home <- html_nodes(html, m) %>% html_text()

  names_home <- names_home[!is.na(names_home)]
  names_home <- names_home[-1:-2]

  players_home_df <- as.data.frame(players_home)
  players_home_df <-
    tibble::rowid_to_column(players_home_df, "merge_id")

  remainers_home <-
    remainers_home[remainers_home != "Did Not Play"]
  remainers_home <-
    remainers_home[remainers_home != "Did Not Dress"] 
  remainers_home <-
    remainers_home[remainers_home != "Player Suspended"] 
  remainers_home <-
    remainers_home[remainers_home != "Not With Team"] 
  remainers_home <-
    matrix(remainers_home, ncol = 20, byrow = TRUE)
  remainers_home_df <- as.data.frame(remainers_home)
  remainers_home_df <-
    tibble::rowid_to_column(remainers_home_df, "merge_id")

  totals_home <- data.frame(totals_home)
  totals_home <- as.data.frame(t(totals_home))

  jointdataset <-
    merge(players_home_df,
      remainers_home_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <- subset(jointdataset, select = -merge_id)

  totals_home_df <-
    tibble::rowid_to_column(totals_home, "merge_id")

  names_home <- c("players", names_home)
  names(jointdataset) <- names_home
  names(totals_home_df) <- names_home

  jointdataset <- rbind(jointdataset, totals_home_df)

  basic_home <- rbind(basic_home, jointdataset)
}

home_2019 <- basic_home 

# away
names_away <- box_score_urls$names_away[3934:5076]
players_away <- box_score_urls$players_away[3934:5076]
remainers_away <- box_score_urls$remainers_away[3934:5076]
totals_away <- box_score_urls$totals_away[3934:5076]

basic_away <- data.frame()

foreach(
  i = urls,
  j = names_away,
  k = players_away,
  l = remainers_away,
  m = totals_away
) %do% {
  html <- session(i)
  names_away <-
    html_nodes(html, j) %>% html_attr("data-stat")
  players_away <-
    html_nodes(html, k) %>% html_text()
  remainers_away <-
    html_nodes(html, l) %>% html_text()
  totals_away <-
    html_nodes(html, m) %>% html_text()

  names_away <- names_away[!is.na(names_away)]
  names_away <- names_away[-1:-2]

  players_away_df <-
    as.data.frame(players_away)
  players_away_df <-
    tibble::rowid_to_column(players_away_df, "merge_id")

  remainers_away <-
    remainers_away[remainers_away != "Did Not Play"]
  remainers_away <-
    remainers_away[remainers_away != "Did Not Dress"]
  remainers_away <-
    remainers_away[remainers_away != "Player Suspended"]
  remainers_away <-
    remainers_away[remainers_away != "Not With Team"]
  remainers_away <-
    matrix(remainers_away, ncol = 20, byrow = TRUE)
  remainers_away_df <-
    as.data.frame(remainers_away)
  remainers_away_df <-
    tibble::rowid_to_column(remainers_away_df, "merge_id")

  totals_away <- data.frame(totals_away)
  totals_away <- as.data.frame(t(totals_away))

  jointdataset <-
    merge(players_away_df,
      remainers_away_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <-
    subset(jointdataset, select = -merge_id)

  totals_away_df <-
    tibble::rowid_to_column(totals_away, "merge_id")

  names_away <- c("players", names_away)
  names(jointdataset) <- names_away
  names(totals_away_df) <- names_away

  jointdataset <-
    rbind(jointdataset, totals_away_df)

  basic_away <-
    rbind(basic_away, jointdataset)
}

away_2019 <- basic_away

###               ###
### code 2020/21  ###
###               ###

# home
urls <- box_score_urls$urls[5077:6247]
names_home <- box_score_urls$names_home[5077:6247]
players_home <- box_score_urls$players_home[5077:6247]
remainers_home <- box_score_urls$remainers_home[5077:6247]
totals_home <- box_score_urls$totals_home[5077:6247]

basic_home <- data.frame()

foreach(
  i = urls,
  j = names_home,
  k = players_home,
  l = remainers_home,
  m = totals_home
) %do% {
  html <- session(i)
  names_home <-
    html_nodes(html, j) %>% html_attr("data-stat")
  players_home <-
    html_nodes(html, k) %>% html_text()
  remainers_home <-
    html_nodes(html, l) %>% html_text()
  totals_home <-
    html_nodes(html, m) %>% html_text()

  names_home <- names_home[!is.na(names_home)]
  names_home <- names_home[-1:-2]

  players_home_df <-
    as.data.frame(players_home)
  players_home_df <-
    tibble::rowid_to_column(players_home_df, "merge_id")

  remainers_home <-
    remainers_home[remainers_home != "Did Not Play"]
  remainers_home <-
    remainers_home[remainers_home != "Did Not Dress"] 
  remainers_home <-
    remainers_home[remainers_home != "Player Suspended"] 
  remainers_home <-
    remainers_home[remainers_home != "Not With Team"] 
  remainers_home <-
    matrix(remainers_home, ncol = 20, byrow = TRUE)
  remainers_home_df <-
    as.data.frame(remainers_home)
  remainers_home_df <-
    tibble::rowid_to_column(remainers_home_df, "merge_id")

  totals_home <- data.frame(totals_home)
  totals_home <- as.data.frame(t(totals_home))

  jointdataset <-
    merge(players_home_df,
      remainers_home_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <-
    subset(jointdataset, select = -merge_id)

  totals_home_df <-
    tibble::rowid_to_column(totals_home, "merge_id")

  names_home <- c("players", names_home)
  names(jointdataset) <- names_home
  names(totals_home_df) <- names_home

  jointdataset <-
    rbind(jointdataset, totals_home_df)

  basic_home <-
    rbind(basic_home, jointdataset)
}


home_2020 <- basic_home  

# away
names_away <- box_score_urls$names_away[5077:6247]
players_away <- box_score_urls$players_away[5077:6247]
remainers_away <- box_score_urls$remainers_away[5077:6247]
totals_away <- box_score_urls$totals_away[5077:6247]

basic_away <- data.frame()

foreach(
  i = urls,
  j = names_away,
  k = players_away,
  l = remainers_away,
  m = totals_away
) %do% {
  html <- session(i)
  names_away <-
    html_nodes(html, j) %>% html_attr("data-stat")
  players_away <-
    html_nodes(html, k) %>% html_text()
  remainers_away <-
    html_nodes(html, l) %>% html_text()
  totals_away <-
    html_nodes(html, m) %>% html_text()

  names_away <- names_away[!is.na(names_away)]
  names_away <- names_away[-1:-2]

  players_away_df <-
    as.data.frame(players_away)
  players_away_df <-
    tibble::rowid_to_column(players_away_df, "merge_id")

  remainers_away <-
    remainers_away[remainers_away != "Did Not Play"]
  remainers_away <-
    remainers_away[remainers_away != "Did Not Dress"]
  remainers_away <-
    remainers_away[remainers_away != "Player Suspended"]
  remainers_away <-
    remainers_away[remainers_away != "Not With Team"]
  remainers_away <-
    matrix(remainers_away, ncol = 20, byrow = TRUE)
  remainers_away_df <-
    as.data.frame(remainers_away)
  remainers_away_df <-
    tibble::rowid_to_column(remainers_away_df, "merge_id")

  totals_away <- data.frame(totals_away)
  totals_away <- as.data.frame(t(totals_away))

  jointdataset <-
    merge(players_away_df,
      remainers_away_df,
      by = "merge_id",
      all = TRUE
    )
  jointdataset <-
    subset(jointdataset, select = -merge_id)

  totals_away_df <-
    tibble::rowid_to_column(totals_away, "merge_id")

  names_away <- c("players", names_away)
  names(jointdataset) <- names_away
  names(totals_away_df) <- names_away

  jointdataset <-
    rbind(jointdataset, totals_away_df)

  basic_away <-
    rbind(basic_away, jointdataset)
}

away_2020 <- basic_away

################################################################################
# code to bind the dfs (separate dfs will be created for home and away teams)

basic_box_scores_home <- rbindlist(list(home_2016, home_2017, home_2018, home_2019, home_2020))
basic_box_scores_away <- rbindlist(list(away_2016, away_2017, away_2018, away_2019, away_2020))

# save basic_box_scores_home and basic_box_scores_away
write.csv(basic_box_scores_home, file = here("data", "raw", "basic_box_scores_home.csv"))
write.csv(basic_box_scores_away, file = here("data", "raw", "basic_box_scores_away.csv"))

################################################################################
# the schedule_and_results and basic_box_score data is sorted and cleaned below

# clean the environment
rm(list = ls())

# read in the processed schedule_and_results_2016_2020.csv (and remove the x column)
schedule_and_results <- read.csv(here("data", "processed", "schedule_and_results_2016_2020.csv"), row.names = "X")

# create two dfs with the columns of schedule_and_results that require stacking
stack_team <- select(schedule_and_results, home_team, away_team)
stack_points <- select(schedule_and_results, points_home, points_away)

# stack home_team and away_team and points_home and points_away
stack_team <- data.frame(team  = c(stack_team [,"home_team"], stack_team [,"away_team"]))
stack_points <- data.frame(points = c(stack_points [,"points_home"], stack_points [,"points_away"]))

# remove the stacking columns in schedule_and_results
schedule_and_results <- select(schedule_and_results, -home_team, -away_team, -points_home, -points_away)

# read in the raw basic_box_score data
basic_box_scores_home <- read.csv(here("data", "raw", "basic_box_scores_home.csv"), row.names = "X")
basic_box_scores_away <- read.csv(here("data", "raw", "basic_box_scores_away.csv"), row.names = "X")

# keep the totals rows
basic_box_scores_home <- filter(basic_box_scores_home, players == "1")
basic_box_scores_away <- filter(basic_box_scores_away, players == "1")

# remove the unneeded columns
basic_box_scores_home <- select(basic_box_scores_home, -players, -mp, -trb, -pts, -plus_minus)
basic_box_scores_away <- select(basic_box_scores_away, -players, -mp, -trb, -pts, -plus_minus)

# create a venue column for basic_box_scores_home and away
basic_box_scores_home <- data.frame(append(basic_box_scores_home, c(venue= 1)))
basic_box_scores_home <- basic_box_scores_home[, c(17, 1:16)]

basic_box_scores_away <- data.frame(append(basic_box_scores_away, c(venue= 2)))
basic_box_scores_away <- basic_box_scores_away[, c(17, 1:16)]

# cbind basic_box_scores_home and schedule_and_results
basic_box_scores_home <-  cbind(basic_box_scores_home, schedule_and_results)

# cbind basic_box_scores_away and schedule_and_results
basic_box_scores_away <-  cbind(basic_box_scores_away, schedule_and_results)

# rbind basic_box_scores_home and basic_box_scores_away
basic_box_scores <- rbind(basic_box_scores_home, basic_box_scores_away)

# cbind basic_box_scores, stack_points and stack_team
basic_box_scores <- cbind(basic_box_scores, stack_points, stack_team)

# reorder the columns
basic_box_scores <- basic_box_scores[, c(1, 26, 18, 23, 25, 2:17, 19:22, 24)]

# save the basic_box_scores
write.csv(basic_box_scores, file = here("data", "processed", "basic_box_scores.csv"))

################################################################################
# separate the seasons
box_2016 <- filter(basic_box_scores, year == "2016")
box_2017 <- filter(basic_box_scores, year == "2017")
box_2018 <- filter(basic_box_scores, year == "2018")
box_2019 <- filter(basic_box_scores, year == "2019")
box_2020 <- filter(basic_box_scores, year == "2020")

# create a list of the season dfs
l <- list(box_2016, box_2017, box_2018, box_2019, box_2020)

# save the season dfs as .csv
mapply(write.csv, x = l, file = c(
  "basic_box_2016.csv", "basic_box_2017.csv", "basic_box_2018.csv",
  "basic_box_2019.csv", "basic_box_2020.csv"
))

# create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed", "seasons"), overwrite = TRUE)

# remove old dfs
file.remove(here(dfs))

################################################################################
# separate home and away games for 2016, 2017 and 2018

#home
home_box_2016 <- filter(box_2016, venue == "1")
home_box_2017 <- filter(box_2017, venue == "1")
home_box_2018 <- filter(box_2018, venue == "1")
home_box_2019 <- filter(box_2019, venue == "1")
home_box_2020 <- filter(box_2020, venue == "1")

# away
away_box_2016 <- filter(box_2016, venue == "2")
away_box_2017 <- filter(box_2017, venue == "2")
away_box_2018 <- filter(box_2018, venue == "2")
away_box_2019 <- filter(box_2019, venue == "2")
away_box_2020 <- filter(box_2020, venue == "2")

# create a list of the home dfs
l_home <- list(home_box_2016, home_box_2017, home_box_2018, home_box_2019, home_box_2020)

# save the home dfs as .csv
mapply(write.csv, x = l_home, file = c(
  "basic_home_box_2016.csv", "basic_home_box_2017.csv", "basic_home_box_2018.csv",
  "basic_home_box_2019.csv", "basic_home_box_2020.csv"))

# create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed", "seasons", "home"), overwrite = TRUE)

# remove old dfs
file.remove(here(dfs))

# create a list of the season dfs
l_away <- list(away_box_2016, away_box_2017, away_box_2018, away_box_2019, away_box_2020)

# save the season dfs as .csv
mapply(write.csv, x = l_away, file = c(
  "basic_away_box_2016.csv", "basic_away_box_2017.csv", "basic_away_box_2018.csv",
  "basic_away_box_2019.csv", "basic_away_box_2020.csv"))

# create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed", "seasons", "away"), overwrite = TRUE)

# remove old dfs
file.remove(here(dfs))

################################################################################
# separate the 2019 and 2020 dfs by bubble and full capacity

# create functions to create a bubble column and a full capacity crowd column

# 2019
myfunc_bubble_home <- function(x, y) {
  home_box_2019[home_box_2019$date >= x & home_box_2019$date <= y, ]
}
myfunc_bubble_away <- function(x, y) {
  away_box_2019[away_box_2019$date >= x & away_box_2019$date <= y, ]
}

# 2020
myfunc_absent_home <- function(x, y) {
  home_box_2020[home_box_2020$date >= x & home_box_2020$date <= y, ]
}
myfunc_absent_away <- function(x, y) {
  away_box_2020[away_box_2020$date >= x & away_box_2020$date <= y, ]
}

# separate bubble games from non-bubble games
box_2019_pre_bubble_home <- myfunc_bubble_home(as.Date("2019-10-22"), as.Date("2020-03-11"))
box_2019_bubble_home <- myfunc_bubble_home(as.Date("2020-07-30"), as.Date("2020-10-11"))

box_2019_pre_bubble_away <- myfunc_bubble_away(as.Date("2019-10-22"), as.Date("2020-03-11"))
box_2019_bubble_away <- myfunc_bubble_away(as.Date("2020-07-30"), as.Date("2020-10-11"))

# separate absent games from full games
box_2020_absent_home <- myfunc_absent_home(as.Date("2020-12-22"), as.Date("2021-05-27"))
box_2020_full_home <- myfunc_absent_home(as.Date("2021-05-28"), as.Date("2021-07-20"))

box_2020_absent_away <- myfunc_absent_away(as.Date("2020-12-22"), as.Date("2021-05-27"))
box_2020_full_away <- myfunc_absent_away(as.Date("2021-05-28"), as.Date("2021-07-20"))

# add in bubble and capacity columns

# bubble
box_2019_pre_bubble_home <- data.frame(append(box_2019_pre_bubble_home, c(bubble = 0), after = 1))
box_2019_bubble_home <- data.frame(append(box_2019_bubble_home, c(bubble = 1), after = 1))

box_2019_pre_bubble_away <- data.frame(append(box_2019_pre_bubble_away, c(bubble = 0), after = 1))
box_2019_bubble_away <- data.frame(append(box_2019_bubble_away, c(bubble = 1), after = 1))

# capacity
box_2020_absent_home <- data.frame(append(box_2020_absent_home, c(full = 0), after = 1))
box_2020_full_home <- data.frame(append(box_2020_full_home, c(full = 1), after = 1))

box_2020_absent_away <- data.frame(append(box_2020_absent_away, c(full = 0), after = 1))
box_2020_full_away <- data.frame(append(box_2020_full_away, c(full = 1), after = 1))

# rbind the dfs 
basic_home_box_2019 <- rbind(box_2019_pre_bubble_home, box_2019_bubble_home)
basic_away_box_2019 <- rbind(box_2019_pre_bubble_away, box_2019_bubble_away)

basic_home_box_2020 <- rbind(box_2020_absent_home, box_2020_full_home)
basic_away_box_2020 <- rbind(box_2020_absent_away, box_2020_full_away)

# save the new dfs
write.csv(basic_home_box_2019, file = here("data", "processed", "seasons", "home", "basic_home_box_2019.csv"))
write.csv(basic_home_box_2020, file = here("data", "processed", "seasons", "home", "basic_home_box_2020.csv"))

write.csv(basic_away_box_2019, file = here("data", "processed", "seasons", "away", "basic_away_box_2019.csv"))
write.csv(basic_away_box_2020, file = here("data", "processed", "seasons", "away", "basic_away_box_2020.csv"))