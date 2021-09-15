# code to load in a file
load(here("file"))

basic_box_scores <- c("players", "mp", "fg", "fga", "fg%", "3p", "3pa",
                      "3p%", "ft", "fta", "ft%", "orb", "drb", "trb", "ast",
                      "stl", "blk", "tov", "pf", "pts", "+/-")

advanced_box_scores<- c("players", "mp", "ts%", "efg%", "3par", "ftr", "orb%", 
                        "drb%", "trb%", "ast%", "stl%", "blk%", "tov%", "usg%", 
                        "ortg", "drtg", "bpm")

# code to select certain columns 
df_2016_2017<-  df_2016_2017%>% 
  select(box_score_urls, home, away)

# code to keep certain rows
df_2016_2017 <- df_2016_2017 %>% slice(1:2)

#  code to read in a df
df_2016_2017<- read.csv(here("data", "processed", "2016_2017", "df_2016_2017.csv"))

# code to make an empty df
boxscores <- data.frame()

schedule_and_results_2016_2020 %>% make_clean_names()

################################################################################ list of dataframes
library(XML)

df_2016_2017<- read.csv(here("data", "processed", "2016_2017", "df_2016_2017.csv"))

df_2016_2017<-  df_2016_2017%>% 
  select(box_score_urls, home, away)

df_2016_2017 <- df_2016_2017 %>% slice(1:3)

urls <- df_2016_2017$box_score_urls

for (url in urls) {
  download_html(url)
  
  htmls <- list.files("...", pattern = ".html")
  
  urltxt <- lapply(htmls, function(x) try(readLines(x)))
}

urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))

# PARSE UNCOMMENTED TEXT
doc <- htmlParse(urltxt)

# RETRIEVE ALL <table> TAGS
tables <- xpathApply(doc, "//table")

# LIST OF DATAFRAMES
away_tables <- lapply(tables[c(6:21, 27:42, 48:63)], function(i) readHTMLTable(i))

################################################################################ for loop attempt one
for (url in urls) {
  download_html(url)
  
  htmls <- list.files("...", pattern = ".html")
  
  webpages <- lapply(htmls, function(x) try(read_html(x)))
  
  with_progress(for (webpage in webpages) {
    p <- progressr::progressor(along = webpages)
    
    for (home in home_abbreviation) {
      home_table_col_names <- paste0("table#box-", home, "-game-basic > thead > tr > th")
      home_table_players <- paste0("table#box-", home, "-game-basic > tbody > tr > th > a")
      home_table_remainers <- paste0("table#box-", home, "-game-basic > tbody > tr > td")
      home_table_totals <- paste0("table#box-", home, "-game-basic > tfoot > tr > th")
      home_table_totals_num <- paste0("table#box-", home, "-game-basic > tfoot > tr > td")
      
      # obtain col_names
      col_names <- webpage %>%  
        html_nodes(home_table_col_names) %>%
        html_attr("data-stat") 
      
      # remove na
      col_names <- col_names[!is.na(col_names)]
      
      # obtain players column
      players <- webpage %>%   
        html_nodes(home_table_players) %>%
        html_text()
      
      # save players as a df
      players_df <- as.data.frame(players)
      
      # add a merge_id
      players_df <- tibble::rowid_to_column(players_df, "merge_id")
      
      # obtain remaining columns
      col_remainers <- webpage %>% 
        html_nodes(home_table_remainers) %>% 
        html_text()
      
      # remove "did not play" 
      col_remainers <- col_remainers[col_remainers != "Did Not Play"]
      
      # remove first two col_names (removal is necessary to create a matrix)
      col_names <- col_names[-1: -2]
      
      # create a matrix using col_names and col_remainers
      col_remainers <- col_remainers %>% matrix(ncol = length(col_names), byrow = TRUE)
      
      # save col_remainers as a df
      col_remainers_df <- as.data.frame(col_remainers)
      
      # add a merge_id
      col_remainers_df <- tibble::rowid_to_column(col_remainers_df, "merge_id")
      
      # obtain totals 
      totals <- webpage %>% 
        html_nodes(home_table_totals) %>% 
        html_text
      
      # save totals as a df
      totals_df <- as.data.frame(totals)
      
      # add a merge_id
      totals_df <- tibble::rowid_to_column(totals_df, "merge_id")
      
      # obtain totals_num 
      totals_num <- webpage %>% 
        html_nodes(home_table_totals_num) %>% 
        html_text
      
      # create a matrix using col_names and totals_num
      totals_num <- totals_num %>% matrix(ncol = length(col_names), byrow = TRUE)
      
      # save totals_num as a df
      num_df <- as.data.frame(totals_num)
      
      # add a merge_id
      num_df <- tibble::rowid_to_column(num_df, "merge_id")
      
      # merge players_df with col_remainers_df
      jointdataseta <- merge(players_df, col_remainers_df, by = 'merge_id', all=TRUE)
      
      # merge totalsdf with num_df
      jointdatasetb <- merge(totals_df, num_df, by = 'merge_id', all=TRUE)
      
      # remove merge_id
      jointdataseta <- subset(jointdataseta, select = -merge_id)
      jointdatasetb <- subset(jointdatasetb, select = -merge_id)
      
      # add players to col_names
      col_names <- c("players", col_names)
      
      # add column names to jointdatasets
      names(jointdataseta) <- col_names
      names(jointdatasetb) <- col_names
      
      # combine merged datasets
      jointdataset <- rbind(jointdataseta, jointdatasetb)
      
      # rbind jointdataset with boxscores (this should create all tables)
      boxscores <- rbind(boxscores, jointdataset)
      
      for (html in htmls) {
        unlink(html)
      }
      
      Sys.sleep(0.001)
      p()
      
    }
    
  }
  )
  
}
################################################################################ dim code
library(here)
library(tidyverse)

load(here("box_scores_2016.Rdata"))

basic_box_scores <- c("starters", "mp", "fg", "fga", "fg%", "3p", "3pa",
                      "3p%", "ft", "fta", "ft%", "orb", "drb", "trb", "ast",
                      "stl", "blk", "tov", "pf", "pts", "+/-")

for (i in 1:length(box_scores_2016)) {
  if (dim(box_scores_2016[[i]]) == 13:21){
    names(box_scores_2016[[i]]) = basic_box_scores
  }
}

for (i in 1:length(box_scores_2016)) {
  if (dim(box_scores_2016[[i]]) == 14:21){
    names(box_scores_2016[[i]]) = basic_box_scores
  }
}

basic_box_scores <- data.table::rbindlist(box_scores_2016, fill = T)

basic_box_scores <- basic_box_scores %>% 
  select("starters", "mp", "fg", "fga", "fg%", "3p", "3pa",
         "3p%", "ft", "fta", "ft%", "orb", "drb", "trb", "ast",
         "stl", "blk", "tov", "pf", "pts", "+/-")

basic_box_scores <- basic_box_scores[rowSums(is.na(basic_box_scores)) != ncol(data), ] 

basic_box_scores <- c("Starters", "MP", "FG", "FGA", "FG%", "3P", "3PA", 
                      "3P%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB", "AST", 
                      "STL", "BLK", "TOV", "PF", "PTS", "+/-")

advanced_box_scores<- c("starters", "mp", "ts%", "efg%", "3par", "ftr", "orb%", 
                        "drb%", "trb%", "ast%", "stl%", "blk%", "tov%", "usg%", 
                        "ortg", "drtg", "bpm")

for (i in 1:length(box_scores_2016)) {
  if (dim(box_scores_2016[[i]]) == 13:17){
    names(box_scores_2016[[i]]) = advanced_box_scores
  }
}

for (i in 1:length(box_scores_2016)) {
  if (dim(box_scores_2016[[i]]) == 14:17){
    names(box_scores_2016[[i]]) = advanced_box_scores
  }
}
################################################################################ attempt two
# obtain all box_score tables

# step one: set up the environment

# clean the environment
rm(list = ls())

# read in box_score_urls_2016_2020.csv
box_scores <- read.csv(here("data", "processed", "box_score_urls_2016_2020.csv"))

# select the column of interest
box_scores <- box_scores %>% select(urls)

# assign box_score_urls$boxscore_links to an object
urls <- box_scores$urls

# step two: ensure that the ordering of htmls remaings as predetermined

# create a new df with solely html names (utilize box_scores) 
df_htmls <- box_scores %>%
  mutate_at("urls", str_replace, "https://www.basketball-reference.com/boxscores/", "")

# assign the correctly ordered htmls to an object  
order_htmls <- df_htmls$urls

# obtain the file paths of the order_htmls object  
paths <- here(order_htmls)

# create a 6247 element vector
a <- c(1:9)
a <- paste0("a000", a)
b <- c(10:99)
b <- paste0("a00", b)
c <- c(100:999)
c <- paste0("a0", c)
d <- c(1000:6247)
d <- paste0("a", d)
a <- as.data.frame(a)
b <- as.data.frame(b)
c <- as.data.frame(c)
d <- as.data.frame(d)

names(b) <- c("a")
names(c) <- c("a")
names(d) <- c("a")
numbers <- rbind(a, b, c, d)

# paste .html behind the numbers
numbers$a <- paste0(numbers$a, ".html")

# assign numbers$a to the object numbers_htmls  
numbers_htmls <- numbers$a

# obtain uncommented text of urls (per season)

# note 
# code below is done in sections, the data is very large in size

###               ###
### code 2016/17  ###
###               ###

# step one: download htmls of the 2016/17 season

for (i in 1:1309) {
  download_html(urls[i])
}

# rename htmls and ensure that they are named in the predetermined order  
# for (i in 1:1309) {
#   file.rename(paths[i], numbers_htmls[i])
# }

# step two: obtain boxscore tables

# create an html file list
html_list <- list.files("...", pattern = ".html")

# use the function readLines to read all text lines of the htmls (and assign them to an object)
urltxt <- lapply(html_list, function(x) try(readLines(x)))

# pull comments (everything that is not listed in the first table of a
# basketball-reference.com webpage is listed as a comment table)  
urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))

# parse the urltxt
doc_2016 <- htmlParse(urltxt)

# retrieve all <table> tags
tables_2016 <- xpathApply(doc_2016, "//table")

# read in the tables of interest
box_scores_2016 <- lapply(tables_2016[c(1:34653)], function(i) readHTMLTable(i))

# save the box scores
save(box_scores_2016, file = "box_scores_2016.RData")

# remove htmls from our folder
for (i in 1:1309) {
  unlink(html_list)
}

################################################################################ for each attempt one
# read in box_score_urls_2016_2020.csv
box_score_urls <- read.csv(here("data", "processed", "box_score_urls_2016_2020.csv"))

# add in html_nodes information
box_score_urls$home<- paste0("table#box-", box_score_urls$home, "-game-basic > tfoot > tr > td")
box_score_urls$away<- paste0("table#box-", box_score_urls$away, "-game-basic > tfoot > tr > td")

# create urls, home_abbreviation and away_abbreviation objects
urls <- box_score_urls$urls
home_abbreviation <- box_score_urls$home
away_abbreviation <- box_score_urls$away

###               ###
### code 2016/17  ###
###               ###

urls_2016 <- urls[1:1309]
home_abbreviation_2016 <- home_abbreviation[1:1309]
away_abbreviation_2016 <- away_abbreviation[1:1309]

totals_2016_home <- data.frame()

foreach(i = urls_2016, j = home_abbreviation_2016) %do% {
  html <- session(i)
  totals <- html_nodes(html, j) %>% html_text()  
  
  totals <- data.frame(totals)
  totals <- as.data.frame(t(totals))
  totals_2016_home <- rbind(totals_2016_home, totals)
}

write.csv(totals_2016_home, file = here("data", "processed", "box_scores_basic_home_2016.csv"))

totals_2016_away <- data.frame()

foreach(i = urls_2016, j = away_abbreviation_2016) %do% {
  html <- session(i)
  totals <- html_nodes(html, j) %>% html_text()  
  
  totals <- data.frame(totals)
  totals <- as.data.frame(t(totals))
  totals_2016_away <- rbind(totals_2016_away, totals)
}

write.csv(totals_2016_away, file = here("data", "processed", "box_scores_basic_away_2016.csv"))

################################################################################
df_htmls <- urls_df %>%
  mutate_at("urls", str_replace, "https://www.basketball-reference.com/boxscores/", "")

names(test)<-tolower(names(test))

variables <- test %>% select(starts_with(c("Starters", "MP", "FG", "FGA", "FG%", "3P", "3PA", 
                                           "3P%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB", "AST", 
                                           "STL", "BLK", "TOV", "PF", "PTS", "+/-")))

meh <- variables %>% select(starts_with("starters")) %>% cbind()
meh <- cbind(meh[1], stack(meh[2:47]))



starters <- cbind((starts_with("MP")))

box_score_variables <- c("Starters", "MP", "FG", "FGA", "FG%", "3P", "3PA", 
                         "3P%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB", "AST", 
                         "STL", "BLK", "TOV", "PF", "PTS", "+/-")

box_score_variables  <- lapply(test, function(x) x[(names(x) %in% box_score_variables)])

box_scores_2016_a <- lapply(box_scores_2016, function(x) x[!(names(x) %in% remove)])

remove_long <-  c(5, 26, 47, 75, 105, 133, 161, 189, 217, 245, 273, 301, 329, 
                  351, 373, 395, 417, 443, 469, 495, 521, 549, 575, 601, 627, 
                  654, 681, 708, 735, 762, 789, 816, 843, 872, 897, 922, 947, 
                  972, 997)

for (i in 1:39) {
  names(box_scores_2016_a[[remove_long[i]]]) <- c("a", "b", "c", "d", "e")
}

remove <- c("a", "b", "c", "d", "e")

lapply(box_scores_2016, function(x) filter(x, Type == "Total"))

box_scores_2016_tables = subset(box_scores_2016_tables, 
                                select = -c(a1:a15))

box_scores_2016_tables <- box_scores_2016_tables[rowSums(is.na(box_scores_2016_tables)) != ncol(box_scores_2016_tables), ] 

ompleterecords <- na.omit(test) 
test = subset(test, 
              select = -c(a1:a15))

test[complete.cases(test), ]

################################################################################
# the schedule_and_results_2016_2020 data is separated blow

# create a function to separated the NBA seasons
myfunc_dates <- function(x, y) {
  df_2016_2020[df_2016_2020$date >= x & df_2016_2020$date <= y, ]
}

# create separated dfs for each season
df_2016_17 <- myfunc_dates(as.Date("2016-10-25"), as.Date("2017-06-12"))
df_2017_18 <- myfunc_dates(as.Date("2017-10-17"), as.Date("2018-06-08"))
df_2018_19 <- myfunc_dates(as.Date("2018-10-16"), as.Date("2019-06-13"))
df_2019_20 <- myfunc_dates(as.Date("2019-10-22"), as.Date("2020-10-11"))
df_2020_21 <- myfunc_dates(as.Date("2020-12-22"), as.Date("2021-07-22"))

# add in a years column to each df (2016 stands for the 2016-2017 season)
df_2016_17$year <- 2016
df_2017_18$year <- 2017
df_2018_19$year <- 2018
df_2019_20$year <- 2019
df_2020_21$year <- 2020

# rbind the seperate dfs
schedule_and_results_2016_2020 <- rbind(df_2016_17, df_2017_18, df_2018_19, df_2019_20, df_2020_21)

# save schedule_and_results_2016_2020 in the processed folder
write.csv(schedule_and_results_2016_2020, file = here("data", "processed", "schedule_and_results_2016_2020.csv"))

# save season dfs

# create a list of the season dfs
l <- list(df_2016_17, df_2017_18, df_2018_19, df_2019_20, df_2020_21)

# save the season dfs as .csv
mapply(write.csv, x = l, file = c(
  "df_2016_17.csv", "df_2017_18.csv", "df_2018_19.csv",
  "df_2019_20.csv", "df_2020_21.csv"
))

# create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed"), overwrite = TRUE)

# remove old dfs
file.remove(here(dfs))

################################################################################
# code to ensure that each column has the correct class
df_2016_2020$date <- mdy(df_2016_2020$date)
df_2016_2020$attendance <- as.numeric(gsub(",", "", df_2016_2020$attendance))

################################################################################
# rename the columns of basic_box_scores_away
names_home <- c("fg_home", "fga_home", "fg_pct_home", "fg3_home", "fg3a_home", "fg3_pct_home",
                "ft_home", "fta_home", "ft_pct_home", "orb_home", "drb_home", "ast_home",
                "stl_home", "blk_home", "tov_home", "pf_home")

names(basic_box_scores_home) <- names_home

# rename the columns of basic_box_scores_away
names_away <- c("fg_away", "fga_away", "fg_pct_away", "fg3_away", "fg3a_away", "fg3_pct_away",
                "ft_away", "fta_away", "ft_pct_away", "orb_away", "drb_away", "ast_away",
                "stl_away", "blk_away", "tov_away", "pf_away")

names(basic_box_scores_away) <- names_away