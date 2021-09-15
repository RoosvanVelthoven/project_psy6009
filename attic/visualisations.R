# Packages
library(here)
library(tidyverse)
library(animation)
library(png)
library(grid)

# Code to read in games.csv
df_2016_2021 <- read.csv(here("data", "processed", "schedules_and_results_of_2016_2021.csv"))

# Code to specify which variables to keep
df_2016_2021 <- df_2016_2021  %>% 
  select(points_home, points_away, home_win, year) 

# Code to recode home_win values true to won and false to lost
df_2016_2021[df_2016_2021 == "TRUE"] <- "Won"
df_2016_2021[df_2016_2021 == "FALSE"] <- "Lost"

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

# Code to remove the season columns
nba_final_df <- select(nba_final_df,-year_16, -year_17, -year_18, -year_19, -year_20)

# Code to save my final data set as a csv file
write.csv(nba_final_df, file = here("data", "processed", "nba_final_df.csv"))
# nba_final_df = data set that I will use for my plots

# Some extra checks to ensure that I have all my data.

# Code to determine the max value of a column
max(nba16$id) 
## [1] 1309
max(nba17$id)
## [1] 1312
max(nba18$id)
## [1] 1312
max(nba19$id)
## [1] 1143
max(nba20$id)
## [1] 1170

max(nba_final_df$id)
## [1] 1312
# All the data points are still there!

# Step one: I need two lists of variables to iterate over
points_home <- c("points_home_16",
                 "points_home_17",
                 "points_home_18",
                 "points_home_19",
                 "points_home_20")
points_away <- c("points_away_16",
                 "points_away_17",
                 "points_away_18",
                 "points_away_19", 
                 "points_away_20") 

# Step two: I need a list of variables for my colour and shape
home_win <- c("home_win_16",
              "home_win_17",
              "home_win_18",
              "home_win_19", 
              "home_win_20") 

# Step three: I need a list of titles
season <- c("Home Advantage Within the NBA: Season 2016", 
            "Home Advantage Within the NBA: Season 2017", 
            "Home Advantage Within the NBA: Season 2018", 
            "Home Advantage Within the NBA: Season 2019",
            "Home Advantage Within the NBA: Season 2020")

# Step four: I need plot parameters
xlab  <-  'Points scored by the home team'
ylab  <-  'Points scored by the away team'
pointsize  <-  2
alpha <- .5
h <- "The home team:"


# Step five: I need xlim and ylim values
min(df_2016_2021$points_home) ## [1] 64
max(df_2016_2021$points_home) ## [1] 161
# I will use 60 - 165 for my xlim
min(df_2016_2021$points_away) ## [1] 68
max(df_2016_2021$points_away) ## [1] 168
# I will use 60 - 170 for my ylim


# Step six: I need a loop to look at points_away and points_home across four seasons

# Code to start saving my GIF
saveGIF({
  # Code to start my loop
  for (i in 1:5){
    
    mx <- mean(nba_final_df[,points_home[i]], na.rm = TRUE)
    # This will be used for my subtitle, it determines the mean value of points scored by the home team in a certain season and then assigns it to an object.   
    # na.rm = TRUE removes missing values in the calculation
    my <- mean(nba_final_df[,points_away[i]], na.rm = TRUE)
    # This will be used for my subtitle, it determines the mean value of points scored by the away team in a certain season and then assigns it to an object.
    
    nba_plots <- 
      ggplot(data = nba_final_df,
             mapping = aes_string (x = points_home[i],
                                   # First lists of variables to iterate over
                                   y =   points_away[i], 
                                   # Second lists of variables to iterate over
                                   colour = home_win[i], 
                                   # Code to have different colours for games won or lost by the home team
                                   shape = home_win[i])) 
    # Code to have different shapes for games won or lost by the home team
    
    # Step seven: time to add plot parameters
    p <- nba_plots +
      geom_point (size = pointsize, alpha = alpha) +
      labs(x = xlab, y = ylab, title = season[i], 
           subtitle = sprintf("Mean points scored by the home team = %.1f, 
mean points scored by the away team = %.1f", mx,my)) +
      xlim(60, 165) +
      ylim(60, 170) +
      scale_colour_discrete(h, na.translate = FALSE) + 
      # Code necessary to have different colours for games won or lost by the home team
      scale_shape_discrete(h, na.translate=FALSE) +
      # Code necessary to have different shapes for games won or lost by the home team
      theme(text = element_text(size = 12))
    # Code to set text to size 12
    
    ggsave(filename = paste("figs/nba",toString(i),".png",sep=""))
    # Code to save the plots (nba1.png, nba2.png, nba3.png, nba4.png, nba5.png)
    
    print(p)}
  
},interval = 8,                            # 8 seconds interval
movie.name = "visualisation.gif",        
ani.height = 500,                        
ani.width = 700, 
ani.res = 100)                           # Resolution
# My GIF was saved as visualisation.gif

file.copy("visualisation.gif", here("figs"), overwrite = TRUE)

