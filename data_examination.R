# Data examination

# Packages
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(Hmisc)

# Correlation matrix

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Subset the data 
box_numeric <- box[c(4, 11:27)]

# Create the correlation matrix
box.cor <- cor(box_numeric)

# Create a correlation matrix df
cor_box <- as.data.frame(box.cor)

# Retain two digits per correlation
cor_box <- cor_box %>% mutate_if(is.numeric, ~round(., 2))

# Change the column names
names(cor_box) <- c(
  "Crowd size", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "FT", "FTA", "FT%", 
  "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV","PF"
  
)

# Export the df as text
write.table(cor_box, file = "cor_box.txt", sep = ",", quote = FALSE, row.names = F)

# Note there are some issues in the correlations
# FG are highly correlated with FG%
# 3P are highly correlated with 3P%
# FT are highly correlated with FTA
# DRB are highly correlated with TRB
# FG are highly correlated with AST

# Box plots of the chosen predictor variables (and ft_pct)

# Clean the environment
rm(list = ls())

# Read in the data (total_basic_box_scores.csv)
box <- read.csv(here("data", "processed", "total_basic_box_scores.csv"), row.names = "X")

# Separate the seasons
box_2016 <- filter(box, season == "2016-17")
box_2017 <- filter(box, season == "2017-18")
box_2018 <- filter(box, season == "2018-19")
box_2019 <- filter(box, season == "2019-20")
box_2020 <- filter(box, season == "2020-21")

# Retain the required columns - for box_2016, box_2017 and box_2018
box_2016 <- box_2016 %>%
  select(venue, season, attendance, fga, fg3a, fta, orb, drb, stl, blk, tov, pf, ft_pct)
box_2017 <- box_2017 %>%
  select(venue, season, attendance, fga, fg3a, fta, orb, drb, stl, blk, tov, pf, ft_pct)
box_2018 <- box_2018 %>%
  select(venue, season, attendance, fga, fg3a, fta, orb, drb, stl, blk, tov, pf, ft_pct)

# rbind box_2016, box_2017 and box_2018
box <- rbind(box_2016, box_2017, box_2018)

# Reform box_2019

# Retain the required columns
box_2019 <- box_2019 %>%
  select(venue, attendance, fga, fg3a, fta, orb, drb, stl, blk, tov, pf, bubble, ft_pct)

# Rename the bubble column to season
box_2019 <- box_2019 %>% 
  rename(
    season = bubble
  )

# Recode the season column
box_2019$season[box_2019$season=="Pre-bubble"] <- "2019-20" 
box_2019$season[box_2019$season=="Bubble"] <- "2019-20b" # the b stands for the bubble data

# Reform box_2020

# Retain the required columns
box_2020 <- box_2020 %>%
  select(venue, attendance, fga, fg3a, fta, orb, drb, stl, blk, tov, pf, zero, ft_pct)

# Rename the zero column to season
box_2020 <- box_2020 %>% 
  rename(
    season = zero
  )

# Recode the order column
box_2020$season[box_2020$season=="True"] <- "2020-21"
box_2020$season[box_2020$season=="Restricted"] <- "2020-21b" # the b stands for the spectator data

# rbind all seasons
box <- rbind(box, box_2019, box_2020)

# Data is prepared!

# Time to code the visualisations!

# Attendance

# Filter box by venue
box_home <- filter(box, venue == "Home")

# Create an attendance ggplot
p <- ggplot(box_home, aes(x = season, y = attendance, fill = season))

# Check the range of the plot
max(box$attendance)
## [1] 22983
min(box$attendance)
## [1] 0

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Attendance") +
  scale_y_continuous(breaks=seq(0,22983,5000)) + # Code to change the min and max values of the y axis (and to determine the distance of the ticks)
  scale_fill_manual(values = c("#F8766D", "#C49A00", "#53B400", "#FB61D7", "#00C094", "#00B6EB", "#A58AFF")) + # code to manually change the colours
  theme(legend.position = "none") # code to remove the legend title

ggsave(here("figs", "at_box.jpg")) # code to save the figure

# Field goal attempts per season

# Create a fga ggplot
p <- ggplot(box, aes(x = season, y = fga, fill = venue))

# Check the range of the plot
max(box$fga)
## [1] 128
min(box$fga)
## [1] 61

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Field Goal Attempts") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(61,128,10)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "fga_box.jpg")) # Code to save figure

# Three- point Field goal attempts per season

# Create a fg3a ggplot
p <- ggplot(box, aes(x = season, y = fg3a, fill = venue))

# Check the range of the plot
max(box$fg3a)
## [1] 70
min(box$fg3a)
## [1] 7

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Three-Point Field Goal Attempts") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(7,70,7)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "fg3a_box.jpg")) # Code to save figure

# Free throw attempts per season

# Create a fta ggplot
p <- ggplot(box, aes(x = season, y = fta, fill = venue))

# Check the range of the plot
max(box$fta)
## [1] 64
min(box$fta)
## [1] 1

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Free Throw Attempts") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(0,64,10)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "fta_box.jpg")) # Code to save figure

# Offensive Rebounds per Season

# Create a orb ggplot
p <- ggplot(box, aes(x = season, y = orb, fill = venue))

# Check the range of the plot
max(box$orb)
## [1] 26
min(box$orb)
## [1] 0

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Offensive Rebounds") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(0,26,4)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "orb_box.jpg")) # Code to save figure

# Defensive Rebounds per Season

# Create a drb ggplot
p <- ggplot(box, aes(x = season, y = drb, fill = venue))

# Check the range of the plot
max(box$drb)
## [1] 56
min(box$drb)
## [1] 18

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Defensive Rebounds") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(18,56,4)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "drb_box.jpg")) # Code to save figure

# Steals per Season

# Create a stl ggplot
p <- ggplot(box, aes(x = season, y = stl, fill = venue))

# Check the range of the plot
max(box$stl)
## [1] 22
min(box$stl)
## [1] 0

# Add in the remaining plot details
p + geom_boxplot() + # tell r that we want a box plot
  labs(x = "Season",
       y = "Steals") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(0,22,5)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "stl_box.jpg")) # Code to save figure

# Blocks per Season

# Create a blk ggplot
p <- ggplot(box, aes(x = season, y = blk, fill = venue))

# Check the range of the plot
max(box$blk)
## [1] 20
min(box$blk)
## [1] 0

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Blocks") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(0,20,2)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "blk_box.jpg"))  # Code to save figure

# Turnovers per Season

# Create a tov ggplot
p <- ggplot(box, aes(x = season, y = tov, fill = venue))

# Check the range of the plot
max(box$tov)
## [1] 29
min(box$tov)
## [1] 1

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Turnovers") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(0,29,4)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "tov_box.jpg")) # Code to save figure

# Personal Fouls per Season

# Create a pf ggplot
p <- ggplot(box, aes(x = season, y = pf, fill = venue))

# Check the range of the plot
max(box$pf)
## [1] 42
min(box$pf)
## [1] 6

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Personal Fouls") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  scale_y_continuous(breaks=seq(0,42,5)) # Code to change the min and max values of the y axis (and to determine the distance of the ticks)

ggsave(here("figs", "pf_box.jpg")) # Code to save figure

# Free throw percentage per Season (a variable that was not retained)

# Create a ft_pct ggplot
p <- ggplot(box, aes(x = season, y = ft_pct, fill = venue))

# Range = 0 - 1

# Add in the remaining plot details
p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Free Throw Percentage") +
  guides(fill=guide_legend(title="Venue"))  # Code to change the title of the legend

ggsave(here("figs", "ft_pct_box.jpg")) # Code to save figure

# Probability density plots - linear regression - outcome

# Clean the environment
rm(list = ls())

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Filter the data for home performance - we could also have done away
home <- filter(box, venue == "Home")

# Point_difference
p <- ggplot(home, aes(x=point_difference)) 

p + geom_density() +
  labs(x = "Point Difference (Home Points - Away Points)",
       y = "Density")

# The data appears to be normally distributed!
# A zero point difference is not possible in total basic box score data
# Overtime is played until one team wins

ggsave(here("figs", "point_dif_density.jpg")) # Code to save figure

# Histogram
p <- ggplot(home, aes(x = point_difference)) 

p + geom_histogram() +
  geom_histogram(binwidth = 1) +
  labs(x = "Point Difference (Home Points - Away Points)",
       y = "Count") +
  theme_bw()

ggsave(here("figs", "point_dif_density_his.jpg")) # Code to save figure

# point difference per season boxplot

# Clean the environment
rm(list = ls())

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Create a season ggplot
p <- ggplot(box, aes(x = season, y = point_difference, fill = venue))

# Check the range of the plot
max(box$point_difference)
## [1] 61
min(box$point_difference)
## [1] -57

p + geom_boxplot() + # tell R that we want a box plot
  labs(x = "Season",
       y = "Point Difference") +
  guides(fill=guide_legend(title="Venue"))  + # Code to change the title of the legend
  ylim(-57, 61) 

ggsave(here("figs", "point_dif_season.jpg"))

# Probability density plots - linear regression - predictors

# Clean the environment
rm(list = ls())

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Field goal attempts
plot(density(box$fga), main='Field goal attempts') 
# Normally distributed!

# Three-point field goal attempts
plot(density(box$fg3a), main='Three-point field goal attempts') 
# Normally distributed!

# Free throw attempts
plot(density(box$fta), main='Free throw attempts') 
# Normally distributed!

# Offensive rebounds
plot(density(box$orb), main='Offensive rebounds') 
# Normally distributed!

# Defensive rebounds
plot(density(box$drb), main='Defensive rebounds') 
# Normally distributed!

# Steals
plot(density(box$stl), main='Steals') 
# Normally distributed!
# The range of steals is small - which is why it keeps dropping

# Blocks
plot(density(box$blk), main='Blocks') 
# Normally distributed!
# The range of blocks is small - which is why it keeps dropping

# Turnovers
plot(density(box$tov), main='Turnovers') 
# Normally distributed!

# Personal Fouls
plot(density(box$pf), main='Personal Fouls') 
# Normally distributed!

# Scatter plots - linear regression

# Clean the environment
rm(list = ls())

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# I need one lists of variables to iterate over (i.e., predictor variables)
variables <- c("attendance",
               "fga", 
               "fg3a", 
               "fta",
               "orb",
               "drb", 
               "stl",
               "blk", 
               "tov", 
               "pf"
               )

# I need plot parameters
alpha <- .5
ha <- "point_difference"
xlab  <- c("Attendance", 
           "Field Goal Attempts",
           "Three-point Field Goals Attempts",
           "Free Throw Attempts", 
           "Offensive Rebounds",
           "Defensive Rebounds",
           "Steals", 
           "Blocks",
           "Turnovers", 
           "Personal Fouls")

for (i in 1:10) { # for loop to create ten scatter plots (one for each predictor variable (except season))
  p <- ggplot(data = box, 
              mapping = aes_string(x = variables[i], # my variables list
                                   y = ha))
  p + geom_point(alpha = alpha) +
    labs(x = xlab[i], y = "Point Difference")
  
  ggsave(filename = paste("figs/scatter_plot",toString(i),".png",sep=""), height = 5) # code to save each plot with a different name
}