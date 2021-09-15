# Descriptive analyses

# Packages
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Note the descriptive analyses only utilize home data

# Read in the data (total_basic_box_scores.csv)
box <- read.csv(here("data", "processed", "total_basic_box_scores.csv"), row.names = "X")

# Retain the home data
box <- filter(box, venue == "Home")

# Visualisation one
# 2016-17, 2017-18 and 2018-19 NBA HA

# Separate the seasons
box_2016 <- filter(box, season == "2016-17")
box_2017 <- filter(box, season == "2017-18")
box_2018 <- filter(box, season == "2018-19")

# Calculate HA

# Check how many games were won by the home team
table(box_2016$home_win)
##   0   1 
## 546 763

# Calculate HA and assign it to an object
ha_2016 <- (763/1309)*100
## [1] 58.28877

table(box_2017$home_win)
##   0   1 
## 542 770

ha_2017 <- (770/1312)*100
## [1] 58.68902

table(box_2018$home_win)
##   0   1 
## 537 775 

ha_2018 <- (775/1312)*100
## [1] 59.07012

# Create a HA and season object
ha <- c(ha_2016, ha_2017, ha_2018)
season <- c("2016-17", "2017-18", "2018-19")

# Create a df utilizing the ha and season objects
df <- data.frame(season, ha)

# Create a HA ggplot
ha_plot <- ggplot(data = df, aes(x = season, y = ha, colour = season))

# Add in the remaining plot details
ha_plot +
  geom_point(aes(), size = 3, alpha = .80) +
  coord_flip() + # Code to flip the x and y axis
  theme_bw() + # Code to add a nice looking theme
  labs(x = "Season", y = "Home Win Percentage") +
  ylim(50, 70) + # Code to determine the min and max values of the Y axis
  scale_colour_manual(name = "Season", values = c("#F8766D", "#C49A00", "#53B400")) + # Code to chose the data point colours
  guides(color = guide_legend(reverse = TRUE)) # Code to reverse the order of the legend

ggsave(here("figs", "pre_covid_ha.jpg"), height = 3.5) # Code to save figure

# Visualisation two
# 2016-17, 2017-18 and 2018-19 NBA HA, per Team

# Determine HA for each team separately (per season)
box_2016 <- box_2016 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)
box_2017 <- box_2017 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)
box_2018 <- box_2018 %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

# Include a season column
box_2016 <- data.frame(append(box_2016, c(season = "2016-17"), after = 2))
box_2017 <- data.frame(append(box_2017, c(season = "2017-18"), after = 2))
box_2018 <- data.frame(append(box_2018, c(season = "2018-19"), after = 2))

# rbind the HA dfs
ha <- rbind(box_2016, box_2017, box_2018)

# Create a HA ggplot
ha_plot <- ggplot(data = ha, aes(x = team, y = win_perc, group = season))

# Check the range of the plot
min(ha$win_perc)
## [1] 21.95122
max(ha$win_perc)
## [1] 90

# Add in the remaining plot details
ha_plot +
  geom_point(aes(color = season), size = 3, alpha = .80) +
  coord_flip() + # Code to flip the X and Y axis
  theme_bw() + # Code to add in a nice looking theme
  labs(x = "Team", y = "Home Win Percentage") +
  theme(legend.position="none") + # Code to remove the legend
  ylim(20, 90) + # Code to determine the min and max values of the Y axis
  theme(axis.title.y=element_blank()) + # Code to remove the Y axis title
  scale_colour_manual(values = c("#F8766D", "#C49A00", "#53B400")) # Code to chose the data point colours

ggsave(here("figs", "pre_covid_ha_per_team.jpg")) # Code to save figure

# Visualisation three
# 2019-20 and 2020-21 NBA HA

# Separate the seasons
box_2019 <- filter(box, season == "2019-20")
box_2020 <- filter(box, season == "2020-21")

# Separate bubble and non-bubble data
bubble <- filter(box_2019, bubble == "Bubble")
pre_covid <- filter(box_2019, bubble == "Pre-bubble")

# Separate zero and non-zero data
absent <- filter(box_2020, zero == "True")
spectators <- filter(box_2020, zero == "Restricted")

# Calculate HA

table(pre_covid$home_win)
##   0   1 
## 436 535 

ha_2019 <- (535/971)*100
## [1] 55.09784

table(bubble$home_win)
## 0  1 
## 82 90 

ha_2019_b <- (90/172)*100
## [1] 52.32558

table(absent$home_win)
## 0   1 
## 286 294  

ha_2020_a <- (294/580)*100
## [1] 50.68966

table(spectators$home_win)
## 0   1 
## 206 272  

ha_2020 <- (272/478)*100
## [1] 56.90377

# Create HA, season and order objects
ha <- c(ha_2019, ha_2019_b, ha_2020_a, ha_2020)
season <- c("2019-20", "2019-20", "2020-21", "2020-21")
order <- c("Pre COVID-19","NBA Bubble", "Zero Spectators", "Crowd Size Restrictions")

# Create a df utilizing the ha, season and order objects
df <- data.frame(season, ha, order)

# Ensure that the order column stays in the correct order
df$order <- factor(df$order, 
                   levels = c("Pre COVID-19", "NBA Bubble",
                              "Zero Spectators", "Crowd Size Restrictions"))

# Create a HA ggplot
ha_plot <- ggplot(data = df, aes(x = season, y = ha, colour = order))

# Add in the remaining plot details
ha_plot +
  geom_point(aes(), size = 3, alpha = .80) +
  coord_flip() + # Code to flip the X and Y axis
  theme_bw() + # Code to add in a nice looking theme
  labs(x = "Season", y = "Home Win Percentage") +
  ylim(50, 70) + # Code to determine the min and max values of the Y axis +
  scale_colour_manual(name = "Sample", values = c("#FB61D7", "#00C094", "#00B6EB", "#A58AFF")) +  # Code to chose the data point colours
  guides(color = guide_legend(reverse = TRUE)) # Code to reverse the order of the legend

ggsave(here("figs", "covid_ha.jpg"), height = 3.5) # Code to save figure

# Visualisation four
# 2019-20 NBA HA, per Team

# Determine HA for each team separately
pre_covid <- pre_covid %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)
bubble <- bubble %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

# Include a bubble column
pre_covid <- data.frame(append(pre_covid, c(bubble = "Pre COVID-19")))
bubble <- data.frame(append(bubble, c(bubble = "NBA Bubble")))

# Merge pre_covid and bubble by team
ha <- merge(pre_covid, bubble, by = "team" ) # the teams that didn't play in the NBA bubble are removed

# Seperate pre_covid and bubble
pre_covid <- select(ha, -win_perc.y, -bubble.y) 
bubble <- select(ha, -win_perc.x, -bubble.x)

# Rename the columns
names(pre_covid) <- c(
  "team", "win_perc", "bubble"
)

names(bubble) <- c(
  "team", "win_perc", "bubble"
)

# rbind pre_covid and bubble
ha <- rbind(pre_covid, bubble)

# Create a HA ggplot
ha_plot <- ggplot(data = ha, aes(x = team, y = win_perc, group = bubble))

# Check the range of the plot
min(ha$win_perc)
## [1] 0
max(ha$win_perc)
## [1] 100

# Add in the remaining plot details
ha_plot +
  geom_point(aes(color = bubble), size = 3, alpha = .80) +
  coord_flip() + # Code to flip the X and Y axis
  theme_bw() + # Code to add in a nice looking theme
  labs(x = "Team", y = "Home Win Percentage") +
  ylim(0, 100) + # Code to determine the min and max values of the Y axis
  theme(axis.title.y = element_blank()) + # Code to remove the y axis title
  theme(legend.position="none") + # Code to remove the legend
  scale_colour_manual(values = c("#00C094", "#FB61D7")) # Code to chose the data point colours

ggsave(here("figs", "covid_ha_bubble.jpg")) # Code to save figure

# Visualisation five
# 2020-21 NBA HA, per Team

# Determine HA for each team separately
absent <- absent %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)
spectators <- spectators %>% group_by(team) %>% summarise(win_perc = mean(home_win) * 100)

# Include a zero column
absent <- data.frame(append(absent, c(zero = "Zero Spectators")))
spectators <- data.frame(append(spectators, c(zero = "Crowd Size Restrictions")))

# Merge absent and spectators by team
ha <- merge(absent, spectators, by = "team")

# Seperate absent and spectators
absent <- select(ha, -win_perc.y, -zero.y)
spectators <- select(ha, -win_perc.x, -zero.x)

# Rename the columns
names(absent) <- c(
  "team", "win_perc", "full"
)

names(spectators) <- c(
  "team", "win_perc", "full"
)

# rbind absent and spectators
ha <- rbind(absent, spectators)

# Create a HA ggplot
ha_plot <- ggplot(data = ha, aes(x = team, y = win_perc, group = full))

# Check the range of the plot
min(ha$win_perc)
## [1] 0
max(ha$win_perc)
## [1] 85.71429

# Add in the remaining plot details
ha_plot +
  geom_point(aes(color = full), size = 3, alpha = .80) +
  coord_flip() + # Code to flip the X and Y axis
  theme_bw() + # Code to add in a nice looking theme
  labs(x = "Team", y = "Home Win Percentage") +
  theme(legend.position="none") + # Code to remove the legend
  ylim(0, 100) + # for consistency 0 and 100 was used
  theme(axis.title.y = element_blank()) + # Code to remove the y axis title
  scale_colour_manual(values = c("#A58AFF", "#00B6EB")) # Code to chose the data point colours

ggsave(here("figs", "covid_ha_zero.jpg")) # Code to save figure

#### ----
# Figure 34 note calculation - see the paper

# Pre COVID-19 the average home win percentage was
(ha_2016 + ha_2017 + ha_2018)/3
## [1] 58.68264