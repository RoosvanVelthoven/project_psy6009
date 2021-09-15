# The first step when analysing data is to plot the data to understand it

# packages
library(here)
library(ggplot2)

# note if you wish to review the data of the away team or another season 
# please read in the correct df and assign it to the object data
# the current r script only utilizes 2018 home data to save space

# read in the 2018 home df
data <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2018.csv"), row.names = "X")

# Lets examine our team performance variables

# point difference

# I need three lists of variables to iterate over
team_performance <- c("orb","drb","ast","stl", "blk", "tov")
titles <- c("Point Difference Based on Offensive Rebounds", "Point Difference Based on Defensive Rebounds",
            "Point Difference Based on Assists", "Point Difference Based on Steals", 
            "Point Difference Based on Blocks", "Point Difference Based on Turnovers")

# I need plot parameters
alpha <- .5
ha <- "point_difference"
xlab  <- c("Offensive Rebounds", "Defensive Rebounds", "Assists", 
           "Steals", "Blocks", "Turnovers")

for (i in 1:6) {
  team_plots <- ggplot(data = data, 
                       mapping = aes_string(x = team_performance[i],
                                            y = ha))
  p <- team_plots + geom_point(alpha = alpha) +
    labs(x = xlab[i], y = "Point Difference", title = titles[i]) +
    geom_smooth(method='lm')
  
  ggsave(filename = paste("figs/team_point",toString(i),".png",sep=""))
}

# home_win

p <- ggplot(data, aes(x = home_win, y = orb, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Offensive Rebounds") +
  ggtitle("Home Win Based on Offensive Rebounds") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "team_home_win_1.png"))

p <- ggplot(data, aes(x = home_win, y = drb, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Defensive Rebounds") +
  ggtitle("Home Win Based on Defensive Rebounds") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "team_home_win_2.png"))

p <- ggplot(data, aes(x = home_win, y = ast, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Assists") +
  ggtitle("Home Win Based on Assists") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "team_home_win_3.png"))

p <- ggplot(data, aes(x = home_win, y = stl, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Steals") +
  ggtitle("Home Win Based on Steals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "team_home_win_4.png"))

p <- ggplot(data, aes(x = home_win, y = blk, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Blocks") +
  ggtitle("Home Win Based on Blocks") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "team_home_win_5.png"))

p <- ggplot(data, aes(x = home_win, y = tov, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Turnovers") +
  ggtitle("Home Win Based on Turnovers") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "team_home_win_6.png"))

# Lets examine our official's performance variables

# I need three lists of variables to iterate over
officials_performance <- c("pf", "fta")
titles <- c("Point Difference Based on Fouls", "Point Difference Based on Free Throw Awarded")
xlab  <- c("Fouls", "Free Throws Awarded")

for (i in 1:2) {
  team_plots <- ggplot(data = data, 
                       mapping = aes_string(x = officials_performance[i],
                                            y = ha))
  p <- team_plots + geom_point(alpha = alpha) +
    labs(x = xlab[i], y = "Point Difference", title = titles[i]) +
    geom_smooth(method='lm')
  
  ggsave(filename = paste("figs/officials_point",toString(i),".png",sep=""))
}

# Lets examine crowd size

# point difference

attendance_plot <- ggplot(data = data,
                          aes(x = attendance, y = point_difference))

p <- attendance_plot + geom_point(alpha = alpha) +
  labs(x = "Crowd Size", y = "Point Difference", title = "Point Difference Based on Crowd Size") +
  geom_smooth(method='lm')

ggsave(here("figs", "crowd_size_point.png"))

# home win
p <- ggplot(data, aes(x = home_win, y = attendance, group = home_win))

p + geom_boxplot() +
  labs(x = "Home Win",
       y = "Crowd Size") +
  ggtitle("Home Win Based on Crowd Size") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2))

ggsave(here("figs", "crowd_size_home_win.png"))