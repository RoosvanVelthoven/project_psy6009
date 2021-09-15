df_2017_2018_regular <- read.csv(here("data", "processed", "df_2017_2018_regular.csv"))


teams <- sort(unique(df_2017_2018_regular$away_team))
standings_2017_2018 <- data.frame(team = teams)

# conference & division information: manually input
standings_2017_2018$conf <- c("East", "East", "East", "East", "East",
                              "East", "West", "West", "East", "West",
                              "West", "East", "West", "West", "West",
                              "East", "East", "West", "West", "East",
                              "West", "East", "East", "West", "West",
                              "West", "West", "East", "West", "East")
standings_2017_2018$div <- c("Southeast", "Atlantic", "Atlantic", "Southeast", "Central",
                             "Central", "Southwest", "Northwest", "Central", "Pacific",
                             "Southwest", "Central", "Pacific", "Pacific", "Southwest",
                             "Southeast", "Central", "Northwest", "Southwest", "Atlantic",
                             "Northwest", "Southeast", "Atlantic", "Pacific", "Northwest",
                             "Pacific", "Southwest", "Atlantic", "Northwest", "Southeast")

# populate W-L column, W pct
standings_2017_2018$win <- 0; standings_2017_2018$loss <- 0
for (i in 1:nrow(standings_2017_2018)) {
  standings_2017_2018$win[i]  <- sum(df_2017_2018_regular$winner == standings_2017_2018$team[i])
  standings_2017_2018$loss[i] <- sum(df_2017_2018_regular$loser  == standings_2017_2018$team[i])
}

standings_2017_2018$wl_pct <- with(standings_2017_2018, win / (win + loss))

# Eastern conference standings
east_standings_2017_2018 <- subset(standings_2017_2018, conf == "East")
east_standings_2017_2018[with(east_standings_2017_2018, order(-wl_pct, team)), 
                         c("team", "win", "loss")]

# Western conference standings
west_standings_2017_2018 <- subset(standings_2017_2018, conf == "West")
west_standings_2017_2018[with(west_standings_2017_2018, order(-wl_pct, team)), 
                         c("team", "win", "loss")]