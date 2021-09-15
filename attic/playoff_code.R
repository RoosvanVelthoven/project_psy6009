# playoffs

# create playoff/regular columns

# create playoff_startdate variables
playoff_startdate_2016_17 <- ymd("2017-04-15")
playoff_startdate_2017_18 <- ymd("2018-04-14")
playoff_startdate_2018_19 <- ymd("2019-04-13")
playoff_startdate_2019_20 <- ymd("2020-08-17")
playoff_startdate_2020_21 <- ymd("2021-05-22")

df_2016_17$season <- with(df_2016_17, ifelse(date >= playoff_startdate_2016_17,
                                             "playoff", "regular"
))
df_2017_18$season <- with(df_2017_18, ifelse(date >= playoff_startdate_2017_18,
                                             "playoff", "regular"
))
df_2018_19$season <- with(df_2018_19, ifelse(date >= playoff_startdate_2018_19,
                                             "playoff", "regular"
))
df_2019_20$season <- with(df_2019_20, ifelse(date >= playoff_startdate_2019_20,
                                             "playoff", "regular"
))
df_2020_21$season <- with(df_2020_21, ifelse(date >= playoff_startdate_2020_21,
                                             "playoff", "regular"
))

################################################################################
# create seperate dfs for playoffs and regular matches
playoff_df_2016_17 <- subset(df_2016_17, season == "playoff")
playoff_df_2017_18 <- subset(df_2017_18, season == "playoff")
playoff_df_2018_19 <- subset(df_2018_19, season == "playoff")
playoff_df_2019_20 <- subset(df_2019_20, season == "playoff")
playoff_df_2020_21 <- subset(df_2020_21, season == "playoff")

regular_df_2016_17 <- subset(df_2016_17, season == "regular")
regular_df_2017_18 <- subset(df_2017_18, season == "regular")
regular_df_2018_19 <- subset(df_2018_19, season == "regular")
regular_df_2019_20 <- subset(df_2019_20, season == "regular")
regular_df_2020_21 <- subset(df_2020_21, season == "regular")

# create a list of the dfs
l <- list(
  playoff_df_2016_17, playoff_df_2017_18, playoff_df_2018_19,
  playoff_df_2019_20, playoff_df_2020_21,
  regular_df_2016_17, regular_df_2017_18, regular_df_2018_19,
  regular_df_2019_20, regular_df_2020_21
)

# save the dfs
mapply(write.csv, x = l, file = c(
  "df_2016_2017_playoff.csv", "df_2017_2018_playoff.csv",
  "df_2018_2019_playoff.csv", "df_2019_2020_playoff.csv",
  "df_2020_2021_playoff.csv",
  "df_2016_2017_regular.csv", "df_2017_2018_regular.csv",
  "df_2018_2019_regular.csv", "df_2019_2020_regular.csv",
  "df_2020_2021_regular.csv"
))

# create a list of the dfs
dfs <- list.files("...", pattern = ".csv")

# copy the dfs into the correct folder
file.copy(from = here(dfs), to = here("data", "processed"), overwrite = TRUE)

# remove the old dfs
file.remove(here(dfs))

################################################################################