##### ----
# Separate the seasons

box_2016 <- filter(box, year == "2016")
box_2017 <- filter(box, year == "2017")
box_2018 <- filter(box, year == "2018")
box_2019 <- filter(box, year == "2019")
box_2020 <- filter(box, year == "2020")

# Create a list of the season dfs
l <- list(box_2016, box_2017, box_2018, box_2019, box_2020)

# Save the season dfs as .csv
mapply(write.csv, x = l, file = c(
  "basic_box_2016.csv", "basic_box_2017.csv", "basic_box_2018.csv",
  "basic_box_2019.csv", "basic_box_2020.csv"
))

# Create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# Copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed", "seasons"), overwrite = TRUE)

# Remove old dfs
file.remove(here(dfs))

##### ----
# Separate home and away games for 2016-17, 2017-18, 2018-19, 2019-20 and 2020-21

# home
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

# Create a list of the home dfs
l_home <- list(home_box_2016, home_box_2017, home_box_2018, home_box_2019, home_box_2020)

# Save the home dfs as .csv
mapply(write.csv, x = l_home, file = c(
  "basic_home_box_2016.csv", "basic_home_box_2017.csv", "basic_home_box_2018.csv",
  "basic_home_box_2019.csv", "basic_home_box_2020.csv"))

# Create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# Copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed", "seasons", "home"), overwrite = TRUE)

# Remove old dfs
file.remove(here(dfs))

# Create a list of the season dfs
l_away <- list(away_box_2016, away_box_2017, away_box_2018, away_box_2019, away_box_2020)

# Save the season dfs as .csv
mapply(write.csv, x = l_away, file = c(
  "basic_away_box_2016.csv", "basic_away_box_2017.csv", "basic_away_box_2018.csv",
  "basic_away_box_2019.csv", "basic_away_box_2020.csv"))

# Create a list of the season dfs
dfs <- list.files("...", pattern = ".csv")

# Copy the dfs to the correct folder
file.copy(from = here(dfs), to = here("data", "processed", "seasons", "away"), overwrite = TRUE)

# Remove old dfs
file.remove(here(dfs))
