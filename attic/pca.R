library(here)
library(psych)
library(corrplot)

# load the data ----

# home data 2016, 2017 and 2018
filenames <- list.files(path=here("data", "processed", "seasons", "pre_covid_home"),
                        pattern="basic_home_box_.*csv")

# create list of data frame names without the ".csv" part 
names <-substr(filenames,1,19)

for(i in names){
  filepath <- file.path(here("data", "processed", "seasons", "pre_covid_home"),paste(i,".csv",sep=""))
  assign(i, read.delim(filepath,
                       sep = ","))
}

# home data 2019 and 2020
basic_home_box_2019_pre_bubble <- read.csv(here("data", "processed", "seasons", "covid_home", "pre_bubble_basic_home_box_2019.csv"))
basic_home_box_2019_bubble <- read.csv(here("data", "processed", "seasons", "covid_home", "bubble_basic_home_box_2019.csv"))

basic_home_box_2020_absent <- read.csv(here("data", "processed", "seasons", "covid_home", "absent_basic_home_box_2020.csv"))
basic_home_box_2020_full <- read.csv(here("data", "processed", "seasons", "covid_home", "full_basic_home_box_2020.csv"))

# away data
filenames <- list.files(path=here("data", "processed", "seasons", "pre_covid_away"),
                        pattern="basic_away_box_.*csv")

# create list of data frame names without the ".csv" part 
names <-substr(filenames,1,19)

for(i in names){
  filepath <- file.path(here("data", "processed", "seasons", "pre_covid_away"),paste(i,".csv",sep=""))
  assign(i, read.delim(filepath,
                       sep = ","))
}

# away data 2019 and 2020
basic_away_box_2019_pre_bubble <- read.csv(here("data", "processed", "seasons", "covid_away", "pre_bubble_basic_away_box_2019.csv"))
basic_away_box_2019_bubble <- read.csv(here("data", "processed", "seasons", "covid_away", "bubble_basic_away_box_2019.csv"))

basic_away_box_2020_absent <- read.csv(here("data", "processed", "seasons", "covid_away", "absent_basic_away_box_2020.csv"))
basic_away_box_2020_full <- read.csv(here("data", "processed", "seasons", "covid_away", "full_basic_away_box_2020.csv"))

# create PCA dfs ----
# create three dfs with the nine variables of interest for the home and away team
home_16 <- basic_home_box_2016[4:12]
home_17 <- basic_home_box_2017[4:12]
home_18 <- basic_home_box_2018[4:12]
home_19_pre_bu <- basic_home_box_2019_pre_bubble[4:12]
home_19_bu <- basic_home_box_2019_bubble[4:12]
home_20_ab <- basic_home_box_2020_absent[4:12]
home_20_full <- basic_home_box_2020_full[4:12]

away_16 <- basic_away_box_2016[4:12]
away_17 <- basic_away_box_2017[4:12]
away_18 <- basic_away_box_2018[4:12]
away_19_pre_bu <- basic_away_box_2019_pre_bubble[4:12]
away_19_bu <- basic_away_box_2019_bubble[4:12]
away_20_ab <- basic_away_box_2020_absent[4:12]
away_20_full <- basic_away_box_2020_full[4:12]

# step one: examine the correlation matrices
home_matrix_16 <- cor(home_16)
corrplot(home_matrix_16, method = "number")
away_matrix_16 <- cor(away_16)
corrplot(away_matrix_16, method = "number")

home_matrix_17 <- cor(home_17)
corrplot(home_matrix_17, method = "number")
away_matrix_17 <- cor(away_17)
corrplot(away_matrix_17, method = "number")

home_matrix_18 <- cor(home_18)
corrplot(home_matrix_18, method = "number")
away_matrix_18 <- cor(away_18)
corrplot(away_matrix_18, method = "number")

home_matrix_19_pre <- cor(home_19_pre_bu)
corrplot(home_matrix_19_pre, method = "number")
away_matrix_19_pre <- cor(away_19_pre_bu)
corrplot(away_matrix_19_pre, method = "number")

home_matrix_19_bu <- cor(home_19_bu)
corrplot(home_matrix_19_bu, method = "number")
away_matrix_19_bu <- cor(away_19_bu)
corrplot(away_matrix_19_bu, method = "number")

home_matrix_20_ab <- cor(home_20_ab)
corrplot(home_matrix_20_ab, method = "number")
away_matrix_20_ab <- cor(away_20_ab)
corrplot(away_matrix_20_ab, method = "number")

home_matrix_20_f <- cor(home_20_full)
corrplot(home_matrix_20_f, method = "number")
away_matrix_20_f <- cor(away_20_full)
corrplot(away_matrix_20_f, method = "number")

# the correlation matrices appear to show similar patterns across the seasons 
# several correlations are moderately large (r > .30)
# this suggests that there may be fewer underlying components than nine different constructs
# moreover two variables were very highly correlated (> .90): free throws and free throw attempts
# based on the underlying theory and the high correlation between free throws and free throw attempts it was decided to:
# retain ft_pct as the team performance variable of free throws (and to not include the ft_pct variable in the PCA)
# include fta as a referee decision variable instead of a team performance variable
# exclude ft (ft are partially included in fta and ft_pct)

# create three dfs with the nine variables of interest for the home and away team
home_16 <- basic_home_box_2016[4:9]
home_17 <- basic_home_box_2017[4:9]
home_18 <- basic_home_box_2018[4:9]
home_19_pre_bu <- basic_home_box_2019_pre_bubble[4:9]
home_19_bu <- basic_home_box_2019_bubble[4:9]
home_20_ab <- basic_home_box_2020_absent[4:9]
home_20_full <- basic_home_box_2020_full[4:9]

away_16 <- basic_away_box_2016[4:9]
away_17 <- basic_away_box_2017[4:9]
away_18 <- basic_away_box_2018[4:9]
away_19_pre_bu <- basic_away_box_2019_pre_bubble[4:9]
away_19_bu <- basic_away_box_2019_bubble[4:9]
away_20_ab <- basic_away_box_2020_absent[4:9]
away_20_full <- basic_away_box_2020_full[4:9]



# PCA ----
pr_home_16 <- prcomp()
pr_home_17 <- prcomp()
pr_home_18 <- prcomp()
pr_home_19_pre_bu <- prcomp() 
pr_home_19_bu <- prcomp()
pr_home_20_ab <- prcomp()
pr_home_20_full <- prcomp()

pr_away_16 <- prcomp()
pr_away_17 <- prcomp()
pr_away_18 <- prcomp()
pr_away_19_pre_bu <- prcomp()
pr_away_19_bu <- prcomp()
pr_away_20_ab <- prcomp()
pr_away_20_full <- prcomp()
