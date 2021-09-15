library(here)
library(lavaan)
library(tidySEM)
library(psych)
library(corrplot)

# load in the data ----
basic_home_box_2019_pre_bubble <- read.csv(here("data", "processed", "seasons", "covid_home", "pre_bubble_basic_home_box_2019.csv"))

correlation <- lowerCor(basic_home_box_2019_pre_bubble[c(4:9, 11:19, 21)])
corrplot(correlation, method = "number")

# theories ----
# we have three theories
# theory one: 
# attendance -> home advantage (f)
# attendance -> officials (e)
# attendance -> team performance (d)
# theory two:
# team performance -> home advantage (c)
# team performance -> officials (a)
# theory three:
# officials -> home advantage (b)
# officials -> team performance (ao)

model1 <- '
point_difference ~ 1 + orb + drb + ast + stl + blk + tov + pf + fta + attendance
'
fit1 <- sem(model1, data = basic_home_box_2019_pre_bubble)
summary(fit1)
graph_sem(fit1, variance_diameter=.2)

model2 <- 

# interaction
basic_home_box_2019_pre_bubble$attendancePF = basic_home_box_2019_pre_bubble$attendance * basic_home_box_2019_pre_bubble$pf