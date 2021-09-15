# packages
library(here)
library(ggplot2)

# read in the 2018 home df
data <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2018.csv"), row.names = "X")

# linear regression ----

plot(density(data$point_difference), main = "Home and away point_difference")

p <- ggplot(data = data, aes(point_difference)) 

p + geom_histogram(binwidth = 1)

# Outcome variable (point_difference) is discreet 
# (i.e., not continuous; it cannot have a fraction of a point)
# It can moreover not be zero (NBA basketball is continued until one team is ahead in points)
# This may cause more of a bugle on either side of zero 
# (values that would have been zero would usually not end up for away from zero)

# one predictor

lm1 <- lm(point_difference ~ orb, data = data) # linear model where point_difference is modelled as a function of orb