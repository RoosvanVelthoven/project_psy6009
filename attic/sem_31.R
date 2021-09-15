# Packages
library(here)
library(lavaan)
library(tidySEM)

# read in the 2018 home df
home_2018 <- read.csv(here("data", "processed", "seasons", "basic_box_2018.csv"), row.names = "X")

home_2018$attendance <- scale(home_2018$attendance, center = TRUE, scale = TRUE)

home_2018$pf <- scale(home_2018$pf, center = TRUE, scale = TRUE)
home_2018$fta <- scale(home_2018$fta, center = TRUE, scale = TRUE)

home_2018$fga <- scale(home_2018$fga, center = TRUE, scale = TRUE)
home_2018$fg3a <- scale(home_2018$fg3a, center = TRUE, scale = TRUE)

home_2018$orb <- scale(home_2018$orb, center = TRUE, scale = TRUE)
home_2018$drb <- scale(home_2018$drb, center = TRUE, scale = TRUE)

home_2018$stl <- scale(home_2018$stl, center = TRUE, scale = TRUE)
home_2018$tov <- scale(home_2018$tov, center = TRUE, scale = TRUE)
home_2018$blk <- scale(home_2018$blk, center = TRUE, scale = TRUE)

home_2018$point_difference <- scale(home_2018$point_difference, center = TRUE, scale = TRUE)

h_2018 <- '
#CFA model
officials_perf =~ fta + pf
rb =~ drb + orb

#Path model
point_difference ~ a*attendance + e*officials_perf + rb
officials_perf ~ b*attendance
rb ~ attendance

'

fit_h_2018 <- sem(h_2018, data = home_2018)

summary(fit_h_2018)


graph_sem(fit_h_2018, variance_diameter=.2)
