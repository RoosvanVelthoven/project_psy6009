library(here)
library(lavaan)
library(tidySEM)

# read in the 2018 home df
data <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2018.csv"), row.names = "X")

# interactions
data$orb_pf <- data$orb * data$pf
data$orb_fta <- data$orb * data$fta

data$drb_pf <- data$drb * data$pf
data$drb_fta <- data$drb * data$fta

data$stl_pf <- data$stl * data$pf
data$stl_fta <- data$stl * data$fta

data$blk_pf <- data$blk * data$pf
data$blk_fta <- data$blk * data$fta

data$tov_pf <- data$tov * data$pf
data$tov_fta <- data$tov * data$fta

data$fga_pf <- data$fga * data$pf
data$fga_fta <- data$fga * data$fta

data$fg3a_pf <- data$fg3a * data$pf
data$fg3a_fta <- data$fg3a * data$fta

data$fg_pct_pf <- data$fg_pct * data$pf
data$fg_pct_fta <- data$fg_pct * data$fta

data$fg3_pct_pf <- data$fg3_pct * data$pf
data$fg3_pct_fta <- data$fg3_pct * data$fta

data$attendance <- (data$attendance/100)

model1 <- '
point_difference ~ 1 + orb + drb + stl + blk + tov + fga + fg3a + fg_pct + fg3_pct +
pf + fta + 
attendance +
orb_pf + orb_fta + drb_pf + drb_fta + stl_pf + stl_fta + blk_pf + blk_fta + 
tov_pf + tov_fta + fga_pf + fga_fta + fg3a_pf + fg3a_fta + fg_pct_pf + fg_pct_fta +
fg3_pct_pf + fg3_pct_fta
orb ~ attendance
drb ~ attendance
stl ~ attendance
blk ~ attendance
tov ~ attendance
fga ~ attendance
fg3a ~ attendance
fg_pct ~ attendance
fg3_pct ~ attendance
pf ~ attendance
fta ~ attendance
'
fit1 <- sem(model1, data = data)

summary(fit1, fit.measures = T)

graph_sem(fit1, variance_diameter=.2)
