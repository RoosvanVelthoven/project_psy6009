# Packages
library(here)
library(tidyverse)
library(ggplot2)
library(broom)
library(tidyr)
library(dplyr)

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Filter the data for home performance
box <- filter(box, venue == "Home")

# Check the data
table(box$season)
## 2016-17 2017-18 2018-19 2019-20 2019-21 2020-21 
## 1309    1312    1312     971     752     478 

#### ---- glm
null_model <- glm(home_win ~ 1, data = box, family = binomial(link = "logit"))
season <- glm(home_win ~ season, data = box, family = binomial(link = "logit"))

anova(null_model, season, test = "LR")
## 2      6128     8364.6  5   16.868 0.004758 **

# Season improves the model

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

attendance <- glm(home_win ~ season + attendance, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, test = "LR")
## 3      6127     8353.7  1   10.902 0.0009609 ***

# Attendance improves the fit of the model

pf <- glm(home_win ~ season + attendance + pf, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, test = "LR")
## 4      6126     8320.0  1   33.633 6.655e-09 ***

# pf improve the fit of the  model

fta <- glm(home_win ~ season + attendance + pf + fta, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, test = "LR")
## 5      6125     8218.0  1  101.987 < 2.2e-16 *** 

# fta improve the fit of the model

stl <- glm(home_win ~ season + attendance + pf + fta + stl, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, test = "LR")
## 6      6124     8079.0  1  139.056 < 2.2e-16 ***
# Steals  improve the fit of the model 

blk <- glm(home_win ~ season + attendance + pf + fta + stl + blk, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, test = "LR")
## 7      6123     7936.3  1  142.632 < 2.2e-16 ***

# blocks improve the fit of the model

tov <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, test = "LR")
## 8      6122     7891.7  1   44.673 2.328e-11 ***

# turnovers improve the fit of the model

fg3a <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + fg3a, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, test = "LR")
## 9      6121     7891.7  1    0.000 0.9892692 

# fg3a do not improve the model!

drb <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + drb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, drb, test = "LR")
## 9      6121     6918.6  1   973.03 < 2.2e-16 ***

# drb improve the fit of the model

orb <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + drb + orb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, drb, orb, test = "LR")
## 10      6120     6912.8  1     5.86 0.0154751 *

# orb improve the fit of the model

# final model

final_model <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + drb + orb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, drb, final_model, test = "LR")

summary(final_model)

#### ---- glm table

table_home <- tidy(final_model)

names(table_home) <- c("Coefficient", "Estimated value", "SE", "z", "p")

table_home$Coefficient[table_home$Coefficient=="season2017-18"] <- "2017-18"
table_home$Coefficient[table_home$Coefficient=="season2018-19"] <- "2018-19"
table_home$Coefficient[table_home$Coefficient=="season2019-20"] <- "2019-20"
table_home$Coefficient[table_home$Coefficient=="season2019-21"] <- "2019-21"
table_home$Coefficient[table_home$Coefficient=="season2020-21"] <- "2020-21"

table_home <- table_home %>% 
  mutate_at(vars(p, `Estimated value`, SE), list(~ round(., 5)))

table_home <- table_home %>% 
  mutate_at(vars(p), list(~ round(., 3)))

table_home <- table_home %>% 
  mutate_at(vars(z), list(~ round(., 2)))

# Export
write.table(table_home, file = "glm_home.txt", sep = ",", quote = FALSE, row.names = F)
