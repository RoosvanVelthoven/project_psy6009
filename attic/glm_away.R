# Packages
library(here)
library(tidyverse)
library(ggplot2)
library(broom)
library(tidyr)
library(dplyr)

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Filter the data for away performance
box <- filter(box, venue == "Away")

# Check the data
table(box$season)
## 2016-17 2017-18 2018-19 2019-20 2019-21 2020-21 
## 1309    1312    1312     971     752     478 

#### ---- glm

null_model <- glm(home_win ~ 1, data = box, family = binomial(link = "logit"))
season <- glm(home_win ~ season, data = box, family = binomial(link = "logit"))

anova(null_model, season, test = "LR")
## 2      6128     8364.6  5   16.868 0.004758 **

# season improves the model

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

attendance <- glm(home_win ~ season + attendance, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, test = "LR")
## 3      6127     8353.7  1   10.902 0.0009609 ***

# Attendance improves the fit of the model

pf <- glm(home_win ~ season + attendance + pf, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, test = "LR")
## 4      6126     8318.8  1   34.868 3.528e-09 ***

# pf improve the fit of the  model

fta <- glm(home_win ~ season + attendance + pf + fta, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, test = "LR")
## 5      6125     8239.6  1   79.229 < 2.2e-16 ***

# fta improve the fit of the model

stl <- glm(home_win ~ season + attendance + pf + fta + stl, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, test = "LR")
## 6      6124     8160.0  1   79.513 < 2.2e-16 ***

# Steals  improve the fit of the model 

blk <- glm(home_win ~ season + attendance + pf + fta + stl + blk, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, test = "LR")
## 7      6123     7981.4  1  178.597 < 2.2e-16 ***

# blocks improve the fit of the model

tov <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, test = "LR")
## 8      6122     7873.9  1  107.558 < 2.2e-16 ***

# turnovers improve the fit of the model

fg3a <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + fg3a, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, test = "LR")
## 9      6121     7868.7  1    5.186 0.0227704 *

# fg3a improve the model

drb <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + fg3a + drb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, drb, test = "LR")
## 10      6120     6794.2  1  1074.53 < 2.2e-16 ***

# drb improve the fit of the model

orb <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + fg3a + drb + orb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, drb, orb, test = "LR")
## 11      6119     6790.7  1     3.48 0.0620010 . 

# orb almost improved the fit of the model

# final model

final_model <- glm(home_win ~ season + attendance + pf + fta + stl + blk + tov + fg3a + drb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, final_model, test = "LR")

summary(final_model)

#### ---- glm table

table_away <- tidy(final_model)

names(table_away) <- c("Coefficient", "Estimated value", "SE", "z", "p")

table_away$Coefficient[table_away$Coefficient=="season2017-18"] <- "2017-18"
table_away$Coefficient[table_away$Coefficient=="season2018-19"] <- "2018-19"
table_away$Coefficient[table_away$Coefficient=="season2019-20"] <- "2019-20"
table_away$Coefficient[table_away$Coefficient=="season2019-21"] <- "2019-21"
table_away$Coefficient[table_away$Coefficient=="season2020-21"] <- "2020-21"

table_away <- table_away %>% 
  mutate_at(vars(p, `Estimated value`, SE), list(~ round(., 5)))

table_away <- table_away %>% 
  mutate_at(vars(p), list(~ round(., 3)))

table_away <- table_away %>% 
  mutate_at(vars(z), list(~ round(., 2)))

# Export
write.table(table_away, file = "glm_away.txt", sep = ",", quote = FALSE, row.names = F)