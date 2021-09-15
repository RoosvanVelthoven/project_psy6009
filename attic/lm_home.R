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

#### ---- lm
null_model <- lm(point_difference ~ 1, data = box)

season <- lm(point_difference ~ season, data = box)
anova(null_model, season)
## 2   6128 1243778  5    4336.7 4.2733 0.0007002 ***

# Season improves the fit of the model

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

attendance <- lm(point_difference ~ season + attendance, data = box)
anova(null_model, season, attendance)
## 3   6127 1240675  1    3102.2 15.3202 9.172e-05 ***

# Attendance improves the fit of the model

pf <- lm(point_difference ~ season + attendance + pf, data = box)
anova(null_model, season, attendance, pf)
## 4   6126 1240044  1     631.2  3.1184  0.077465 .

# Personal fouls are close to improving the fit of the model
# Lets continue with them

fta <- lm(point_difference ~ season + attendance + pf + fta, data = box)
anova(null_model, season, attendance, pf, fta)
## 5   6125 1237115  1    2928.6 14.4996 0.0001416 ***

# fta improve the fit of the model

stl <- lm(point_difference ~ season + attendance + pf + fta + stl, data = box)
anova(null_model, season, attendance, pf, fta, stl)
## 6   6124 1196375  1     40741 208.5435 < 2.2e-16 ***

# stl improve the fit of the model

blk <- lm(point_difference ~ season + attendance + pf + fta + stl + blk, data = box)
anova(null_model, season, attendance, pf, fta, stl, blk)
## 7   6123 1165641  1     30734 161.4419 < 2.2e-16 ***

# blk improve the fit of the model

tov <- lm(point_difference ~ season + attendance + pf + fta + stl + blk + tov, data = box)
anova(null_model, season, attendance, pf, fta, stl, blk, tov)
## 8   6122 1150042  1     15599  83.0365 < 2.2e-16 ***

# tov improve the fit of the model

fg3a <- lm(point_difference ~ season + attendance + pf + fta + stl + blk + tov + fg3a, data = box)
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a)
## 9   6121 1146788  1      3254  17.3685 3.121e-05 ***

# fg3a improve the fit of the model

drb <- lm(point_difference ~ season + attendance + pf + fta + stl + blk + tov + fg3a + drb, data = box)
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, drb)
## 10   6120  878526  1    268262 1868.7721 < 2.2e-16 ***

# drb improve the fit of the model

orb <- lm(point_difference ~ season + attendance + pf + fta + stl + blk + tov + fg3a + drb + orb, data = box)
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, drb, orb)
## 11   6119  878033  1       493    3.4323   0.06398 .

# orb are close to improving the fit of the model

# Final model
final_model <- lm(point_difference ~ season + attendance + pf + fta + stl + blk + tov + fg3a + drb, data = box)
anova(null_model, season, attendance, pf, fta, stl, blk, tov, fg3a, final_model)

summary(final_model)

anova(null_model, final_model)

#### ---- lm table

table_home <- tidy(final_model)

names(table_home) <- c("Coefficient", "Estimated value", "SE", "t", "p")

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
  mutate_at(vars(t), list(~ round(., 2)))

# Export
write.table(table_home, file = "lm_home.txt", sep = ",", quote = FALSE, row.names = F)