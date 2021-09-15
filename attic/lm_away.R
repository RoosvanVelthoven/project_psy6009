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
## 4   6126 1240568  1     107.1  0.5287 0.4671933

# Personal fouls do not improve the fit of the model!

fta <- lm(point_difference ~ season + attendance + fta, data = box)
anova(null_model, season, attendance, fta)
## 4   6126 1238470  1    2204.8 10.9061  0.000964 ***

# fta improve the fit of the model

stl <- lm(point_difference ~ season + attendance + fta + stl, data = box)
anova(null_model, season, attendance, fta, stl)
## 5   6125 1215885  1   22585.1 113.7722 < 2.2e-16 ***

# stl improve the fit of the model

blk <- lm(point_difference ~ season + attendance + fta + stl + blk, data = box)
anova(null_model, season, attendance, fta, stl, blk)
## 6   6124 1175362  1     40524 211.1412 < 2.2e-16 ***

# blk improve the fit of the model

tov <- lm(point_difference ~ season + attendance + fta + stl + blk + tov, data = box)
anova(null_model, season, attendance, fta, stl, blk, tov)
## 7   6123 1138276  1     37086 199.4927 < 2.2e-16 ***

# tov improve the fit of the model

fg3a <- lm(point_difference ~ season + attendance + fta + stl + blk + tov + fg3a, data = box)
anova(null_model, season, attendance, fta, stl, blk, tov, fg3a)
## 8   6122 1131768  1      6508  35.2018 3.135e-09 ***

# fg3a improve the fit of the model

drb <- lm(point_difference ~ season + attendance + fta + stl + blk + tov + fg3a + drb, data = box)
anova(null_model, season, attendance, fta, stl, blk, tov, fg3a, drb)
## 9   6121  861580  1    270188 1919.5229 < 2.2e-16 ***

# drb improve the fit of the model

orb <- lm(point_difference ~ season + attendance + fta + stl + blk + tov + fg3a + drb + orb, data = box)
anova(null_model, season, attendance, fta, stl, blk, tov, fg3a, drb, orb)
## 10   6120  861370  1       210    1.4892    0.2224

# orb do not improve the fit of the model

# Final model
final_model <- lm(point_difference ~ season + attendance + fta + stl + blk + tov + fg3a + drb, data = box)
anova(null_model, season, attendance, fta, stl, blk, tov, fg3a, final_model)

summary(final_model)

summary(final_model)

anova(null_model, final_model)

#### ---- lm table

table_away <- tidy(final_model)

names(table_away) <- c("Coefficient", "Estimated value", "SE", "t", "p")

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
  mutate_at(vars(t), list(~ round(., 2)))

# Export
write.table(table_away, file = "lm_away.txt", sep = ",", quote = FALSE, row.names = F)