# Code of the linear model

# Packages
library(here)
library(tidyverse)
library(ggplot2)
library(broom)
library(tidyr)
library(dplyr)

# Read in the data (total_box_scores_glm_lm.csv)
box <- read.csv(here("data", "processed", "total_box_scores_glm_lm.csv"), row.names = "X")

# Check the data
table(box$season)

# null_model
null_model <- lm(abs_point_dif ~ 1, data = box)

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

season <- lm(abs_point_dif ~ season, data = box)
anova(null_model, season)
## 2  12262 905159  5    1873.8 5.0769 0.0001186 ***

# Season improves the model

attendance <- lm(abs_point_dif ~ season + attendance, data = box)
anova(null_model, season, attendance)
## 3  12261 904280  1    878.74 11.9146 0.0005588 ***

# Attendance improves the model

fga <- lm(abs_point_dif ~ season + attendance + fga, data = box)
anova(null_model, season, attendance, fga)
## 4  12260 904176  1    103.62  1.4050 0.2359049

# fga do not improve the model

fg3a <- lm(abs_point_dif ~ season + attendance + fg3a, data = box)
anova(null_model, season, attendance, fg3a)
## 4  12260 904065  1    214.96  2.9151 0.0877776 .  

# fg3a do not improve the model

fta <- lm(abs_point_dif ~ season + attendance + fta, data = box)
anova(null_model, season, attendance, fta)
## 4  12260 894522  1    9758.3 133.7432 < 2.2e-16 ***

# fta improve the model

orb <- lm(abs_point_dif ~ season + attendance + fta + orb, data = box)
anova(null_model, season, attendance, fta, orb)
## 5  12259 894052  1     469.9   6.4433 0.0111491 *  

# orb improve the model

drb <- lm(abs_point_dif ~ season + attendance + fta + orb + drb, data = box)
anova(null_model, season, attendance, fta, orb, drb)
## 6  12258 893776  1     275.8   3.7824 0.0518181 . 

# drb do not improve the model

stl <- lm(abs_point_dif ~ season + attendance + fta + orb + stl, data = box)
anova(null_model, season, attendance, fta, orb, stl)
## 6  12258 892319  1    1732.7  23.8025 1.081e-06 ***

# stl improve the model

blk <- lm(abs_point_dif ~ season + attendance + fta + orb + stl + blk, data = box)
anova(null_model, season, attendance, fta, orb, stl, blk)
## 7  12257 891916  1     403.5   5.5447 0.0185523 *

# blk improve the model

tov <- lm(abs_point_dif ~ season + attendance + fta + orb + stl + blk + tov, data = box)
anova(null_model, season, attendance, fta, orb, stl, blk, tov)
## 8  12256 891307  1     609.0   8.3741 0.0038128 ** 

# tov improve the model

pf <- lm(abs_point_dif ~ season + attendance + fta + orb + stl + blk + tov + pf, data = box)
anova(null_model, season, attendance, fta, orb, stl, blk, tov, pf)
## 9  12255 882343  1    8963.7 124.4977 < 2.2e-16 ***

# pf improve the model

# Final model
final_model <- lm(abs_point_dif ~ season + attendance + fta + orb + stl + blk + tov + pf, data = box)
anova(null_model, season, attendance, fta, orb, stl, blk, tov, final_model)

summary(final_model)

# Venue
hagl <- lm(abs_point_dif ~ season + attendance + fta + orb + stl + blk + tov + pf + venue, data = box)
anova(null_model, season, attendance, fta, orb, stl, blk, tov, final_model, hagl)

anova(null_model, hagl)
summary(hagl)

# Residuals (point_difference)
hagl_2 <- lm(point_difference ~ season + attendance + fta + orb + stl + blk + tov + pf + venue, data = box)

summary(hagl_2)

plot(hagl_2)

# Summary table (abs_point_dif)

# Create a summary table and assign it to an object 
table_hagl <- tidy(hagl)

# Rename the columns
names(table_hagl) <- c("Coefficient", "Estimated value", "SE", "t", "p")

# Rename the seasons
table_hagl$Coefficient[table_hagl$Coefficient=="season2017-18"] <- "2017-18"
table_hagl$Coefficient[table_hagl$Coefficient=="season2018-19"] <- "2018-19"
table_hagl$Coefficient[table_hagl$Coefficient=="season2019-20"] <- "2019-20"
table_hagl$Coefficient[table_hagl$Coefficient=="season2019-21"] <- "2019-21"
table_hagl$Coefficient[table_hagl$Coefficient=="season2020-21"] <- "2020-21"

# Round the columns down
table_hagl <- table_hagl %>% 
  mutate_at(vars(p, `Estimated value`, SE), list(~ round(., 5)))

table_hagl <- table_hagl %>% 
  mutate_at(vars(p), list(~ round(., 3)))

table_hagl <- table_hagl %>% 
  mutate_at(vars(t), list(~ round(., 2)))

# Export
write.table(table_hagl, file = "lm.txt", sep = ",", quote = FALSE, row.names = F)

# Point difference per season per venue
tapply(box$points, list(box$venue, box$season), mean)

#### ----
# Average score - see table 3 in the paper
abs_pf_2016 <- 14.40271
abs_pf_2017 <- -0.42455
abs_pf_2018 <- 0.54736
abs_pf_2019 <- 0.15067
abs_pf_zero <- 2.31639
abs_pf_res <- 2.64222
home_team <- 0.00980

# 2017-18
abs_pf_2017 <- abs_pf_2016 + abs_pf_2017
## [1] 13.97816

# 2018-19
abs_pf_2018 <- abs_pf_2016 + abs_pf_2018
## [1] 14.95007

# 2019-20
abs_pf_2019 <- abs_pf_2016 + abs_pf_2019
## [1] 14.55338

# 2019-21
abs_pf_zero <- abs_pf_2016 + abs_pf_zero
## [1] 16.7191

# 2020-21
abs_pf_res <- abs_pf_2016 + abs_pf_res
## [1] 17.04493

# Home team
home_team <- abs_pf_2016 + home_team
## [1] 14.41251

# Pre COVID-19, during the 2016-17, 2017-18, 2018-19 seasons and the 2019-20 
# sample the average score of absolute point difference was
(abs_pf_2016 + abs_pf_2017 + abs_pf_2018 + abs_pf_2019)/4
## [1] 14.47108