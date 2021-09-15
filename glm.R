# Code of the generalized linear model

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

# null model
null_model <- glm(home_win ~ 1, data = box, family = binomial(link = "logit"))

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

season <- glm(home_win ~ season, data = box, family = binomial(link = "logit"))
anova(null_model, season, test = "LR")
## 2     12262      16729  5   33.735 2.688e-06 ***

# Season improves the model

attendance <- glm(home_win ~ season + attendance, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, test = "LR")
## 3     12261      16707  1   21.803 3.021e-06 ***

# Attendance improves the model

fga <- glm(home_win ~ season + attendance + fga, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, fga, test = "LR")
## 4     12260      16707  1    0.524    0.4692 

# fga do not improve the model

fg3a <- glm(home_win ~ season + attendance + fg3a, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, fg3a, test = "LR")
## 4     12260      16704  1    2.963   0.08521 .  

# fg3a do not improve the model

fta <- glm(home_win ~ season + attendance + fta, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, fta, test = "LR")
## 4     12260      16706  1    1.214    0.2705  

# fta do not improve the model

orb <- glm(home_win ~ season + attendance + orb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, orb, test = "LR")
## 4     12260      16707  1    0.047    0.8286

# orb do not improve the model

drb <- glm(home_win ~ season + attendance + drb, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, drb, test = "LR")
## 4     12260      16706  1    1.275    0.2588  

# drb do not improve the model

stl <- glm(home_win ~ season + attendance + stl, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, stl, test = "LR")
## 4     12260      16704  1    3.283   0.06998 .  

# stl do not improve the model

blk <- glm(home_win ~ season + attendance + blk, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, blk, test = "LR")
## 4     12260      16707  1    0.060    0.8073  

# blk do not improve the model

tov <- glm(home_win ~ season + attendance + tov, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, tov, test = "LR")
## 4     12260      16698  1    9.647  0.001896 ** 

# tov improve the model

pf <- glm(home_win ~ season + attendance + tov + pf, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, tov, pf, test = "LR")
## 5     12259      16698  1    0.103  0.748749  

# pf do not improve the model

# Final model
final_model <- glm(home_win ~ season + attendance + tov, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, final_model, test = "LR")

summary(final_model)

# Venue
hagl <- glm(home_win ~ season + attendance + tov + venue, data = box, family = binomial(link = "logit"))
anova(null_model, season, attendance, final_model, hagl, test = "LR")

summary(hagl)

# confidence intervals - logit values
conf_int <- confint(hagl)

# confidence intervals - odd ratios
ex_conf_int <- exp(confint(hagl))

# Summary table
table_hagl <- tidy(hagl)

# Rename the columns
names(table_hagl) <- c("Coefficient", "Estimated value", "SE", "z", "p")

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
  mutate_at(vars(z), list(~ round(., 2)))

# Export
write.table(table_hagl, file = "glm.txt", sep = ",", quote = FALSE, row.names = F)

# conf_int table
conf_int <- as.data.frame(conf_int)

# Round the columns down
conf_int <- conf_int %>% 
  mutate_at(vars(`2.5 %`, `97.5 %`), list(~ round(., 5)))

# Export
write.table(conf_int, file = "conf.txt", sep = ",", quote = FALSE, row.names = F)

# ex_conf_int table
ex_conf_int <- as.data.frame(ex_conf_int)

# Round the columns down
ex_conf_int  <- ex_conf_int  %>% 
  mutate_at(vars(`2.5 %`, `97.5 %`), list(~ round(., 5)))

# Export
write.table(ex_conf_int, file = "ex_conf.txt", sep = ",", quote = FALSE, row.names = F)

#### ----
# Average estimated log odds - see table 4 in the paper
odds_2016 <- -0.58638
odds_2017 <-  0.00860
odds_2018 <-  0.03107
odds_2019 <- -0.13117
odds_zero <-  0.42845
odds_res  <-  0.46331
odds_home <-  0.00059

# 2017-18
odds_2017 <- odds_2016 + odds_2017
##  [1] -0.57778

# 2018-19
odds_2018 <- odds_2016 + odds_2018
## [1] -0.55531

# 2019-20
odds_2019 <- odds_2016 + odds_2019
## [1] -0.71755

# Home team
odds_home <- odds_2016 + odds_home
## [1] -0.58579

# 2019-21
odds_zero <- odds_2016 + odds_zero
## [1] -0.15793

# 2020-21
odds_res <- odds_2016 + odds_res
## [1] -0.12307

# Pre COVID-19, during the 2016-17, 2017-18 and 2018-19 seasons the average
# estimated log odds were
(odds_2016 + odds_2017 + odds_2018)/3
## [1] -0.5731567