# Packages
library(here)
library(tidyverse)

# Read in the data
box <- read.csv(here("data", "processed", "total_basic_box_scores.csv"), row.names = "X")

# Create a crowd column
box <- box %>% 
  mutate(crowd = case_when(attendance == 0 ~ "False", # 0 = no crowd
                           attendance > 0 ~ "True", # > 0 = crowd
  )
  )

# Re-code bubble
box <- box %>% 
  mutate(bubble = case_when(bubble == "Bubble" ~ "True", # bubble game
                            bubble == "Post-bubble" ~ "False", # not a bubble game
                            bubble == "Pre-bubble" ~ "False", # not a bubble game
  )
  )

# null_model
null_model <- glm(home_win ~ 1, data = box, family = binomial(link = "logit"))

# Create an intercept model that includes season, venue, crowd and bubble
# Note one: zero was re-coded to crowd
# Note two: bubble was re-coded 
# Neither variable could be defined due to singularities

model1 <- glm(home_win ~ season + venue + crowd + bubble, data = box, family = binomial(link = "logit"))

model1$coefficients
##  (Intercept) season2017-18 season2018-19 season2019-20 season2020-21     
## 8.430111e-02  1.648546e-02  3.222588e-02 -1.300146e-01 -5.671316e-02 

## venueHome     crowdTrue      bubbleTrue 
## -2.867322e-15  2.503379e-01  1.388039e-01 

# Our reference levels are:
# Season = 2016-17
# Venue = Away
# Crowd = False (no crowd)
# Bubble = False (not a bubble game)

anova(null_model, model1, test = "LR")
## Analysis of Deviance Table

## Model 1: home_win ~ 1
## Model 2: home_win ~ season + venue + crowd + bubble
## Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1     12267      16763                          
## 2     12260      16729  7   34.019 1.708e-05 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Solely season and crowd significantly improved the null model 
# However, for completeness of our understanding of the data
# venue and bubble were included in the model

# To illustrate:
season <- glm(home_win ~ season, data = box, family = binomial(link = "logit"))
anova(null_model, season, test = "LR")
## 2     12263      16738  4    24.97 5.101e-05 ***
# Significant
venue <- glm(home_win ~ venue, data = box, family = binomial(link = "logit"))
anova(null_model, venue, test = "LR")
## 2     12266      16763  1 7.276e-12        1
# Non-significant
crowd <- glm(home_win ~ crowd, data = box, family = binomial(link = "logit"))
anova(null_model, crowd, test = "LR")
## 2     12266      16738  1   24.816 6.308e-07 ***
# Significant
bubble <- glm(home_win ~ bubble, data = box, family = binomial(link = "logit"))
anova(null_model, bubble, test = "LR")
## 2     12266      16760  1   3.1906  0.07406 .
# Close to being significant

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model
attendance <- glm(home_win ~ season + venue + crowd + bubble + attendance, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, test = "LR")
## 3     12259      16707  1   21.803 3.021e-06 ***

# Attendance improves the model!

pf <- glm(home_win ~ season + venue + crowd + bubble + attendance + pf, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, pf, test = "LR")
## 4     12258      16707  1    0.029    0.8648  

# pf do not improve the model

fta <- glm(home_win ~ season + venue + crowd + bubble + attendance + fta, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, fta, test = "LR")
## 4     12258      16706  1    1.154    0.2828 

# fta do not improve the model

stl <- glm(home_win ~ season + venue + crowd + bubble + attendance + stl, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, test = "LR")
## 4     12258      16704  1    3.310   0.06886 . 

# stl do not improve the model
# Although it is close to being significant

blk <- glm(home_win ~ season + venue + crowd + bubble + attendance + blk, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, blk, test = "LR")
## 4     12258      16707  1    0.052    0.8196    

# blk do not improve the model

tov <- glm(home_win ~ season + venue + crowd + bubble + attendance + tov, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, tov, test = "LR")
## 4     12258      16697  1    9.605   0.00194 ** 

# tov improve the model!

fga <- glm(home_win ~ season + venue + crowd + bubble + attendance + tov + fga, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, tov, fga, test = "LR")
## 5     12257      16697  1    0.057   0.81058 

# fga do not improve the model

fg3a <- glm(home_win ~ season + venue + crowd + bubble + attendance + tov + fg3a, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, tov, fg3a, test = "LR")
## 5     12257      16695  1    2.532   0.11154 

# fg3a do not improve the model

drb <- glm(home_win ~ season + venue + crowd + bubble + attendance + tov + drb, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, tov, drb, test = "LR")
## 5     12257      16695  1    2.489   0.11464   

# drb do not improve the model

orb <- glm(home_win ~ season + venue + crowd + bubble + attendance + tov + orb, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, tov, orb, test = "LR")
## 5     12257      16697  1    0.030   0.86341 

# orb do not improve the model

# Final model
final_model <- glm(home_win ~ season + venue + crowd + bubble + attendance + tov, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, final_model, test = "LR")

summary(final_model)