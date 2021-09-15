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
null_model <- lm(point_difference ~ 1, data = box)

# Create an intercept model that includes season, venue, crowd and bubble
# Note one: zero was re-coded to crowd (False or True)
# Note two: bubble was re-coded to False and True
# Neither variable could be defined due to singularities

model1 <- lm(point_difference ~ season + venue + crowd + bubble, data = box)

model1$coefficients
##   (Intercept) season2017-18 season2018-19 season2019-20 season2020-21    
##  7.467526e-01 -7.166538e-01 -2.905867e-01 -9.092223e-01 -8.364077e-01 

## venueHome     crowdTrue      bubbleTrue 
## -2.728548e-14  2.336517e+00  1.511307e+00 

# Our reference levels are:
# Season = 2016-17
# Venue = Away
# Crowd = False (no crowd)
# Bubble = False (not a bubble game)

anova(null_model, model1)
## Analysis of Variance Table

## Model 1: point_difference ~ 1
## Model 2: point_difference ~ season + venue + crowd + bubble
## Res.Df     RSS Df Sum of Sq      F   Pr(>F)    
## 1  12267 2496228                                 
## 2  12260 2487006  7    9222.4 6.4947 1.15e-07 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Solely season and crowd significantly improved the null model 
# However, for completeness of our understanding of the data
# venue and bubble were included in the model

# To illustrate:
season <- lm(point_difference ~ season, data = box)
anova(null_model, season)
## 2  12263 2490066  4    6162.2 7.5869 4.227e-06 ***
# Significant
venue <- lm(point_difference ~ venue, data = box)
anova(null_model, venue)
## 2  12266 2496228  1         0  0      1
# Not significant
crowd <- lm(point_difference ~ crowd, data = box)
anova(null_model, crowd)
## 2  12266 2488878  1      7350 36.223 1.81e-09 ***
# Significant
bubble <- lm(point_difference ~ bubble, data = box)
anova(null_model, bubble)
## 2  12266 2495901  1    327.02 1.6071 0.2049
# Not significant 

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

attendance <- lm(point_difference ~ season + venue + crowd + bubble + attendance, data = box)
anova(model1, attendance)
## 2  12259 2480802  1    6204.5 30.66 3.138e-08 ***

# Attendance improves the model!

pf <- lm(point_difference ~ season + venue + crowd + bubble + attendance + pf, data = box)
anova(model1, attendance, pf)
## 3  12258 2480650  1     151.6  0.7491    0.3868

# pf do not improve the model

fta <- lm(point_difference ~ season + venue + crowd + bubble + attendance + fta, data = box)
anova(model1, attendance, fta)
## 3  12258 2480801  1       0.4  0.0019    0.9654

# fta do not improve the model

stl <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl, data = box)
anova(model1, attendance, stl)
## 3  12258 2479542  1    1259.9  6.2288   0.01258 * 

# stl improve the model!

blk <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + blk, data = box)
anova(model1, attendance, stl, blk)
## 4  12257 2479499  1      42.2  0.2086  0.64786

# blk do not improve the model

tov <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + tov, data = box)
anova(model1, attendance, stl, tov)
## 4  12257 2476945  1    2596.9 12.8506 0.0003387 ***

# tov improve the model!

fga <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + tov + fga, data = box)
anova(model1, attendance, stl, tov, fga)
## 5  12256 2476936  1       8.5  0.0422 0.8373064   

# fga do not improve the model

fg3a <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + tov + fg3a, data = box)
anova(model1, attendance, stl, tov, fg3a)
## 5  12256 2476547  1     398.1  1.9704 0.1604356 

# fg3a do not improve the model

drb <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + tov + drb, data = box)
anova(model1, attendance, stl, tov, drb)
## 5  12256 2476830  1     114.4  0.5660 0.4518729 

# drb do not improve the model

orb <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + tov + orb, data = box)
anova(model1, attendance, stl, tov, orb)
## 5  12256 2476940  1       4.4  0.0219 0.8824695  

# orb do not improve the model

# Final model
final_model <- lm(point_difference ~ season + venue + crowd + bubble + attendance + stl + tov, data = box)

anova(null_model, model1, attendance, stl, final_model)

summary(final_model)

anova(null_model, final_model)