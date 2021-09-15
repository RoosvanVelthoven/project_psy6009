# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

attendance <- lm(point_difference ~ season + venue + attendance, data = box)
anova(null_model, venue, attendance)
## 3  12259 2480802  1    6204.5 30.6597 3.138e-08 ***

# Attendance improves the fit of the model

pf <- lm(point_difference ~ season + venue + attendance + pf, data = box)
anova(null_model, venue, attendance, pf)

# Pf do not improve the fit of the model

fta <- lm(point_difference ~ season + venue + attendance + fta, data = box)
anova(null_model, venue, attendance, fta)
## 4  12258 2480801  1       0.4  0.0019    0.9654 

# Fta do not improve the fit of the model

stl <- lm(point_difference ~ season + venue + attendance + stl, data = box)
anova(null_model, venue, attendance, stl)
## 4  12258 2479542  1    1259.9  6.2288   0.01258 * 

# Steals improve the fit of the model

blk <- lm(point_difference ~ season + venue + attendance + stl + blk, data = box)
anova(null_model, venue, attendance, stl, blk)
## 5  12257 2479499  1      42.2  0.2086   0.64786 

# blocks do not improve the fit of the model

tov <- lm(point_difference ~ season + venue + attendance + stl + tov, data = box)
anova(null_model, venue, attendance, stl, tov)
## 5  12257 2476945  1    2596.9 12.8506 0.0003387 ***

# turnovers improve the fit of the model

fga <- lm(point_difference ~ season + venue + attendance + stl + tov + fga, data = box)
anova(null_model, venue, attendance, stl, tov, fga)
## 6  12256 2476936  1       8.5  0.0422 0.8373064

# fga do not improve the fit of the model

fg3a <- lm(point_difference ~ season + venue + attendance + stl + tov + fg3a, data = box)
anova(null_model, venue, attendance, stl, tov, fg3a)
## 6  12256 2476547  1     398.1  1.9704 0.1604356

# fg3a do not improve the fit of the model

drb <- lm(point_difference ~ season + venue + attendance + stl + tov + drb, data = box)
anova(null_model, venue, attendance, stl, tov, drb)
## 6  12256 2476830  1     114.4  0.5660 0.4518729 

# drb do not improve the fit of the model

orb <- lm(point_difference ~ season + venue + attendance + stl + tov + orb, data = box)
anova(null_model, venue, attendance, stl, tov, orb)
## 6  12256 2476940  1       4.4  0.0219 0.8824695 

# orb do not improve the fit of the model

# Final model

final_model <- lm(point_difference ~ season + venue + attendance + stl + tov, data = box)
anova(null_model, venue, attendance, stl, final_model)

summary(final_model)

final_model <- lm(point_difference ~ season*venue + attendance + stl + tov, data = box)

summary(final_model)
