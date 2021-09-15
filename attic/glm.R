
#### ---- glm

null_model <- glm(home_win ~ 1, data = box, family = binomial(link = "logit"))
model1 <- glm(home_win ~ season + venue, data = box, family = binomial(link = "logit"))

anova(null_model, model1, test = "LR")
## 2     12261      16729  6   33.735 7.568e-06 ***

# model1 improves the model

# Time to find the best fit model through a statistical approach
# Lets add one predictor variable at a time and see if it improves the fit of the model

attendance <- glm(home_win ~ season + venue + attendance, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, test = "LR")
## 3     12260      16707  1   21.803 3.021e-06 ***

# Attendance improves the fit of the model

pf <- glm(home_win ~ season + venue + attendance + pf, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, pf, test = "LR")
## 4     12259      16707  1    0.049    0.8244 

# pf do not improve the fit of the  model

fta <- glm(home_win ~ season + venue + attendance + fta, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, fta, test = "LR")
## 4     12259      16706  1    1.217    0.2699 

# fta do not improve the fit of the  model

stl <- glm(home_win ~ season + venue + attendance + stl, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, test = "LR")
## 4     12259      16704  1    3.284   0.06996 .
# Steals almost improve the fit of the model lets continue

blk <- glm(home_win ~ season + venue + attendance + stl + blk, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, blk, test = "LR")
## 5     12258      16704  1    0.083   0.77330    

# blocks do not improve the fit of the model

tov <- glm(home_win ~ season + venue + attendance + stl + tov, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, tov, test = "LR")
## 5     12258      16695  1    8.646  0.003278 ** 

# turnovers improve the fit of the model

fga <- glm(home_win ~ season + venue + attendance + stl + tov + fga, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, tov, fga, test = "LR")
## 6     12257      16695  1    0.001  0.981833

# fga do not improve the fit of the model

fg3a <- glm(home_win ~ season + venue + attendance + stl + tov + fg3a, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, tov, fg3a, test = "LR")
## 6     12257      16693  1    2.576  0.108521 

# fg3a do not improve the model

drb <- glm(home_win ~ season + venue + attendance + stl + tov + drb, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, tov, drb, test = "LR")
## 6     12257      16694  1    1.747  0.186287

# drb do not improve the fit of the model

orb <- glm(home_win ~ season + venue + attendance + stl + tov + orb, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, tov, orb, test = "LR")
## 6     12257      16695  1    0.016  0.898213

# orb do not improve the fit of the model

# final model

final_model <- glm(home_win ~ season + venue + attendance + stl + tov, data = box, family = binomial(link = "logit"))
anova(null_model, model1, attendance, stl, final_model, test = "LR")

summary(final_model)
