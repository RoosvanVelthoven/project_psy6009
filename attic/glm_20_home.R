# packages
library(here)
library(tidyverse)

# read in the 2019 home df
home <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2020.csv"), row.names = "X")

home_full <- filter(home, full == "1")
home <- filter(home, full == "0")

# home
null <- glm(home_win ~ 1, data = home, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = home, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = home, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(home_win ~ drb + ast, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, test = "LR")
# continue with ast

stl <- glm(home_win ~ drb + ast + stl, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(home_win ~ drb + ast + stl + blk, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, test = "LR")
# continue with blk

tov <- glm(home_win ~ drb + ast + stl + blk + tov, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, test = "LR")
# continue with tov

pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, test = "LR")
# continue without pf

fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, test = "LR")
# continue with fta

attendance <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, test = "LR")
# continue with attendance

fga <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga + fg3a, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, fg3a, test = "LR")
# continue without fg3a

# interactions
drb_fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga +
                 drb*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, drb_fta, test = "LR")
# continue without drb_fta 

ast_fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga +
                 ast*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, ast_fta, test = "LR")
# continue without ast_fta 

stl_fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga +
                 stl*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, stl_fta, test = "LR")
# continue without stl_fta

blk_fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga +
                 blk*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, blk_fta, test = "LR")
# continue without blk_fta

tov_fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga +
                 tov*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, tov_fta, test = "LR")
# continue without tov_fta

fga_fta <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga +
                 fga*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, fga, fga_fta, test = "LR")
# continue without fga_fta 

# final model

final_model <- glm(home_win ~ drb + ast + stl + blk + tov + fta + attendance + fga, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, fta, attendance, final_model, test = "LR")

# home full
null <- glm(home_win ~ 1, data = home_full, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = home_full, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = home_full, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(home_win ~ drb + ast, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, test = "LR")
# continue with ast

stl <- glm(home_win ~ drb + ast + stl, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue without stl

blk <- glm(home_win ~ drb + ast + blk, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, blk, test = "LR")
# continue without blk

tov <- glm(home_win ~ drb + ast + tov, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, tov, test = "LR")
# continue with tov

pf <- glm(home_win ~ drb + ast + tov + pf, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, tov, pf, test = "LR")
# continue without pf

fta <- glm(home_win ~ drb + ast + tov + fta, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, tov, fta, test = "LR")
# continue without fta

attendance <- glm(home_win ~ drb + ast + tov + attendance, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, tov, attendance, test = "LR")
# continue without attendance

fga <- glm(home_win ~ drb + ast + tov + fga, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, tov, fga, test = "LR")
# continue without fga

fg3a <- glm(home_win ~ drb + ast + tov + fg3a, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, tov, fg3a, test = "LR")
# continue without fg3a

# final model
final_model <- glm(home_win ~ drb + ast + tov, data = home_full, family = binomial(link = "logit"))

anova(null, drb, ast, final_model, test = "LR")
