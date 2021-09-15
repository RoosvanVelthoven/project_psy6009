# packages
library(here)
library(tidyverse)

# read in the 2019 home df
away <- read.csv(here("data", "processed", "seasons", "away", "basic_away_box_2019.csv"), row.names = "X")

away_bubble <- filter(away, bubble == "1")
away <- filter(away, bubble == "0")

# away

null <- glm(home_win ~ 1, data = away, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = away, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = away, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(home_win ~ drb + ast, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, test = "LR")
# continue with ast

stl <- glm(home_win ~ drb + ast + stl, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(home_win ~ drb + ast + stl + blk, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, test = "LR")
# continue with blk

tov <- glm(home_win ~ drb + ast + stl + blk + tov, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, test = "LR")
# continue with tov

pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, test = "LR")
# continue with pf

fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, test = "LR")
# continue with fta

attendance <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, test = "LR")
# continue with attendance

fga <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, test = "LR")
# continue without fg3a

# interactions
drb_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                drb*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, drb_pf, test = "LR")
# continue without drb_pf

drb_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                drb*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, drb_fta, test = "LR")
# continue without drb_fta

ast_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 ast*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, ast_pf, test = "LR")
# continue without ast_pf

ast_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 ast*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, ast_fta, test = "LR")
# continue without ast_fta

stl_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 stl*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, stl_pf, test = "LR")
# continue without stl_pf

stl_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 stl*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, stl_fta, test = "LR")
# continue without stl_fta

blk_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 blk*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, blk_pf, test = "LR")
# continue without blk_pf

blk_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 blk*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, blk_fta, test = "LR")
# continue without blk_fta

tov_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 tov*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, tov_pf, test = "LR")
# continue without tov_pf

tov_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 tov*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, tov_fta, test = "LR")
# continue without tov_fta

fga_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 fga*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fga_pf, test = "LR")
# continue without fga_pf

fga_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga +
                 fga*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fga_fta, test = "LR")
# continue without fga_fta

# final_model
final_model <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, final_model, test = "LR")

# away bubble
null <- glm(home_win ~ 1, data = away_bubble, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = away_bubble, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(home_win ~ drb + ast, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, test = "LR")
# continue with ast

stl <- glm(home_win ~ drb + ast + stl, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(home_win ~ drb + ast + stl + blk, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, test = "LR")
# continue without blk

tov <- glm(home_win ~ drb + ast + stl + tov, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, test = "LR")
# continue with tov

pf <- glm(home_win ~ drb + ast + stl + tov + pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, test = "LR")
# continue with pf

fta <- glm(home_win ~ drb + ast + stl + tov + pf + fta, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fta, test = "LR")
# continue without fta

fga <- glm(home_win ~ drb + ast + stl + tov + pf + fga, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + ast + stl + tov + pf + fga + fg3a, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, fg3a, test = "LR")
# continue without fg3a

# interactions

drb_pf <- glm(home_win ~ drb + ast + stl + tov + pf + fga +
                drb*pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, drb_pf, test = "LR")
# continue without drb_pf

ast_pf <- glm(home_win ~ drb + ast + stl + tov + pf + fga +
                ast*pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, ast_pf, test = "LR")
# continue with ast_pf 

stl_pf <- glm(home_win ~ drb + ast + stl + tov + pf + fga +
                ast*pf + stl*pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, ast_pf, stl_pf, test = "LR")
# continue without stl_pf 

tov_pf <- glm(home_win ~ drb + ast + stl + tov + pf + fga +
                ast*pf + tov*pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, ast_pf, tov_pf, test = "LR")
# continue without tov_pf 

fga_pf <- glm(home_win ~ drb + ast + stl + tov + pf + fga +
                ast*pf + fga*pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, ast_pf, fga_pf, test = "LR")
# continue without fga_pf 

# final model

final_model <- glm(home_win ~ drb + ast + stl + tov + pf + fga +
                ast*pf, data = away_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, fga, final_model, test = "LR")
