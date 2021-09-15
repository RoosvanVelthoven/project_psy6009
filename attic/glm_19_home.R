# packages
library(here)
library(tidyverse)

# read in the 2019 home df
home <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2019.csv"), row.names = "X")

home_bubble <- filter(home, bubble == "1")
home <- filter(home, bubble == "0")

# home
null <- glm(home_win ~ 1, data = home, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = home, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = home, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue without drb

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
# continue with pf

fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, test = "LR")
# continue with fta

attendance <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, test = "LR")
# continue with attendance

fga <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, test = "LR")
# continue with fg3a

# interactions
drb_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                drb*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, drb_pf, test = "LR")
# continue without drb_pf

drb_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                drb*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, drb_fta, test = "LR")
# continue without drb_fta

ast_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 ast*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, ast_pf, test = "LR")
# continue without ast_pf

ast_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 ast*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, ast_fta, test = "LR")
# continue without ast_fta

stl_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 stl*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, stl_pf, test = "LR")
# continue without stl_pf

stl_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 stl*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, stl_fta, test = "LR")
# continue without stl_fta

blk_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 blk*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, blk_pf, test = "LR")
# continue without blk_pf

blk_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 blk*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, blk_fta, test = "LR")
# continue without blk_fta

tov_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 tov*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_pf, test = "LR")
# continue without tov_pf

tov_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 tov*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_fta, test = "LR")
# continue with tov_fta

fga_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 tov*fta + fga*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_fta, fga_pf, test = "LR")
# continue without fga_pf

fga_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 tov*fta + fga*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_fta, fga_fta, test = "LR")
# continue without fga_fta

fg3a_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                  tov*fta + fg3a*pf, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_fta, fg3a_pf, test = "LR")
# continue without fg3a_pf

fg3a_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 tov*fta + fg3a*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_fta, fg3a_fta, test = "LR")
# continue without fg3a_fta

# final model
final_model <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a +
                 tov*fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, tov_fta, test = "LR")

# home bubble

null <- glm(home_win ~ 1, data = home_bubble, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = home_bubble, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(home_win ~ drb + ast, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, test = "LR")
# continue with ast

stl <- glm(home_win ~ drb + ast + stl, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(home_win ~ drb + ast + stl + blk, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, test = "LR")
# continue without blk

tov <- glm(home_win ~ drb + ast + stl + tov, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, test = "LR")
# continue with tov

pf <- glm(home_win ~ drb + ast + stl + tov + pf, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, test = "LR")
# continue without pf

fta <- glm(home_win ~ drb + ast + stl + tov + fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, test = "LR")
# continue with fta

fga <- glm(home_win ~ drb + ast + stl + tov + fta + fga, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, test = "LR")
# continue without fga

fg3a <- glm(home_win ~ drb + ast + stl + tov + fta + fga + fg3a, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fg3a, test = "LR")
# continue without fg3a

# interactions
drb_fta <- glm(home_win ~ drb + ast + stl + tov + fta + 
              drb*fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, drb_fta, test = "LR")
# continue without drb_fta

ast_fta <- glm(home_win ~ drb + ast + stl + tov + fta + 
                 ast*fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, ast_fta, test = "LR")
# continue without ast_fta

stl_fta <- glm(home_win ~ drb + ast + stl + tov + fta + 
                 stl*fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, stl_fta, test = "LR")
# continue without stl_fta

tov_fta <- glm(home_win ~ drb + ast + stl + tov + fta +
                 tov*fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, tov_fta, test = "LR")
# continue without tov_fta

fga_fta <- glm(home_win ~ drb + ast + stl + tov + fta +
                 fga*fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga_fta, test = "LR")
# continue without tov_fta

# final model
final_model <- glm(home_win ~ drb + ast + stl + tov + fta, data = home_bubble, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, final_model, test = "LR")