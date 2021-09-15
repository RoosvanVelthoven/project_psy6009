# packages
library(here)

# read in the 2017 home df
home <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2017.csv"), row.names = "X")

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
# contiue with pf

fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, test = "LR")
# continue with fta

attendance <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, test = "LR")
# continue without attendance

fga <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + fg3a, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, fg3a, test = "LR")
# continue without fg3a

# interactions ----

drb_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + drb*pf, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, drb_pf, test = "LR")
# continue without drb*pf

drb_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + drb*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, drb_fta, test = "LR")
# continue without drb * fta

ast_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, test = "LR")
# continue with ast * pf

ast_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                 ast*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, ast_fta, test = "LR")
# continue without ast * fta

stl_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                stl*pf, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, stl_pf, test = "LR")
# continue without stl * pf

stl_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                 stl*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, stl_fta, test = "LR")
# continue without stl * fta

blk_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                blk*pf, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, blk_pf, test = "LR")
# continue without blk * pf

blk_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                 blk*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, blk_fta, test = "LR")
# continue without blk * fta

tov_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                tov*pf, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, tov_pf, test = "LR")
# continue without tov * pf

tov_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                 tov*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, tov_fta, test = "LR")
# continue without tov * fta

fga_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                 fga*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, fga_pf, test = "LR")
# continue without fga*pf

fga_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                 fga*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, fga_fta, test = "LR")
# continue with fga*fta

# final model

home_2017 <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + ast*pf +
                                     fga*fta, 
                                   data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, home_2017, test = "LR")