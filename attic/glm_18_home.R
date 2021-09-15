# packages
library(here)

# read in the 2018 home df
home <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2018.csv"), row.names = "X")

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
# continue with pf

fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, test = "LR")
# continue with fta

attendance <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + attendance, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, test = "LR")
# continue without attendance

fga <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga, 
           data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga + fg3a, data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, fg3a, test = "LR")
# continue without fga

# interactions ----

drb_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                drb*pf, 
           data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, drb_pf, test = "LR")
# continue without drb*pf

drb_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                drb*fta, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, drb_fta, test = "LR")
# continue without drb*fta

ast_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 ast*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_pf, test = "LR")
# continue without ast*pf

ast_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                ast*fta, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, ast_fta, test = "LR")
# continue without ast*fta

stl_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 stl*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, stl_pf, test = "LR")
# continue without stl*pf

stl_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                stl*fta, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, stl_fta, test = "LR")
# continue without stl*fta

blk_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 blk*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, blk_pf, test = "LR")
# continue without blk*pf

blk_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 blk*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, blk_fta, test = "LR")
# continue without blk*fta

tov_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 tov*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, tov_pf, test = "LR")
# continue without tov*pf

tov_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 tov*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, tov_fta, test = "LR")
# continue without tov*fta

fga_pf <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 fga*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, fga_pf, test = "LR")
# continue without fga*pf

fga_fta <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga +
                 fga*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, fga, fga_fta, test = "LR")
# continue without fga*fta

# final model

home_2018 <- glm(home_win ~ drb + ast + stl + blk + tov + pf + fta + fga, 
                               data = home, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, home_2018, test = "LR")

summary(home_2018)
