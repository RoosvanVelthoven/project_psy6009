# packages
library(here)

# read in the 2017 away df
away <- read.csv(here("data", "processed", "seasons", "away", "basic_away_box_2017.csv"), row.names = "X")

null <- glm(away_win ~ 1, data = away, family = binomial(link = "logit"))

orb <- glm(away_win ~ orb, data = away, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(away_win ~ drb, data = away, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(away_win ~ drb + ast, data = away, family = binomial(link = "logit"))

anova(null, drb, ast,test = "LR")
# continue with ast

stl <- glm(away_win ~ drb + ast + stl, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(away_win ~ drb + ast + stl + blk, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, test = "LR")
# continue with blk

tov <- glm(away_win ~ drb + ast + stl + blk + tov, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, test = "LR")
# continue with tov

pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, test = "LR")
# continue with pf

fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, test = "LR")
# continue with fta

attendance <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, test = "LR")
# continue with attendance

fga <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, test = "LR")
# continue with fga

fg3a <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + fg3a, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fg3a, test = "LR")
# continue without fg3a

# interactions
drb_pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                drb*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, drb_pf, test = "LR")
# continue without drb * pf

drb_fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                drb*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, drb_fta, test = "LR")
# continue without drb * fta

ast_pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 ast*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, ast_pf, test = "LR")
# continue without ast * pf

ast_fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 ast*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, ast_fta, test = "LR")
# continue without ast * fta

stl_pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 stl*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, stl_pf, test = "LR")
# continue without stl * pf

stl_fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 stl*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, stl_fta, test = "LR")
# continue without stl * fta

blk_pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 blk*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, blk_pf, test = "LR")
# continue without blk * pf

blk_fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 blk*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, blk_fta, test = "LR")
# continue without blk * fta

tov_pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 tov*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, tov_pf, test = "LR")
# continue without tov * pf

tov_fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 tov*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, tov_fta, test = "LR")
# continue without tov * fta

fga_pf <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 fga*pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fga_pf, test = "LR")
# continue without fga * pf

fga_fta <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + attendance + fga + 
                 fga*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, fga, fga_fta, test = "LR")
# continue without fga * fta

# final model

away_2017 <- glm(away_win ~ drb + ast + stl + blk + tov + pf + fta + 
                     attendance + fga, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, tov, pf, fta, attendance, away_2017, test = "LR")

summary(away_2017)