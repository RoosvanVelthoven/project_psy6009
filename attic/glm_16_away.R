# packages
library(here)

# read in the 2016 away df
away <- read.csv(here("data", "processed", "seasons", "away", "basic_away_box_2016.csv"), row.names = "X")

null <- glm(away_win ~ 1, data = away, family = binomial(link = "logit"))

orb <- glm(away_win ~ orb, data = away, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue with orb

drb <- glm(away_win ~ orb + drb, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, test = "LR")
# continue with drb

ast <- glm(away_win ~ orb + drb + ast, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, test = "LR")
# continue with ast

stl <- glm(away_win ~ orb + drb + ast + stl, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(away_win ~ orb + drb + ast + stl + blk, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, blk, test = "LR")
# continue without blk

tov <- glm(away_win ~ orb + drb + ast + stl + tov, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, test = "LR")
# continue with tov

pf <- glm(away_win ~ orb + drb + ast + stl + tov + pf, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, pf, test = "LR")
# continue without pf

fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, test = "LR")
# continue with fta

attendance <- glm(away_win ~ orb + drb + ast + stl + tov + fta + attendance, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, attendance, test = "LR")
# continue without attendance

fga <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, test = "LR")
# continue with fga

fg3a <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + fg3a, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, fg3a, test = "LR")
# continue without fg3a

# interactions
orb_fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + 
                 orb*fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, orb_fta, test = "LR")
# continue without orb * fta

drb_fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + 
                 drb*fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, drb_fta, test = "LR")
# continue without drb * fta

ast_fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + 
                 ast*fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, ast_fta, test = "LR")
# continue without ast* fta

stl_fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + 
                 stl*fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, stl_fta, test = "LR")
# continue without stl* fta

tov_fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + 
                 tov*fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, tov_fta, test = "LR")
# continue without tov* fta

fga_fta <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga + 
                 fga*fta, data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, fga, fga_fta, test = "LR")
# continue without fga* fta

# final model ------------------------------------------------------------------

away_2016 <- glm(away_win ~ orb + drb + ast + stl + tov + fta + fga, 
                 data = away, family = binomial(link = "logit"))

anova(null, orb, drb, ast, stl, tov, fta, away_2016, test = "LR")

summary(away_2016)