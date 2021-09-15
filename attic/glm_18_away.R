# packages
library(here)

# read in the 2018 away df
away <- read.csv(here("data", "processed", "seasons", "away", "basic_away_box_2018.csv"), row.names = "X")

null <- glm(away_win ~ 1, data = away, family = binomial(link = "logit"))

orb <- glm(away_win ~ orb, data = away, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(away_win ~ drb, data = away, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

ast <- glm(away_win ~ drb + ast, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, test = "LR")
# continue with ast

stl <- glm(away_win ~ drb + ast + stl, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, test = "LR")
# continue with stl

blk <- glm(away_win ~ drb + ast + stl + blk, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, blk, test = "LR")
# continue without blk

tov <- glm(away_win ~ drb + ast + stl + tov, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, test = "LR")
# continue with tov

pf <- glm(away_win ~ drb + ast + stl + tov + pf, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, pf, test = "LR")
# continue without pf

fta <- glm(away_win ~ drb + ast + stl + tov + fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, test = "LR")
# continue with fta

attendance <- glm(away_win ~ drb + ast + stl + tov + fta + attendance, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, attendance, test = "LR")
# continue without attendance

fga <- glm(away_win ~ drb + ast + stl + tov + fta + fga, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, test = "LR")
# continue with fga

fg3a <- glm(away_win ~ drb + ast + stl + tov + fta + fga + fg3a, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, fg3a, test = "LR")
# continue without fg3a

# interactions

drb_fta <- glm(away_win ~ drb + ast + stl + tov + fta + fga + 
                 drb*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, drb_fta, test = "LR")
# continue without drb_fta

ast_fta <- glm(away_win ~ drb + ast + stl + tov + fta + fga + 
                 ast*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, ast_fta, test = "LR")
# continue with ast_fta

stl_fta <- glm(away_win ~ drb + ast + stl + tov + fta + fga + 
                 ast*fta + stl*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, ast_fta, stl_fta, test = "LR")
# continue without stl_fta

tov_fta <- glm(away_win ~ drb + ast + stl + tov + fta + fga + 
                 ast*fta + tov*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, ast_fta, tov_fta, test = "LR")
# continue without tov_fta

fga_fta <- glm(away_win ~ drb + ast + stl + tov + fta + fga + 
                 ast*fta + fga*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, ast_fta, fga_fta, test = "LR")
# continue without fga_fta

# final model

away_2018 <- glm(away_win ~ drb + ast + stl + tov + fta + fga + 
                 ast*fta, data = away, family = binomial(link = "logit"))

anova(null, drb, ast, stl, tov, fta, fga, away_2018, test = "LR")

summary(away_2018)
