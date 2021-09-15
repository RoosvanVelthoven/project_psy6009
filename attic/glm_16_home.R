# packages
library(here)

# read in the 2016 home df
home <- read.csv(here("data", "processed", "seasons", "home", "basic_home_box_2016.csv"), row.names = "X")

null <- glm(home_win ~ 1, data = home, family = binomial(link = "logit"))

orb <- glm(home_win ~ orb, data = home, family = binomial(link = "logit"))

anova(null, orb, test = "LR")
# continue without orb

drb <- glm(home_win ~ drb, data = home, family = binomial(link = "logit"))

anova(null, drb, test = "LR")
# continue with drb

stl <- glm(home_win ~ drb + stl, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, test = "LR")
# continue with stl

blk <- glm(home_win ~ drb + stl + blk, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, test = "LR")
# continue with blk

tov <- glm(home_win ~ drb + stl + blk + tov, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, test = "LR")
# continue with tov

pf <- glm(home_win ~ drb + stl + blk + tov + pf, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, test = "LR")
# contiue with pf

fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, test = "LR")
# continue with fta

attendance <- glm(home_win ~ drb + stl + blk + tov + pf + fta + attendance, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, attendance, test = "LR")
# continue without attendance

fga <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga, data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, test = "LR")
# continue with fga

fg3a <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg3a, 
           data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg3a, test = "LR")
# continue without fg3a

fg_pct <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct, 
            data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, test = "LR")
# continue with fg_pct

fg3_pct <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, test = "LR")
# continue with fg3_pct

# interactions ----

drb_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                drb*pf, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, test = "LR")
# continue with drb*pf

drb_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + drb*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, drb_fta, test = "LR")
# continue without drb * fta

stl_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + stl*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, stl_pf, test = "LR")
# continue without stl * pf

stl_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + stl*fta, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, stl_fta, test = "LR")
# continue without stl * fta

blk_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + blk*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, blk_pf, test = "LR")
# continue without blk * pf

blk_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                drb*pf + blk*fta, 
              data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, blk_fta, test = "LR")
# continue without blk * fta

tov_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + tov*pf, 
               data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, tov_pf, test = "LR")
# continue without tov * pf

tov_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + tov*fta, 
               data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, tov_fta, test = "LR")
# continue without tov* fta

fga_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                drb*pf + fga*pf, 
              data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, fga_pf, test = "LR")
# continue without fga*pf

fga_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                drb*pf + fga*fta, 
              data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, fga_fta, test = "LR")
# continue without fga*fta

fg_pct_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                 drb*pf + fg_pct*pf, 
               data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, fg_pct_pf, test = "LR")
# continue without fg_pct_pf 

fg_pct_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                   drb*pf + fg_pct*fta, 
                 data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, fg_pct_fta, test = "LR")
# continue without fg_pct_fta

fg3_pct_pf <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                     drb*pf + fg3_pct*pf, 
                   data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, fg3_pct_pf, test = "LR")
# continue without fg3_pct_pf

fg3_pct_fta <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                    drb*pf + fg3_pct*fta, 
                  data = home, family = binomial(link = "logit"))
anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, drb_pf, fg3_pct_fta, test = "LR")
# continue without fg3_pct_fta

# final model 2016 -------------------------------------------------------------

home_2016 <- glm(home_win ~ drb + stl + blk + tov + pf + fta + fga + fg_pct + fg3_pct +
                   drb*pf,
                   data = home, family = binomial(link = "logit"))

anova(null, drb, stl, blk, tov, pf, fta, fga, fg_pct, fg3_pct, home_2016, test = "LR")

summary(home_2016)
