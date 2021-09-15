# packages
library(here)

# For the below analyses I would like to focus on the variables behind home wins.
# For each game, it was recorded whether the home team won (1) or lost (0).
# The below analyses build models to test which variables are predictive of home wins.

# To specify a statistical model that tests which factors are predictive of home wins the function
# glm(s) will be used to create generalized linear models.
# To clarify, generalized linear models are chosen as the outcome variable (home_win) is
# binominal (it has two possible outcomes), and n does not equal 1.
# Furthermore, to be able to fit my linear model, the logit link was used to transform the 
# binominal distribution.

# The models are built in steps; the models are built through a statistical approach,
# all possible predictors are assessed, and only significant predictors are retained.