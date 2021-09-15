library(here)
library(ggplot2)
library(psych)
library(corrplot)

# read in the data
basic_away_box_2019_pre_bubble <- read.csv(here("data", "processed", "seasons", "covid_away", "pre_bubble_basic_away_box_2019.csv"))
away_19_pre_bu <- basic_away_box_2019_pre_bubble[4:9]

# correlation matrices
away_matrix_19_pre <- cor(away_19_pre_bu)

# visualisation
corrplot(away_matrix_19_pre, method = "number")

# scree plot
scree(away_matrix_19_pre, factors = FALSE)


# eigenvalues 
eigen(away_matrix_19_pre)
## [1] 2.579659114 1.729879800 1.215310710 0.460651581 0.012012772 0.002486023

# FA ----
fa_away_19_pre_bu <- factanal(away_19_pre_bu, factors = 3,rotation = "varimax")

# proportion var & cumulative var
fa_away_19_pre_bu
##                  Factor1 Factor2 Factor3
## SS loadings      2.084   1.922   1.312
## Proportion Var   0.347   0.320   0.219
## Cumulative Var   0.347   0.668   0.886

apply(fa_away_19_pre_bu$loadings^2,1,sum) 
##        fg       fga    fg_pct       fg3      fg3a   fg3_pct 
## 0.9960085 0.9953923 0.9959042 0.9950694 0.6405610 0.6954502 

# loadings
## Loadings:
##          Factor1 Factor2 Factor3
## fg       0.917   0.105   0.380 
## fga                      0.994 
## fg_pct   0.962          -0.260 
## fg3      0.221   0.973         
## fg3a    -0.207   0.725   0.267 
## fg3_pct  0.475   0.655  -0.201