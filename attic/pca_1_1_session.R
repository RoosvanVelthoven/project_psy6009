# packages
library(here)
library(psych)
library(corrplot)

# PCA steps: ----

# 1) Compute the correlation matrix
# Variables have to be correlated for PCA to be appropriate 
# (i.e., if they are not correlated they are unlikely to share common factors)
# - as a guide look for correlations > 0.35 in absolute size

# 2) Calculate the eigenvalues from the correlation matrix
# The total variance explained by each factor is the eigenvalue
# Retain factors that account for variance > 1

# 3) Sort factors by the eigenvalues and choose which number fits with your screeplot

# 4) Examine the factor matrix 
# Determine which loadings load heavely on which components 
# Does it make sense theoretically that they are loading heavely on a certain component?
# Look for variables which are highly related to the same factor
# i.e., have large loadings for the same factor (usually > .3)
# Then try to identify what the factor is by looking at what these variables have in common

# 5) Examine the communalties 
# Communalties fall since only a subset of the factors is used
# Low communalties suggest that a variable may need to be excluded 
# (i.e., it is not explain well by your components)

# 6) Explore how rotations influence your PCA(if needed)

# 7) Transform your data

# Home 2016 ----

home_2016 <- read.csv(here("data", "processed", "seasons", "basic_box_2016.csv"))

# subset the data 
var.home_2016 <- home_2016[c(4:12, 28)]

# 1) Compute the correlation matrix
lowerCor(var.home_2016)

# It was decided to remove fta and ft for the PCA (they are very highly correlated > .90)
# fta will be utilized as a officials performance variable instead of a team performance variable
# ft will be removed from the analyses
# Moreover, it was decided to remove ft_pct from the PCA it was not correlated highly 
# or even moderately with any of the other variables (except ft)

# visualisation matrix
matrix <- cor(var.home_2016)
corrplot(matrix, method = "number")

# subset the data 
var.home_2016 <- home_2016[c(6, 9)]

# compute PCA ----
pc.home_2016 <- principal(var.home_2016, nfactors = 4, rotate = "varimax") # 6) Explore how rotations influence your PCA(if needed)

# 2) Calculate the eigenvalues from the correlation matrix
pc.home_2016$values

## [1] 1.5235882 0.4764118

# 3) Sort factors by the eigenvalues and choose which number fits with your screeplot
plot_matrix <- cor(var.home_2016)
scree(plot_matrix, factors = FALSE)

# 4) Examine the factor matrix 
pc.home_2016$loadings
## Loadings:
##   PC1  
## fg_pct  0.873
## fg3_pct 0.873

## PC1
## SS loadings    1.524
## Proportion Var 0.762

# 5) Examine the communalities
pc.home_2016$communality

## fg_pct   fg3_pct 
## 0.7617941 0.7617941

# 7) Transform your data
home_2016$pct <- pc.home_2016$scores
