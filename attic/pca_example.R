library(here)
library(ggplot2)
library(corrplot)
library(tidyverse)

# load the data ----

# read in the processed home basic box scores data
bubble <- read.csv(here("data", "processed", "seasons", "covid_home", "bubble_basic_home_box_2019.csv"))

var_bubble <- bubble[, 4:12]

# centre and scale the data
var_bubble.scaled <- scale(var_bubble, center = TRUE, scale = TRUE)

# compute the correlation matrix
res.cor <- cor(var_bubble.scaled)
round(res.cor,2)

# calculate the eigenvalues/eigenvectors of the correlation matrix
res.eig <- eigen(res.cor)
res.eig


var_bubble <- prcomp(var_bubble, scale = TRUE)
eig.val <- get_eigenvalue(var_bubble)

# compute the new dataset

# transpose eigenvectors
eigenvectors.t <- t(res.eig$vectors)

# transpose the adjusted data
df.scaled.t <- t(var_bubble.scaled)

# the new dataset
df.new <- eigenvectors.t %*% df.scaled.t

df.new <- t(df.new)

colnames(df.new) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")