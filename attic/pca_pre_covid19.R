
# PCA ----
myPr_home_2016 <- prcomp(variables_home_2016, scale = TRUE) 
# scale = TRUE standardizes the data (i.e., scale is a logical value which indicates whether the variables should be scaled to have unit variance before the analysis takes place)
# note: center = TRUE is the default 
# variables have, thus, already been centered (mean = 0)

# visualisation to demonstrate what scaling does
plot(variables_home_2016$fg, variables_home_2016$fg_pct)
plot(scale(variables_home_2016$fg), scale(variables_home_2016$fg_pct))
# relationship between variables stays the same - scaling and centering just makes it easier to intepret the data

myPr_home_2017 <- prcomp(variables_home_2017, scale = TRUE)
myPr_home_2018 <- prcomp(variables_home_2018, scale = TRUE)

myPr_away_2016 <- prcomp(variables_away_2016, scale = TRUE)
myPr_away_2017 <- prcomp(variables_away_2017, scale = TRUE)
myPr_away_2018 <- prcomp(variables_away_2018, scale = TRUE)

# summary ----
summary(myPr_home_2016)
summary(myPr_home_2017)
summary(myPr_home_2018)

summary(myPr_away_2016)
summary(myPr_away_2017)
summary(myPr_away_2018)
# Scree plots ----

# home_2016
screeplot(myPr_home_2016, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Home 2016: Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

# away_2016
screeplot(myPr_away_2016, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Away 2016:Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

# home_2017
screeplot(myPr_home_2017, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Home 2017:Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

# away_2017
screeplot(myPr_away_2017, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Away 2017:Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

# home_2018
screeplot(myPr_home_2018, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Home 2018:Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

# away_2018
screeplot(myPr_away_2018, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Away 2018:Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

# variance plots ----

cumpro <- cumsum(myPr_home_2016$sdev^2 / sum(myPr_home_2016$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="blue", lty=5)
abline(h = 0.9384, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.5)

cumpro <- cumsum(myPr_away_2016$sdev^2 / sum(myPr_away_2016$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="blue", lty=5)
abline(h = 0.9384, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.5)

# extract scores ----
myPr_home_2016$x

Pr_home_2016 <- cbind(basic_away_box_2016, myPr_home_2016$x[, 1:5])


# correlations between vars and pcs ----
cor(comp_home_2016, Pr_home_2016[, 28:32])
