library(here)
library(ggplot2)
library(corrplot)
library(factoextra)

# read in the processed basic_box_scores_home data
filenames <- list.files(path=here("data", "processed", "seasons", "pre_covid_home"),
                        pattern="basic_home_box_.*csv")

# create list of data frame names without the ".csv" part 
names <-substr(filenames,1,19)

for(i in names){
  filepath <- file.path(here("data", "processed", "seasons", "pre_covid_home"),paste(i,".csv",sep=""))
  assign(i, read.delim(filepath,
                       sep = ","))
}

# create a new df of just the 9 variables of interests for the 2016 data
throws_2016 <- basic_home_box_2016[c(4:12)]

# Lets have a look at our correlations, to determine if pca is appropriate
throws_2016_matrix <- cor(throws_2016)
corrplot(throws_2016_matrix, method = "number")

# it appears that we have groups of variables that are correlated
# e.g., ft is correlated with fta (0.93) and ft_pct (0.34)

# pca should thus be warrented 

# compute pca
pca.throws_2016 <- prcomp(throws_2016, scale = TRUE)

# time to examine eigenvalues
eig.val <- get_eigenvalue(pca.throws_2016)

eig.val
##        eigenvalue variance.percent cumulative.variance.percent
## Dim.1 2.756502589      30.62780654                    30.62781
## Dim.2 1.977646716      21.97385240                    52.60166
## Dim.3 1.491275495      16.56972772                    69.17139
## Dim.4 1.215242363      13.50269292                    82.67408
## Dim.5 1.005178263      11.16864737                    93.84273
## Dim.6 0.527566623       5.86185136                    99.70458
## Dim.7 0.016378027       0.18197808                    99.88656
## Dim.8 0.007551255       0.08390284                    99.97046
## Dim.9 0.002658670       0.02954078                   100.00000

# the data suggest to retain 5 components (eigenvalue > 1)

# time to examine scree plots
fviz_eig(pca.throws_2016)

screeplot(pca.throws_2016, type = "l", npcs = 9, main = "Screeplot")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=1)

cumpro <- cumsum(pca.throws_2016$sdev^2 / sum(pca.throws_2016$sdev^2))
plot(cumpro[0:9], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 5, col="blue", lty=5)
abline(h = 0.9384, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.5)

# the scree plots also appear to suggest that after five factors the debree of the plot starts

# time to examine the loadings

print(pca.throws_2016)
## Standard deviations (1, .., p=9):
##   [1] 1.66027184 1.40628828 1.22117791 1.10238032 1.00258579 0.72633782 0.12797667 0.08689796
## [9] 0.05156229

## Rotation (n x k) = (9 x 9):
##                PC1          PC2         PC3         PC4           PC5          PC6          PC7
## fg      -0.45628263 -0.040900053  0.17698728 -0.54716053  0.0496294812 -0.127692956  0.034964212
## fga     -0.12957636  0.338293311 -0.29827450 -0.64164571 -0.0050666352  0.421779787 -0.019545956
## fg_pct  -0.40621535 -0.291271087  0.40863319 -0.13681304  0.0597074581 -0.437553421 -0.050083206
## fg3     -0.45765249 -0.225240006 -0.40036657  0.23786020  0.0125414321  0.106882441  0.713691742
## fg3a    -0.23910585  0.003232093 -0.69858520  0.07984187 -0.0018622306 -0.441251063 -0.502749546
## fg3_pct -0.40945225 -0.329103042  0.11620932  0.26566937  0.0155905172  0.636432526 -0.483272099
## ft       0.29559609 -0.566256224 -0.15904439 -0.26088445  0.0006263221  0.039733305  0.012921982
## fta      0.29288013 -0.515954644 -0.15795591 -0.23412455  0.3584588967  0.038818331 -0.007276409
## ft_pct   0.05080214 -0.230517573 -0.02573467 -0.11739218 -0.9300802496 -0.009220062 -0.001509292
##                PC8           PC9
## fg      -0.0056496023 -0.6628734557
## fga      0.0042105973  0.4356226316
## fg_pct  -0.0005081739  0.6057431745
## fg3      0.0106566460  0.0435851770
## fg3a    -0.0090284275 -0.0296200857
## fg3_pct -0.0050049716 -0.0329973892
## ft      -0.7048852156  0.0034289154
## fta      0.6619414202 -0.0053506999
## ft_pct   0.2543626493 -0.0008239571





comp_home_box_2016 <- pca.home_box_2016$x[,1:5]
test <- cbind(basic_home_box_2016[c(13:23, 25)], comp_home_box_2016)












################################################################################
# the below code focusses on 2016, 2017, 2018

# Lets have a look at our correlations, to determine if a factor analysis is appropriate
throws_2016_matrix <- cor(throws_2016)
corrplot(throws_2016_matrix, method = "number")

perf_box_2017_matrix <- cor(perf_box_2017)
corrplot(perf_box_2017_matrix, method = "number")

perf_box_2018_matrix <- cor(perf_box_2018)
corrplot(perf_box_2018_matrix, method = "number")

# Variables must be related to each other for factor analysis to be appropriate.
# We have found correlations of ≥ 0.35, factor analysis should thus be appropriate.

# check two: Kaiser-Meyer-Olkin (KMO)
# the KMO is a better measure of factorability than the correlation matrix

x <- perf_box_2016

fafitfree <- fa(perf_box_2018,nfactors = ncol(x), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

KMO(x)

pca.var <- box_2016.pr$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree Plot", xlab = "Principal Component",
        ylab = "Percent Variation")
