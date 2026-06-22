##############################
# Principal Component Analysis
# Christian Supsup
# 14 June 2026
#############################

## Multivariate Analysis (Unconstrained and Constrained)
## A. Unconstrained (e.g., Principal Component Analysis
##        and Non-metric Multidimensional Scaling)
## B. Constrained (e.g., Canonical Correspondence Analysis)

## Principal Component Analysis (PCA) - is dimensionality reduction technique 
## that transforms a set of correlated variables into a smaller set of 
## uncorrelated variables called principal components (PCs).

## PCA Goal:
## - Reduce the number of variables
## - Remove redundancy due to correlation
## - Retain as much variation (information) as possible
## - Facilitate visualization and interpretation

## Steps:
## A. Standardization
## B. Correlation matrix
## C. Eigen decomposition and explained variance
## D. Scree plot
## E. PCA biplot
## F. PCA with prcomp()
## G. Broken-stick analysis
## H. Interpretation

######################
## A. Subset and scale
######################
## Load the Iris dataset
data(iris)

## Select only the numeric variables
iris_data <- iris[, 1:4]

## Standardize
iris_scaled <- scale(iris_data)

## Check means
colMeans(iris_scaled)

## Check histogram
iris_df <- as.data.frame(iris_scaled)
hist(df$Sepal.Length, col = "lightblue")

######################
## B. Correlation matrix
######################
## Compute pairwise correlation betwee variables
## PCA is based on finding structure in how variables co-vary
cor_mat <- cor(iris_scaled)

round(cor_mat, 2)

######################
## C. Eigen decomposition and explained variance
######################
## Solves for:
## - eigenvectors = directions of maximum variation
## - eigenvalues = amount of variation in each direction

eig <- eigen(cor_mat)

## Compute proportion of variance
prop_var <- eig$values / sum(eig$values)

## Converts eigenvalues into percentages of total variance
prop_var_per <- (eig$values / sum(eig$values)) * 100

## Comptute PC scores
eig_vec <- eig$vectors
scores <- iris_scaled %*% eig_vec

head(scores)

######################
## D. Scree plot
######################
## Scree plot
barplot(prop_var,
        names.arg = paste0("PC", 1:4),
        ylim = c(0, 1),
        col = "steelblue",
        main = "Explained Variance (Scree Plot)",
        ylab = "Proportion of Variance")

######################
## E. PCA biplot
######################
plot(scores[,1], scores[,2],
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA Score Plot (PC1 vs PC2)",
     pch = 19,
     col = iris$Species)
legend("topright",
       legend = levels(iris$Species),
       col = 1:3,
       pch = 19)

## Get eigenvectors for PC1 and 2
loadings <- eig$vectors[, 1:2]

## Scale to match PC scores
load_scale <- 3  # adjust for visibility
loadings_scaled <- loadings * load_scale

## Recreate biplot
plot(scores[,1], scores[,2],
     xlab = "PC1",
     ylab = "PC2",
     main = "PCA Score Plot with Eigenvector Loadings",
     pch = 19,
     col = iris$Species)

## Add points legend
legend("topright",
       legend = levels(iris$Species),
       col = 1:3,
       pch = 19)

## Add eigenvector arrows
arrows(0, 0,
       loadings_scaled[,1],
       loadings_scaled[,2],
       col = "black",
       lwd = 2)

## Add labels for variables
text(loadings_scaled[,1],
     loadings_scaled[,2],
     labels = colnames(iris_data),
     col = "black",
     pos = 3)

######################
## F. PCA with prcomp()
######################
## Run PCA
pca <- prcomp(iris_data, scale. = TRUE)

## Scree plot
eig_values <- pca$sdev^2
prop_var <- eig_values / sum(eig_values)
prop_var

barplot(prop_var,
        names.arg = paste0("PC", 1:length(prop_var)),
        ylim = c(0, 1),
        col = "steelblue",
        main = "Scree Plot (Proportion of Variance)",
        ylab = "Proportion of Variance Explained")

## Biplot
biplot(pca)

######################
## G. Broken-stick
######################

## p = number of variables
## k = component index

## Function
broken_stick <- function(p){
  sapply(1:p, function(k){
    sum(1 / (k:p)) / p
  })
}

bs <- broken_stick(ncol(iris[,1:4]))
bs

## Combine observed and broken stick variance
comparison <- data.frame(
  PC = paste0("PC", 1:4),
  Observed = prop_var,
  BrokenStick = bs
)

comparison

## Plot
plot(prop_var,
     type = "b",
     pch = 19,
     ylim = c(0, max(prop_var)),
     xlab = "Principal Component",
     ylab = "Proportion of Variance",
     main = "Broken-Stick Analysis")

lines(bs,
      type = "b",
      pch = 17,
      col = "red")

legend("topright",
       legend = c("Observed", "Broken Stick"),
       pch = c(19, 17),
       col = c("black", "red"))


######################
## H. Interpretation
######################

## Variance explained (eigenvalues)
## Scree plot / broken-stick
## Loadings (which variables matter?)
## Signs and magnitudes of loadings
## PC score plot (clusters, outliers)
## Biplot (variable relationships)
## Biological or practical meaning of each principal component


## Assignment
https://arleyc.github.io/PCAtest/
https://peerj.com/articles/12967/


