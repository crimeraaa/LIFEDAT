##############################
# Clustering (K-means)
# Christian Supsup
# 28 May 2026
#############################

## 1. Choose the Number of Clusters (K)
## 2. Select Initial Centroids
## 3. Assign Data Points to the Closest Centroid
## 4. Calculate New Centroids
## 5. Reassign Data Points


######################
## A. K-means
######################

## Load the Iris dataset
data(iris)

## Select only the numeric variables
iris_data <- iris[, 1:4]

## Standardize
iris_scaled <- scale(iris_data)

## Set seed for reproducibility
set.seed(123)

## Perform K-means clustering with 3 clusters
kmeans_result <- kmeans(iris_scaled, centers = 3, nstart = 25)

## View cluster assignments
head(kmeans_result$cluster)

## Summary of results
kmeans_result

## Compare clusters with actual species
table(Cluster = kmeans_result$cluster,
      Species = iris$Species)

## Scatter plot using first two variables
plot(
  iris_scaled[, 1:2],
  col = kmeans_result$cluster,
  pch = 19,
  xlab = "Sepal Length (scaled)",
  ylab = "Sepal Width (scaled)",
  main = "K-means Clustering of Iris Data"
)

## Add centroids
points(
  kmeans_result$centers[, 1:2],
  col = 1:3,
  pch = 8,
  cex = 2,
  lwd = 2
)

######################
## B. Cluster centroids
######################
iris_scaled <- scale(iris[, 1:4])

## Use first two variables for plotting
x <- iris_scaled[, 1]
y <- iris_scaled[, 2]

## Create a 3x3 plotting layout
par(mfrow = c(3, 3))

## Run K-means with different random starts
for (i in 1:9) {

  set.seed(i)

  km <- kmeans(
    iris_scaled,
    centers = 3,
    nstart = 1
  )

  plot(
    x, y,
    col = km$cluster,
    pch = 19,
    main = paste("Seed =", i),
    xlab = "Sepal Length",
    ylab = "Sepal Width"
  )

  points(
    km$centers[, 1],
    km$centers[, 2],
    pch = 8,
    cex = 2,
    lwd = 2
  )
}

######################
## C. Elbow Curve
######################
#install.packages("factoextra")
library(factoextra)

## Scale the data
iris_scaled <- scale(iris[, 1:4])

## Elbow method
fviz_nbclust(
  iris_scaled,
  kmeans,
  method = "wss",
  k.max = 10
)

