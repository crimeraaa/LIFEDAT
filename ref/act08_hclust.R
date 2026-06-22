##############################
# Clustering (Hierarchical)
# Christian Supsup
# 10 June 2026
#############################

## Hierarchical Clustering
## 1. Agglomerative (Bottom-Up)
##    - Single Linkage (Distance between closest points)
##    - Complete Linkage (Distance between farthest points)
##    - Average Linkage (Average pairwise distance)
##    - Centroid Linkage (Distance between cluster centroids)
##    - Median Linkage (Distance between weighted centroids)
##    - Ward's Method (Minimize increase in within-cluster variance)
## 2. Divisive (Top-Down)
##    - DIANA (Divisive Analysis; Recursively split clusters)

######################
## A. Subset and scale
######################
## Load the Iris dataset
data(iris)

## Select only the numeric variables
iris_data <- iris[, 1:4]

## Standardize
iris_scaled <- scale(iris_data)

######################
## B. Agglomerative
######################
## Compute distance matrix using Euclidean distance
d <- dist(iris_scaled, method = "euclidean")

## 1. Single linkage
hc_single <- hclust(d, method = "single")

plot(hc_single,
     main = "Single Linkage")

## Use species as tip labels
## Attach species as row labels
rownames(iris_scaled) <- iris$Species

plot(hc_single,
     main = "Single Linkage",
     labels = rownames(iris_scaled),
     cex = 0.7)

## 2. Complete linkage
hc_complete <- hclust(d, method = "complete")

plot(hc_complete,
     main = "Complete Linkage",
     labels = rownames(iris_scaled),
     cex = 0.7)

## 3. Average Linkage (UPGMA)
hc_average <- hclust(d, method = "average")

plot(hc_average,
     main = "Average Linkage",
     labels = rownames(iris_scaled),
     cex = 0.7)

## 4. Centroid linkage
hc_centroid <- hclust(d, method = "centroid")

plot(hc_centroid,
     main = "Centroid Linkage",
     labels = rownames(iris_scaled),
     cex = 0.7)

## 5. Median Linkage (WPGMC)
hc_median <- hclust(d, method = "median")

plot(hc_median,
     main = "Median Linkage",
     labels = rownames(iris_scaled),
     cex = 0.7)

## 6. Ward's Method
hc_ward <- hclust(d, method = "ward.D2")

plot(hc_ward,
     main = "Ward D2",
     labels = rownames(iris_scaled),
     cex = 0.7)

######################
## C. K selection using Silhouette
######################
clusters <- cutree(hc_ward, k = 3)

sil <- silhouette(clusters, d)
avg_sil_width <- mean(sil[, 3])

avg_sil_width


######################
## D. Divisive
######################
install.packages("cluster")
library(cluster)

diana_fit <- diana(iris_scaled)

plot(diana_fit,
     main = "DIANA")

