################################################################################
## Long Exam 2: Coding Problem
##
## You are working wit hthe iris dataset, which contains physical measurements
## of flowers. Your goal is to use the Random Forest algorithm to model the
## relationship between the flower's Species and its four (4) physical attributes,
## namely:
##
##  1) Sepal.Length
##  2) Sepal.Width
##  3) Petal.Length
##  4) Petal.Width
##
################################################################################
library(randomForest)

set.seed(123)

p     <- ncol(iris) - 1
ntree <- 200
mtry  <- p |> sqrt() |> floor()

rf_model <- randomForest::randomForest(Species ~ .,
    data  = iris,
    ntree = ntree,
    mtry  = mtry,
    importance = TRUE)

rf_model

randomForest::importance(rf_model)
randomForest::varImpPlot(rf_model)