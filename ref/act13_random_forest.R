##############################
# Ensemble learning - Random Forest
# Christian Supsup
# 12 July 2026
#############################

## Random Forest is an ensemble (suverpised) machine learning algorithm
## that combines many decision trees to improve prediction
## accuracy and reduce overfitting.
##
## Instead of building only one decision tree, Random Forest
## builds hundreds/thousands of trees using different
## random samples of the data.
##
## Each tree makes its own prediction, and the forest combines
## all predictions to produce the final result.

######################
## A. Classification
######################
## Classification Random Forest is used when the response
## variable is categorical (factor). Final prediction is based
## on majority vote.
##
## Split criterion: classification Random Forest uses Gini impurity
## to measure node purity.   
##
## G = 1 - Σ(pi²)
##
## where
##
## pi = proportion of class i
##
## Smaller Gini means a purer node.

######################
## B. Regression
######################
## Regression Random Forest is used when the response
## variable is continuous. The final prediction is
## based on average of all tree predictions.
##
## Split criterion: regression Random Forest minimizes
## the Residual Sum of Squares (RSS).
##
## RSS = Σ(yi - ȳ)²
##
## where
##
## yi = observed value
## ȳ  = mean of the node
##
## Smaller RSS indicates that observations
## within the node are more similar.

## STEPS:
##
## a. Prepare the dataset
## b. Specify the response variable
## c. Draw a bootstrap sample
## d. Randomly select mtry predictors
## e. Evaluate all possible splits
## f. Choose the best split
##      Classification → smallest weighted Gini
##      Regression → smallest RSS
## g. Grow the tree to completion
## h. Repeat until ntree trees are built
## i. Combine predictions
##      Classification → Majority vote
##      Regression → Average prediction
## j. Compute Out-of-Bag error
## k. Compute variable importance


## Perform Random Forest Manually
## Step a. Prepare the dataset
data(iris)

## Predictor variables
X <- iris[, 1:4]

## Response variable
Y <- iris$Species

## Number of observations
n <- nrow(iris)

## Number of predictors
p <- ncol(X)

## Step b. Specify response variable
response <- "Species"

## Set hyperparameters
set.seed(123)

## Number of trees
ntree <- 5

## Number of randomly selected predictors
mtry <- floor(sqrt(p))

## Define Gini impurity function
gini <- function(y){
  p <- table(y) / length(y)
  1 - sum(p^2)
}

## Create a vector to store results
forest <- vector("list", ntree)

oob_error <- numeric(ntree)

importance <- rep(0, p)

names(importance) <- names(X)

## Build the forest
for(tree in 1:ntree){
  cat("Building Tree:", tree, "\n")

  ## Step c. Bootstrap sample
  boot_index <- sample(1:n,
                       size = n,
                       replace = TRUE)
  boot_data <- iris[boot_index, ]

  ## Out-of-Bag observations
  oob_index <- setdiff(1:n, unique(boot_index))

  ## Step d. Randomly select mtry predictors
  selected <- sample(names(X), mtry)

  cat("Selected predictors:\n")
  print(selected)

  ## Step e. Evaluate all possible splits
  best_gini <- Inf
  best_split <- NA
  best_variable <- NA

  for(variable in selected){
    values <- sort(unique(boot_data[[variable]]))
    if(length(values) < 2)
      next
    splits <- (values[-1] + values[-length(values)]) / 2

    for(split in splits){
      left <- boot_data[boot_data[[variable]] <= split, ]
      right <- boot_data[boot_data[[variable]] > split, ]

      if(nrow(left)==0 || nrow(right)==0)
        next

      left_gini <- gini(left$Species)
      right_gini <- gini(right$Species)

      weighted_gini <-
        nrow(left)/nrow(boot_data)*left_gini +
        nrow(right)/nrow(boot_data)*right_gini

      if(weighted_gini < best_gini){
        best_gini <- weighted_gini
        best_split <- split
        best_variable <- variable
      }
    }
  }

  ## Step f. Choose the best split
  cat("\nBest variable :", best_variable, "\n")
  cat("Best split    :", best_split, "\n")
  cat("Weighted Gini :", best_gini, "\n")

  ## Step g. Grow the tree
  ## (This example performs only the root split.
  ## A complete tree would recursively repeat the
  ## splitting process on each child node.)

  left <- boot_data[boot_data[[best_variable]] <= best_split, ]
  right <- boot_data[boot_data[[best_variable]] > best_split, ]

  left_prediction <- names(which.max(table(left$Species)))
  right_prediction <- names(which.max(table(right$Species)))

  cat("\nLeft node predicts :", left_prediction, "\n")
  cat("Right node predicts:", right_prediction, "\n")

  ## Save the tree
  forest[[tree]] <- list(
    variable = best_variable,
    split = best_split,
    left_class = left_prediction,
    right_class = right_prediction
  )

  ## Step j. Compute Out-of-Bag error
  if(length(oob_index)>0){
    oob <- iris[oob_index, ]
    prediction <- ifelse(
      oob[[best_variable]] <= best_split,
      left_prediction,
      right_prediction
    )

    prediction <- factor(prediction,
                         levels = levels(Y))
    oob_error[tree] <-
      mean(prediction != oob$Species)
    cat("\nOOB Error:", oob_error[tree], "\n")
  }

  ## Step k. Variable importance
  ## Count how many times each predictor
  ## becomes the best split.

  importance[best_variable] <-
    importance[best_variable] + 1

}

## Step h. Forest completed
cat("Forest construction completed.\n")

## Step i. Combine predictions
new_flower <- iris[1, ]
votes <- character(ntree)

for(i in 1:ntree){
  tree <- forest[[i]]
  if(new_flower[[tree$variable]] <= tree$split){
    votes[i] <- tree$left_class
  } else{
    votes[i] <- tree$right_class
  }
}

cat("\nVotes from each tree:\n")
print(votes)

final_prediction <- names(which.max(table(votes)))

cat("\nMajority Vote Prediction:\n")
print(final_prediction)

## Average Out-of-Bag error
cat("\nAverage OOB Error:\n")
print(mean(oob_error))

## Variable importance
cat("\nVariable Importance\n")
print(importance)


## Using the "randomForest" package

## Install package (run once)
## install.packages("randomForest")

## Load package
library(randomForest)

## Load the iris dataset
data(iris)

## Response variable
response <- "Species"

## Set Random Forest parameters
set.seed(123)

## Number of trees
ntree <- 500

## Number of randomly selected predictors
mtry <- floor(sqrt(ncol(iris) - 1))

## Build the Random Forest
rf_model <- randomForest(
  Species ~ .,          ## Response ~ Predictors
  data = iris,
  ntree = ntree,
  mtry = mtry,
  importance = TRUE
)

## View model
print(rf_model)

## Combine predictions (Majority Vote)
predictions <- predict(rf_model)
head(predictions)

## Confusion Matrix
table(
  Actual = iris$Species,
  Predicted = predictions
)

## Classification Accuracy
accuracy <- mean(predictions == iris$Species)
cat("Accuracy =", accuracy, "\n")

## Out-of-Bag (OOB) Error
cat("OOB Error =",
    rf_model$err.rate[ntree, "OOB"],
    "\n")
## Variable Importance
importance(rf_model)

## Plot Variable Importance
varImpPlot(rf_model)

## Plot OOB Error versus Number of Trees
plot(rf_model$err.rate[, "OOB"],
     type = "l",
     col = "black",
     lwd = 2,
     ylim = c(0, max(rf_model$err.rate)),
     xlab = "Number of Trees",
     ylab = "Error Rate",
     main = "Out-of-Bag Error vs Number of Trees")

## Add the class-specific OOB error lines
lines(rf_model$err.rate[, "setosa"],
      col = "red",
      lwd = 2)
lines(rf_model$err.rate[, "versicolor"],
      col = "green",
      lwd = 2)
lines(rf_model$err.rate[, "virginica"],
      col = "blue",
      lwd = 2)

## Add a legend
legend("topright",
       legend = c("Overall OOB",
                  "setosa",
                  "versicolor",
                  "virginica"),
       col = c("black",
               "red",
               "green",
               "blue"),
       lwd = 2,
       bty = "n")

## Plot tree
library(rpart)
library(rpart.plot)

tree <- rpart(Species ~ ., data = iris)
rpart.plot(tree)
