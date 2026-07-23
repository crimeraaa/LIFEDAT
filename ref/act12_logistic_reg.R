##############################
# Logistic Regression
# Christian Supsup
# 08 July 2026
#############################

## Logistic regression is a statistical method used to model
## the relationship between one or more predictor (independent)
## variables and a binary response (dependent) variable.
##
## Instead of predicting the response directly, logistic
## regression predicts the probability that the response
## belongs to a particular class (usually coded as 1).
##
## The predicted probabilities are constrained between
## 0 and 1 using the logistic (sigmoid) function.

######################
## A. Simple Logistic Regression
######################

## Simple logistic regression models the relationship between
## one predictor variable and one binary response variable.
##
## Response Variable (Y):
## Y = 1 : Event occurs
## Y = 0 : Event does not occur

## 1. Linear Predictor

## z = β0 + β1X
## β0 = intercept
## β1 = regression coefficient
## X  = predictor variable
## z  = linear predictor

# z <- beta0 + beta1 * X

## 2. Logistic Regression Equation

## P(Y = 1) = 1 / (1 + exp(-(β0 + β1X)))

## P(Y = 1) = probability of the event occurring

# P <- 1 / (1 + exp(-(beta0 + beta1 * X)))

## 3. Logit (Log-Odds) Equation

## log(P / (1 - P)) = β0 + β1X
## P = predicted probability

# logit <- log(P / (1 - P))


## 4. Perform Simple Logistic Regression

## STEPS:
## a. Prepare the data
## b. Build the design matrix
## c. Initialize coefficients
## d. Repeat until convergence
##       Compute linear predictor
##       Compute probabilities
##       Compute weights
##       Compute working response
##       Update coefficients
##       Compute log-likelihood
##       Check convergence
## e. Compute final statistics
## f. Plot the logistic curve

### a. Sample data
x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(0,0,0,1,0,1,1,1,1,1)

df <- data.frame(x,y)

### b. Design matrix (with constant 1 to estimate intercept)
X <- cbind(1, x)

X

### c. Initialize coefficients
beta <- matrix(c(0, 0), ncol = 1)

tolerance <- 1e-8
max_iter <- 100 ## iteration

logLik_history <- c()


### d. Reweighted Least Squares (IRLS)
for(i in 1:max_iter){

  ## Linear predictor
  eta <- X %*% beta

  ## Mean (Predicted probability)
  mu <- 1 / (1 + exp(-eta))

  ## Weights
  W <- diag(as.vector(mu * (1 - mu)))

  ## Working response
  z <- eta + (y - mu) / (mu * (1 - mu))

  ## IRLS update (Weighted Least Squares)
  beta_new <- solve(t(X) %*% W %*% X) %*%
              t(X) %*% W %*% z

  ## Log-likelihood
  logLik_history[i] <- sum(
    y * log(mu) +
    (1 - y) * log(1 - mu)
  )

  ## Check convergence
  if(max(abs(beta_new - beta)) < tolerance){

    beta <- beta_new

    cat("Converged after", i, "iterations\n\n")

    break

  }

  beta <- beta_new

}

### e. Compute Final Estimates
eta <- X %*% beta
mu <- 1 / (1 + exp(-eta))
odds <- mu / (1 - mu)
prediction <- ifelse(mu >= 0.5, 1, 0)
likelihood <- mu^y * (1 - mu)^(1 - y)
logLik <- sum(
  y * log(mu) +
  (1 - y) * log(1 - mu)
)

deviance <- -2 * logLik
odds_ratio <- exp(beta)
accuracy <- mean(prediction == y)


### Results
results <- data.frame(
  Hours = x,
  Actual = y,
  LinearPredictor = round(eta,4),
  Probability = round(mu,4),
  Odds = round(odds,4),
  Prediction = prediction,
  Likelihood = round(likelihood,4)

)

print(results)

cat("\nEstimated Coefficients\n")
print(beta)

cat("\nOdds Ratio\n")
print(odds_ratio)

cat("\nLog-Likelihood =", logLik, "\n")
cat("Deviance =", deviance, "\n")
cat("Accuracy =", accuracy, "\n")

cat("\nConfusion Matrix\n")
print(table(Observed = y,
            Predicted = prediction))

### Plot logistic curve
x_new <- seq(min(x), max(x), length.out = 200)
X_new <- cbind(1, x_new)

eta_new <- X_new %*% beta

mu_new <- 1 / (1 + exp(-eta_new))

plot(x, y,
     pch = 19,
     ylim = c(0,1),
     xlab = "Hours Studied",
     ylab = "Probability",
     main = "Simple Logistic Regression")
lines(x_new,
      mu_new,
      col = "blue",
      lwd = 2)
points(x,
       mu,
       col = "red",
       pch = 19)
legend("topleft",
       legend = c("Observed",
                  "Predicted",
                  "Logistic Curve"),
       pch = c(19,19,NA),
       lty = c(NA,NA,1),
       col = c("black","red","blue"))

### Convergence Plot
plot(logLik_history,
     type = "b",
     pch = 19,
     col = "darkgreen",
     xlab = "Iteration",
     ylab = "Log-Likelihood",
     main = "IRLS Convergence")


## Logistic Regression using glm() ##

## Data
x <- c(1,2,3,4,5,6,7,8,9,10)
y <- c(0,0,0,1,0,1,1,1,1,1)

data <- data.frame(x, y)

## Fit logistic regression model
model <- glm(y ~ x,
             data = data,
             family = binomial(link = "logit"))

## Display model summary
summary(model)

## Estimated coefficients
coef(model)

## Odds ratios
exp(coef(model))

## Predicted probabilities
data$Probability <- predict(model,
                            type = "response")

## Predicted class (cutoff = 0.5)
data$Prediction <- ifelse(data$Probability >= 0.5, 1, 0)

## View results
print(data)

## Confusion Matrix
table(Observed = data$y,
      Predicted = data$Prediction)

## Plot Logistic Regression

## Create smooth x values
x_new <- seq(min(x), max(x), length.out = 200)

## Predict probabilities
pred <- predict(model,
                newdata = data.frame(x = x_new),
                type = "response")

## Plot observed data
plot(x, y,
     pch = 19,
     xlab = "Hours Studied",
     ylab = "Probability",
     ylim = c(0,1),
     main = "Logistic Regression using glm()")

## Add fitted logistic curve
lines(x_new,
      pred,
      col = "blue",
      lwd = 2)

## Add predicted probabilities
points(x,
       data$Probability,
       pch = 19,
       col = "red")

## Decision threshold
abline(h = 0.5,
       lty = 2,
       col = "darkgreen")

## Legend
legend("bottomright",
       legend = c("Observed Data",
                  "Predicted Probability",
                  "Logistic Curve",
                  "0.5 Threshold"),
       pch = c(19,19,NA,NA),
       lty = c(NA,NA,1,2),
       col = c("black","red","blue","darkgreen"),
       lwd = c(NA,NA,2,1))


######################
## B. Multiple Logistic Regression
######################

## Multiple logistic regression models the relationship between
## two or more predictor variables and one binary response variable.

## 1. Linear Predictor

## z = β0 + β1X1 + β2X2 + ... + βkXk
##
## β0 = intercept
## β1 = coefficient for predictor X1
## β2 = coefficient for predictor X2
## ...
## βk = coefficient for predictor Xk

# z <- beta0 +
#      beta1 * X1 +
#      beta2 * X2 +
#      beta3 * X3

## 2. Logistic Regression Equation

## P(Y = 1) =
##      1
## --------------------------
## 1 + exp(-(β0 + β1X1 + β2X2 + ... + βkXk))
##

# P <- 1 / (1 + exp(-(beta0 +
#                     beta1 * X1 +
#                     beta2 * X2 +
#                     beta3 * X3)))

## 3. Logit (Log-Odds) Equation

## log(P / (1 - P))
##      =
## β0 + β1X1 + β2X2 + ... + βkXk

# logit <- log(P / (1 - P))


### Perfom with glm() using random data
set.seed(123)

n <- 100

x1 <- rnorm(n, 50, 10)
x2 <- rnorm(n, 30, 5)

eta <- -15 + 0.20*x1 + 0.25*x2
p <- 1/(1 + exp(-eta))

y <- rbinom(n, 1, p)

data <- data.frame(x1, x2, y)

model <- glm(y ~ x1 + x2,
             family = binomial,
             data = data)

summary(model)


# Sequence of x1 values
newdata <- data.frame(
  x1 = seq(min(data$x1), max(data$x1), length.out = 100),
  x2 = mean(data$x2)          # Hold x2 constant
)

# Predicted probabilities
newdata$prob <- predict(model,
                        newdata = newdata,
                        type = "response")

# Plot
plot(data$x1, data$y,
     pch = 19,
     xlab = "x1",
     ylab = "Probability",
     ylim = c(0,1))

lines(newdata$x1,
      newdata$prob,
      col = "blue",
      lwd = 3)