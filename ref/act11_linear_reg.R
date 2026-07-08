##############################
# Linear Regression
# Christian Supsup
# 04 July 2026
#############################

## Linear regression is a statistical method used to model and quantify 
## the relationships between the response (dependent) variable and the predictor (independent)
## variable. It fits a straight line that best describes the relationships by minimizing
## the sum of squared differences (residuals) between the observed and predicted values.

## Regression Equation:
## y = a + bx
##
## a = intercept
## b = slope
## x = independent variable
## y = dependent variable

## Intercept:
## a = ȳ - bx̄
##
## ȳ = mean of dependent variable
## x̄ = mean of independent variable

a <- mean(y) - b * mean(x)

## Slope:
## b = Σ[(x - x̄)(y - ȳ)] / Σ[(x - x̄)^2]
##
## x̄ = mean of independent variable
## ȳ = mean of dependent variable

b <- sum((x - mean(x)) * (y - mean(y))) /
     sum((x - mean(x))^2)

######################
## A. Simple linear regression - Manual
######################
## Height (independent)
x <- c(150, 155, 160, 165, 170, 175, 180, 185, 190, 195)

## Arm Length (dependent)
y <- c(58, 61, 63, 60, 67, 69, 66, 74, 72, 80)

## Mean of variables
xbar <- mean(x)
ybar <- mean(y)

xbar
ybar

## Calculate the slope

b <- sum((x - xbar) * (y - ybar)) /
     sum((x - xbar)^2)
b

## Calculate the intercept
a <- ybar - b * xbar
a

## Write the equation
cat("Regression Equation:\n")
cat("y =", round(a,3), "+", round(b,3), "x\n")

## Calculate the predicted arm length
yhat <- a + b * x

data.frame(
  Height = x,
  ArmLength = y,
  Predicted = round(yhat,2)
)

## Plot the data with fitted line
plot(x, y,
     pch = 19,
     col = "blue",
     xlab = "Height (cm)",
     ylab = "Arm Length (cm)",
     main = "Simple Linear Regression")

abline(a, b,
       col = "red",
       lwd = 2)

## Plot with predicted values

## Observed data
plot(x, y,
     pch = 19,
     col = "blue",
     xlab = "Height (cm)",
     ylab = "Arm Length (cm)",
     main = "Simple Linear Regression")

## Add fitted regression line
abline(a, b,
       col = "red",
       lwd = 2)

## Add predicted values
points(x, yhat,
       pch = 17,      # Triangle
       col = "darkgreen",
       cex = 1.2)

## Draw residual lines
segments(x0 = x, y0 = y,
         x1 = x, y1 = yhat,
         lty = 2,
         col = "gray50")

## Add legend
legend("topleft",
       legend = c("Observed", "Predicted", "Regression Line"),
       pch = c(19, 17, NA),
       lty = c(NA, NA, 1),
       col = c("blue", "darkgreen", "red"),
       lwd = c(NA, NA, 2),
       bty = "n")

## Calculate R squared
## Sum of Squared Errors (SSE)
## SSE = Σ(y - ŷ)²

SSE <- sum((y - yhat)^2)

## Total Sum of Squares (SST)
## SST = Σ(y - ȳ)²

SST <- sum((y - ybar)^2)

## Coefficient of Determination
## R² = 1 - (SSE / SST)

R2 <- 1 - (SSE / SST)
R2

######################
## B. Simple linear regression using 'lm' function
######################
## Height (independent)
x <- c(150, 155, 160, 165, 170, 175, 180, 185, 190, 195)

## Arm Length (dependent)
y <- c(58, 61, 63, 60, 67, 69, 66, 74, 72, 80)

## Fit regression model
model <- lm(y ~ x + x1 + x2 + x2)

## Display model summary
summary(model)

## Regression coefficients
coef(model)

## Intercept
a <- coef(model)[1]

## Slope
b <- coef(model)[2]

cat("Regression Equation:\n")
cat("y =", round(a,3), "+", round(b,3), "x\n\n")

## Predicted Values
yhat <- predict(model)

## Residuals
residuals <- residuals(model)

## R-squared
summary(model)$r.squared

## Results Table
results <- data.frame(
  Height = x,
  Observed = y,
  Predicted = round(yhat,2),
  Residual = round(residuals,2)
)

print(results)

## Plot
plot(x, y,
     pch = 19,
     col = "blue",
     xlab = "Height (cm)",
     ylab = "Arm Length (cm)",
     main = "Simple Linear Regression")

## Regression line
abline(model,
       col = "red",
       lwd = 2)

## Predicted values
points(x, yhat,
       pch = 17,
       col = "darkgreen",
       cex = 1.2)

## Residuals
segments(x0 = x,
         y0 = y,
         x1 = x,
         y1 = yhat,
         col = "gray50",
         lty = 2)

## Legend
legend("topleft",
       legend = c("Observed",
                  "Predicted",
                  "Regression Line"),
       pch = c(19,17,NA),
       lty = c(NA,NA,1),
       col = c("blue","darkgreen","red"),
       lwd = c(NA,NA,2),
       bty = "n")