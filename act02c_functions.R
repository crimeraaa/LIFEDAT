##############################
# Functions and R compact loop
# Christian Supsup
# 18 May 2026
#############################

## ------------------- ##
## A. R functions
## ------------------- ##
f <- function() {
        ## this is an empty function
}

## Your first function
f <- function() {
        cat("Hello, world!\n")
}

f()

## A simple one
add <- function(x, y) {
  x + y
}

add(1, 2)

## ------------------- ##
## B. R compact loop
## ------------------- ##

lapply() ## Loop over a list and evaluate a function on each element
sapply() ## Same as lapply but try to simplify the result
split()  ## Takes a vector or other objects and splits it into groups determined by a factor or list of factors
tapply() ## Apply a function over subsets of a vector

######################
## 01. lapply()
######################
lapply  ## show function
?lapply ## check help 

x <- list(a = 1:5, b = rnorm(10)) ## create a sample list
lapply(x, mean) ## apply the function

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5)) ## more samples
lapply(x, sd)

## Create a list with two matrices
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2)) 
x 

lapply(x, function(elt) { elt[,1] }) ## extract the first column of each matrix

## Define a basic function
f <- function(elt) {
         elt[, 1]
     }

lapply(x, f)

######################
## 02. sapply()
######################
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)

sapply(x, mean) ## sapply differs only with return values (i.e., simplifies it)

######################
## 03. split()
######################
str(split)
function (x, f, drop = FALSE, ...)  

## Sample
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10) ## this generates levels
split(x, f)

lapply(split(x, f), mean)

## Use a dataframe
library(datasets)
head(airquality)

## Use a single factor
s <- split(airquality, airquality$Month)

str(s)

## Use multiple factors
lapply(s, function(x) {
    colMeans(x[, c("Ozone", "Solar.R", "Wind")])
})

sapply(s, function(x) {
    colMeans(x[, c("Ozone", "Solar.R", "Wind")])
})

######################
## 04. tapply()
######################
str(tapply)

## Generate random data
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)  ## Define some groups with a factor variable
f

tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)
tapply(x, f, range)

