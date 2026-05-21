##############################
# Writing My First R Script
# Christian Supsup
# 14 May 2026
#############################

## Reset R's brain
rm(list = ls())

## Check working directory
getwd()

## Set working directory
setwd("/Users/ces/Desktop/DLSU_T3_AY2025-26/LBBBI27/activities/act_01")

## Check files within working directory
list.files()

######################
## 01. Enter an input
######################
x <- 1
msg <- "hello"

## Print an input
print(x)
print(msg)
x
ms

## Case of incomplete expression
x <-  ## Incomplete expression

## use CTRL+C to terminate or supply the missing expression

## More input examples
x <- 11:30

######################
## 02. R objects
######################
x <- c(0.5, 0.6)       ## numeric
x <- c(TRUE, FALSE)    ## logical
x <- c(T, F)           ## logical
x <- c("a", "b", "c")  ## character
x <- 9:29              ## integer
x <- c(1+0i, 2+4i)     ## complex

## Mixing object
y <- c(1.7, "a")   ## character
y <- c(TRUE, 2)    ## numeric
y <- c("a", TRUE)  ## character

######################
## 03. Coercion/Conversion
######################
x <- 0:6
class(x)         # shows the data type/class of x
as.numeric(x)    # converts x to numeric values
as.logical(x)    # converts x to TRUE/FALSE logical values
as.character(x)  # converts x to character/text strings

## Coercion to NAs
x <- c("a", "b", "c")
as.numeric(x)

######################
## 04. Building a matrix
######################
m <- matrix(nrow = 2, ncol = 3)  # create a 2x3 matrix
dim(m)                           # returns matrix dimensions (rows, columns)
attributes(m)                    # shows object attributes like dimensions

## Add some values
m <- matrix(1:6, nrow = 2, ncol = 3)

## Create from a vector
m <- 1:10 
m
dim(m) <- c(2, 5)
m

## Create through column/row binding
x <- 1:3
y <- 10:12
cbind(x, y)
rbind(x, y)

######################
## 04. Making a list
######################
x <- list(1, "a", TRUE, 1 + 4i)

## An empty list
x <- vector("list", length = 5)

## Fill list
x[[1]] <- 10
x[[2]] <- "hello"
x[[3]] <- TRUE
x[[4]] <- 3.14
x[[5]] <- c(1, 2, 3)

######################
## 05. Factor
######################
x <- factor(c("yes", "yes", "no", "yes", "no")) 

table(x) # count how many times each value appears in x

######################
## 06. Missing values
######################
x <- c(1, 2, NA, 10, 3) # create a vector with NAs in it

is.na(x)  # return a logical vector indicating which elements are NA
is.nan(x) # return a logical vector indicating which elements are NaN

######################
## 07. Dataframe
######################
x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
nrow(x)
ncol(x)

######################
## 08. Names
######################
x <- 1:3
names(x) <- c("New York", "Seattle", "Los Angeles")
names(x)

## List can also have names
x <- list("Los Angeles" = 1, Boston = 2, London = 3)

## Matrices can also have names
m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))

## Useful commands
## For dataframe
names()
row.names()

## For matrix
colnames()
rownames()

