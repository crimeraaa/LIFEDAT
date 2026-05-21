##############################
# Subsetting R Objects
# Christian Supsup
# 18 May 2026
#############################

######################
## 01. Subsetting a vector
######################
x <- c("a", "b", "c", "c", "d", "a")  
x[1]    ## extract the first element
x[2]    ## extract the second element

## Extract multiple elements
x[1:4]

x[c(1, 3, 4)] ## any arbitrary integer

## Extract what comes after "a"
## with logical vetor
u <- x > "a" 
u
x[u]

## without logical vector
x[x > "a"]

######################
## 02. Subsetting a matrix
######################
x <- matrix(1:6, 2, 3) ## create a matrix
x

## Access elements
x[1, 2]
x[1, 2] <- 5 ## replace value

## Extract rows and columns
x[1, ]  ## extract the first row
x[, 2]  ## extract the second column

## Automatic dropping matrix dimension
x <- matrix(1:6, 2, 3)
x[1, 2] 				## Single length
x[1, 2, drop = FALSE]   ## 1 x 1 by setting drop = FALSE

## retrieve single row
x[1, ]
x[1, , drop = FALSE]

######################
## 03. Subsetting a list
######################
x <- list(foo = 1:4, bar = 0.6)

x[[1]]		## access foo using [[]] operator
x[["bar"]]  ## access bar using element name
x$bar 		## access bar using $ operator

x <- list(foo = 1:4, bar = 0.6, baz = "hello")

name <- "foo"  ## use computed index

x[[name]] 
x$name
x$foo

## [[]] operator can be used with computed indices
## $ operator can only be used with literal names

######################
## 04. Subsetting nested elements of a list
######################
x <- list(a = list(10, 12, 14), b = c(3.14, 2.81))

x[[c(1, 3)]] ## get 3rd element of the 1st element
x[[1]][[3]]  ## same as above

x[[c(2, 1)]] ## 1st element of the 2nd element

######################
## 04. Subsetting multiple elements of a list
######################
x <- list(foo = 1:4, bar = 0.6, baz = "hello")

x[c(1, 3)]

######################
## 05. Removing NAs
######################
x <- c(1, 2, NA, 4, NA, 5)
bad <- is.na(x)
bad

x[!bad] 	 ## remove NAs

y <- x[!bad] ## assign to a new variable

## Use the function "complete.cases"
head(airquality)
good <- complete.cases(airquality)

head(airquality[good, ])

cleaned <- airquality[good, ]

