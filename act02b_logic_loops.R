##############################
# Loop functions
# Christian Supsup
# 18 May 2026
#############################

## if and else: testing a condition and acting on it
## for: execute a loop a fixed number of times
## while: execute a loop while a condition is true
## break: break the execution of a loop
## next: skip an interation of a loop

######################
## 01. if-else
######################

## Missing clause
if(<condition>) {
        ## do something
} 
		## Continue with rest of code

## With clause
if(<condition>) {
        ## do something
} 
else {
        ## do something else
}

## With multiple clauses
if(<condition1>) {
        ## do something
} else if(<condition2>)  {
        ## do something different
} else {
        ## do something different
}

## Generate a uniform random number
runif(n, min, max)

x <- runif(1, 0, 10)  
if(x > 3) {
        y <- 10
} else {
        y <- 0
}

######################
## 02. for loop
######################
## Iterate and assing values
for(i in 1:10) {
    print(i)
}


## Generate a sequence based on length of 'x'
for(i in seq_along(x)) {   
	print(x[i])
}


## Loop takes i
x <- c("a", "b", "c", "d")
for(i in 1:4) {
	print(x[i])  ## print out each element of 'x'
}


for(letter in x) {
	print(letter)
}

## Single line 
for(i in 1:4) print(x[i])


######################
## 03. nested loop
######################
x <- matrix(1:6, 2, 3)

for(i in seq_len(nrow(x))) {
        for(j in seq_len(ncol(x))) {
                print(x[i, j])
        }   
}


######################
## 04. while loop
######################
count <- 0

while(count < 10) {
	print(count)
	count <- count + 1
}


######################
## 05. next and break
######################

## Next
for(i in 1:100) {
        if(i <= 20) {
                ## skip the first 20 iterations
                next                 
        }
        ## do something here
}

for(i in 1:100) {
        if(i <= 20) {
                ## skip the first 20 iterations
                next                 
        }
        print(i)
}

## Break
for(i in 1:100) {
      print(i)

      if(i > 20) {
              ## stop loop after 20 iterations
              break  
      }     
}

