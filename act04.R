################################################################################
# Activity 04 - Tidyverse
# Jerome Poblete
# 25 May 2026
################################################################################

# A) Read and filter out
## 1) Install tidyverse and load.
install.packages("tidyverse")
library("tidyverse")

## Clear environment for reproduceability.
##
## NOTE: Ensure you are in the directory of the CSV file!
## Run `getwd()` to check, then run `setwd(<path>)` to move around.
rm(list = ls())


## 2) Read in the CSV file as a dataframe.
ht <- read.csv("habitat_types.csv")


## 3) Show the data structure using the tidyverse equivalent for R's base `str`.
##    Tidyverse can work by using the modern R pipe operator `|>` instead
##    of explicitly specifying function arguments. This operator implicitly
##    passes the left-hand side expresion as the first argument to the function
##    call on the right-hand side. It is (arguably) cleaner.
ht |> glimpse()


## 4) Subset the data by taking only the following columns:
##    i)   Transect
##    ii)  Htypes
##    iii) SmallTrees
##    iv)  FloweringTrees
##
##    Note that just like in base R, some functions allow you to pass
##    identifiers as unquoted strings.
ht2 <- ht |> select(Transect, Htypes, SmallTrees, FloweringTrees)
# ht2 |> glimpse()


## 5) From the subset of the data,  filter the habitat type "CVT" and sort the
##    "SmallTrees" variable in descending order. Which transect has the most
##    count of small trees, and how many?
##
##    Since `filter()` returns the now-filtered data frame, we can use it
##    in subsequent pipes.
##
##    We can use the `arrange()` function to sort by providing some expression
##    to sort the input data frame by.
##
##    `desc(SmallTrees)` allows us to arrange `ht2` in descending order of the
##    "SmallTrees" field. Note that thanks to dplyr, this is a lazy data frame
##    (i.e. it's not evaluated at the point we call it, it's only evaluted
##    inside of `arrange()` itself).
cvt <- ht2 |> filter(Htypes == "CVT") |> arrange(desc(SmallTrees))
cvt

## 5) Answer 
##  Transect IT2P7 and IT3P0 have the most number of small trees, both of them
##  have 10 small trees each.


## 6) Also, calculate the mean number of flowering trees per habitat type. Which
##    habitat types do not differ significantly in the mean number of flowering
##    trees?
ftrees <- ht2 |> group_by(Htypes) |> summarize(mean_ft = mean(FloweringTrees))
ftrees

## 6) Answer
##  The habitat types that do not differ significantly in the mean number of
##  flowering trees are ASG and ESG, at 5.26 and 5.38 flowering trees,
##  respectively. CVT on the other hand has an average of 8.86 flowering trees
##   making it more than 3.00 higher than the other 2.


## 7) In your subset of data, e.g. from step #4, add a new column showing the
##    difference between HerbsCover and GroundCover. Name the new column as
##    "HerbGroundDiff". Make sure to assign the output of this step to a new
##    variable called "sel_df_mod". Are there any transects with no difference?
##    Can you identify at least three (3) transects?
##
##    Note that even though `c(HerbsCover, GroundCover)` don't exist in `ht2`,
##    we can still of course use the original `ht`.
sel_df_mod <- ht2 |> mutate(HerbGroundDiff = ht$HerbsCover - ht$GroundCover)
sel_df_mod

## 7) Answer
##  Some transects do have no difference (i.e. HerbGroundDiff == 0):
##         IT1P5
##         IT2P5
##         IT3P1
##         ST1P2
##         ST1P3
##         ST1P7
##         ST2P5
##         ST3P1
##         ST3P2
sel_df_mod |> filter(HerbGroundDiff == 0)


## 8) Convert your sel_df_mod output to a wider version (i.e. via `pivot_wider`)
##    The output should have columns like the following:
##    i)   SmallTrees_ESG
##    ii)  FloweringTrees_ESG
##    iii) HerbGround_Diff_ESG
##    iv)  SmallTrees_CVT
##    etc.
sel_df_mod |> pivot_wider(
  # Will suffix to all the names in `values_from`
  names_from  = Htypes,
  
  # Each new column is the combination of this + `names_from`.
  # Many values will be NA, as each transect only has 1 Htype. So the
  # combinations with non-existent Htype (e.g. IT1P0 with CVT) will be NA.
  values_from = c(SmallTrees, FloweringTrees, HerbGroundDiff, SmallTrees))


# B) Reflection
#    How does using tidyverse compare to Activity 03, which primarily used 
#    Base R? Which functions do you find more intuitive, and which ones are
#    less so?

# B) Answer
#    I felt that the data frame manipulation functions were a lot easier to
#    follow especially since we now know about the pipe operator. `filter()`,
#    `arrange()`, `group_by()` and `summarize()` are very helpful since in
#    Activity 03 I was confused on how to achieve the same results in plain R.
#
#    However I'm still trying to wrap my head around `pivot_longer()` and
#    `pivot_wider()` because they do so much, kind of like the plot functions
#    we had to to learn how to use back in Activity 03. I think with a bit more
#    practice these will become more intuiive.