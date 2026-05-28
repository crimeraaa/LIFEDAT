################################################################################
# LIFEDAT Lab - Activity 3
# Jerome Poblete
# 28 May 2026
################################################################################

# Clean the environment.
rm(list=ls())

# A. Read and perform descriptive statistics.
#
## 1. Read in the CSV file as a data frame, cleaning up NA.
df <- read.csv("herpetozoa-033-095-s001.csv")

## Select non-NA rows and all their data columns.
df <- df[complete.cases(df),]


## 2. Show the data structure.
class(df) # Expected: "data.frame"
str(df)   # Expected: prints everything


## 3. Subset the elevation data and provide the following:
##    i.   mean
##    ii.  standard deviation
##    iii. range
##
##    Select all rows for just the Elev column.
elev.data <- df[,"Elev"]
mean(elev.data)
sd(elev.data)
range(elev.data)

## 4. Subset the species data (columns G "Babu" to AG "Trsu"). Count the total
##    number of observations per species. Show 2 methods, using a typical loop
##    and a compact loop. Which species has the most and fewest observations?
##
## TODO: Can we determine the index of "Babu" programatically?
species.data  <- df[7:length(df)]
species.min <- attributes(species.data[1])$names
species.max <- species.min

### 4.a. Typical for loop
###      Iterate over all the species data frames by name.
###      Using this name, we can index the data frame. We need to use this
###      rather than iterating the data.frame directly because we want to
###      save the species name for the extremes.
###
###      NOTE: Our min gives different results than `which.min`. Maybe we
###      need to loop twice?
for (k in colnames(species.data)) {
  n <- sum(species.data[k])
  if (n > sum(species.data[species.max])) {
    species.max <- k
  }
}

for (k in colnames(species.data)) {
  n <- sum(species.data[k])
  if (n < sum(species.data[species.min])) {
    species.min <- k
  }
}

species.data[species.max]
species.data[species.min]

### 4.b. Compact for loop
###
### Create a list of named integers for each species' count sum,
### then sort it so we can quickly get the min and max.
species.sums <- sort(sapply(species.data, sum))
species.min2 <- attributes(species.sums[1])$names
species.max2 <- attributes(species.sums[length(species.sums)])$names

species.min2
species.max2

species.min == species.min2
species.max == species.max2

## 5. Combine the columns of species with the most and fewest observations
##    with habitat types (Htypes). Determine the mean count of these species
##    in each habitat type. Which habitat type has the most species counts?
##
##    First save the type names we want data is the raw, unfiltered list of type
##    names. Indexes are maintained. `names` is the list of unique type names.
combined.data  <- df[, c("Htypes", species.max, species.min)]
combined.data  <- split(combined.data, combined.data$Htypes)
combined.sums  <- sapply(combined.data, function(v) sum(v[[species.max]] + v[[species.min]]))
combined.means <- sapply(combined.data, function(v) mean(v[[species.max]] + v[[species.min]]))

combined.sums
combined.means
## ANSWER
##  Based on the raw counts, OGF has the highest count (348) for
##  these 2 speceis while ESG has the lowest (0) as neither species is
##  present there.
##
##  Based on the means per habitat, OGF still has the highest (15.13)
##  while ESG still has the lowest (0.00).


# B. Graphical representation.
#
## 1. From the original dataframe, combine the following columns:
###   i.   species total count column (SPPTotal)
###   ii.  elevation
###   iii. habitat type columns
##
##  Combine the combined columns into a scatter plot (see ?plot), with the
##  following properties:
###   i.  x-axis: elevation
###   ii. y-axis: species total count
##
##   Change the point colors based on habitat type. Are there habitat types
##   with no species observations? What are these?
combined <- df[, c("SPPTotal", "Elev", "Htypes")]


## Map each row's Htype so some color so that when we plot the correspoinding
## elevation, we can visualize them better.
htypes.colors_map <- c(ASG="green", ESG="blue", CVT="red", OGF="orange")
htypes.colors <- sapply(df[, "Htypes"], function(k) {htypes.colors_map[k]})

## Create a simple scatterplot. It only plots points, no lines are added.
plot(x    = combined[, "Elev"],
     y    = combined[, "SPPTotal"],
     xlab = "Elevation",
     ylab = "Species total count",
     col  = htypes.colors,
     bg   = htypes.colors,
     pch  = 17)


## ANSWER:
##  Based on above plot, all 4 habitat types have species observations since
##  the 4 valid colors (green, blue, purple and orange) are seen.


## 2. From the combined data of the following:
##
##    i.   Species total counts
##    ii.  Elevation
##    iii. habitat types
##
##    ...determine the total/sum number of species observed per habitat
##    and create a bar plot. Use the bar plot help.
##
##    The bar plot's properties should be as follows:
##
##    i.   x-axis: habitat types.
##    ii.  y-axis  total/sum number of species for a given habitat.

## Split the combined data by htypes and get only the species count sums.
## Returns a named vector of integers where each name is the species count.
barplot(
  height = sapply(split(combined, combined$Htypes), function(v) sum(v$SPPTotal)),
  xlab   = "Habitat type",
  ylab   = "Total species count"
)

## 3. Lastly, create a histogram of total species counts using the histogram
##    function. Make sure all axes are properly labelled.
hist(combined[, "SPPTotal"], xlab="Species count total", main="Species count total frequency")


# C. Interpretation.
#    Based on the summary statistics and preliminary graphs you just created,
#    how do you describe the relationship between the following:
#
#    1. total species observed
#    2. habitat types
#    3. elevation
#
# ANSWER
#    1. The total count of species observed seems to have a very loose inverse
#       relationship with elevation.
#
#       That is, in the Elevation-Species total count scatterplot, increasing
#       elevation leads to decrease in species count. Species also seem to be
#       most abundant at ASG and least abundant at CVT.
#
#       This may explain by lower counts of species are more common than higher
#       ones as seen in the histogram.
