################################################################################
# LIFEDAT Lab - Activity 3
# Jerome Poblete
# 21 May 2026
################################################################################

# Clean the environment.
rm(list=ls())

# A. Read and perform descriptive statistics.
## 1. Read in the CSV file as a data frame, cleaning up NA.
df <- read.csv("herpetozoa-033-095-s001.csv")

## Select non-NA rows and all their data columns.
df <- df[complete.cases(df),]


## 2. Show the data structure.
class(df) # Expected: "data.frame"
str(df)   # Expected: prints everything


## 3. Subset the elevation data and provide the following:
###   i.   mean
###   ii.  standard deviation
###   iii. range

# Select all rows for just the Elev column.
elev.data <- df[,"Elev"]
mean(elev.data)
sd(elev.data)
range(elev.data)

## 4. Subset the species data (columns G "Babu" to AG "Trsu"). Count the total
##    number of observations per species. Show 2 methods, using a typical loop
##    and a compact loop. Which species has the most and fewest observations?


# TODO: Can we determine the index of "Babu" programatically?
species.data  <- df[7:length(df)]

# max and min
most  <- list(species = "",  count = -1)
least <- list(species = "",  count = -1)

### 4.a. Typical for loop
###      Iterate over all the species data frames by name.
###      Using this name, we can index the data frame. We need to use this
###      rather than iterating the data.frame directly because we want to
###      save the species name for the extremes.
for (k in colnames(species.data)) {
  n <- sum(species.data[k])

  # Ugly but works for our purposes
  if (n > most$count) {
    most$species <- k
    most$count   <- n
  }
  else if (least$count == -1 || n < least$count) {
    least$species <- k
    least$count   <- n
  }
}


str(most)
str(least)

### 4.b. Compact for loop

# List of names.
species.names <- colnames(species.data)

# List of sums. Indexes here also refer to the index in `species.names`.
species.sums  <- sapply(species.data, sum)

# Return a list containing the fields "species", and "count".
species.apply <- function(f) {
  i <- f(species.sums)
  k <- species.names[i]
  list(species = k, count = species.sums[i])
}

# which.max and which.min return the index of the value in question.
# Originally found by looking at the "See also" section for ?min.
most2  <- species.apply(which.max)
least2 <- species.apply(which.min)

str(most2)
str(least2)

## 5. Combine the columns of species with the most and fewest observations
##    with habitat types (Htypes). Determine the mean count of these species
##    in each habitat type. Which habitat type has the most species counts?

# First save the type names we want
# data is the raw, unfiltered list of type names. Indexes are maintained.
# names is the list of unique type names.
htypes.data  <- df[, "Htypes"]
htypes.names <- unique(htypes.data)

# Then get the mean counts of the 2 species per habitat. Each index refers
# to a type name in `htypes.data`. In `apply`, the second (2nd) argument is
# MARGIN. THe possible values are as follows:
#
#     i.   1       - apply FUN row-wise.
#     ii.  2       - apply FUN column-wise.
#     iii. c(1, 2) - apply FUN to rows and columns.
#     iv.  chr[]   - if X has named dimnames.
combined.means <- apply(df[, c(most$species, least$species)], 1, mean)

# We now have the information to map Htypes to mean counts.
#combined <- cbind(htypes.data, combined.means)
#colnames(combined) <- c("types", "means")

# First save the type names with the counts for the 2 species.
# However, Htypes may have duplicates so we need to combine them
# For some reason this fails???
# data.frame(rep(list(total=0, count=0), 4), row.names = htypes.names)
combined.totals <- data.frame(rep(0, 4), row.names = htypes.names)
combined.counts <- data.frame(rep(0, 4), row.names = htypes.names)

for (i in seq_along(htypes.data)) {
  k <- htypes.data[i]
  combined.totals[k,] <- combined.totals[k,] + combined.means[i]
  combined.counts[k,] <- combined.counts[k,] + 1
}

combined <- data.frame(combined.totals / combined.counts, row.names = htypes.names)
# Ok now what?
  

# B. Graphical representation.

## 1. From the original dataframe, combine the following columns:
##    i.   species total count column (SPPTotal)
##    ii.  elevation
##    iii. habitat type columns
##
##    Combine the combined columns into a scatter plot (see ?plot), with the
##    following properties:
##
#     i.  x-axis: elevation
##    ii. y-axis: species total count
##
##    Change the point colors based on habitat type. Are there habitat types
##    with no species observations? What are these?

# Create a new column in the data frame for color.
# We want to map each row's Htype so some color so that when we plot
# the correspoinding elevation, we can visualize them better.
htypes.colors <- htypes.data
htypes.colors_map <- c(ASG = "green", ESG = "blue", CVT = "red", OGF = "orange")
for (i in seq_along(htypes.colors)) {
  htype <- htypes.colors[i]
  htypes.colors[i] <- htypes.colors_map[htype]
}


# Create a simple scatterplot. It only plots points, no lines are added.
plot(x    = elev.data,
     y    = df[, "SPPTotal"],
     xlab = "Elevation",
     ylab = "Species total count",
     col  = htypes.colors)


# ANSWER: Based on above plot, all 4 habitat types have species observations
#         since the 4 valid colors (green, blue, purple and orange) are seen.


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

# Create a list of all species summed up per row. Each index maps to a row.
df.species.sums <- apply(species.data, 1, sum)

# Now, sum up the total species counts for each habitat type.
df.htypes.sums <- data.frame(rep(0, 4), row.names = htypes.names)


## 3. Lastly, create a histogram of total species counts using the histogram
##    function. Make sure all axes are properly labelled.


# C. Interpretation.
#    Based on the summary statistics and preliminary graphs you just created,
#    hod do you describe the relationship between the following:
#
#    1. total species observed
#    2. habitat types
#    3. elevation
