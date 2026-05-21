################################################################################
# LIFEDAT Lab - Activity 3
# Jerome Poblete
# 21 May 2026
################################################################################

# Clean the environment.
rm(list=ls())

# A. Read and perform descriptive statistics.
## 1. Read in the CSV file as a data frame, cleaning up NA.
dframe <- read.csv("herpetozoa-033-095-s001.csv")

## Select non-NA rows and all their data columns.
dframe <- dframe[complete.cases(dframe),]


## 2. Show the data structure.
class(df) # Expected: "data.frame"
str(df)   # Expected: prints everything


## 3. Subset the elevation data and provide the following:
###   i.   mean
###   ii.  standard deviation
###   iii. range

## Select all rows for just the Elev column.
elev_data <- dframe[,"Elev"]
mean(elev_data)
sd(elev_data)
range(elev_data)

## 4. Subset the species data (columns G "Babu" to AG "Trsu"). Count the total
##    number of observations per species. Show 2 methods, using a typical loop
##    and a compact loop. Which species has the most and fewest observations?


## TODO: Can we determine the index of "Babu" programatically?
species_data  <- dframe[7:length(dframe)]

## max and min
most1  <- list(species = "",  count = -1)
least1 <- list(species = "",  count = -1)

### 4.a. Typical for loop
for (k in colnames(species_data)) {
  n <- sum(species_data[k])

  # Ugly but works for our purposes
  if (n > most1$count) {
    most1$species <- k
    most1$count   <- n
  }
  else if (least1$count == -1 || n < least1$count) {
    least1$species <- k
    least1$count   <- n
  }
}

print(most1)
print(least1)

### 4.b. Compact for loop
species_sums <- sapply(species_data, sum)

### TODO: How do we get the species name from this?
most2  <- max(species_sums)
least2 <- min(species_sums)
print(most2)
print(least2)

## 5. Combine the columns of species with the most and fewest observations
##    with habitat types (Htypes). Determine the mean count of these species
##    in each habitat type. Which habitat type has the most species counts?
combined <- dframe[c("Htypes", most1$species, least1$species)]


# B. Graphical representation.
