##############################
# Tidyverse - Data Wrangling
# Christian Supsup
# 22 May 2026
#############################

## Install tidyverse
install.packages("tidyverse")

## Load package
library(tidyverse)

## Some of the key “verbs” provided by the dplyr package are
## select(): return a subset of the columns of a data frame, using a flexible notation
## filter(): extract a subset of rows from a data frame based on logical conditions
## arrange(): reorder rows of a data frame
## rename(): rename variables in a data frame
## mutate(): add new variables/columns or transform existing variables
## summarise/summarize(): generate summary statistics of different variables in the data frame, possibly within strata
## %>%: the “pipe” operator is used to connect multiple verb actions together into a pipeline

## Load data
data("iris")
str(iris)

## With tidyverse
iris |> glimpse()

## Filter row using R base code
iris[iris$Species == "setosa",]

## Using dplyr
filter(iris, Species == "setosa")

## Select columns
iris[,c("Sepal.Length", "Sepal.Width", "Species")]
select(iris, Sepal.Length, Sepal.Width, Species)

## Selecting with matches to "Sepal"
select(iris, contains("Sepal"))

## Inverse matching 
select(iris, -contains("Sepal")) ## inverted match uses - or !

## Select columns
select(iris, 1:4) ## numeric index
select(iris, Sepal.Length:Petal.Width) ## column names

## Extract rows 4,7, & 10 (use pipe)
iris |> slice(c(4,7,10))
iris %>% slice(c(4,7,10))

## Arrange
arrange(iris, Species)
arrange(iris, desc(Sepal.Length)) ## note: default is ascending
     
## Capture data with bidirectional operator
iris |>  
  mutate(Sep.LWratio = Sepal.Length/Sepal.Width) |> 
  arrange(Sep.LWratio) -> 
  sepal.sort 

print(sepal.sort)

## More on filtering!
## filter: to remove rows where sex is NA
## group_by: species and sex
## summarize: with mean

## remotes::install_github("allisonhorst/palmerpenguins")
library(palmerpenguins)

penguins |> 
  filter(!is.na(sex)) |>
  group_by(species, sex) |> 
  summarize(mean_mass = mean(body_mass_g)) 

## Data joins
## merge(): to bind separate datasets using a common variable.
hab <- read.csv("habitat_types.csv")
sp <- read.csv("species.csv")

df <- merge(hab, sp)

## Using tidyverse
df <- full_join(hab, sp)

## Tidying data
## tidyr package includes several functions to support data tidying. 
## But often it is simply a case of going from “wide” to “long” format.
set.seed(1)
plots <- paste0("plot", LETTERS[1:4])
sunflower <- rpois(n=4,20)
aster <- rpois(n=4, 50)
dandelion <- rpois(n=4, 80) 

Flowers <- data.frame(plots,sunflower,aster,dandelion)
print(Flowers)

## pivot_longer(): makes long (tidy) data
Flowers |> pivot_longer(cols = !plots,
                        names_to = "flowers",
                        values_to = "counts") -> FlowersLong
print(FlowersLong)

## pivot_wider(): transforms long data into wide
FlowersLong |> pivot_wider(names_from = flowers, values_from = counts)
