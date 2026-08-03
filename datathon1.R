library(dplyr)
library(ggplot2)

rm(list = ls())

## 1) Read in the damned data.
bird_df <- read.delim("data/Rhipidura_nigritorquis.csv")

## Get only the years and coordinates, tossing out all NAs.
raw_line_df <- bird_df |>
    dplyr::select(year, decimalLatitude:decimalLongitude) |>
    na.omit(raw_line_df) |>
    dplyr::arrange(year)


## A sequence of all the years we care about. Since the raw data frame is sorted
## in ascending order, we can assume the range is at the lowest/highest indexes.
years   <- raw_line_df$year[1]:tail(raw_line_df$year, n = 1L)
bastard <- vector("numeric", length(years))


## Map some values to the years 1858:2026, but use 1-based indexes for simplicity
line_df <- data.frame(Year = years,
    Count     = bastard,
    Latitude  = bastard,
    Longitude = bastard,
    Elevation = bastard)


for (curr_year in years) {
    ## Format:
    ##  | <index> | $ year | $ decimalLatitude | $ decimalLongitude |
    ##  |---------|--------|-------------------|--------------------|
    ##  |       1 |   1858 |              -7.4 |                110 |
    ##  |  <nrow> |   1858 |              -6.7 |                6.9 |
    curr_year_df <- dplyr::filter(raw_line_df, year == curr_year)
    curr_count   <- nrow(curr_year_df) ## Occurrence count for this year
    
    ## Actual index is offset by whatever the first year is, e.g. 1858
    i <- curr_year - years[1]
    
    ## Yay printf debugging! Right-align the bastards
    # print(sprintf("i = %3i, curr_year = %4i, curr_count = %4i",
    #     i, curr_year, curr_count))

    line_df$Count[i]     <- curr_count
    line_df$Latitude[i]  <- mean(curr_year_df$decimalLatitude)
    line_df$Longitude[i] <- mean(curr_year_df$decimalLongitude)
    
    ## TODO: uhh... umm...
}
