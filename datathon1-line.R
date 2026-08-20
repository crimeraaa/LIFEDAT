library(ggplot2)
library(dplyr)
library(sf)
library(terra)

rm(list = ls())

## 1) Read in the administrative boundary data. This will allow us to 'extract'
##    only the Philippines from the world map raster later on and also filter
##    out occurrences that go beyond the Philippines' extent.
PH_sf  <- sf::st_read("data/PHL_adm/PHL_adm0.shp")
PH_ext <- terra::ext(PH_sf)


## 2) Read in the occurrence data.
##
##    For (A), the double-line graph, we only care about the overall counts per
##    year and the mean elevation. Said elevation will have to be extrapolated
##    from the WorldClim data and each occurrence's coordinates.
##
bird_df <-
    utils::read.delim("data/Rhipidura_nigritorquis.csv") |>
    dplyr::rename(Year      = year,
                  Latitude  = decimalLatitude,
                  Longitude = decimalLongitude) |>
    dplyr::select(Year, Latitude:Longitude) |>
    dplyr::filter(dplyr::between(Longitude, PH_ext$xmin, PH_ext$xmax),
                  dplyr::between(Latitude,  PH_ext$ymin, PH_ext$ymax)) |>
    dplyr::filter_out(is.na(Year)) |>
    dplyr::arrange(Year) ## Ascending order.

## 2.1) For (B), we only care about each occurrence's coordinates.
bird_map_df  <- dplyr::select(bird_df, !Year)

## 3) A lot is happening here. We read in the raster of the world map but
##    specifically extract the section that belongs to the Philippines' extent.
PH_elev_spatr <-
    terra::rast("data/wc2.1_2.5m_elev.tif") |>
    terra::crop(PH_sf) |>
    terra::mask(PH_sf)


## 4) There's only one (1) name - Band_1. This is actually the elevation,
##    which is also our demarcation data. Rename it so the resulting map
##    legend makes more sense.
names(PH_elev_spatr) <- "Elevation"


## 5) Translate Latitude and Longitude to raster points. Don't include years.
bird_epsg_sf <- bird_map_df |>
    sf::st_as_sf(
        ## Must be x-y order, specifically when we call `terra::extract()`.
        coords = c("Longitude", "Latitude"),
        ## This is actually passed onto `sf::st_sf(0`. See `?st-crs`.)
        crs    = "EPSG:4326")


## 6) We can now get the elevation data for each occurrence.
##    Some occurrences may result in NA- don't remove them just yet.
bird_elev_df <- terra::extract(PH_elev_spatr, bird_epsg_sf)


## 7) With that, we can combine the above data frames. In the previous step
##    we noted that some elevations would be NA- remove them.
bird_graph_df <- cbind(bird_df, bird_epsg_sf, bird_elev_df) |> stats::na.omit()


## 8) Prepare the mean elevation data frame.
first_year <- min(bird_graph_df$Year)
last_year  <- max(bird_graph_df$Year)


## 8.1) An ordered sequence (i.e. ascending) of all possible years that the
##      occurrence data could span.
years_seq <- first_year:last_year


## 8.2) Maps each possible year to some total count and mean elevation.
##      By default all the counts and means are zero, which is useful for years
##      that have no recorded occurrences.
year_sum_elev <- data.frame(
    Year  = years_seq,
    Count = vector("numeric", length(years_seq)),
    Mean  = vector("numeric", length(years_seq)))

for (i in years_seq) {
    ## E.g. 1902, the first year, should map to index 1, not 0.
    row_index <- i - first_year + 1
    tmp_vect  <- dplyr::filter(bird_graph_df, Year == i)$Elevation
    
    ## Otherwise don't set it as the mean of a zero-length vector is NaN,
    ## which we don't want. We assume it's already been zero-initialized.
    if (length(tmp_vect) > 0) {
        year_sum_elev[row_index, "Count"] <- length(tmp_vect)
        year_sum_elev[row_index, "Mean"]  <- mean(tmp_vect)
    }
}

## 9) Plot the double-line graph.
ggplot2::ggplot(year_sum_elev) +
    ## 9.1) Primary plot (i.e. y-axis on the left) Based on the reference, this
    ##      will be the total counts per year.
    ggplot2::geom_col(
        mapping   = ggplot2::aes(Year, Count),
        colour    = "black",   ## Outline color.
        fill      = "#4077A5", ## Shape fill color.
        linewidth = 0.9) +

    ## 9.2) Primary plot's labels.
    ggplot2::labs(
        title = "A. Philippine Pied Fantail Annual Observations along Elevation",
        x     = "Year",
        y     = "Number of Observations") +

    ## 9.?) Finalization. This forgoes the gray-ish background and grid lines.
    ggplot2::theme_classic()
