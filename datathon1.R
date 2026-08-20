library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)
library(terra)


################################################################################
## Common stuff
################################################################################

## Clear the environment for reproducability.
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
bird_df <- utils::read.delim("data/Rhipidura_nigritorquis.csv") |>
    ## 2.1) Rename the columns for ease of use. These are just my (Jerome's)
    ##      personal preferences. Just be consistent.
    dplyr::rename(Year      = year,
                  Latitude  = decimalLatitude,
                  Longitude = decimalLongitude) |>
    
    ## 2.2) Between the double-line graph and the occurrence map, we only
    ##      need the years and coordinates.
    dplyr::select(Year, Latitude:Longitude) |>
    
    ## 2.1) Ensure all the coordinates are in range of the country's extent.
    ##      This will also filter out NA's.
    dplyr::filter(dplyr::between(Longitude, PH_ext$xmin, PH_ext$xmax),
                  dplyr::between(Latitude,  PH_ext$ymin, PH_ext$ymax)) |>
    
    ## 2.2) Remove occurrences without tagged years so we can safely treat
    ##      the entire column as an integer vector.
    dplyr::filter_out(is.na(Year)) |>
    dplyr::arrange(Year) ## Ascending order.


## 2.3) For (B), we only care about each occurrence's coordinates.
bird_map_df  <- dplyr::select(bird_df, !Year)


## 3) A lot is happening here. We read in the raster of the world map but
##    specifically extract the section that belongs to the Philippines' extent.
PH_elev_spatr <-
    terra::rast("data/wc2.1_2.5m_elev.tif") |>
    terra::crop(PH_sf) |>
    terra::mask(PH_sf)


## 4) There's only one (1) name - Band_1. This is actually the elevation,
##    which is also our demarcation data. Rename it to better reflect its
##    intended meaning and usage for succeeding steps.
names(PH_elev_spatr) <- "Elevation"


################################################################################
## A. Philippine Pied Fantail Annual Observations along Elevation
################################################################################

## A1) Translate Latitude and Longitude to raster points. Don't include years.
bird_epsg_sf <- bird_map_df |>
    ## `coords` must be in x-y order, for `terra::extract()`.
    ## `crs` is actually passed onto `sf::st_sf()`. See `?st_crs`.)
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326")


## A2) We can now get the elevation data for each occurrence. Note that some
##     occurrences may result in NA.
bird_elev_df <- terra::extract(PH_elev_spatr, bird_epsg_sf)


## A3) With that, we can combine the above data frames. In the previous step
##     we noted that some elevations would be NA- so remove them only after
##     the full data frame is created.
bird_graph_df <- cbind(bird_df, bird_epsg_sf, bird_elev_df) |> stats::na.omit()


## A4) Prepare the line graph data frame. See the succeeding sub-steps.
first_year <- min(bird_graph_df$Year)
last_year  <- max(bird_graph_df$Year)


## A4.1) An ordered sequence, in ascending order of all possible years that the
##       occurrence data could span. Not all years in this sequence are
##       necessarily in the actual data.
years_seq <- first_year:last_year


## A4.2) Maps each possible year to some total count and mean elevation.
##      By default all the counts and means are zero (0), which is useful for
##      years that have no recorded occurrences.
bird_line_df <- data.frame(
    Year  = years_seq,
    Count = vector("numeric", length(years_seq)),
    Mean  = vector("numeric", length(years_seq)))


## A5) Fill in the line graph data frame.
for (i in years_seq) {
    ## A5.1) Normalize the year to a 1-based row index such that the first year,
    ##       which is 1902, maps to index 1, rather than 0.
    row_index <- i - first_year + 1
    
    ## A5.2) Let dplyr handle the dirty work of searching for all occurrences
    ##       from this year. Note that the call will always return a proper
    ##       data frame, though it could be empty (i.e. #rows == 0).
    tmp_vect  <- dplyr::filter(bird_graph_df, Year == i)$Elevation
    
    ## A5.3) Since the found data frame could be empty, our mean may be NaN.
    n <- length(tmp_vect)
    if (n > 0) {
        y_bar <- mean(tmp_vect)
        bird_line_df[row_index, c("Count", "Mean")] <- c(n, y_bar)
    }
}

## A6) Plot the double-line graph.
MEAN_ELEV_SCALE <- 5.75
bird_line_plot <- ggplot2::ggplot(dplyr::filter_out(bird_line_df, Mean == 0.0)) +
    ## A6.1) Primary plot (i.e. y-axis on the left) Based on the reference, this
    ##       will be the total counts per year.
    ggplot2::geom_col(
        ggplot2::aes(Year, Count),
        colour = "black",
        fill = "#0477A5") +
    
    ## A6.2) Secondary plot (i.e. y-axis on the right) data. Based on the
    ##       reference, this will be the mean elevation. Note that because
    ##       ggplot2 doesn't actually support two (2) *independent* y-axes,
    ##       we need to scale the secondary y-axis to the primary one instead.
    ggplot2::geom_point(
        mapping = ggplot2::aes(Year, Mean * MEAN_ELEV_SCALE),
        shape   = 21,    ## Circle.
        size    = 2.0,
        colour  = "red",
        fill    = "red") +

    ggplot2::geom_line(
        mapping   = ggplot2::aes(Year, Mean * MEAN_ELEV_SCALE),
        colour    = "red",
        linewidth = 1.25) +
    
    ## A6.3) Primary plot's x-axis and y-axis labels.
    ggplot2::labs(
        title = "A. Philippine Pied Fantail Annual Observations along Elevation",
        x     = "Year",
        y     = "Number of Observations") +
    
    ## A6.4) Primary and secondary plot's shared x-axis. As in the reference
    ##       plot, we start from 1902 and increment by 10 years.
    ggplot2::scale_x_continuous(breaks = seq(first_year, last_year, by = 10)) +
    
    ## A6.4) Secondary plot's y-axis labels. Again, as the secondary y-axis
    ##       is *not* independent of the primary one, we need to scale it
    ##       appropriately.
    ggplot2::scale_y_continuous(
        sec.axis = ggplot2::sec_axis(
            transform = ~ . / MEAN_ELEV_SCALE,
            name      = "Mean Elevation (m)")) +

    ## A6.?) Finalization. This forgoes the gray-ish background and grid lines.
    ggplot2::theme_classic() +
    
    ## Adjust the x-axis tick labels so that they are rotated by 45 degrees
    ## and horizontally justified from the tick.
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))


################################################################################
## B. Philippine Pied Fantail Occurrence Records
################################################################################


## B1) Set up the elevation scale gradient, from lowest to highest values.
##    Cherry-picking courtesy of: Thoms.
gradient_vect <- c("#f1f1f1", "#ffc59e", "#e1bb4e", "#9bb306", "#26a63a")


## B2) Plot DEM. There are a lot of sub-steps here, so see each one's comment.
bird_map_plot <- ggplot2::ggplot(terra::as.data.frame(PH_elev_spatr, xy = TRUE)) +
    ggplot2::geom_tile(aes(x, y, fill = Elevation)) +

    ## B2.1) Change color manually, with a discrete color legend such that the
    ##       color of the lowest point goes on top and the color of the highest
    ##       possible point goes on the bottom.
    ggplot2::scale_fill_stepsn(
        name   = "Elevation (m)",
        colors = gradient_vect,
        guide  = "legend") +
    
    ## B2.2) Draw the land boundaries.
    ggplot2::geom_sf(
        data  = PH_sf, ## Administrative boundary data.
        fill  = NA,    ## Don't color inside each boundary!
        color = "black") +
  
    ## B2.3) Plot the occurrence sampling points as well.
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(
        data    = bird_map_df, ## Override the implicit argument!
        mapping = aes(Longitude, Latitude),
        shape   = 21,
        colour  = "black", ## Outline color
        fill    = "red",   ## Color *inside* the point
        size    = 2.5,
        stroke  = 1.0) +
    
    ## B2.4) Finalization. Unlike in GIS, don’t use `ggplot2::coord_equal()`
    ##       since we now have administrative boundary lines to worry about.
    ggplot2::coord_sf() +
    ggplot2::labs(title = "B. Philippine Pied Fantail Occurrence Records",
                  x     = "Longitude",
                  y     = "Latitude") +
    ggplot2::theme_classic()

bird_patchwork <- bird_line_plot + bird_map_plot
bird_patchwork