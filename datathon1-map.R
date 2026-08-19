library(dplyr)
library(ggplot2) 
library(ggnewscale)
library(sf)
library(terra)

rm(list = ls())

## 1) Read in the data. For the occurrence map, we only care about the
##    coordinates. Even years aren't useful to us as this is cumulative data,
##    i.e. we want to see where the bird occurred throughout history.
bird_df <- utils::read.delim("data/Rhipidura_nigritorquis.csv") |>
    dplyr::rename(Latitude = decimalLatitude, Longitude = decimalLongitude) |>
    dplyr::select(Latitude:Longitude) |>
    na.omit()
  

## 2) Read in the administrative boundary data. This will allow us to 'extract'
##    only the Philippines from the world map raster later on.
PH_sf  <- sf::st_read("data/PHL_adm/PHL_adm0.shp")
PH_ext <- terra::ext(PH_sf)


## 3) Just for our sanity, get only the occurrences that have non-empty
##    coordinates that fit in the known extent.
bird_map_df <- bird_df |>
    dplyr::filter(dplyr::between(Longitude, PH_ext$xmin, PH_ext$xmax),
                  dplyr::between(Latitude,  PH_ext$ymin, PH_ext$ymax)) |>
    dplyr::select(Latitude:Longitude)



## 4) A lot is happening here. We read in the raster of the world map but
##    specifically extract the section that belongs to the Philippines' extent.
PH_elev_spatr <-
    terra::rast("data/wc2.1_2.5m_elev.tif") |>
    terra::crop(PH_sf) |>
    terra::mask(PH_sf)


## 5) There's only one (1) name - Band_1. This is actually the elevation,
##    which is also our demarcation data. Rename it so the resulting map
##    legend makes more sense.
names(PH_elev_spatr) <- "Elevation"


## 6) Set up the elevation scale gradient, from lowest to highest values.
##    Cherry-picking courtesy of: Thoms.
gradient_vect <- c("#f1f1f1", "#ffc59e", "#e1bb4e", "#9bb306", "#26a63a")



## 7) Plot DEM. There are a lot of sub-steps here, so see each one's comment.
ggplot2::ggplot(terra::as.data.frame(PH_elev_spatr, xy = TRUE)) +
    ggplot2::geom_tile(aes(x, y, fill = Elevation)) +

    ## Change color manually, with a discrete color legend such that the color
    ## of the lowest point goes on top and the color of the highest possible
    ## point goes on the bottom.
    ggplot2::scale_fill_stepsn(
        name   = "Elevation (m)",
        colors = gradient_vect,
        guide  = "legend") +
    
    ## Draw the land boundaries.
    ggplot2::geom_sf(
        data      = PH_sf, ## Administrative boundaries.
        fill      = NA,    ## Don't color inside each boundary!
        color     = "black",
        linewidth = 1.0) +
  
    ## Plot the occurrence sampling points as well.
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(
        data    = bird_map_df, ## Override the implicit argument!
        mapping = aes(Longitude, Latitude),
        shape   = 21,
        colour  = "black", ## Outline color
        fill    = "red",   ## Color *inside* the point
        size    = 2.5,
        stroke  = 1.0) +
    
    ## Finalization: label stuff. Unlike in GIS, don’t use `ggplot2::coord_equal()`
    ## because we now have the administrative boundary lines to worry about.
    ggplot2::coord_sf() +
    ggplot2::labs(title = "B. Philippine Pied Fantail Occurrence Records",
                  x     = "Longitude",
                  y     = "Latitude") +
    ggplot2::theme_bw()
