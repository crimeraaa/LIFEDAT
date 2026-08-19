library(dplyr)
library(ggplot2)
library(sf)
library(terra)

rm(list = ls())

## 1) Read in the data.
bird_df <- utils::read.delim("data/Rhipidura_nigritorquis.csv")

## Get only the years and coordinates, tossing out all NAs.
raw_line_df <- bird_df |>
    dplyr::select(year, decimalLatitude:decimalLongitude) |>
    na.omit(raw_line_df) |>
    dplyr::arrange(year)

PH_sf       <- sf::st_read("data/PHL_adm/PHL_adm1.shp")
PH_ext      <- terra::ext(PH_sf)
bird_map_df <- dplyr::filter(raw_line_df,
    dplyr::between(decimalLongitude, PH_ext$xmin, PH_ext$xmax),
    dplyr::between(decimalLatitude, PH_ext$ymin, PH_ext$ymax))

PH_dem_spatr <- terra::rast("data/ph_strm_geo.tif")

## Transform if needed.
PH_sf <- PH_sf |> sf::st_transform(PH_dem_spatr |> sf::st_crs())

## Clip the raster.
PH_dem_spatr <- PH_dem_spatr |> terra::crop(PH_sf) |> terra::crop(PH_sf)

## There's only one (1) name - Band_1. This is actually the elevation,
## a.k.a. our demarcation data.
names(PH_dem_spatr) <- "Elevation"

gradient_vect <- c("#2B89BA", "#ABDDA4", "#FFFFBF", "#FDAE61", "#D7191C")

## Plot DEM.
ggplot2::ggplot(terra::as.data.frame(PH_dem_spatr, xy = TRUE)) +
    ggplot2::geom_tile(aes(x, y, fill = Elevation)) +

    ## Change color manually.
    ## NOTE: Comment out the '+' at the end as needed. See below!
    ggplot2::scale_fill_gradientn(colors = gradient_vect, name = "Elevation (m)") # +
  
    ############################################################################
    ## NOTE: Comment out everything contained within this box if you want to
    ##       test ONLY the map-making.
    ## Plot sampling points as well.
    ggnewscale::new_scale_fill() +
    ggplot2::geom_point(data = bird_map_df, # Override the implicit argument!
        mapping = aes(decimalLongitude, decimalLatitude),
        shape   = 21,
        color   = "black",
        size    = 1.0,
        stroke  = 1.0) +
    
    ## Finalization.
    ggplot2::coord_equal() +
    ggplot2::labs(title="Palawan Province", x="Longitude", y="Latitude") +
    ggplot2::theme_bw()
    ############################################################################