##############################
# Geographic Information System - creating your first map
# Christian Supsup
# 22 June 2026
#############################

## Geographic Information System (GIS) is a powerful set of tools for collecting, 
## storing, retrieving at will, transforming, and displaying spatial data from the 
## real world for a particular set of purpose.- Burrough & McDonnel 1998

## Two main types of spatial data:

## 1. Vector data - vector data represents geographic features as points, lines, 
## and polygons.
##	- Points: single locations (e.g., buildings, trees, weather stations)
##	- Lines: linear features (e.g., roads, rivers, power lines)
##	- Polygons: areas (e.g., lakes, land parcels, countries)
##  - Format: .csv, .kml, .gpkg, .shp (shapefile)

## Advantages:
##	- High positional accuracy
##	- Efficient for discrete features
##	- Supports topology (connectivity and adjacency)
## Examples:
##	- City locations
##	- Road networks
##	- Administrative boundaries

## 2. Raster data - raster data represents geographic space as a grid of cells (pixels), 
## where each cell stores a value.Each pixel has a value representing a characteristic such as:
##	- Elevation
##	- Temperature
##	- Vegetation index

## Advantages:
##	- Ideal for continuous phenomena
##	- Works well with satellite imagery and environmental modeling
## Examples:
##	- Satellite images
##	- Digital elevation models (DEMs)
##	- Climate data grids

######################
## A. Vector data
######################
## 1. Points
## Sample point:
  ## Latitude: 9°22'26.9868"N; 
  ## Longitude: 118°23'48.876"E

lat_dd <- 9 + 22/60 + 6.9868/3600
lon_dd <- 118 + 23/60 + 48.876/3600

lat_dd
lon_dd

## 2. Polygons
## Install the following packages:
## install.packages(c("maps", "osmdata", "sf", "terra"))
library(maps)
library(osmdata)
library(sf)
library(terra)
library(ggplot2)

## Get base map and define its extent
map("world",
    xlim = c(117, 120),
    ylim = c(8, 11),
    fill = TRUE,
    col = "lightgray")

## Add sample point
points(lon_dd, lat_dd,
       pch = 19,
       col = "red",
       cex = 1.5)

text(lon_dd, lat_dd,
     labels = "Sample Location",
     pos = 3)

## 3. Lines
## Download roads within the map extent
roads <- opq(
  bbox = c(117, 8, 120, 11)) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

## Base map
map("world",
    xlim = c(117, 120),
    ylim = c(8, 11),
    fill = TRUE,
    col = "lightgray")

## Add roads (lines)
plot(st_geometry(roads$osm_lines),
     col = "blue",
     lwd = 0.5,
     add = TRUE)

## Add sample point
points(lon_dd, lat_dd,
       pch = 19,
       col = "red",
       cex = 1.5)

text(lon_dd, lat_dd,
     labels = "Sample Location",
     pos = 3)

## 4. Use ggplot
ggplot() +
  geom_polygon(data = world,
               aes(long, lat, group = group),
               fill = "lightgray",
               color = "black",
               linewidth = 0.2) +
  ## Add roads
  geom_sf(data = roads$osm_lines,
          inherit.aes = FALSE,
          color = "blue",
          linewidth = 0.3) +
  ## Add sample point
  geom_point(aes(x = lon_dd, y = lat_dd),
             color = "red",
             size = 3) +
  annotate("text",
           x = lon_dd,
           y = lat_dd,
           label = "Sample Location",
           vjust = -1) +
  ## Map extent
  coord_sf(
    xlim = c(117, 120),
    ylim = c(8, 11),
    expand = FALSE) +
  ## X axis
  scale_x_continuous(
    breaks = seq(117, 120, by = 1)) +
  ## Y axis
  scale_y_continuous(
    breaks = seq(8, 11, by = 1)) +
  ## X and Y labels
  labs(
    x = "Longitude (°E)",
    y = "Latitude (°N)",
    title = "Map with Roads") +
  ## Theme
  theme_bw()

## 5. Working with raw data

## Read in csv file with coordinates
df <- read.csv("herpetozoa-033-095-s001.csv")
## Remove NAs
good <- complete.cases(df)
head(df[good, ])
cleaned <- df[good, ]

## Read in admin boundary
palawan <- st_read("palawan_adm.gpkg")

st_geometry_type(palawan)

## Plot (single point color)
ggplot() +
  ## Palawan admin bounds
  geom_sf(data = palawan,
          fill = "lightblue",
          color = "black",
          linewidth = 0.3) +
  ## Add sampling points
  geom_point(aes(x = cleaned$Long, y = cleaned$Lat,
             fill = "Sample Location"),
             shape = 21,
             color = "black",
             size = 3,
             stroke = 1.2) +
  scale_fill_manual(values = c("Sample Location" = "red"),
                     name = "Legend") +
  labs(title = "Palawan Province",
       x = "Longitude",
       y = "Latitude") +
  theme_bw()

## Plot (multiple point colors)
ggplot(cleaned, aes(x = Long, y = Lat)) +
  geom_sf(data = palawan,
          fill = "lightblue",
          color = "black",
          linewidth = 0.3,
          inherit.aes = FALSE) +
  geom_point(aes(fill = Htypes),
             shape = 21,
             color = "black",
             size = 3,
             stroke = 1.2) +
  labs(title = "Palawan Province",
       x = "Longitude",
       y = "Latitude",
       fill = "Habitat Type") +
  
  theme_bw()

######################
## B. Raster data
######################
pal <- st_read("palawan_adm.gpkg")
phil_dem <- rast("ph_strm_geo.tif")

## Check Coordinate Reference System
print(st_crs(pal) != st_crs(phil_dem))

## Transform if needed
pal <- st_transform(pal, st_crs(phil_dem))

## Clip raster
dem <- crop(phil_dem, pal) |> mask(pal)

## Check data info
crs(dem)        ## coordinate reference system
ext(dem)        ## spatial extent
nlyr(dem)       ## number of layers (bands)

## Plot
plot(dem)

## Convert for ggplot
names(dem) <- "elevation"
r_df <- as.data.frame(dem, xy = TRUE)

ggplot(r_df) +
  geom_tile(aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(name = "Elevation (m)") +
  coord_equal() +
  labs(title = "Palawan Province",
       x = "Longitude",
       y = "Latitude") +
  theme_bw()

## Change color manually
ggplot(r_df) +
  geom_tile(aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(
    colors = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
    name = "Elevation (m)") +
  coord_equal() +
  labs(title = "Palawan Province",
       x = "Longitude",
       y = "Latitude") +
  theme_bw()

## Plot both dem and sampling points
library(ggnewscale) # Install if needed

## Plot dem
ggplot() +
  geom_tile(data = r_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(
    colors = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
    name = "Elevation (m)") +

## Add sampling points 
  new_scale_fill() +
  geom_point(data = cleaned,
             aes(x = Long, y = Lat, fill = Htypes),
             shape = 21,
             color = "black",
             size = 3,
             stroke = 1.2) +
  scale_fill_brewer(palette = "Set2", name = "Habitat Type") +
  coord_equal() +
  labs(title = "Palawan Province",
       x = "Longitude",
       y = "Latitude") +
  theme_bw()

## Adjust point color manually
hab_colors <- c(
  "ASG"   = "#1b9e77",
  "CVT"= "#d95f02",
  "ESG"  = "#7570b3",
  "OGF"    = "#e7298a")

ggplot() +
  geom_tile(data = r_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(
    colors = c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c"),
    name = "Elevation (m)") +
  
  ## Reset fill scale for points
  new_scale_fill() +
  
  geom_point(data = cleaned,
             aes(x = Long, y = Lat, fill = Htypes),
             shape = 21, color = "black", size = 3, stroke = 1.2) +
  ## Switch to manual fill
  scale_fill_manual(values = hab_colors, name = "Habitat Type") +
  coord_equal() +
  labs(title = "Palawan Province",
       x = "Longitude",
       y = "Latitude") +
  theme_bw()

## Useful data links

## Quantum GIS - https://www.qgis.org/
## DIVA-GIS Data - https://diva-gis.org/data.html
## Earth Explorer Data - https://earthexplorer.usgs.gov/
## Climate data (WorldClim) - https://www.worldclim.org/data/index.html
## OpenStreetMap - https://www.openstreetmap.org/
## Philippine Geoportal - https://www.geoportal.gov.ph/
## Protected and Conserved Areas - https://www.protectedplanet.net/en
## Global Biodiversity Information Facility - https://www.gbif.org/

## Read in shapefile
my_shape <- st_read("shape/PHL_adm1.shp")

## Subset data by province or "NAME_1" column
pal <- my_shape[my_shape$NAME_1 == "Palawan", ]

## Use dplyr
library(dplyr)
mindanao <- my_shape %>%
  filter(NAME_1 %in% c("Agusan del Norte",
                       "Agusan del Sur",
                       "Davao del Norte"))
## Plot
plot(st_geometry(mindanao))

ggplot() +
  geom_sf(data = mindanao)
