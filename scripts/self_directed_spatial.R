library(tidyverse)
library(sf)
library(ggspatial)

# 1. Read your spatial data (e.g., shapefile, GeoJSON)
map_data <- st_read("data/City_Limits")

penn_medicine_facilities <- read_csv("data/penn_medicine_facilities.csv")
penn_medicine_facilities %>%
  View()

points_sf <- st_as_sf(penn_medicine_facilities[1:5,], coords = c("lon", "lat"), crs = 4326)
points_sf <- st_transform(points_sf, st_crs(map_data))


# 2. Build the map layer by layer
ggplot() +
  geom_sf(data = map_data, color = "white", size = 0.2) +
  geom_sf(data = points_sf, color = "black", size = 2) +

  # Add automated map accessories using ggspatial
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true") +
  
  # Ensure proper spatial projections
  coord_sf() + 
  theme_minimal()
