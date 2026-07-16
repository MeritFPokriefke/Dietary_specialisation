
#######################################################
###### Drivers of dietary specialisation study #######
######################################################
### Script: 01#Plot_map

### Map that depicts study plots, density treatments and frass collector locations for the year 2024 (as example)


### Accessed last: September 2025
######################################################


### packages and data
#install.packages("devtools")
#devtools::install_github("ropensci/rnaturalearthhires")

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(osmdata)
library(ggspatial)

location <- read.csv2("Data/Extracted/Density.csv") # location nest boxes
collectors <- read.csv("Data/Extracted/collectors_clean.csv" ) # location frass collectors

nest_boxes <- location %>%
  filter(Year == 2023)

any(is.na(nest_boxes$Longitude))
any(is.na(nest_boxes$Latitude))

### convert Latitude and Longitude to numeric
nest_boxes$Latitude <- as.numeric(nest_boxes$Latitude)
nest_boxes$Longitude <- as.numeric(nest_boxes$Longitude)

nest_boxes_sf <- st_as_sf(nest_boxes, coords = c("Longitude", "Latitude"), crs = 4326) # crs is coordinate sys and convert to sf object

world <- ne_countries(scale = "medium", returnclass = "sf") # download a map

### add paths/roads to the map
### define the bounding box for your area of interest
bbox <- st_bbox(nest_boxes_sf)

### fetch roads data using the bounding box
roads <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

### extract the roads geometry
roads_sf <- roads$osm_lines


### define colors for each Treatment
treatment_colors <- c(
  "GL_BH" = "#3182bd",
  "GH_BL" = "#fec44f",
  "GL_BL" = "#66c2a4",
  "GH_BH" = "#006d2c"
)

### calculate the range of longitude and latitude with a buffer
buffer <- 0.0025  # small buffer for the map limits
xlim_values <- range(nest_boxes$Longitude) + c(-buffer, buffer)
ylim_values <- range(nest_boxes$Latitude) + c(-buffer, buffer)

nest_hulls <- nest_boxes_sf |>
  group_by(Treatment, Plot) |>                 # group by both Treatment and Plot
  summarise(geometry = st_union(geometry), .groups = "drop") |> 
  st_convex_hull()                             # wrap each group in one polygon

### convert to sf object
collectors_sf <- st_as_sf(
  collectors,
  coords = c("lon", "lat"),
  crs = 4326
)

### make polygons bigger by buffering (e.g. 20 meters)
nest_hulls_wide <- nest_hulls |>
  st_transform(3857) |>        # project to metric CRS (meters)
  st_buffer(dist = 40) |>      # expand each polygon by 20m
  st_transform(4326)           # back to lat/lon

### create the plot
ggplot(data = world) +
  geom_sf(fill = "#e5f5e0") +
  geom_sf(data = roads_sf, color = "grey50", size = 0.5, alpha = 0.7) +
  
  ### polygons per Treatment × Plot (wider, no outline)
  geom_sf(
    data = nest_hulls_wide,
    aes(fill = Treatment),
    alpha = 0.8,
    color = NA
  ) +
  
  ### collectors mapped to shape aesthetic so they get a legend
  geom_sf(
    data = collectors_sf,
    aes(shape = "Collector"),   # <-- creates legend key
    color = "#1f0f07",
    fill = "#4a2c16",
    size = 5,
    stroke = 0.8,
    alpha = 0.8
  ) +
  
  coord_sf(xlim = xlim_values, ylim = ylim_values, expand = FALSE) +
  
  ### legends for treatments (fill) and collectors (shape)
  scale_fill_manual(values = treatment_colors, name = "Treatment") +
  scale_shape_manual(
    values = c("Collector" = 18),  # solid diamond
    name = "Collector"
  ) +
  
  theme_minimal() +
  labs(title = NULL) +
  
  ### north arrow
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering(
                           fill = c("grey", "grey"),
                           line_col = "grey"),
                         height = unit(0.5, "cm"),
                         width = unit(0.5, "cm")) +
  annotate("text", x = -Inf, y = Inf, label = "N", color = "grey",
           size = 3, hjust = -0.2, vjust = 1.5) +
  
  ### scale bar
  annotation_scale(location = "br",
                   width_hint = 0.3,
                   style = "ticks",
                   bar_cols = c("grey", "grey"),
                   text_col = "grey") +
  
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
