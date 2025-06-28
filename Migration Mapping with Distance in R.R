

''''' How to create a Bangladesh migration map with 50+ demo data points, 
showing migration between major districts or cities, 
with distances and a visualization in R

'''''

##Step 1: Install & Load Required Packages

install.packages(c("ggplot2", "geosphere", "dplyr", "maps", "mapdata"))
library(ggplot2)
library(geosphere)
library(dplyr)
library(maps)
library(mapdata)


## Step 2: Get Major Cities in Bangladesh (with coordinates)
# Sample of major cities with approximate coordinates
cities <- data.frame(
  city = c("Dhaka", "Chittagong", "Khulna", "Rajshahi", "Sylhet", "Barisal", "Rangpur", "Mymensingh", "Comilla", "Narayanganj", 
           "Gazipur", "Jessore", "Cox's Bazar", "Bogura", "Dinajpur", "Tangail", "Pabna", "Noakhali", "Brahmanbaria", "Jamalpur"),
  lat = c(23.8103, 22.3569, 22.8456, 24.3636, 24.8949, 22.7010, 25.7465, 24.7471, 23.4607, 23.6238, 
          23.9999, 23.1667, 21.4272, 24.8465, 25.6279, 24.2513, 24.0064, 22.8231, 23.9574, 24.9375),
  lon = c(90.4125, 91.7832, 89.5403, 88.6241, 91.8687, 90.3535, 89.2500, 90.4203, 91.1809, 90.5000, 
          90.4203, 89.2167, 92.0058, 89.3773, 88.6411, 89.9167, 89.2333, 91.1000, 91.1110, 89.9375)
)




##Step 3: Generate 50 Random Migration Flows

set.seed(123)

# Generate 50 random pairs of origin and destination
migration_data <- data.frame(
  origin_index = sample(1:nrow(cities), 50, replace = TRUE),
  dest_index = sample(1:nrow(cities), 50, replace = TRUE)
)

# Avoid same city as origin and destination
migration_data <- migration_data %>% 
  filter(origin_index != dest_index)

# Merge with city coordinates
migration_data <- migration_data %>%
  mutate(
    origin_city = cities$city[origin_index],
    origin_lat = cities$lat[origin_index],
    origin_lon = cities$lon[origin_index],
    dest_city = cities$city[dest_index],
    dest_lat = cities$lat[dest_index],
    dest_lon = cities$lon[dest_index]
  ) %>%
  rowwise() %>%
  mutate(distance_km = distHaversine(c(origin_lon, origin_lat), c(dest_lon, dest_lat)) / 1000)


## Step 4: Draw the Map of Bangladesh with Migration Paths

# Use world map for context and filter to Bangladesh area
bd_map <- map_data("world") %>% filter(region == "Bangladesh")

ggplot() +
  geom_polygon(data = bd_map, aes(x = long, y = lat, group = group),
               fill = "lightyellow", color = "gray40") +
  geom_curve(data = migration_data,
             aes(x = origin_lon, y = origin_lat,
                 xend = dest_lon, yend = dest_lat,
                 size = distance_km),
             color = "darkblue", curvature = 0.3,
             arrow = arrow(length = unit(0.15, "cm"))) +
  geom_point(data = cities, aes(x = lon, y = lat), color = "red", size = 2) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city), hjust = 0, vjust = 0, size = 3) +
  geom_text(data = migration_data,
            aes(x = (origin_lon + dest_lon) / 2,
                y = (origin_lat + dest_lat) / 2,
                label = paste0(round(distance_km), " km")),
            size = 2.5, color = "black") +
  labs(title = "Migration Map of Bangladesh with Distances",
       size = "Distance (km)") +
  coord_fixed(1.2) +
  theme_minimal()
