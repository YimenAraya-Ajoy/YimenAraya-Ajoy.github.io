library(ggplot2)
library(ggmap)
library(dplyr)
library(ggrepel)
library(sf)
library(maps)
library(geosphere)
library(tidygeocoder)

# Read CSV file
data <- read.csv("/home/yi/Downloads/InstMaps.csv")

# Get unique locations for geocoding
locations <- unique(c(data$Location_1, data$Location_2))

# Geocode locations using OpenStreetMap (OSM)
geocode_results <- data.frame(Location = locations) %>%
  geocode(Location, method = "osm")

# Merge coordinates into main data frame
data <- data %>%
  left_join(geocode_results, by = c("Location_1" = "Location")) %>%
  rename(lon1 = long, lat1 = lat) %>%
  left_join(geocode_results, by = c("Location_2" = "Location")) %>%
  rename(lon2 = long, lat2 = lat)

# Get world map
gg <- map_data("world")

# Plot the map
target_bbox <- c(left = -120, bottom = 8, right = 25, top = 65) # Adjust these values as needed

data <- data %>% mutate(Institution = as.character(Institution))
data$Papers<-data$Papers


p <- ggplot() +
  geom_polygon(data = gg, aes(x = long, y = lat, group = group), fill = "gray", color = "white") +
  geom_curve(data = data, aes(x = lon1, y = lat1, xend = lon2, yend = lat2, linewidth = Papers),
              color = "blue", curvature = 0.3, alpha = 0.2) +
    geom_point(data = data, aes(x = lon1, y = lat1, size = Papers), color = "red") +
  geom_text_repel(data = data, aes(x = lon1, y = lat1, label = Institution),
                  size = 3, color = "black", max.overlaps = 10) +
  coord_cartesian(xlim = c(target_bbox["left"], target_bbox["right"]), 
                  ylim = c(target_bbox["bottom"], target_bbox["top"])) +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "Longitude", y = "Latitude") +
  scale_size(range = c(0.5, 5))


# Add Trondheim manually as a point with NTNU label
trondheim_point <- data.frame(lon1 = geocode_results$long[geocode_results$Location == "Trondheim"],
                              lat1 = geocode_results$lat[geocode_results$Location == "Trondheim"],
                              Institution = "NTNU",
                              Papers = max(data$Papers, na.rm = TRUE))

p <- p + 
  geom_point(data = trondheim_point, aes(x = lon1, y = lat1), color = "blue", size=6) +
  geom_text_repel(data = trondheim_point, aes(x = lon1, y = lat1, label = Institution),
                  size = 5, color = "black")

print(p)

ggsave("/home/yi/Dropbox/YimenAraya-Ajoy.github.io/assets/images/Map.png", plot = p, width = 10, height = 6, dpi = 600)
