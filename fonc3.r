library(ggplot2)
library(dplyr)
library(maps)

# Load and clean data (as in your original code)
data <- read.csv("vessel-clean-final.csv", stringsAsFactors = FALSE)
data_map <- data %>%
  filter(!is.na(LAT), !is.na(LON), LAT > -90, LAT < 90, LON > -180, LON < 180)

# Base map
world_map <- map_data("world")

# Define Gulf of Mexico limits (adjust if needed)
lon_min <- -98
lon_max <- -80
lat_min <- 18
lat_max <- 31

# All trajectories in the Gulf of Mexico
ggplot() +
  # Make map borders more visible, keep fill light
  borders("world", colour = "grey60", fill = "white") + # Darker grey borders, white fill
  geom_path(data = data_map, aes(x = LON, y = LAT, group = MMSI),
            color = "red", alpha = 0.15, size = 0.3) + # Slightly increased alpha and size, new color
  coord_fixed(1.9, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  labs(title = "Trajectoires des bateaux – Golfe du Mexique",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA), # Ensure white background
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    axis.line = element_line(colour = "grey50"), # Subtle axis lines
    axis.ticks = element_line(colour = "grey50"), # Add axis ticks for more definition
    axis.text = element_text(colour = "grey30"), # Make axis text slightly darker
    axis.title = element_text(colour = "grey30") # Make axis titles slightly darker
  )
