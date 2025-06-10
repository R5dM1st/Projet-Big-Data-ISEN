library(ggplot2)
library(dplyr)
library(maps)

# Chargement et nettoyage des données
data <- read.csv("vessel-clean-final.csv", stringsAsFactors = FALSE)
data_map <- data %>%
  filter(!is.na(LAT), !is.na(LON), LAT > -90, LAT < 90, LON > -180, LON < 180)

# Carte de base (optionnel, tu peux la rendre très claire ou la retirer complètement)
world_map <- map_data("world")

# Définir les limites du Golfe du Mexique (ajuste si besoin)
lon_min <- -98
lon_max <- -80
lat_min <- 18
lat_max <- 31

# Toutes les trajectoires dans le Golfe du Mexique
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "grey95", color = "grey80") +
  geom_path(data = data_map, aes(x = LON, y = LAT, group = MMSI), alpha = 0.3, color = "blue") +
  coord_fixed(1.3, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  labs(title = "Trajectoires des bateaux – Golfe du Mexique",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

