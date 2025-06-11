library(ggplot2)
library(dplyr)
library(maps)
# install.packages("hexbin")  # À faire une seule fois si besoin
# library(hexbin)  # Pas utilisé ici

# Charger et nettoyer les données
data <- read.csv("vessel-clean-final.csv", stringsAsFactors = FALSE)
data_map <- data %>%
  filter(!is.na(LAT), !is.na(LON), LAT > -90, LAT < 90, LON > -180, LON < 180)

# Définir les limites du Golfe du Mexique
lon_min <- -98
lon_max <- -80
lat_min <- 18
lat_max <- 31

# ----------- A. Calcul des 10 points d'arrêt les plus fréquents -----------

# Arrondir les coordonnées pour regrouper
data_ports <- data_map %>%
  mutate(
    LAT = round(LAT, 0),
    LON = round(LON, 0)
  )

# Filtrer les arrêts (SOG, COG ou Heading à zéro)
filtered_ports <- data_ports %>%
  filter(SOG == 0 | COG == 0 | Heading == 0)

# Compter occurrences par coordonnée
coord_freq <- filtered_ports %>%
  group_by(LAT, LON) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  arrange(desc(Freq))

# Top 10 points les plus fréquents
top_coords <- head(coord_freq, 10)

# ----------- 1. Affichage de toutes les trajectoires (PNG) -----------

png("trajectoires_golfe_mexique.png", width = 1200, height = 900, res = 120)
ggplot() +
  borders("world", colour = "grey60", fill = "white") +
  geom_path(data = data_map, aes(x = LON, y = LAT, group = MMSI),
            color = "blue", alpha = 0.15, size = 0.3) +
  geom_point(data = top_coords, aes(x = LON, y = LAT, size = Freq),
             color = "red", alpha = 0.3) +
  scale_size(range = c(4, 16), name = "Nombre d'arrêts") +
  coord_fixed(1.4, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  labs(title = "Trajectoires des bateaux – Golfe du Mexique",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 14)
dev.off()

# ----------- 2. Trajectoire individuelle d'un bateau (PNG) -----------

data_map <- data_map %>%
  mutate(BaseDateTime = as.POSIXct(BaseDateTime, format = "%Y-%m-%d %H:%M:%S"))

mmsi_input <- readline(prompt = "Entrez le MMSI du bateau à afficher : ")
mmsi_input <- as.numeric(mmsi_input)

# Filtrer et trier les données pour ce bateau
data_bateau <- data_map %>%
  filter(MMSI == mmsi_input) %>%
  arrange(BaseDateTime)

png(paste0("trajectoire_bateau_", mmsi_input, ".png"), width = 1200, height = 900, res = 120)
ggplot() +
  borders("world", colour = "grey60", fill = "white") +
  geom_path(data = data_bateau, aes(x = LON, y = LAT), color = "red", size = 1, alpha = 0.5) +
  geom_point(data = data_bateau, aes(x = LON, y = LAT), color = "darkred", size = 1.5, alpha = 0.8) +
  geom_point(data = top_coords, aes(x = LON, y = LAT, size = Freq),
             color = "blue", alpha = 0.3) +
  scale_size(range = c(4, 16), name = "Nombre d'arrêts") +
  coord_fixed(1.4, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  labs(title = paste("Trajectoire du bateau MMSI", mmsi_input, "– Golfe du Mexique"),
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 14)
dev.off()