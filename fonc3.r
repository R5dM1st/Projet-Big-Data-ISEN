library(ggplot2)
library(dplyr)
library(maps)

data <- read.csv("vessel-clean-final.csv", stringsAsFactors = FALSE)

# Nettoyage des coordonnées valides
data_map <- subset(data, !is.na(LAT) & !is.na(LON) & LAT > -90 & LAT < 90 & LON > -180 & LON < 180)

# Limites du Golfe du Mexique
lon_min <- -98
lon_max <- -80
lat_min <- 18
lat_max <- 31

# ----------- A. Calcul des 10 points d'arrêt les plus fréquents -----------

#arrondir les coordonnées
data_ports <- data_map
data_ports$LAT <- round(data_ports$LAT, 0)
data_ports$LON <- round(data_ports$LON, 0)

#filtrer les arrêts (SOG, COG ou Heading à 0)
filtered_ports <- subset(data_ports, SOG == 0 | COG == 0 | Heading == 0)

# Compter les occurrences
coord_freq <- aggregate(
  list(Freq = rep(1, nrow(filtered_ports))),
  by = list(LAT = filtered_ports$LAT, LON = filtered_ports$LON),
  FUN = sum
)
coord_freq <- coord_freq[order(-coord_freq$Freq), ]

# Top 10 points d'arrêt
top_coords <- head(coord_freq, 10)

# ----------- 1. Affichage de toutes les trajectoires -----------

png("trajectoires_golfe_mexique.png", width = 1200, height = 900, res = 120)

ggplot() +
  borders("world", colour = "grey60", fill = "white") +
  geom_path(
    data = data_map,
    aes(x = LON, y = LAT, group = MMSI),
    color = "blue", alpha = 0.15, size = 0.3
  ) +
  geom_point(
    data = top_coords,
    aes(x = LON, y = LAT, size = Freq),
    color = "red", alpha = 0.3
  ) +
  scale_size(range = c(4, 16), name = "Nombre d'occurrences") +
  coord_fixed(1.4, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  labs(
    title = "Trajectoires des bateaux – Golfe du Mexique",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 14)

dev.off()

# ----------- 2. Trajectoire individuelle d’un bateau -----------

#conversion des dates
data_map$BaseDateTime <- as.POSIXct(data_map$BaseDateTime, format = "%Y-%m-%d %H:%M:%S")

#entrée utilisateur
mmsi_input <- readline(prompt = "Entrez le MMSI du bateau à afficher : ")
mmsi_input <- as.numeric(mmsi_input)

# Filtrer et trier les données de ce bateau
data_bateau <- subset(data_map, MMSI == mmsi_input)
data_bateau <- data_bateau[order(data_bateau$BaseDateTime), ]

# Affichage de la trajectoire individuelle
png(paste0("trajectoire_bateau_", mmsi_input, ".png"), width = 1200, height = 900, res = 120)

ggplot() +
  borders("world", colour = "grey60", fill = "white") +
  geom_path(
    data = data_bateau,
    aes(x = LON, y = LAT),
    color = "blue", size = 1, alpha = 0.5
  ) +
  geom_point(
    data = data_bateau,
    aes(x = LON, y = LAT),
    color = "darkblue", size = 1.5, alpha = 0.8
  ) +
  geom_point(
    data = top_coords,
    aes(x = LON, y = LAT, size = Freq),
    color = "red", alpha = 0.3
  ) +
  scale_size(range = c(4, 16), name = "Nombre d'occurrences") +
  coord_fixed(1.4, xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  labs(
    title = paste("Trajectoire du bateau MMSI", mmsi_input, "– Golfe du Mexique"),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal(base_size = 14)

dev.off()
