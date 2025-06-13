
# Partie 2 – Visualisation des types de navires et ports les plus fréquentés

library(ggplot2)
library(dplyr)

# Nettoyer les données : enlever les longueurs manquantes
donnees_plot <- data %>%
  filter(!is.na(Length))

# Créer le boxplot
png("figures/boxplot_longueur_par_type.png", width = 900, height = 700)
ggplot(donnees_plot, aes(x = as.factor(VesselType), y = Length, fill = as.factor(VesselType))) +
  geom_boxplot() +
  labs(
    title = "Distribution des longueurs de bateaux par type",
    x = "Type de navire (VesselType)",
    y = "Longueur (mètres)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
dev.off()

# Calcul du nombre de bateaux uniques par type
type_counts <- data %>%
  group_by(VesselType) %>%
  summarise(n_bateaux = n_distinct(MMSI)) %>%
  arrange(desc(n_bateaux)) %>%
  mutate(pourcentage = round(n_bateaux / sum(n_bateaux) * 100, 1),
         etiquette = paste0(VesselType, " (", pourcentage, "%)"))

# Camembert : répartition des bateaux par type
png("figures/camembert_repartition_bateaux.png", width = 700, height = 700)
ggplot(type_counts, aes(x = "", y = n_bateaux, fill = etiquette)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des bateaux par type") +
  theme_void() +
  theme(legend.title = element_blank())
dev.off()

# Arrondir les coordonnées
df <- data %>%
  mutate(
    LAT = round(LAT, 0),
    LON = round(LON, 0)
  )

# Filtrer SOG, COG ou Heading à zéro
filtered <- df %>%
  filter(SOG == 0 | COG == 0 | Heading == 0)

# Compter occurrences des coordonnées arrondies
coord_freq <- filtered %>%
  group_by(LAT, LON) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  arrange(desc(Freq))

# Top 10 points les plus fréquents
top_coords <- head(coord_freq, 10)
print(top_coords)

# Ajout noms des ports
ports_utilises <- c("New Orleans", "Houston", "Miami", "Lake Jackson",
                    "Road1", "Lake Charles", "Road2", "Corpus Christi",
                    "Tampa", "Donaldsonvile")

top_coords$Coord <- ports_utilises

# Histogramme top 10 ports
png("figures/histogramme_ports_frequentes.png", width = 1000, height = 700)
ggplot(top_coords, aes(x = reorder(Coord, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Top 10 des ports les plus utilisés",
    x = "Ports",
    y = "Nombre d'occurences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Visualisation interactive avec leaflet (non PNG, affichage dans RStudio ou navigateur)
library(leaflet)

leaflet(data = top_coords) %>%
  addTiles() %>%
  addCircleMarkers(
    ~LON, ~LAT,
    radius = ~sqrt(Freq)/10,
    color = "red",
    fillOpacity = 0.2,
    label = ~paste0("Lat: ", LAT, ", Lon: ", LON, " (n=", Freq, ")")
  )