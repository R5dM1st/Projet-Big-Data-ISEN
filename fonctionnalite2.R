
# Partie 2 – Visualisation des types de navires et ports les plus fréquentés

library(ggplot2)
library(dplyr)

# Diagramme en barres : répartition des bateaux par type
png("figures/repartition_bateaux_par_type.png", width = 800, height = 600)
barplot(table(data$VesselType),
        main = "Répartition des bateaux par type",
        xlab = "Type de bateau",
        ylab = "Nombre de bateaux",
        col = "skyblue")
dev.off()


# Calcul du nombre de bateaux uniques par type
type_counts <- data %>%
  group_by(VesselType) %>%
  summarise(n_bateaux = n_distinct(MMSI)) %>%
  arrange(desc(n_bateaux))

# Ajouter une part en pourcentage pour la lisibilité
type_counts <- type_counts %>%
  mutate(pourcentage = round(n_bateaux / sum(n_bateaux) * 100, 1),
         etiquette = paste0(VesselType, " (", pourcentage, "%)"))

# Tracer le camembert
ggplot(type_counts, aes(x = "", y = n_bateaux, fill = etiquette)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Répartition des bateaux par type") +
  theme_void() +
  theme(legend.title = element_blank())

library(dplyr)

# Charger les données (ex : base R)
df <- read.csv("vessel-clean-final.csv", header = TRUE, sep = ",")

# Arrondir les coordonnées
df <- df %>%
  mutate(
    LAT = round(LAT, 0),
    LON = round(LON, 0)
  )

# Filtrer SOG, COG ou Heading à zéro
filtered <- df %>%
  filter(SOG == 0 | COG == 0 | Heading == 0)

# Compter les occurrences de chaque coordonnée arrondie
coord_freq <- filtered %>%
  group_by(LAT, LON) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  arrange(desc(Freq))

# Afficher les 10 points les plus fréquents
top_coords <- head(coord_freq, 10)
print(top_coords)

#Affichage de la carte avec les Ports les + utilisés
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
#On crée des cercles de taille proportionnelle avec leur importance
#sur les coordonnées arrondies LAT,LON


# Nommination des ports
ports_utilises <- c("New Orleans", "Houston", "Miami", "Lake Jackson",
              "Road1", "Lake Charles", "Road2", "Corpus Christi",
              "Tampa","Donaldsonvile")

# Ajouter cette colonne au data frame
top_coords$Coord <- ports_utilises

# Affichage de l’histogramme
ggplot(top_coords, aes(x = reorder(Coord, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Top 10 des ports les plus utilisés",
    x = "Ports",
    y = "Nombre d'ocurrences"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Téléchargement en png
ggsave("figures/histogramme_ports_frequentes_p2.png", width = 10, height = 6)

