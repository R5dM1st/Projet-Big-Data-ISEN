# Histogramme du nombre de bateaux par catégorie (VesselType)
png("histogramme_categories_bateaux.png", width = 800, height = 600)
hist(as.numeric(data$VesselType),
     main = "Histogramme des catégories de bateaux",
     xlab = "Catégorie (VesselType)",
     ylab = "Nombre",
     col = "lightgreen",
     breaks = length(unique(data$VesselType)))
dev.off()

# Diagramme en barres : répartition des bateaux par type
png("repartition_bateaux_par_type.png", width = 800, height = 600)
barplot(table(data$VesselType),
        main = "Répartition des bateaux par type",
        xlab = "Type de bateau",
        ylab = "Nombre de bateaux",
        col = "skyblue")
dev.off()

library(dplyr)

# Charger les données (ex : base R)
df <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",")

# Arrondir les coordonnées
df <- df %>%
  mutate(
    LAT = round(LAT, 3),
    LON = round(LON, 3)
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

library(leaflet)

leaflet(data = top_coords) %>%
  addTiles() %>%
  addCircleMarkers(
    ~LON, ~LAT,
    radius = ~sqrt(Freq)/8,
    color = "red",
    fillOpacity = 0.2,
    label = ~paste0("Lat: ", LAT, ", Lon: ", LON, " (n=", Freq, ")")
  )

