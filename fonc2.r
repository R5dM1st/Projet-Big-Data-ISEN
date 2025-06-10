# Visualisation des données sur les bateaux
# Utilisation d'un nouveau fichier CSV (exemple: "vessel-clean-final.csv")

# Chargement des données
data <- read.csv("vessel-clean-final.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

# Conversion rapide si besoin
cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
data[cols_to_numeric] <- lapply(data[cols_to_numeric], as.numeric)
data$VesselType <- as.factor(data$VesselType)

# 1. Répartition des bateaux suivant leur type (barplot)
png("repartition_bateaux_par_type.png", width = 800, height = 600)
barplot(table(data$VesselType),
        main = "Répartition des bateaux par type",
        xlab = "Type de bateau",
        ylab = "Nombre de bateaux",
        col = "skyblue")
dev.off()

# 2. Histogramme sur la longueur des bateaux
png("histogramme_longueur_bateaux.png", width = 800, height = 600)
hist(data$Length,
     main = "Histogramme de la longueur des bateaux",
     xlab = "Longueur (mètres)",
     ylab = "Nombre",
     col = "lightgreen",
     breaks = 30)
dev.off()

# 3. Ports les plus utilisés (si la colonne existe)
if ("Port" %in% names(data)) {
  png("ports_plus_utilises.png", width = 900, height = 600)
  port_counts <- sort(table(data$Port), decreasing = TRUE)
  barplot(head(port_counts, 10),
          main = "Top 10 des ports les plus utilisés",
          xlab = "Port",
          ylab = "Nombre de bateaux",
          col = "orange",
          las = 2)
  dev.off()
}

# 4. Exemple avec ggplot2 si tu veux un rendu plus moderne (optionnel)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  p <- ggplot(data, aes(x = VesselType)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Répartition des bateaux par type",
         x = "Type de bateau", y = "Nombre de bateaux") +
    theme_minimal()
  ggsave("ggplot_repartition_bateaux_par_type.png", plot = p, width = 8, height = 6)
}