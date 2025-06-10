# Visualisation des données sur les bateaux
# Utilisation d'un nouveau fichier CSV (exemple: "vessel-clean-final.csv")
getwd()
# Chargement des données
data <- read.csv("vessel-clean-final.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

# 1. Histogramme des différentes catégories de bateaux
barplot(table(data$VesselType),
        main = "Répartition des bateaux par catégorie",
        xlab = "Type de bateau",
        ylab = "Nombre de bateaux",
        col = "skyblue")

# 2. Histogramme des ports les plus utilisés (si la colonne existe)
if ("Port" %in% colnames(data)) {
  ports_valides <- data$Port[!is.na(data$Port) & data$Port != ""]
  if (length(ports_valides) > 0) {
    port_counts <- sort(table(ports_valides), decreasing = TRUE)
    barplot(head(port_counts, 10),
            main = "Top 10 des ports les plus utilisés",
            xlab = "Port",
            ylab = "Nombre de bateaux",
            col = "orange",
            las = 2)
  } else {
    cat("Aucun port valide trouvé dans la colonne 'Port'.\n")
  }
} else {
  cat("La colonne 'Port' n'existe pas dans les données.\n")
}

# 3. Clustering K-means sur les dimensions (Length et Width)
# Préparation des données pour le clustering
donnees_kmeans <- na.omit(data[, c("Length", "Width")])

# Si au moins 2 colonnes et assez de points, on lance le k-means
if (nrow(donnees_kmeans) > 0) {
  set.seed(123) # Pour reproductibilité
  # Choix du nombre de clusters (par exemple 3)
  k <- 3
  km <- kmeans(donnees_kmeans, centers = k)
  
  # Ajoute la classe cluster au jeu de données original (lignes sans NA)
  data$Cluster <- NA
  data$Cluster[as.numeric(rownames(donnees_kmeans))] <- km$cluster
  
  # Histogramme du nombre de bateaux par cluster
  barplot(table(data$Cluster),
          main = "Répartition des bateaux par cluster K-means",
          xlab = "Cluster",
          ylab = "Nombre de bateaux",
          col = "lightcoral")
  
  # Visualisation des clusters sur un plot 2D (optionnel)
  plot(donnees_kmeans$Length, donnees_kmeans$Width, col = km$cluster,
       pch = 19, xlab = "Longueur", ylab = "Largeur", 
       main = "Clustering K-means des bateaux (Length vs Width)")
  legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 19)
} else {
  cat("Pas de données suffisantes pour appliquer le clustering K-means.\n")
}

