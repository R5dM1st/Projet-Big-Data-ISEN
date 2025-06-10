getwd()
data <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data[data == "\\N"] <- NA

cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
data[cols_to_numeric] <- lapply(data[cols_to_numeric], as.numeric)
data$VesselType <- as.factor(data$VesselType)
data$Status <- as.factor(data$Status)
data$TransceiverClass <- as.factor(data$TransceiverClass)

# Ajout du graphique ggplot2 pour la répartition des types de navires
library(ggplot2)
ggplot(data, aes(x = as.factor(VesselType))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Répartition des types de navires",
    x = "Type de navire (VesselType)",
    y = "Nombre d'enregistrements"
  ) +
  theme_minimal()

head(data,5)

# --- Fonctionnalité 1 : Description et Exploration des Données ---

# 1. Description du jeu de données
cat("Dimensions du jeu de données :", dim(data), "\n")
cat("Aperçu des premières lignes :\n")
print(head(data, 5))
cat("Structure des variables :\n")
str(data)

# 2. Statistiques descriptives univariées
cat("\nStatistiques descriptives globales :\n")
print(summary(data)) # Pour toutes les colonnes

# Pour les variables numériques uniquement :
cat("\nStatistiques pour les variables numériques :\n")
print(summary(data[sapply(data, is.numeric)]))

# Pour les variables catégorielles :
cat("\nRésumé des variables catégorielles :\n")
cat("VesselType :\n")
print(table(data$VesselType, useNA = "ifany"))

# 3. Nettoyage des données
# (déjà fait dans ton script) : remplacement des "\\N" par NA, conversion des types, etc.

# 4. Valeurs manquantes
cat("\nNombre de valeurs manquantes par colonne :\n")
print(colSums(is.na(data)))

# 5. Valeurs aberrantes (exemple pour Length, SOG)
cat("\nValeurs aberrantes potentielles (par exemple, Length < 0 ou SOG > 50) :\n")
print(data[data$Length < 0 | data$SOG > 50, ])

# 6. Doublons
cat("\nNombre de doublons (lignes identiques) :\n")
print(sum(duplicated(data)))

# Pour supprimer les doublons :
# data <- data[!duplicated(data), ]

