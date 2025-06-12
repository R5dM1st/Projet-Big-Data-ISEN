# Fonctionnalité 1 : Nettoyage, filtrage, statistiques et export CSV

# Affiche le répertoire de travail courant
getwd()

# Charge le fichier CSV des données de navires
data <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

# Remplace les valeurs "\N" par NA dans tout le data frame
data[data == "\\N"] <- NA

# Affiche le nombre de bateaux (lignes) après chargement
cat("\nNombre de bateaux restants :", nrow(data), "\n")

# Conversion des colonnes numériques et catégorielles pour préparer les données à l'analyse
cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
data[cols_to_numeric] <- lapply(data[cols_to_numeric], function(x) as.numeric(as.character(x)))
data$VesselType <- as.factor(data$VesselType)
data$Status <- as.factor(data$Status)
data$TransceiverClass <- as.factor(data$TransceiverClass)

# Filtrage des valeurs aberrantes et application des conditions sur les colonnes
data <- data[
  (data$SOG <= 40 | is.na(data$SOG)) &
    (data$Heading >= 0 & data$Heading <= 359 | is.na(data$Heading)) &
    (data$Length >= 10 & data$Length <= 400 | is.na(data$Length)) &
    (data$Width >= 3 & data$Width <= 80 | is.na(data$Width)) &
    (data$Draft > 0 | is.na(data$Draft)) &
    (is.na(data$IMO) | nchar(as.character(data$IMO)) <= 10) &
    (data$LAT >= 20 & data$LAT <= 31 | is.na(data$LAT)) &
    (data$LON >= -98 & data$LON <= -78 | is.na(data$LON)) &
    !( (data$VesselType == 60 & (is.na(data$Cargo) | data$Cargo == 0 | data$Cargo == 99)) |
         (data$VesselType == 80 & is.na(data$Cargo)) ),
]

# Si une des colonnes Heading, COG ou SOG vaut 0, alors toutes sont mises à 0 sur la ligne
zero_idx <- (data$Heading == 0 | data$COG == 0 | data$SOG == 0)
data$Heading[zero_idx] <- 0
data$COG[zero_idx] <- 0
data$SOG[zero_idx] <- 0

library(dplyr)

cols_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading") # adapte si besoin

for (col in cols_numeric) {
  data <- data %>%
    group_by(VesselType) %>%
    mutate(
      !!sym(col) := ifelse(is.na(.data[[col]]), median(.data[[col]], na.rm = TRUE), .data[[col]])
    ) %>%
    ungroup()
}

# Vérification : nombre de NA par colonne numérique
print(colSums(is.na(data[cols_numeric])))
# Affiche le nombre de valeurs manquantes par colonne
print(colSums(is.na(data)))



# Affiche le nombre de bateaux restants après filtrage
cat("\nNombre de bateaux restants :", nrow(data), "\n")

# Suppression des doublons (hors colonne "id")
data <- data[!duplicated(data[, setdiff(names(data), "id")]), ]
cat("\nNombre de lignes restantes après suppression des doublons :", nrow(data), "\n")

# Affiche le nombre de valeurs manquantes après nettoyage
print(colSums(is.na(data)))

# Charge le package dplyr pour l'analyse
library(dplyr)

# Comptage du nombre de bateaux uniques (MMSI) par type de bateau
type_counts <- data %>%
  group_by(VesselType) %>%
  summarise(n_bateaux = n_distinct(MMSI)) %>%
  arrange(desc(n_bateaux))
print(type_counts)

# Affiche les MMSI dont la longueur (Length) est NA
mmsi_na_length <- data$MMSI[is.na(data$Length)]
print(mmsi_na_length)
cat("Nombre de MMSI où Length est NA :", length(mmsi_na_length), "\n")

# Export des données nettoyées vers un nouveau fichier CSV
write.csv(data, file = "vessel-clean-final.csv", row.names = FALSE)
cat("Fichier CSV nettoyé exporté sous le nom : vessel-clean-final.csv\n")

