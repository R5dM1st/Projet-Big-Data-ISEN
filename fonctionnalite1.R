#affiche le répertoire de travail courant
getwd()

data <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

#remplacer les valeurs "\N" par NA
data[data == "\\N"] <- NA

cat("\nNombre de bateaux restants :", nrow(data), "\n")

#cnversion des colonnes numériques
cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
for (col in cols_to_numeric) {
  data[[col]] <- as.numeric(as.character(data[[col]]))
}

# Conversion en facteurs
data$VesselType <- as.factor(data$VesselType)
data$Status <- as.factor(data$Status)
data$TransceiverClass <- as.factor(data$TransceiverClass)

# Filtrage des valeurs aberrantes
data <- data[
  (data$SOG <= 40 | is.na(data$SOG)) &
    (data$Heading >= 0 & data$Heading <= 359 | is.na(data$Heading)) &
    (data$Length >= 10 & data$Length <= 400 | is.na(data$Length)) &
    (data$Width >= 3 & data$Width <= 80 | is.na(data$Width)) &
    (data$Draft > 0 | is.na(data$Draft)) &
    (is.na(data$IMO) | nchar(as.character(data$IMO)) <= 10) &
    (data$LAT >= 20 & data$LAT <= 31 | is.na(data$LAT)) &
    (data$LON >= -98 & data$LON <= -78 | is.na(data$LON)) &
    !((data$VesselType == 60 & (is.na(data$Cargo) | data$Cargo == 0 | data$Cargo == 99)) |
        (data$VesselType == 80 & is.na(data$Cargo))),
]

#remet heading cOG et SOG a 0 si l'une des trois est à 0
zero_idx <- (data$Heading == 0 | data$COG == 0 | data$SOG == 0)
data$Heading[zero_idx] <- 0
data$COG[zero_idx] <- 0
data$SOG[zero_idx] <- 0

#remplacement des NA par la médiane par VesselType
cols_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading")
for (col in cols_numeric) {
  # Pour chaque type de bateau
  for (vtype in levels(data$VesselType)) {
    idx <- which(data$VesselType == vtype & is.na(data[[col]]))
    if (length(idx) > 0) {
      med <- median(data[[col]][data$VesselType == vtype & !is.na(data[[col]])], na.rm = TRUE)
      data[[col]][idx] <- med
    }
  }
}

#vérification des NA
print(colSums(is.na(data[cols_numeric])))
print(colSums(is.na(data)))

#nombre de bateaux restants après filtrage
cat("\nNombre de bateaux restants :", nrow(data), "\n")

#suppression des doublons (hors colonne "id")
data <- data[!duplicated(data[, setdiff(names(data), "id")]), ]
cat("\nNombre de lignes restantes après suppression des doublons :", nrow(data), "\n")

# Comptage du nombre de bateaux uniques (MMSI) par type de bateau (sans %>%)
vtypes <- unique(data$VesselType)
type_counts <- data.frame(VesselType = character(), n_bateaux = numeric(), stringsAsFactors = FALSE)
for (vt in vtypes) {
  count <- length(unique(data$MMSI[data$VesselType == vt]))
  type_counts <- rbind(type_counts, data.frame(VesselType = vt, n_bateaux = count))
}
type_counts <- type_counts[order(-type_counts$n_bateaux), ]
print(type_counts)

# MMSI où la longueur est NA
mmsi_na_length <- data$MMSI[is.na(data$Length)]
print(mmsi_na_length)
cat("Nombre de MMSI où Length est NA :", length(mmsi_na_length), "\n")

#export des données nettoyées
write.csv(data, file = "vessel-clean-final.csv", row.names = FALSE)
cat("Fichier CSV nettoyé exporté sous le nom : vessel-clean-final.csv\n")

#résumé statistique des colonnes numériques
cols_to_summarize <- c("LAT", "LON", "Length", "Width", "Draft", "SOG", "COG", "Heading")
for (col in cols_to_summarize) {
  cat("\nRésumé statistique de la colonne :", col, "\n")
  print(summary(data[[col]]))
}
