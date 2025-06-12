# Affiche le répertoire de travail courant
getwd()

# Charge le fichier CSV des données de navires
data <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

# Remplace les valeurs "\N" par NA dans tout le data frame
data[data == "\\N"] <- NA

# Affiche le nombre de bateaux (lignes) après chargement
cat("\nNombre de bateaux restants :", nrow(data), "\n")

# Conversion des colonnes numériques et catégorielles
cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
existing_cols <- intersect(cols_to_numeric, names(data))
data[existing_cols] <- lapply(data[existing_cols], function(x) as.numeric(as.character(x)))

# Conversion des colonnes catégorielles si elles existent
if ("VesselType" %in% names(data)) data$VesselType <- as.factor(data$VesselType)
if ("Status" %in% names(data)) data$Status <- as.factor(data$Status)
if ("TransceiverClass" %in% names(data)) data$TransceiverClass <- as.factor(data$TransceiverClass)

# Filtrage des valeurs aberrantes
data <- data[
  (is.na(data$SOG) | data$SOG <= 40) &
    (is.na(data$Heading) | (data$Heading >= 0 & data$Heading <= 359)) &
    (is.na(data$Length) | (data$Length >= 10 & data$Length <= 400)) &
    (is.na(data$Width) | (data$Width >= 3 & data$Width <= 80)) &
    (is.na(data$Draft) | data$Draft > 0) &
    (is.na(data$IMO) | nchar(as.character(data$IMO)) <= 10) &
    (is.na(data$LAT) | (data$LAT >= 20 & data$LAT <= 31)) &
    (is.na(data$LON) | (data$LON >= -98 & data$LON <= -78)) &
    !( (!is.na(data$VesselType) & data$VesselType == 60 & (is.na(data$Cargo) | data$Cargo == 0 | data$Cargo == 99)) |
         (!is.na(data$VesselType) & data$VesselType == 80 & is.na(data$Cargo)) ),
]

# Si une des colonnes Heading, COG ou SOG vaut 0, alors toutes sont mises à 0 sur la ligne
zero_idx <- (data$Heading == 0 | data$COG == 0 | data$SOG == 0)
data$Heading[zero_idx] <- 0
data$COG[zero_idx] <- 0
data$SOG[zero_idx] <- 0

# Imputation des NA par la médiane par type de navire
library(dplyr)
cols_numeric <- intersect(c("Length", "Width", "Draft", "SOG", "COG", "Heading"), names(data))

for (col in cols_numeric) {
  data <- data %>%
    group_by(VesselType) %>%
    mutate(
      !!sym(col) := ifelse(is.na(.data[[col]]), median(.data[[col]], na.rm = TRUE), .data[[col]])
    ) %>%
    ungroup()
}

# Vérification des valeurs manquantes
print(colSums(is.na(data[cols_numeric])))
print(colSums(is.na(data)))

# Nombre de bateaux après nettoyage
cat("\nNombre de bateaux restants :", nrow(data), "\n")

# Suppression des doublons (hors colonne "id")
data <- data[!duplicated(data[, setdiff(names(data), "id")]), ]
cat("\nNombre de lignes restantes après suppression des doublons :", nrow(data), "\n")

# Affiche le nombre de valeurs manquantes après nettoyage
print(colSums(is.na(data)))

# Analyse du nombre de bateaux par type
if ("MMSI" %in% names(data) & "VesselType" %in% names(data)) {
  type_counts <- data %>%
    group_by(VesselType) %>%
    summarise(n_bateaux = n_distinct(MMSI)) %>%
    arrange(desc(n_bateaux))
  print(type_counts)
}

# MMSI dont la longueur est NA (peu probable après imputation, mais au cas où)
if ("Length" %in% names(data) & "MMSI" %in% names(data)) {
  mmsi_na_length <- data$MMSI[is.na(data$Length)]
  print(mmsi_na_length)
  cat("Nombre de MMSI où Length est NA :", length(mmsi_na_length), "\n")
}

# Export vers CSV
write.csv(data, file = "vessel-clean-final.csv", row.names = FALSE)
cat("Fichier CSV nettoyé exporté sous le nom : vessel-clean-final.csv\n")
