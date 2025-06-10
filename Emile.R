getwd()
data <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data[data == "\\N"] <- NA
cat("\nNombre de bateaux restants :", nrow(data), "\n")
cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
data[cols_to_numeric] <- lapply(data[cols_to_numeric], as.numeric)
data$VesselType <- as.factor(data$VesselType)
data$Status <- as.factor(data$Status)
data$TransceiverClass <- as.factor(data$TransceiverClass)

# Valeurs manquantes
print(colSums(is.na(data)))

# Suppression des valeurs aberrantes
data <- data[
  (data$SOG <= 40 | is.na(data$SOG)) &
    (data$Heading >= 0 & data$Heading <= 359 | is.na(data$Heading)) &
    (data$Length >= 10 & data$Length <= 400 | is.na(data$Length)) &
    (data$Width >= 3 & data$Width <= 80 | is.na(data$Width)) &
    (data$Draft > 0 | is.na(data$Draft)) &
    (is.na(data$IMO) | nchar(as.character(data$IMO)) <= 10) &
    (data$LAT >= 20 & data$LAT <= 30 | is.na(data$LAT)) &
    (data$LON >= -98 & data$LON <= -78 | is.na(data$LON)) &
    !( (data$VesselType == 60 & (is.na(data$Cargo) | data$Cargo == 0 | data$Cargo == 99)) |
         (data$VesselType == 80 & is.na(data$Cargo)) ),
]

cat("\nNombre de bateaux restants :", nrow(data), "\n")

data <- data[!duplicated(data[, setdiff(names(data), "id")]), ]
cat("\nNombre de lignes restantes après suppression des doublons :", nrow(data), "\n")


#remplacer les NAN par des moyennes ou des bateaux existent 
print(colSums(is.na(data)))
library(dplyr)

# Comptage du nombre de bateaux uniques (MMSI) par type de bateau
type_counts <- data %>%
  group_by(VesselType) %>%
  summarise(n_bateaux = n_distinct(MMSI)) %>%
  arrange(desc(n_bateaux))
#caca
print(type_counts)

mmsi_na_length <- data$MMSI[is.na(data$Length)]
print(mmsi_na_length)
cat("Nombre de MMSI où Length est NA :", length(mmsi_na_length), "\n")

