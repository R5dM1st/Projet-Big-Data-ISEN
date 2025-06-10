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
    (data$Heading >= 0 & data$Heading <= 360 | is.na(data$Heading)) &
    (data$Length > 0 & data$Length <= 400 | is.na(data$Length)) &
    (data$Width > 0 & data$Width <= 80 | is.na(data$Width)) &
    (data$Draft > 0 | is.na(data$Draft)) &
    (data$LAT >= 20 & data$LAT <= 31 | is.na(data$LAT)) &
    (data$LON >= -98 & data$LON <= -79 | is.na(data$LON)),
]

cat("\nNombre de bateaux restants :", nrow(data), "\n")

data <- data[!duplicated(data), ]
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
