getwd()
data <- read.csv("vessel-total-clean.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data[data == "\\N"] <- NA

# 4. Conversion des colonnes numériques (si elles contiennent des NA)
cols_to_numeric <- c("Length", "Width", "Draft", "SOG", "COG", "Heading", "LAT", "LON")
data[cols_to_numeric] <- lapply(data[cols_to_numeric], as.numeric)

# 5. Conversion de certaines colonnes en facteur
data$VesselType <- as.factor(data$VesselType)
data$Status <- as.factor(data$Status)
data$TransceiverClass <- as.factor(data$TransceiverClass)

# 6. Nettoyage des doublons
data <- distinct(data)

# 7. Export dans un fichier Excel propre
write_xlsx(data, path = "vessel-total-clean-nettoye.xlsx")
