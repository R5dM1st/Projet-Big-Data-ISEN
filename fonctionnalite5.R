library(dplyr)
library(nnet)
library(ggplot2)

data <- read.csv("vessel-clean-final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#on enlève les colonnes qui ne servent pas pour la modélisation
data_model <- subset(data, select = -c(id, MMSI, Status, BaseDateTime))

#regroupeent des soustypes en grandes classes (60, 70, 80)
data_model$VesselType <- with(data_model, ifelse(VesselType >= 60 & VesselType <= 69, 60,
                                                 ifelse(VesselType >= 70 & VesselType <= 79, 70,
                                                        ifelse(VesselType >= 80 & VesselType <= 89, 80,
                                                               as.numeric(as.character(VesselType))))))

#on garde uniquement les trois grandes classes
data_model <- data_model[data_model$VesselType %in% c(60, 70, 80), ]

cat("Nombre de navires par type :\n")
print(table(data_model$VesselType))

#suppression des lignes avec NA
data_model <- na.omit(data_model)

#création d'un échantillon train/test
set.seed(123)  # Pour la reproductibilité
train_frac <- 0.8

#melange des données
data_model <- data_model[sample(nrow(data_model)), ]

#indices pour split
train_index <- floor(nrow(data_model) * train_frac)

train_data <- data_model[1:train_index, ]
test_data <- data_model[(train_index + 1):nrow(data_model), ]

#entrainement du modèle sur train
model <- multinom(VesselType ~ Length + Width + Draft + Cargo, data = train_data)

#résumé des coefficients
summary(model)

#prédiction sur test
predictions <- predict(model, newdata = test_data)

#matrice de confusion sur test
confusion <- table(Prédit = predictions, Réel = test_data$VesselType)
cat("\nMatrice de confusion sur test :\n")
print(confusion)

#visualisation matrice de confusion
confusion_df <- as.data.frame(confusion)
colnames(confusion_df) <- c("Prévu", "Réel", "Nombre")

ggplot(confusion_df, aes(x = Réel, y = Prévu, fill = Nombre)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Nombre), color = "black", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Matrice de confusion (test)", x = "Véritable type", y = "Type prédit") +
  theme_minimal()

#calcul de la précision sur test
accuracy <- mean(predictions == test_data$VesselType)
cat("\nPrécision sur le test :", round(accuracy * 100, 2), "%\n")
