library(dplyr)
library(ggplot2)
library(ROCR)  # Pour la courbe ROC

# Chargement des données
data <- read.csv("vessel-clean-final.csv", stringsAsFactors =TRUE)

# On ne garde que deux types de bateaux
data_bin <- data %>% filter(VesselType %in% c(60,70,80))
data_bin$VesselType <- factor(data_bin$VesselType)

# Variables explicatives
vars <- c("Length", "Width", "Draft", "SOG", "COG", "Heading")
data_bin <- data_bin %>% select(VesselType, all_of(vars)) %>% na.omit()

# Split train/test
set.seed(123)
train_idx <- sample(1:nrow(data_bin), 0.7 * nrow(data_bin))
train_data <- data_bin[train_idx, ]
test_data <- data_bin[-train_idx, ]

# Régression logistique
model <- glm(VesselType ~ ., data = train_data, family = "binomial")
summary(model)

# Prédictions
probs <- predict(model, newdata = test_data, type = "response")
pred_class <- ifelse(probs > 0.5, 80, 70)
pred_class <- factor(pred_class, levels = levels(data_bin$VesselType))
true_class <- test_data$VesselType

# Matrice de confusion
conf_mat <- table(True = true_class, Predicted = pred_class)
print(conf_mat)

# ---- Affichage graphique : matrice de confusion ----
conf_df <- as.data.frame(conf_mat)
ggplot(conf_df, aes(x = True, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 8) +
  scale_fill_gradient(low = "skyblue", high = "darkred") +
  labs(title = "Matrice de confusion – Régression logistique",
       x = "Classe réelle", y = "Classe prédite") +
  theme_minimal(base_size = 16)

# ---- Affichage graphique : courbe ROC ----
pred_roc <- prediction(probs, true_class)
perf_roc <- performance(pred_roc, "tpr", "fpr")
plot(perf_roc, col = "blue", lwd = 3, main = "Courbe ROC", xlab = "FPR", ylab = "TPR")
abline(a = 0, b = 1, lty = 2, col = "gray")
auc <- performance(pred_roc, "auc")@y.values[[1]]
legend("bottomright", legend = paste("AUC =", round(auc, 3)), col = "blue", lwd = 2)
