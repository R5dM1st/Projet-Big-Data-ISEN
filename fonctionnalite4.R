# 1. Lecture du fichier -----------------------------------------------------
data <- read.csv("vessel-clean-final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# 2. Exclure les variables non pertinentes (identifiants, texte, etc.) ------
data_clean <- subset(data, select = -c(id, Status, MMSI, Cargo))

# 3. Sélection des variables numériques (y compris VesselType) -------------
data_quant <- data_clean[ , sapply(data_clean, is.numeric) ]
summary(data_quant)

# 4. Matrice de corrélation -------------------------------------------------
mcor_data <- cor(
  data_quant,
  use = "pairwise.complete.obs",
  method = "pearson"
)

# 5. Affichage avec corrplot ------------------------------------------------
# install.packages("corrplot")  # si besoin
library(corrplot)

par(mar = c(1, 1, 1, 1))
corrplot(
  mcor_data,
  method = "color",
  order = "hclust",
  col = colorRampPalette(c("blue", "white", "red"))(200),
  addCoef.col = "black",
  diag = FALSE,
  tl.col = "black",
  tl.srt = 45
)
# 6. Sélection des variables qualitatives -----------------------------------
vars_qual <- c("Status", "Cargo")

# 7. Analyse bivariée VesselType vs chaque variable qualitative EN PNG -------
for (var2 in vars_qual) {
  cat("\n====> Tableau croisé : VesselType vs", var2, "<====\n")
  
  # Tableau croisé
  tab <- table(data$VesselType, data[[var2]])
  print(tab)
  
  # Test du chi2
  test <- chisq.test(tab)
  print(test)
  
  # Mosaicplot ENREGISTRÉ EN PNG
  png(paste0("mosaicplot_VesselType_vs_", var2, ".png"), width=900, height=700)
  mosaicplot(tab, main = paste("VesselType vs", var2),
             xlab = "VesselType", ylab = var2, color = TRUE, shade = TRUE)
  dev.off()
  
  # Interprétation simple
  if (test$p.value < 0.05) {
    cat("Relation significative (p <", formatC(test$p.value, digits=3), ")\n")
  } else {
    cat("Pas de relation significative (p =", formatC(test$p.value, digits=3), ")\n")
  }
}