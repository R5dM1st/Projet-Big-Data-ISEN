data <- read.csv("vessel-clean-final.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
# 2. Exclure les variables non pertinentes ----------------------------------
data_clean <- subset(data, select = -c(id, Status, MMSI, Cargo))

# 3. seelection des variables numériques (y compris VesselType) -------------
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

png("matrice_correlation_quantitatives.png", width = 900, height = 800)
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
dev.off()

# 6. selection des variables qualitatives -----------------------------------
vars_qual <- c("Status", "Cargo")

# 7. analyse bivariée VesselType vs chaque variable qualitative ------------
for (var2 in vars_qual) {
  cat("\n====> Tableau croisé : VesselType vs", var2, "<====\n")
  
  # tableau croisé
  tab <- table(data$VesselType, data[[var2]])
  print(tab)
  
  # test du chi2
  test <- chisq.test(tab)
  print(test)
  
  # enregistrement du graphique en PNG
  png(paste0("mosaicplot_VesselType_vs_", var2, ".png"), width = 900, height = 700)
  mosaicplot(tab, main = paste("VesselType vs", var2),
             xlab = "VesselType", ylab = var2, color = TRUE, shade = TRUE)
  dev.off()
  
  # Interprétation simple
  if (test$p.value < 0.05) {
    cat("Relation significative (p <", formatC(test$p.value, digits = 3), ")\n")
  } else {
    cat("Pas de relation significative (p =", formatC(test$p.value, digits = 3), ")\n")
  }
}
