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
# 1. Sélection des colonnes qualitatives
cat_vars <- names(data)[sapply(data, is.factor)]
cat_vars

# 2. Boucle sur toutes les paires de variables qualitatives
for (i in 1:(length(cat_vars)-1)) {
  for (j in (i+1):length(cat_vars)) {
    
    var1 <- cat_vars[i]
    var2 <- cat_vars[j]
    
    cat("\n\n========================")
    cat("\nAnalyse : ", var1, "vs", var2, "\n")
    
    # Sous-ensemble sans NA pour ces deux variables
    df_sub <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), ]
    
    # Vérifie qu’il reste assez de données
    if (nrow(df_sub) == 0) {
      cat(" --> Données insuffisantes (que des NA)\n")
      next
    }
    
    # Table croisée
    tab <- table(df_sub[[var1]], df_sub[[var2]])
    
    # Si la table est vide ou trop déséquilibrée
    if (min(dim(tab)) < 2) {
      cat(" --> Trop peu de modalités pour", var1, "ou", var2, "\n")
      next
    }
    
    # Test du Chi2
    chi <- chisq.test(tab)
    print(chi)
    
    # Mosaicplot
    mosaicplot(
      tab,
      main = paste("Mosaicplot :", var1, "vs", var2),
      col = rainbow(nrow(tab)),
      shade = TRUE,
      las = 1,
      cex.axis = 0.8
    )
  }
}
# 2. Sélection des variables qualitatives
vars_qual <- c("VesselType", "Status", "TransceiverClass", "Cargo")

# 3. Boucle sur toutes les paires qualitatives
for (i in 1:(length(vars_qual)-1)) {
  for (j in (i+1):length(vars_qual)) {
    var1 <- vars_qual[i]
    var2 <- vars_qual[j]
    cat("\n====> Tableau croisé :", var1, "vs", var2, "<====\n")
    # Tableau croisé
    tab <- table(data[[var1]], data[[var2]])
    print(tab)
    # Test du chi2
    test <- chisq.test(tab)
    print(test)
    # Mosaicplot (affichage interactif)
    mosaicplot(tab, main = paste(var1, "vs", var2),
               xlab = var1, ylab = var2, color = TRUE, shade = TRUE)
    # Interprétation simple
    if (test$p.value < 0.05) {
      cat("Relation significative (p <", formatC(test$p.value, digits=3), ")\n")
    } else {
      cat("Pas de relation significative (p =", formatC(test$p.value, digits=3), ")\n")
    }
    readline(prompt="Appuie sur [entrée] pour continuer...")
  }
}
