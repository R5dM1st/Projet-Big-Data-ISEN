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
