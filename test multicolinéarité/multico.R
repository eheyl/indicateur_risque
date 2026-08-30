library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(purrr)
library(stringr)
library(Polychrome)
library(ggplot2)
library(plm)
library(car)
library(reshape2)


path <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/test multicolinéarité/Donnees_RFW_ext.xlsx"

data <- read_excel(path, sheet = "Donnees")
data$`Spread HY` <- data$`Spread HY` - data$`Spread IG`
colnames(data)

library(performance)

lm_ <- lm(`Dette Ménages` ~   `Dette SNF` + `DSR Ménages`+  `DSR SNF` + `Taux crédits nouveaux ménages`  +  `Taux crédits nouveaux SNF`  
          +  `Durcissement conditions d'octroi - SNF`    +`Durcissement conditions d'octroi - Ménages` +`Part SNF avec contraintes de crédit`
          +`Surrévaluation prix immo`
          + `Prix immo / revenus ménages`   +  `Spread HY`  + `Spread IG`  +
            + `Spread OAT-Bund`  +     `Taux OAT 10Y` +   `Prime de CDS moyen` + `Price Earning Ratio`  , data = data)
check_collinearity(lm_)

correlation_matrix <- cor(data)
melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "") + # Remove axis labels
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  theme(axis.text=element_text(size=15))

# Extraire les paires avec une corrélation > 0.8 (en valeur absolue)
high_corr_pairs <- which(abs(correlation_matrix) > 0.8 & correlation_matrix < 1, arr.ind = TRUE)
# Filtrer pour éviter les doublons (ex: (A,B) et (B,A))
high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], ]
# Ajouter les noms des variables et les valeurs de corrélation
high_corr_pairs_df <- data.frame(
  Var1 = colnames(data)[high_corr_pairs[, 1]],
  Var2 = colnames(data)[high_corr_pairs[, 2]],
  Correlation = correlation_matrix[high_corr_pairs]
)
# Afficher les paires
print(high_corr_pairs_df)

#On retire IG ici
lm_ <- lm(`Dette Ménages` ~   `Dette SNF` + `DSR Ménages`+  `DSR SNF` + `Taux crédits nouveaux ménages`  +  `Taux crédits nouveaux SNF`  
          +  `Durcissement conditions d'octroi - SNF`    +`Durcissement conditions d'octroi - Ménages` +`Part SNF avec contraintes de crédit`
          +`Surrévaluation prix immo`
          + `Prix immo / revenus ménages`   +  `Spread HY`  + 
            + `Spread OAT-Bund`  +     `Taux OAT 10Y` +   `Prime de CDS moyen` + `Price Earning Ratio`  , data = data)
check_collinearity(lm_)

#On retire OAT en plus
lm_ <- lm(`Dette Ménages` ~   `Dette SNF` + `DSR Ménages`+  `DSR SNF` + `Taux crédits nouveaux ménages`  +  `Taux crédits nouveaux SNF`  
          +  `Durcissement conditions d'octroi - SNF`    +`Durcissement conditions d'octroi - Ménages` +`Part SNF avec contraintes de crédit`
          +`Surrévaluation prix immo`
          + `Prix immo / revenus ménages`   +  `Spread HY`  + 
            + `Spread OAT-Bund`  +   `Prime de CDS moyen` + `Price Earning Ratio`  , data = data)
check_collinearity(lm_)

data2 <- subset(data, select = -c(`Taux OAT 10Y`, `Spread IG`))
correlation_matrix <- cor(data2)
melted_corr_matrix <- melt(correlation_matrix)
ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() + # Minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "") + # Remove axis labels
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  theme(axis.text=element_text(size=15))

# Extraire les paires avec une corrélation > 0.8 (en valeur absolue)
high_corr_pairs <- which(abs(correlation_matrix) > 0.8 & correlation_matrix < 1, arr.ind = TRUE)
# Filtrer pour éviter les doublons (ex: (A,B) et (B,A))
high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], ]
# Ajouter les noms des variables et les valeurs de corrélation
high_corr_pairs_df <- data.frame(
  Var1 = colnames(data)[high_corr_pairs[, 1]],
  Var2 = colnames(data)[high_corr_pairs[, 2]],
  Correlation = correlation_matrix[high_corr_pairs]
)
# Afficher les paires
print(high_corr_pairs_df)

# colnames(data)
# 
# 
# `Dette Ménages`   +   `Dette SNF` + `DSR Ménages`+  `DSR SNF` + `Taux crédits nouveaux ménages`  +  `Taux crédits nouveaux SNF`                  `Durcissement conditions d'octroi - SNF`    +`Durcissement conditions d'octroi - Ménages` `Part SNF avec contraintes de crédit`        `Surrévaluation prix immo`
# + `Prix immo / revenus ménages`   +  `Spread HY`  + `Spread IG`  +
#   + `Spread OAT-Bund`  +     `Taux OAT 10Y` +   `Prime de CDS moyen` + `Price Earning Ratio`

