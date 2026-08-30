# --- 1. Chargement des bibliothèques ---
library(ranger)
library(pROC)
library(caret)
library(dplyr)

# --- 2. Préparation des données (Split) ---
set.seed(123)
sample_vec <- sample(c(TRUE, FALSE), nrow(df_model), replace = TRUE, prob = c(0.7, 0.3))
train      <- df_model[sample_vec, ]
test       <- df_model[!sample_vec, ]

# --- 3. Entraînement du modèle Random Forest ---
# On utilise la version pondérée et probabiliste pour optimiser les performances
set.seed(123)
rf_final <- ranger(
  formula         = crise ~ .,
  data            = train,
  num.trees       = 180,
  importance      = "permutation",
  probability     = TRUE,               # Nécessaire pour l'AUC-ROC
  class.weights   = c("0" = 1, "1" = 5), # Ajustement du déséquilibre
  write.forest    = TRUE
)

# --- 4. Prédictions sur l'échantillon test ---
# Obtention des probabilités (classe 1)
pred_prob <- predict(rf_final, data = test)$predictions[, "1"]

# Transformation en classes (seuil par défaut à 0.5)
pred_class <- as.factor(ifelse(pred_prob > 0.5, 1, 0))
# On s'assure que les facteurs ont les mêmes niveaux pour la matrice de confusion
test$crise <- as.factor(test$crise)

# --- 5. Évaluation des performances ---
# Matrice de confusion détaillée
cat("\n--- Matrice de Confusion ---\n")
print(confusionMatrix(pred_class, test$crise, positive = "1"))

# Courbe ROC et AUC
roc_obj <- roc(test$crise, pred_prob, levels = c("0", "1"), direction = "<")
plot(roc_obj, main = "Courbe ROC - Random Forest", col = "blue", lwd = 2)
cat("\nAUC-ROC final:", round(auc(roc_obj), 3), "\n")

# --- 6. Importance des variables ---
importance_df <- data.frame(
  variable = names(rf_final$variable.importance),
  importance = rf_final$variable.importance
) %>%
  mutate(importance_norm = pmax(importance, 0) / sum(pmax(importance, 0))) %>%
  arrange(desc(importance_norm))

cat("\n--- Importance des variables (Top 10) ---\n")
print(head(importance_df, 10))