#Essayer de faire des prévisions avec les variables laggées

## ----------------------------------------------------------- ##
##  Random Forest sur données laggées de 4 trimestres          ##
##  Objectif : prévision de la crise à horizon 1 an            ##
## ----------------------------------------------------------- ##

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/0bis. Setup.R")
# source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/1. Construction_data.R")

load("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/data_long.RData")
# Transformation type -------------------------------------------------------
#data (liste) en df_final (dataframe)

# Fonction
`%||%` <- function(a, b) if (!is.null(a)) a else b

flatten_data <- function(x, parent_name = NULL) {
  if (inherits(x, c("data.frame", "tbl_df"))) {
    nm <- parent_name %||% "var"
    df <- x %>%
      rename(!!nm := OBS_VALUE)
    return(list(df))
  }
  
  if (is.list(x)) {
    out <- imap(x, function(value, name) {
      new_parent <- if (is.null(parent_name)) name else paste(parent_name, name, sep = "_")
      flatten_data(value, new_parent)
    })
    return(unlist(out, recursive = FALSE))
  }
  
  list()
}

flat_list <- flatten_data(data)

flat_list_ok <- flat_list |>
  keep(~ "TIME_PERIOD" %in% names(.x))

flat_list_ok <- flat_list_ok |>
  map(\(df) {
    df |>
      group_by(TIME_PERIOD) |>
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop")
  })

df_final <- reduce(flat_list_ok, full_join, by = "TIME_PERIOD") |>
  relocate(TIME_PERIOD) |>
  arrange(TIME_PERIOD)

df_final_filtre <- df_final %>%
  filter(TIME_PERIOD >= "1999-Q1",
         TIME_PERIOD != "NA-QNA") %>%
  arrange(TIME_PERIOD)

trimestres_incomplets <- df_final_filtre %>%
  filter(if_any(-TIME_PERIOD, is.na)) %>%
  pull(TIME_PERIOD)

cols_donnees <- setdiff(names(df_final), "TIME_PERIOD")
df_zscore <- df_final
df_zscore[cols_donnees] <- lapply(df_zscore[cols_donnees], function(x) as.numeric(scale(x)))

# Standardisation -------------------------------------------------------------
df_zscore_rf <- df_zscore %>% filter(TIME_PERIOD >= "1999-Q1" & TIME_PERIOD != "NA-QNA") 

cols_donnees <- setdiff(names(df_final), "TIME_PERIOD")
df_zscore <- df_final
df_zscore[cols_donnees] <- lapply(df_zscore[cols_donnees], function(x) as.numeric(scale(x)))

df_esrb <- read_excel("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/esrb.fcdb20220120.en.xlsx") %>% 
  filter(Country == "FR")
df_syst <- read_excel("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/esrb.fcdb20220120.en.xlsx", sheet = "Residual events") %>% 
  filter(Country == "FR") 
colnames(df_esrb)[5] <- "System back \"normal\" date"
df_crisis <- rbind(df_syst,df_esrb)
df_crisis$Event <- seq_len(nrow(df_crisis))
df_crisis <- df_crisis %>% select(`Start date`,`End of crisis management date`)

df_crisis <- df_crisis %>%
  rename(
    start_date    = `Start date`,
    end_date = `End of crisis management date`
  ) %>%
  mutate(
    start_date    = ifelse(start_date    == "n.a.", NA, start_date),
    end_date = ifelse(end_date == "n.a.", NA, end_date)
  )

df_crisis2 <- df_crisis %>%
  mutate(
    start_q = as.yearqtr(as.yearmon(start_date)),
    end_q   = as.yearqtr(as.yearmon(end_date))
  )

crisis_periods <- c() 

for (i in 1:nrow(df_crisis2)) {
  
  start_i <- df_crisis2$start_q[i]
  end_i   <- df_crisis2$end_q[i]
  
  q_seq <- seq(start_i, end_i, by = 0.25)
  
  q_seq <- format(q_seq, "%Y-Q%q")
  
  crisis_periods <- c(crisis_periods, q_seq)
}

crisis_periods <- unique(crisis_periods)
# 1 crise (tout au long de la crise), 0 pas de crise 
df_zscore_rf$crise <- ifelse(df_zscore_rf$TIME_PERIOD %in% crisis_periods, 1, 0)
df_zscore_rf$crise <- factor(df_zscore_rf$crise, levels = c(0,1))

df_model <- df_zscore_rf %>%
  select(-TIME_PERIOD) 

# 1. Construction de la base laggée -----------------------------------------

# Nombre de trimestres manquants EN FIN de série, variable par variable.
# On ignore volontairement les NA de début de période : on ne compte que
# les NA situés APRÈS la dernière observation disponible.

vars_X <- setdiff(names(df_zscore_rf), c("TIME_PERIOD", "crise", 
                                         "encours_dette_BSI1_TC_creditSNF","financement_credit_BLS_Menages", 
                                         "financement_credit_BLS_Entreprises","financement_marche_SPREADHY",
                                         "financement_marche_SPREAD_OATBD",
                                         "marche_surrevaluation_CDS"))

count_trailing_na <- function(x) {
  obs <- which(!is.na(x))
  if (length(obs) == 0L) return(0L)   # série entièrement vide -> aucun lag
  length(x) - max(obs)
}

# "Fin de série" n'a de sens que sur des données triées par date
df_zscore_rf <- df_zscore_rf %>% arrange(TIME_PERIOD)

# Vecteur nommé : nb de lags retenu pour chaque variable explicative
lag_counts <- sapply(df_zscore_rf[vars_X], count_trailing_na)

# Contrôle (à inspecter : aucun ne devrait dépasser 3)
print(data.frame(var = names(lag_counts), n_lag = as.integer(lag_counts)))

# Lag spécifique à chaque variable = son nombre de trimestres manquants en fin
df_lag <- df_zscore_rf %>%
  mutate(across(all_of(vars_X),
                ~ dplyr::lag(.x, n = lag_counts[[dplyr::cur_column()]]))) %>%
  filter(TIME_PERIOD >= "2000-Q1")

# On retire TIME_PERIOD pour le RF
df_model_lag <- df_lag %>% select(-TIME_PERIOD)

# 2. Split train / test -----------------------------------------------------
set.seed(123)
sample_idx <- sample(c(TRUE, FALSE), nrow(df_model_lag),
                     replace = TRUE, prob = c(0.7, 0.3))
train_lag <- df_model_lag[sample_idx, ]
test_lag  <- df_model_lag[!sample_idx, ]

cat("Train :", nrow(train_lag), "obs |",
    sum(train_lag$crise == 1), "crises\n")
cat("Test  :", nrow(test_lag),  "obs |",
    sum(test_lag$crise == 1),  "crises\n")


# 3. Estimation du Random Forest --------------------------------------------
set.seed(123)
rf_lag <- ranger(
  formula      = crise ~ .,
  data         = train_lag,
  num.trees    = 180,
  importance   = "permutation",
  write.forest = TRUE
)

print(rf_lag)

# Performance hors échantillon
pred_test <- predict(rf_lag, data = test_lag)$predictions
cat("\nMatrice de confusion (test) :\n")
print(table(observe = test_lag$crise, predit = pred_test))

# 4. Extraction des poids ---------------------------------------------------
imp_df_lag <- data.frame(
  var = names(rf_lag$variable.importance),
  imp = rf_lag$variable.importance
) %>%
  mutate(imp_pos = pmax(imp, 0)) %>%
  mutate(weight  = imp_pos / sum(imp_pos)) %>%
  arrange(desc(weight))

print(imp_df_lag)


# 5. Export des poids laggés ------------------------------------------------
# A executer une seule fois pour ne pas ecraser
# write_xlsx(
#   imp_df_lag,
#   path = "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Poids_variables_lag4.xlsx"
# )


# 6. Construction de l'indicateur à horizon 4 trimestres --------------------
# Les poids sont appliques aux valeurs CONTEMPORAINES de df_zscore.
# Lecture de l'indicateur : valeur a la date t = probabilite signal de crise 
# dans 4 trimestres (puisque les poids sont calibres sur cette relation).

w_lag <- setNames(imp_df_lag$weight, imp_df_lag$var)

# Verification de coherence des noms
# stopifnot(identical(colnames(df_zscore[, imp_df_lag$var]), imp_df_lag$var))
# stopifnot(identical(names(w_lag[imp_df_lag$var]), imp_df_lag$var))
df_lag$crise <- NULL 

df_weighted_lag <- df_lag %>%
  mutate(indicator_lagged = as.numeric(
    as.matrix(df_lag[, imp_df_lag$var]) %*% w_lag[imp_df_lag$var]
  ))

df_weighted_lag$crise <- NULL

# df_weighted_lag <- df_lag %>% 
#   mutate(indicator_lagged = as.numeric(
#     as.matrix(df_lag[, imp_df_lag$var]) %*% w_lag[imp_df_lag$var]
#   ))

# df_weighted_lag$crise <- NULL

# --- 1. Chargement des bibliothèques ---
library(ranger)
library(pROC)
library(caret)
library(dplyr)

# --- 2. Préparation des données (Split) ---
set.seed(123)
sample_idx <- sample(c(TRUE, FALSE), nrow(df_model_lag),
                     replace = TRUE, prob = c(0.7, 0.3))
train <- df_model_lag[sample_idx, ]
test  <- df_model_lag[!sample_idx, ]

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
plot(roc_obj, main = "Courbe ROC - Random Forest (Pondéré)", col = "blue", lwd = 2)
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