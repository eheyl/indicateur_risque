## ---------------------------------------------------------------------------##
##   Reference vs variante alignee : performances sur donnees identiques       ##
## ---------------------------------------------------------------------------##
#
# Produit les metriques citees dans la section \ref{sec:lag} du memoire.
#
# Le bloc de performance situe a la fin de 5_bis. Random_Forest_lag4.R evalue la
# variante alignee sur df_model_lag (2000-T1 ->) et la reference sur df_model
# (1999-T1 ->). Les deux jeux test different alors, en taille comme en nombre de
# trimestres de crise, et l'ecart mesure melange l'effet de l'alignement et
# l'effet de la periode. Ce script corrige ce point : les deux modeles sont
# estimes sur les trimestres COMMUNS aux deux bases, avec le MEME tirage.
#
# A lancer apres 5_bis. Random_Forest_lag4.R, qui fournit df_zscore_rf et df_lag.

library(ranger); library(pROC); library(caret); library(dplyr)

n_trees <- 180; seed_ref <- 123; prop_train <- 0.7

# Specification des metriques : le bloc Perf du script 5_bis utilise une foret
# de probabilite ponderee (class.weights 1/5). La foret qui definit les poids
# publies est une foret de classification simple. Les deux sont calculees, la
# comparaison ne devant pas dependre de ce choix.
metriques <- function(train, test, ponderee) {
  set.seed(seed_ref)
  args <- list(formula = crise ~ ., data = train, num.trees = n_trees,
               importance = "permutation", probability = TRUE, write.forest = TRUE)
  if (ponderee) args$class.weights <- c("0" = 1, "1" = 5)
  rf   <- do.call(ranger, args)
  prob <- predict(rf, data = test)$predictions[, "1"]
  obs  <- factor(test$crise, levels = c(0, 1))
  pred <- factor(ifelse(prob > 0.5, 1, 0), levels = c(0, 1))
  cm   <- confusionMatrix(pred, obs, positive = "1")
  data.frame(exactitude  = unname(cm$overall["Accuracy"]),
             kappa       = unname(cm$overall["Kappa"]),
             sensibilite = unname(cm$byClass["Sensitivity"]),
             specificite = unname(cm$byClass["Specificity"]),
             auc = as.numeric(auc(roc(obs, prob, levels = c("0", "1"),
                                      direction = "<", quiet = TRUE))))
}

# Trimestres communs : c'est la condition "sur donnees identiques".
communs <- intersect(df_zscore_rf$TIME_PERIOD, df_lag$TIME_PERIOD)
ref <- df_zscore_rf %>% filter(TIME_PERIOD %in% communs) %>%
  arrange(TIME_PERIOD) %>% select(-TIME_PERIOD)
ali <- df_lag %>% filter(TIME_PERIOD %in% communs) %>%
  arrange(TIME_PERIOD) %>% select(-TIME_PERIOD)
stopifnot(nrow(ref) == nrow(ali), identical(ref$crise, ali$crise))

set.seed(seed_ref)
idx <- sample(c(TRUE, FALSE), nrow(ref), replace = TRUE,
              prob = c(prop_train, 1 - prop_train))
cat("Fenetre commune :", min(communs), "->", max(communs),
    "|", nrow(ref), "trimestres\n")
cat("Jeu test :", sum(!idx), "trimestres dont",
    sum(ref$crise[!idx] == "1"), "de crise\n\n")

resultats <- bind_rows(
  lapply(c(TRUE, FALSE), function(p) bind_rows(
    metriques(ref[idx, ], ref[!idx, ], p) %>%
      mutate(modele = "Référence (contemporaine)", .before = 1),
    metriques(ali[idx, ], ali[!idx, ], p) %>%
      mutate(modele = "Variante alignée", .before = 1)) %>%
    mutate(specification = ifelse(p, "pondérée 1/5 + probabilité",
                                  "classification simple"), .before = 1)))

print(resultats %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
      row.names = FALSE)
