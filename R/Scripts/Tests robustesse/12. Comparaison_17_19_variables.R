## ---------------------------------------------------------------------------##
##   Comparaison des modeles a 17 et a 19 variables, sur donnees identiques    ##
## ---------------------------------------------------------------------------##
#
# Objet : chiffrer ce que le retrait du spread investment grade et du taux de
# l'OAT a dix ans change, pour completer la section 2.2 du memoire
# ("Effet sur la ponderation et sur la performance"), dont deux passages sont
# encore entre crochets :
#   - les ponderations du modele a dix-neuf variables, pour chiffrer le partage
#     de l'importance entre le spread High Yield et le spread investment grade,
#     et entre le taux de l'OAT et les taux de credit ;
#   - les metriques des deux modeles, "a verifier sur donnees identiques".
#
# La comparaison est menee sur donnees identiques au sens strict :
#   - meme fenetre temporelle, celle ou les dix-neuf series sont disponibles ;
#   - memes trimestres, donc meme cible et meme nombre d'observations ;
#   - meme tirage apprentissage / test, tire une fois et applique aux deux ;
#   - memes hyperparametres, et notamment le meme mtry (floor(sqrt(17)) et
#     floor(sqrt(19)) valent tous deux 4, la comparaison n'est donc pas
#     confondue par un tirage de variables different a chaque noeud).
#
# Un seul tirage 70/30 ne laisse qu'une trentaine de trimestres de test, dont
# une poignee de crise. Le script complete donc le tirage de reference par une
# distribution sur N tirages independants (section 6), seule lecture honnete de
# l'ecart de performance entre les deux modeles.
#
# A lancer apres 5. Random_Forest.R, qui fournit df_final, df_zscore, df_model
# et imp_df. Sorties : Comparaison_17_19.xlsx et deux tabulars LaTeX.

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/5. Random_Forest.R")

library(ranger)
library(pROC)
library(caret)
library(openxlsx)

# ---------------------------------------------------------------------------
# 1. Parametres
# ---------------------------------------------------------------------------
base_projet   <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB"
chemin_hy_ig  <- file.path(base_projet, "R/Donnees/HY_IG.xlsx")
chemin_bloom  <- file.path(base_projet, "R/Donnees/Bloomberg_data_v2.xlsx")
chemin_export <- file.path(base_projet, "R/Résultats/Comparaison_17_19.xlsx")
dossier_tex   <- file.path(base_projet, "R/Résultats/LaTeX")
dir.create(dossier_tex, showWarnings = FALSE, recursive = TRUE)

n_trees      <- 180
seed_ref     <- 123
prop_train   <- 0.7
n_repetitions <- 200      # tirages independants pour la distribution (section 6)
debut_periode <- "1999-Q1"
fin_periode   <- "2025-Q3"

# ---------------------------------------------------------------------------
# 2. Reconstruire les deux series ecartees
# ---------------------------------------------------------------------------
# Elles sont commentees dans 1. Construction_data.R depuis leur retrait. On les
# relit ici a l'identique, avec la meme conversion trimestrielle que celle
# appliquee au groupe financement_marche (annee-Qtrimestre, puis moyenne par
# trimestre dans 3. Standardisation.R).
en_trimestres <- function(df) {
  df %>%
    mutate(TIME_PERIOD = paste0(year(as.Date(TIME_PERIOD)), "-Q",
                                quarter(as.Date(TIME_PERIOD)))) %>%
    group_by(TIME_PERIOD) %>%
    summarise(OBS_VALUE = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")
}

# Spread investment grade : HY_IG.xlsx, colonnes 1 et 2 (le HY est en colonne 3)
IG <- read_xlsx(chemin_hy_ig, sheet = "Données", skip = 7, col_names = FALSE)[, 1:2]
colnames(IG) <- c("TIME_PERIOD", "OBS_VALUE")
IG <- IG %>%
  mutate(TIME_PERIOD = as.Date(as.numeric(TIME_PERIOD), origin = "1899-12-30"),
         OBS_VALUE   = as.numeric(OBS_VALUE)) %>%
  en_trimestres() %>%
  rename(financement_marche_SPREADIG = OBS_VALUE)

# Taux de l'OAT a dix ans : Bloomberg_data_v2.xlsx, colonne OAT10Y
OAT <- read_xlsx(chemin_bloom, sheet = "Données") %>%
  select(TIME_PERIOD, OAT10Y) %>%
  mutate(TIME_PERIOD = as.Date(as.numeric(TIME_PERIOD), origin = "1899-12-30"),
         OBS_VALUE   = as.numeric(OAT10Y)) %>%
  select(TIME_PERIOD, OBS_VALUE) %>%
  en_trimestres() %>%
  rename(financement_marche_OAT10Y = OBS_VALUE)

# ---------------------------------------------------------------------------
# 3. Standardiser les dix-neuf series exactement comme les dix-sept
# ---------------------------------------------------------------------------
# Meme traitement que 3. Standardisation.R : z-score sur l'historique complet
# (scale() ignore les NA), puis report de la derniere valeur connue APRES le
# z-score. Les deux series ajoutees subissent donc le meme traitement que les
# autres, et les z-scores des dix-sept variables communes sont inchanges.
df_final_19 <- df_final %>%
  left_join(IG,  by = "TIME_PERIOD") %>%
  left_join(OAT, by = "TIME_PERIOD")

cols_19    <- setdiff(names(df_final_19), "TIME_PERIOD")
df_zscore_19 <- df_final_19
df_zscore_19[cols_19] <- lapply(df_zscore_19[cols_19], function(x) as.numeric(scale(x)))
df_zscore_19 <- df_zscore_19 %>% fill(-TIME_PERIOD, .direction = "down")

# ---------------------------------------------------------------------------
# 4. Fenetre commune et cible
# ---------------------------------------------------------------------------
# Les deux series ajoutees peuvent demarrer plus tard que les dix-sept autres.
# On restreint alors LES DEUX modeles a la fenetre ou les dix-neuf sont
# disponibles : c'est la condition "sur donnees identiques".
vars_17 <- setdiff(names(df_zscore), "TIME_PERIOD")
vars_19 <- c(vars_17, "financement_marche_SPREADIG", "financement_marche_OAT10Y")

df_19 <- df_zscore_19 %>%
  filter(TIME_PERIOD >= debut_periode, TIME_PERIOD <= fin_periode) %>%
  filter(if_all(all_of(vars_19), ~ !is.na(.)))

fenetre <- range(df_19$TIME_PERIOD)
cat("Fenetre commune aux 19 series :", fenetre[1], "->", fenetre[2],
    "(", nrow(df_19), "trimestres )\n")
if (nrow(df_19) < nrow(df_zscore_rf))
  cat("  Note : la reference a 17 variables porte sur", nrow(df_zscore_rf),
      "trimestres. Le modele a 17 variables est ici re-estime sur la fenetre\n",
      "  commune, afin que la comparaison ne melange pas effet des variables et\n",
      "  effet de la periode.\n")

# Cible : reprise de la colonne crise deja construite dans 5. Data_pour_RF.R
df_19$crise <- df_zscore_rf$crise[match(df_19$TIME_PERIOD, df_zscore_rf$TIME_PERIOD)]
stopifnot(!anyNA(df_19$crise))

df_model_19 <- df_19 %>% select(all_of(vars_19), crise)
df_model_17 <- df_19 %>% select(all_of(vars_17), crise)   # memes lignes exactement
stopifnot(nrow(df_model_17) == nrow(df_model_19))

# ---------------------------------------------------------------------------
# 5. Estimation et performances
# ---------------------------------------------------------------------------
# Reprend les conventions de 9bis. Robustesse_splits_simple.R : une foret de
# classification pour les poids (importance par permutation), une foret de
# probabilite sur le meme echantillon pour l'AUC.
perf_split <- function(rf, rf_prob, test) {
  prob <- predict(rf_prob, data = test)$predictions[, "1"]
  obs  <- factor(test$crise, levels = c("0", "1"))
  pred <- factor(predict(rf, data = test)$predictions, levels = c("0", "1"))
  cm   <- tryCatch(confusionMatrix(pred, obs, positive = "1"), error = function(e) NULL)
  tibble(
    n_train      = rf$num.samples,
    n_test       = nrow(test),
    n_crise_test = sum(obs == "1"),
    accuracy     = if (is.null(cm)) NA_real_ else unname(cm$overall["Accuracy"]),
    kappa        = if (is.null(cm)) NA_real_ else unname(cm$overall["Kappa"]),
    sensibilite  = if (is.null(cm)) NA_real_ else unname(cm$byClass["Sensitivity"]),
    specificite  = if (is.null(cm)) NA_real_ else unname(cm$byClass["Specificity"]),
    auc          = tryCatch(as.numeric(auc(roc(obs, prob, levels = c("0", "1"),
                                               direction = "<", quiet = TRUE))),
                            error = function(e) NA_real_),
    oob_error    = rf$prediction.error)
}

estimer <- function(df_model, idx, seed = seed_ref) {
  train <- df_model[idx, ]; test <- df_model[-idx, ]
  set.seed(seed)
  rf <- ranger(crise ~ ., data = train, num.trees = n_trees,
               importance = "permutation", write.forest = TRUE)
  set.seed(seed)
  rf_prob <- ranger(crise ~ ., data = train, num.trees = n_trees,
                    probability = TRUE, write.forest = TRUE)
  poids <- tibble(var    = names(rf$variable.importance),
                  imp    = rf$variable.importance,
                  weight = pmax(rf$variable.importance, 0) /
                           sum(pmax(rf$variable.importance, 0))) %>%
    arrange(desc(weight))
  list(rf = rf, poids = poids, perf = perf_split(rf, rf_prob, test))
}

# Un seul tirage, partage par les deux modeles : c'est le point cle.
set.seed(seed_ref)
idx_ref <- which(sample(c(TRUE, FALSE), nrow(df_model_19), replace = TRUE,
                        prob = c(prop_train, 1 - prop_train)))
cat("mtry par defaut :", floor(sqrt(length(vars_17))), "(17 var) /",
    floor(sqrt(length(vars_19))), "(19 var)\n")

res_17 <- estimer(df_model_17, idx_ref)
res_19 <- estimer(df_model_19, idx_ref)

perf_ref <- bind_rows(
  res_19$perf %>% mutate(modele = "19 variables", .before = 1),
  res_17$perf %>% mutate(modele = "17 variables (retenu)", .before = 1))
print(perf_ref)

# ---------------------------------------------------------------------------
# 6. Distribution sur N tirages independants
# ---------------------------------------------------------------------------
# Le jeu test du tirage de reference ne compte qu'une poignee de trimestres de
# crise : l'ecart de performance entre les deux modeles sur ce seul tirage n'est
# pas interpretable. On repete le tirage, chaque replication etant partagee par
# les deux modeles (donnees et split identiques a chaque fois).
graines <- seq_len(n_repetitions)
distribution <- map_dfr(graines, function(g) {
  set.seed(1000 + g)
  idx <- which(sample(c(TRUE, FALSE), nrow(df_model_19), replace = TRUE,
                      prob = c(prop_train, 1 - prop_train)))
  if (length(idx) == 0 || length(idx) == nrow(df_model_19)) return(NULL)
  test_crises <- sum(df_model_19$crise[-idx] == "1")
  if (test_crises == 0) return(NULL)          # AUC indefinie
  bind_rows(
    estimer(df_model_19, idx, seed = 1000 + g)$perf %>% mutate(modele = "19 variables"),
    estimer(df_model_17, idx, seed = 1000 + g)$perf %>% mutate(modele = "17 variables (retenu)")
  ) %>% mutate(tirage = g)
})

resume_distribution <- distribution %>%
  group_by(modele) %>%
  summarise(n_tirages = n(),
            across(c(accuracy, kappa, sensibilite, specificite, auc),
                   list(moyenne = ~ mean(.x, na.rm = TRUE),
                        ecart_type = ~ sd(.x, na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

# Ecarts apparies : le meme tirage sert aux deux modeles, on peut donc
# differencier tirage par tirage plutot que comparer deux moyennes.
ecarts_apparies <- distribution %>%
  select(tirage, modele, accuracy, kappa, sensibilite, specificite, auc) %>%
  pivot_longer(c(accuracy, kappa, sensibilite, specificite, auc),
               names_to = "metrique") %>%
  pivot_wider(names_from = modele, values_from = value) %>%
  mutate(ecart = `17 variables (retenu)` - `19 variables`) %>%
  group_by(metrique) %>%
  summarise(ecart_moyen  = mean(ecart, na.rm = TRUE),
            ecart_median = median(ecart, na.rm = TRUE),
            ecart_type   = sd(ecart, na.rm = TRUE),
            part_favorable_17 = mean(ecart > 0, na.rm = TRUE),
            part_ex_aequo     = mean(ecart == 0, na.rm = TRUE),
            # les ex aequo sont frequents sur une sensibilite qui sature a 1 :
            # le test signe de Wilcoxon les gere, mais renonce alors a la
            # p-valeur exacte, d'ou l'avertissement que l'on neutralise ici.
            p_value_wilcoxon = tryCatch(
              suppressWarnings(wilcox.test(ecart, mu = 0)$p.value),
              error = function(e) NA_real_),
            .groups = "drop")
print(ecarts_apparies)

# ---------------------------------------------------------------------------
# 7. Ponderations : le partage de l'importance
# ---------------------------------------------------------------------------
libelles <- c(
  encours_dette_CNFSI_DetteH       = "Dette ménages",
  encours_dette_CNFSI_DetteSNF     = "Dette SNF",
  encours_dette_BSI1_TC_creditH    = "Croissance des crédits ménages",
  encours_dette_BSI1_TC_creditSNF  = "Croissance des crédits SNF",
  encours_dette_DSR_DSRH           = "Service de la dette ménages",
  encours_dette_DSR_DSRSNF         = "Service de la dette SNF",
  financement_credit_MIR_menages   = "Taux crédits nouveaux ménages",
  financement_credit_MIR_snf       = "Taux crédits nouveaux SNF",
  financement_credit_BLS_Entreprises = "Durcissement critères d'octroi SNF",
  financement_credit_BLS_Menages   = "Durcissement critères d'octroi ménages",
  financement_credit_SAFE          = "Contraintes de crédit SNF (SAFE)",
  marche_immo_ISPI                 = "Surévaluation prix immobiliers",
  marche_immo_OCDE                 = "Prix immobiliers / revenus",
  financement_marche_SPREADHY      = "Spread HY",
  financement_marche_SPREAD_OATBD  = "Spread OAT-Bund",
  financement_marche_SPREADIG      = "Spread IG",
  financement_marche_OAT10Y        = "Taux OAT 10 ans",
  marche_surrevaluation_CDS        = "Prime de CDS",
  marche_surrevaluation_PER        = "Price earning ratio")

comparaison_poids <- full_join(
  res_19$poids %>% select(var, poids_19 = weight),
  res_17$poids %>% select(var, poids_17 = weight), by = "var") %>%
  mutate(Variable  = libelles[var],
         categorie = str_extract(var, "^[a-z]+_[a-z]+"),
         ecart     = poids_17 - poids_19) %>%
  arrange(desc(coalesce(poids_17, 0))) %>%
  select(Variable, var, categorie, poids_19, poids_17, ecart)

poids_categories <- comparaison_poids %>%
  group_by(categorie) %>%
  summarise(poids_19 = sum(poids_19, na.rm = TRUE),
            poids_17 = sum(poids_17, na.rm = TRUE), .groups = "drop") %>%
  mutate(ecart = poids_17 - poids_19)

# Les deux partages que la section 2.2 demande de chiffrer.
part <- function(d, v) sum(d$weight[d$var %in% v], na.rm = TRUE)
partage <- tibble(
  Groupe = c("Spread HY seul", "Spread IG seul", "HY + IG",
             "Taux OAT 10 ans", "Taux crédits nouveaux (ménages + SNF)",
             "OAT + taux de crédit"),
  `Modèle 19 variables` = c(
    part(res_19$poids, "financement_marche_SPREADHY"),
    part(res_19$poids, "financement_marche_SPREADIG"),
    part(res_19$poids, c("financement_marche_SPREADHY", "financement_marche_SPREADIG")),
    part(res_19$poids, "financement_marche_OAT10Y"),
    part(res_19$poids, c("financement_credit_MIR_menages", "financement_credit_MIR_snf")),
    part(res_19$poids, c("financement_marche_OAT10Y", "financement_credit_MIR_menages",
                         "financement_credit_MIR_snf"))),
  `Modèle 17 variables` = c(
    part(res_17$poids, "financement_marche_SPREADHY"), NA_real_,
    part(res_17$poids, "financement_marche_SPREADHY"), NA_real_,
    part(res_17$poids, c("financement_credit_MIR_menages", "financement_credit_MIR_snf")),
    part(res_17$poids, c("financement_credit_MIR_menages", "financement_credit_MIR_snf"))))
print(partage)

# ---------------------------------------------------------------------------
# 8. Exports
# ---------------------------------------------------------------------------
arrondir <- function(d, n = 3) d %>% mutate(across(where(is.numeric), ~ round(.x, n)))

lecture <- tibble(
  Feuille = c("Performances_tirage_ref", "Distribution_resume", "Ecarts_apparies",
              "Distribution_detail", "Poids_variables", "Poids_categories", "Partage_importance"),
  Contenu = c(
    "Métriques des deux modèles sur le tirage de référence (graine 123)",
    "Moyenne et écart type des métriques sur les tirages répétés",
    "Écart 17 moins 19 tirage par tirage, et test de Wilcoxon apparié",
    "Détail par tirage, pour refaire les calculs",
    "Pondérations comparées, variable par variable",
    "Pondérations agrégées par catégorie",
    "Partage de l'importance entre HY et IG, et entre OAT et taux de crédit"),
  `Destination` = c(
    "Section 2.2, second crochet (métriques sur données identiques)",
    "Section 2.2, à substituer au tirage unique",
    "Section 2.2, pour qualifier l'écart de performance",
    "Archives", "Section 2.2, premier crochet (partage de l'importance)",
    "Section 2.2, effet sur la hiérarchie des catégories",
    "Section 2.2, premier crochet"))

feuilles <- list(
  Lecture                  = lecture,
  Performances_tirage_ref  = arrondir(perf_ref),
  Distribution_resume      = arrondir(resume_distribution),
  Ecarts_apparies          = arrondir(ecarts_apparies, 4),
  Distribution_detail      = arrondir(distribution),
  Poids_variables          = arrondir(comparaison_poids, 4),
  Poids_categories         = arrondir(poids_categories, 4),
  Partage_importance       = arrondir(partage, 4))

wb <- createWorkbook()
entete <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                      wrapText = TRUE, fgFill = "#DDEBF7", border = "TopBottom")
for (nm in names(feuilles)) {
  addWorksheet(wb, nm); writeData(wb, nm, feuilles[[nm]], headerStyle = entete)
  freezePane(wb, nm, firstActiveRow = 2); setColWidths(wb, nm, 1:ncol(feuilles[[nm]]), "auto")
}
saveWorkbook(wb, chemin_export, overwrite = TRUE)
cat("Ecrit :", chemin_export, "\n")

# Tabulars LaTeX, au format du script 11 (booktabs, a inclure via \input) ------
pct <- function(x) ifelse(is.na(x), "--", sub("\\.", ",", sprintf("%.1f", 100 * x)))
num <- function(x, n = 2) ifelse(is.na(x), "--", sub("\\.", ",", sprintf(paste0("%.", n, "f"), x)))

tex_perf <- c(
  "\\begin{tabular}{lcccccc}", "\\toprule",
  "Modèle & Exactitude & Kappa & Sensibilité & Spécificité & AUC \\\\",
  "\\midrule",
  sprintf("%s & %s\\,\\%% & %s & %s\\,\\%% & %s\\,\\%% & %s \\\\",
          perf_ref$modele, pct(perf_ref$accuracy), num(perf_ref$kappa),
          pct(perf_ref$sensibilite), pct(perf_ref$specificite), num(perf_ref$auc)),
  "\\bottomrule", "\\end{tabular}")
writeLines(tex_perf, file.path(dossier_tex, "comparaison_17_19_performances.tex"), useBytes = TRUE)

tex_partage <- c(
  "\\begin{tabular}{lcc}", "\\toprule",
  "Groupe de variables & Modèle à 19 variables & Modèle à 17 variables \\\\",
  "\\midrule",
  sprintf("%s & %s\\,\\%% & %s\\,\\%% \\\\", partage$Groupe,
          pct(partage$`Modèle 19 variables`), pct(partage$`Modèle 17 variables`)),
  "\\bottomrule", "\\end{tabular}")
writeLines(tex_partage, file.path(dossier_tex, "comparaison_17_19_partage.tex"), useBytes = TRUE)
cat("Ecrit :", dossier_tex, "(2 tabulars)\n")
