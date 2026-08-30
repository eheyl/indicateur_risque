## ---------------------------------------------------------------------------##
##   Tests de robustesse : methodes alternatives de separation train / test   ##
##   Random Forest (ranger) -> poids -> indicateur, pour chaque methode        ##
## ---------------------------------------------------------------------------##
#
# Principe : chaque methode est un bloc independant (commente). On decommente
# un bloc, on le fait tourner, il range son resultat dans la liste `resultats`.
# La section "Export" en fin de script ecrit dans un Excel dedie tout ce qui
# est present dans `resultats` (donc on peut exporter apres 1, 3 ou 8 blocs).
#
# Objets attendus (fournis par 5. Random_Forest.R -> 5. Data_pour_RF.R) :
#   - df_zscore     : TIME_PERIOD + z-scores (serie complete, pour l'indicateur)
#   - df_zscore_rf  : TIME_PERIOD + z-scores + crise (1999-Q1 -> 2025-Q3)
#   - df_model      : df_zscore_rf sans TIME_PERIOD (base d'estimation)
#   - imp_df        : poids publies (Poids_variables.xlsx) = reference
#   - df_weighted   : indicateur de reference (indicator_rf)
#   - trimestres_incomplets

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/5. Random_Forest.R")

# Parametres ------------------------------------------------------------------
chemin_export     <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Robustesse_splits_RF.xlsx"
date_debut_export <- "2009-Q1"     # meme fenetre que Donnees_RFW_ext.xlsx
n_trees           <- 180
seed_ref          <- 123
prop_train        <- 0.7

vars_X   <- setdiff(names(df_model), "crise")
n_obs    <- nrow(df_model)
periodes <- df_zscore_rf$TIME_PERIOD      # meme ordre que df_model

if (anyNA(df_model)) warning("df_model contient des NA : ranger va planter, verifier la fenetre de Data_pour_RF.")

resultats <- list()                       # conteneur des resultats

# Fonctions -------------------------------------------------------------------

# Poids a partir d'un ranger : importance par permutation, negatives -> 0, normalisees
poids_depuis_rf <- function(rf) {
  data.frame(var = names(rf$variable.importance),
             imp = rf$variable.importance,
             row.names = NULL) %>%
    mutate(imp_pos = pmax(imp, 0),
           weight  = imp_pos / sum(imp_pos)) %>%
    arrange(desc(weight))
}

# Performance sur un jeu test (robuste au cas ou le test ne contient qu'une classe)
perf_test <- function(rf_cls, rf_prob, test) {
  out <- data.frame(n_test = nrow(test), n_crise_test = sum(test$crise == 1),
                    accuracy = NA_real_, kappa = NA_real_, sensibilite = NA_real_,
                    specificite = NA_real_, auc = NA_real_)
  if (nrow(test) == 0) return(out)
  pred_class <- factor(predict(rf_cls, data = test)$predictions, levels = c("0", "1"))
  pred_prob  <- predict(rf_prob, data = test)$predictions[, "1"]
  obs        <- factor(test$crise, levels = c("0", "1"))
  cm <- tryCatch(confusionMatrix(pred_class, obs, positive = "1"), error = function(e) NULL)
  if (!is.null(cm)) {
    out$accuracy    <- unname(cm$overall["Accuracy"])
    out$kappa       <- unname(cm$overall["Kappa"])
    out$sensibilite <- unname(cm$byClass["Sensitivity"])
    out$specificite <- unname(cm$byClass["Specificity"])
  }
  out$auc <- tryCatch(
    as.numeric(auc(roc(obs, pred_prob, levels = c("0", "1"), direction = "<", quiet = TRUE))),
    error = function(e) NA_real_)
  out
}

# Estimation complete sur un split : deux forets (classes pour les poids, proba pour l'AUC)
estimer_rf <- function(train, test, seed = seed_ref) {
  set.seed(seed)
  rf_cls <- ranger(crise ~ ., data = train, num.trees = n_trees,
                   importance = "permutation", write.forest = TRUE)
  set.seed(seed)
  rf_prob <- ranger(crise ~ ., data = train, num.trees = n_trees,
                    probability = TRUE, write.forest = TRUE)
  perf <- perf_test(rf_cls, rf_prob, test) %>%
    mutate(n_train = nrow(train), n_crise_train = sum(train$crise == 1),
           oob_error = rf_cls$prediction.error, .before = 1)
  list(rf = rf_cls, poids = poids_depuis_rf(rf_cls), perf = perf)
}

# Indicateur = z-scores (serie complete) x poids
construire_indicateur <- function(poids) {
  w <- setNames(poids$weight, poids$var)
  as.numeric(as.matrix(df_zscore[, poids$var]) %*% w[poids$var])
}

# Hyperparametres effectivement utilises par un ranger
hyperparams_rf <- function(rf, nom) {
  champ <- function(x, defaut) if (is.null(x)) defaut else x
  repl  <- champ(rf$replace, TRUE)
  data.frame(
    modele    = nom,
    parametre = c("num.trees", "mtry", "min.node.size", "splitrule", "replace",
                  "sample.fraction", "max.depth", "importance", "nb variables", "n obs"),
    valeur    = as.character(c(rf$num.trees, rf$mtry, rf$min.node.size,
                  champ(rf$splitrule, "gini"), repl, ifelse(isTRUE(repl), 1, 0.632),
                  "illimitee", champ(rf$importance.mode, "permutation"),
                  rf$num.independent.variables, rf$num.samples)),
    stringsAsFactors = FALSE)
}

# Enregistrement homogene d'un resultat (train/test uniques)
enregistrer <- function(nom, description, res) {
  list(nom = nom, description = description, poids = res$poids, perf = res$perf,
       indicateur = construire_indicateur(res$poids), rf = res$rf)
}

# Affichage rapide
resume <- function(r) {
  cat("\n==== ", r$nom, " : ", r$description, "\n", sep = "")
  print(r$perf)
  print(head(r$poids %>% select(var, weight), 8))
}

# ============================================================================ #
#  M0. Reference : split Bernoulli 70/30, seed 123 (methode du Tresor-Eco)
#  Sert a verifier que l'on retrouve bien Poids_variables.xlsx
# ============================================================================ #
set.seed(seed_ref)
idx_ref <- sample(c(TRUE, FALSE), n_obs, replace = TRUE, prob = c(prop_train, 1 - prop_train))
res <- estimer_rf(df_model[idx_ref, ], df_model[!idx_ref, ])
resultats[["M0_reference"]] <- enregistrer("M0_reference",
  "Split Bernoulli 70/30 (sample avec prob), seed 123 - methode de reference", res)
resume(resultats[["M0_reference"]])
# Ecart aux poids publies (doit etre ~0 si meme version des donnees)
print(left_join(imp_df %>% select(var, w_publie = weight),
                res$poids %>% select(var, w_M0 = weight), by = "var") %>%
        mutate(ecart = w_M0 - w_publie))

# ============================================================================ #
#  M1. Aleatoire stratifie sur la variable crise (caret::createDataPartition)
#  Garantit exactement 70 % des trimestres en train ET la meme part de crises
#  dans train et test (le Bernoulli de reference ne le garantit pas)
# ============================================================================ #
set.seed(seed_ref)
idx_strat <- createDataPartition(df_model$crise, p = prop_train, list = FALSE)[, 1]
res <- estimer_rf(df_model[idx_strat, ], df_model[-idx_strat, ])
resultats[["M1_stratifie"]] <- enregistrer("M1_stratifie",
  "Split aleatoire stratifie sur crise (70/30 exact), seed 123", res)
resume(resultats[["M1_stratifie"]])

# ============================================================================ #
#  M2. Chronologique : 70 % premiers trimestres en train, 30 % derniers en test
#  Split "honnete" pour une serie temporelle (pas de fuite du futur vers le
#  passe). Attention : si le test ne contient aucune crise, AUC/sensibilite = NA
# ============================================================================ #
n_train_chrono <- floor(prop_train * n_obs)
idx_chrono <- seq_len(n_obs) <= n_train_chrono
cat("Train :", periodes[1], "->", periodes[n_train_chrono],
    "| Test :", periodes[n_train_chrono + 1], "->", periodes[n_obs], "\n")
res <- estimer_rf(df_model[idx_chrono, ], df_model[!idx_chrono, ])
resultats[["M2_chrono"]] <- enregistrer("M2_chrono",
  paste0("Split chronologique : train ", periodes[1], "-", periodes[n_train_chrono],
         ", test ", periodes[n_train_chrono + 1], "-", periodes[n_obs]), res)
resume(resultats[["M2_chrono"]])

# ============================================================================ #
#  M3. Chronologique inverse : 30 % premiers trimestres en test, 70 % derniers
#  en train. Teste si les poids dependent de la periode d'apprentissage
# ============================================================================ #
n_test_inv <- n_obs - floor(prop_train * n_obs)
idx_inv <- seq_len(n_obs) > n_test_inv
res <- estimer_rf(df_model[idx_inv, ], df_model[!idx_inv, ])
resultats[["M3_chrono_inverse"]] <- enregistrer("M3_chrono_inverse",
  paste0("Split chronologique inverse : test ", periodes[1], "-", periodes[n_test_inv],
         ", train ", periodes[n_test_inv + 1], "-", periodes[n_obs]), res)
resume(resultats[["M3_chrono_inverse"]])

# ============================================================================ #
#  M4. Blocs annuels : on tire au sort des annees entieres (70/30) plutot que
#  des trimestres isoles. Limite la fuite d'information entre trimestres
#  adjacents (forte autocorrelation des z-scores)
# ============================================================================ #
annees <- substr(periodes, 1, 4)
set.seed(seed_ref)
annees_train <- sample(unique(annees), size = round(prop_train * length(unique(annees))))
idx_bloc <- annees %in% annees_train
cat("Annees test :", paste(sort(setdiff(unique(annees), annees_train)), collapse = " "), "\n")
res <- estimer_rf(df_model[idx_bloc, ], df_model[!idx_bloc, ])
resultats[["M4_blocs_annuels"]] <- enregistrer("M4_blocs_annuels",
  paste0("Split par blocs annuels 70/30, seed 123. Annees test : ",
         paste(sort(setdiff(unique(annees), annees_train)), collapse = " ")), res)
resume(resultats[["M4_blocs_annuels"]])

# ============================================================================ #
#  M5. Leave-one-crisis-out : pour chaque episode de crise (suite de trimestres
#  crise = 1), le test = l'episode + une marge de `marge` trimestres avant et
#  apres ; le train = tout le reste. Poids retenus = moyenne sur les episodes.
#  Repond a la critique "le modele ne fait que reconnaitre 2008"
# ============================================================================ #
marge <- 4
crise_num <- as.integer(as.character(df_model$crise))
r <- rle(crise_num)
fin_ep   <- cumsum(r$lengths)
debut_ep <- fin_ep - r$lengths + 1
episodes <- data.frame(debut = debut_ep[r$values == 1], fin = fin_ep[r$values == 1])
episodes$libelle <- paste0(periodes[episodes$debut], " -> ", periodes[episodes$fin])
print(episodes)

loco <- lapply(seq_len(nrow(episodes)), function(k) {
  idx_test <- seq(max(1, episodes$debut[k] - marge), min(n_obs, episodes$fin[k] + marge))
  res_k <- estimer_rf(df_model[-idx_test, ], df_model[idx_test, ])
  res_k$perf$episode <- episodes$libelle[k]
  res_k
})
poids_loco <- bind_rows(lapply(loco, function(x) x$poids), .id = "episode") %>%
  group_by(var) %>%
  summarise(imp = mean(imp), imp_pos = mean(imp_pos), weight = mean(weight), .groups = "drop") %>%
  mutate(weight = weight / sum(weight)) %>%        # renormalisation
  arrange(desc(weight))
perf_loco <- bind_rows(lapply(loco, function(x) x$perf))
print(perf_loco)
resultats[["M5_leave_one_crisis_out"]] <- list(
  nom = "M5_leave_one_crisis_out",
  description = paste0("Leave-one-crisis-out (marge ", marge, " trimestres), poids moyens sur ",
                       nrow(episodes), " episodes"),
  poids = poids_loco,
  perf  = perf_loco %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))),
  perf_detail = perf_loco,
  indicateur = construire_indicateur(poids_loco),
  rf = loco[[1]]$rf)
resume(resultats[["M5_leave_one_crisis_out"]])

# ============================================================================ #
#  M6. Repetition du split de reference sur N seeds : distribution des poids
#  et des performances. Mesure la sensibilite au tirage aleatoire
# ============================================================================ #
n_rep <- 200
rep_splits <- lapply(seq_len(n_rep), function(s) {
  set.seed(s)
  idx <- sample(c(TRUE, FALSE), n_obs, replace = TRUE, prob = c(prop_train, 1 - prop_train))
  res_s <- estimer_rf(df_model[idx, ], df_model[!idx, ], seed = s)
  res_s$rf <- NULL                                  # ne pas garder 200 forets en memoire
  res_s$poids$seed <- s
  res_s$perf$seed  <- s
  res_s
})
poids_rep_long <- bind_rows(lapply(rep_splits, function(x) x$poids))
poids_rep_stats <- poids_rep_long %>%
  group_by(var) %>%
  summarise(imp = mean(imp), imp_pos = mean(imp_pos),
            weight = mean(weight), sd_weight = sd(weight),
            q05 = quantile(weight, 0.05), q95 = quantile(weight, 0.95),
            part_nulle = mean(weight == 0), .groups = "drop") %>%
  mutate(weight = weight / sum(weight)) %>%
  arrange(desc(weight))
perf_rep <- bind_rows(lapply(rep_splits, function(x) x$perf))
resultats[["M6_repetitions"]] <- list(
  nom = "M6_repetitions",
  description = paste0("Split Bernoulli 70/30 repete sur ", n_rep, " seeds, poids moyens"),
  poids = poids_rep_stats,
  perf  = perf_rep %>% select(-seed) %>%
    summarise(across(everything(), list(moy = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)))),
  perf_detail = perf_rep,
  indicateur = construire_indicateur(poids_rep_stats),
  rf = NULL)
resume(resultats[["M6_repetitions"]])

# ============================================================================ #
#  M7. Validation croisee temporelle (rolling origin, fenetre croissante)
#  caret::createTimeSlices : on entraine sur [1 ; t] et on teste sur les
#  `horizon` trimestres suivants, en avancant de `pas` trimestres. Poids = moyenne
# ============================================================================ #
fenetre_init <- floor(0.5 * n_obs)      # premiere fenetre d'apprentissage
horizon      <- 8                       # taille du test (2 ans)
pas          <- 7                       # skip : un split tous les 8 trimestres
slices <- createTimeSlices(seq_len(n_obs), initialWindow = fenetre_init,
                           horizon = horizon, fixedWindow = FALSE, skip = pas)
cat("Nombre de splits :", length(slices$train), "\n")
roll <- lapply(seq_along(slices$train), function(k) {
  res_k <- estimer_rf(df_model[slices$train[[k]], ], df_model[slices$test[[k]], ])
  res_k$perf$fenetre <- paste0(periodes[min(slices$test[[k]])], " -> ", periodes[max(slices$test[[k]])])
  res_k$rf <- NULL
  res_k
})
poids_roll <- bind_rows(lapply(roll, function(x) x$poids)) %>%
  group_by(var) %>%
  summarise(imp = mean(imp), imp_pos = mean(imp_pos), weight = mean(weight), .groups = "drop") %>%
  mutate(weight = weight / sum(weight)) %>%
  arrange(desc(weight))
perf_roll <- bind_rows(lapply(roll, function(x) x$perf))
print(perf_roll)
resultats[["M7_rolling_origin"]] <- list(
  nom = "M7_rolling_origin",
  description = paste0("Validation croisee temporelle (fenetre croissante, ", length(roll),
                       " splits, horizon ", horizon, " trimestres), poids moyens"),
  poids = poids_roll,
  perf  = perf_roll %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))),
  perf_detail = perf_roll,
  indicateur = construire_indicateur(poids_roll),
  rf = NULL)
resume(resultats[["M7_rolling_origin"]])

# ============================================================================ #
#  M8. Sans separation : RF sur l'echantillon complet, performance evaluee
#  sur les predictions out-of-bag (OOB). Justification : chaque arbre n'a vu
#  qu'environ 63 % des observations, l'OOB joue le role du test
# ============================================================================ #
set.seed(seed_ref)
rf_full_cls <- ranger(crise ~ ., data = df_model, num.trees = n_trees,
                      importance = "permutation", write.forest = TRUE)
set.seed(seed_ref)
rf_full_prob <- ranger(crise ~ ., data = df_model, num.trees = n_trees,
                       probability = TRUE, write.forest = TRUE)
oob_prob  <- rf_full_prob$predictions[, "1"]        # predictions OOB
oob_class <- factor(ifelse(oob_prob > 0.5, 1, 0), levels = c("0", "1"))
obs_full  <- factor(df_model$crise, levels = c("0", "1"))
cm_oob <- confusionMatrix(oob_class, obs_full, positive = "1")
perf_oob <- data.frame(
  n_train = n_obs, n_crise_train = sum(df_model$crise == 1),
  oob_error = rf_full_cls$prediction.error,
  n_test = NA, n_crise_test = NA,
  accuracy    = unname(cm_oob$overall["Accuracy"]),
  kappa       = unname(cm_oob$overall["Kappa"]),
  sensibilite = unname(cm_oob$byClass["Sensitivity"]),
  specificite = unname(cm_oob$byClass["Specificity"]),
  auc = as.numeric(auc(roc(obs_full, oob_prob, levels = c("0", "1"), direction = "<", quiet = TRUE))))
resultats[["M8_sans_split_OOB"]] <- enregistrer("M8_sans_split_OOB",
  "Echantillon complet, performance out-of-bag", list(rf = rf_full_cls, poids = poids_depuis_rf(rf_full_cls), perf = perf_oob))
resume(resultats[["M8_sans_split_OOB"]])

# ============================================================================ #
#  Export Excel (fichier distinct de Donnees_RFW_ext.xlsx)
#  A lancer apres avoir fait tourner un ou plusieurs blocs
# ============================================================================ #
stopifnot(length(resultats) > 0)

# 1. Indicateurs : reference publiee + une colonne par methode
df_indicateurs <- df_weighted %>%
  select(TIME_PERIOD, Reference_publiee = indicator_rf)
for (nm in names(resultats)) df_indicateurs[[nm]] <- resultats[[nm]]$indicateur
df_indicateurs <- df_indicateurs %>%
  filter(TIME_PERIOD >= date_debut_export, TIME_PERIOD != "NA-QNA") %>%
  mutate(incomplet = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)) %>%
  arrange(TIME_PERIOD)

# 2. Poids : une colonne par methode (+ ecart-type pour M6 si present)
df_poids <- imp_df %>% select(var, Reference_publiee = weight)
for (nm in names(resultats)) {
  df_poids <- left_join(df_poids, resultats[[nm]]$poids %>% select(var, !!nm := weight), by = "var")
  if ("sd_weight" %in% names(resultats[[nm]]$poids))
    df_poids <- left_join(df_poids, resultats[[nm]]$poids %>% select(var, !!paste0(nm, "_sd") := sd_weight), by = "var")
}
df_poids <- df_poids %>% arrange(desc(Reference_publiee))

# 3. Performances
df_perf <- bind_rows(lapply(resultats, function(r) r$perf %>% mutate(methode = r$nom, .before = 1)))

# 4. Comparaison a la reference : correlation des indicateurs, distance des poids
cols_ind <- setdiff(names(df_indicateurs), c("TIME_PERIOD", "incomplet"))
mat_cor  <- cor(df_indicateurs[cols_ind], use = "pairwise.complete.obs")
df_cor   <- as.data.frame(mat_cor) %>% mutate(methode = rownames(mat_cor), .before = 1)
df_dist  <- bind_rows(lapply(names(resultats), function(nm) {
  p <- left_join(imp_df %>% select(var, w_ref = weight),
                 resultats[[nm]]$poids %>% select(var, w = weight), by = "var")
  data.frame(methode = nm,
             cor_pearson_poids  = cor(p$w_ref, p$w),
             cor_spearman_poids = cor(p$w_ref, p$w, method = "spearman"),
             ecart_abs_moyen    = mean(abs(p$w_ref - p$w)),
             ecart_abs_max      = max(abs(p$w_ref - p$w)),
             var_ecart_max      = p$var[which.max(abs(p$w_ref - p$w))],
             nb_poids_nuls      = sum(p$w == 0),
             cor_indicateur_ref = mat_cor["Reference_publiee", nm])
}))

# 5. Descriptif des methodes et hyperparametres
df_methodes <- bind_rows(lapply(resultats, function(r) data.frame(methode = r$nom, description = r$description)))
df_hyper <- bind_rows(lapply(resultats, function(r) if (!is.null(r$rf)) hyperparams_rf(r$rf, r$nom) else NULL))

# 6. Details (episodes / repetitions / fenetres) quand disponibles
feuilles <- list(
  "Indicateurs"     = df_indicateurs,
  "Poids"           = df_poids,
  "Performances"    = df_perf,
  "Comparaison"     = df_dist,
  "Correlations"    = df_cor,
  "Methodes"        = df_methodes,
  "Hyperparametres" = df_hyper
)
for (nm in names(resultats)) {
  if (!is.null(resultats[[nm]]$perf_detail))
    feuilles[[paste0("Detail_", substr(nm, 1, 2))]] <- resultats[[nm]]$perf_detail
}
if (exists("poids_rep_long")) feuilles[["Poids_repetitions"]] <- poids_rep_long

write_xlsx(feuilles, path = chemin_export)
cat("Export termine :", chemin_export, "\n")

# 7. Un fichier par methode au format de Donnees_RFW_ext.xlsx (dans Résultats/)
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/Tests robustesse/Export_format_RFW.R")
for (nm in names(resultats)) exporter_format_rfw(resultats[[nm]]$poids, nm)
