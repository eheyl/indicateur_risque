## ---------------------------------------------------------------------------##
##   Tests de robustesse : XGBoost a la place du Random Forest                 ##
##   Importance (gain ou permutation) -> poids -> indicateur                   ##
## ---------------------------------------------------------------------------##
#
# Meme logique que 9. Robustesse_splits_RF.R : un bloc par variante, a
# decommenter au fur et a mesure ; l'export en fin de script ecrit tout ce qui
# est dans `resultats_xgb` dans un Excel dedie.
#
# Points d'attention pour le memoire :
#  - l'importance "gain" de XGBoost (reduction totale de la perte apportee par
#    la variable) n'est pas conceptuellement la meme chose que l'importance par
#    permutation de ranger ; le bloc X1 recalcule une importance par permutation
#    pour que les poids soient comparables a ceux du RF ;
#  - XGBoost a beaucoup plus d'hyperparametres que ranger : ils sont calibres par
#    validation croisee stratifiee sur le train (bloc X2) et exportes.
#
# Objets attendus : df_model, df_zscore, df_zscore_rf, imp_df, df_weighted,
# trimestres_incomplets (fournis par 5. Random_Forest.R)

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/5. Random_Forest.R")
library(xgboost)

# Parametres ------------------------------------------------------------------
chemin_export     <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Robustesse_XGBoost.xlsx"
date_debut_export <- "2009-Q1"
seed_ref          <- 123
prop_train        <- 0.7
n_perm            <- 30              # nb de permutations pour l'importance par permutation

vars_X   <- setdiff(names(df_model), "crise")
n_obs    <- nrow(df_model)
periodes <- df_zscore_rf$TIME_PERIOD

resultats_xgb <- list()

# Hyperparametres par defaut (adaptes a un petit echantillon : arbres peu
# profonds, apprentissage lent, ponderation de la classe crise)
ratio_classes <- sum(df_model$crise == 0) / sum(df_model$crise == 1)
params_defaut <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.1,
  max_depth        = 3,
  min_child_weight = 1,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  gamma            = 0,
  lambda           = 1,
  alpha            = 0,
  scale_pos_weight = ratio_classes
)
nrounds_defaut <- 200

# Fonctions -------------------------------------------------------------------

en_matrice <- function(df) {
  list(X = as.matrix(df[, vars_X]), y = as.integer(as.character(df$crise)))
}

# Importance "gain" de XGBoost -> poids (meme format que imp_df)
poids_gain <- function(model) {
  imp <- as.data.frame(xgb.importance(model = model))   # Feature, Gain, Cover, Frequency
  data.frame(var = vars_X, stringsAsFactors = FALSE) %>%
    left_join(imp %>% select(var = Feature, imp = Gain), by = "var") %>%
    mutate(imp = replace_na(imp, 0),            # variable jamais utilisee -> 0
           imp_pos = pmax(imp, 0),
           weight  = imp_pos / sum(imp_pos)) %>%
    arrange(desc(weight))
}

# Importance par permutation : baisse d'AUC quand on melange une variable
# (calculee sur le jeu passe en argument, par defaut le test)
poids_permutation <- function(model, X, y, n_perm = 30, seed = seed_ref) {
  auc_de <- function(Xm) {
    p <- predict(model, xgb.DMatrix(Xm))
    as.numeric(auc(roc(y, p, levels = c(0, 1), direction = "<", quiet = TRUE)))
  }
  auc_base <- auc_de(X)
  set.seed(seed)
  imp <- sapply(vars_X, function(v) {
    mean(sapply(seq_len(n_perm), function(i) {
      Xp <- X
      Xp[, v] <- sample(Xp[, v])
      auc_base - auc_de(Xp)
    }))
  })
  data.frame(var = names(imp), imp = unname(imp), stringsAsFactors = FALSE) %>%
    mutate(imp_pos = pmax(imp, 0), weight = imp_pos / sum(imp_pos)) %>%
    arrange(desc(weight))
}

perf_test_xgb <- function(model, test) {
  out <- data.frame(n_test = nrow(test), n_crise_test = sum(test$crise == 1),
                    accuracy = NA_real_, kappa = NA_real_, sensibilite = NA_real_,
                    specificite = NA_real_, auc = NA_real_)
  if (nrow(test) == 0) return(out)
  m <- en_matrice(test)
  pred_prob  <- predict(model, xgb.DMatrix(m$X))
  pred_class <- factor(ifelse(pred_prob > 0.5, 1, 0), levels = c("0", "1"))
  obs        <- factor(m$y, levels = c("0", "1"))
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

# Estimation sur un split
estimer_xgb <- function(train, test, params = params_defaut, nrounds = nrounds_defaut,
                        importance = c("gain", "permutation"), seed = seed_ref) {
  importance <- match.arg(importance)
  m_tr <- en_matrice(train)
  set.seed(seed)
  model <- xgb.train(params = params, data = xgb.DMatrix(m_tr$X, label = m_tr$y),
                     nrounds = nrounds, verbose = 0)
  poids <- if (importance == "gain") {
    poids_gain(model)
  } else {
    m_te <- en_matrice(test)
    poids_permutation(model, m_te$X, m_te$y, n_perm = n_perm, seed = seed)
  }
  perf <- perf_test_xgb(model, test) %>%
    mutate(n_train = nrow(train), n_crise_train = sum(train$crise == 1), .before = 1)
  list(model = model, poids = poids, perf = perf, params = params, nrounds = nrounds,
       importance = importance)
}

# Calibration des hyperparametres par validation croisee stratifiee (xgb.cv)
# sur le train uniquement. Retourne les meilleurs params, le nrounds optimal et
# la grille complete (a exporter / mettre en annexe)
tuner_xgb <- function(train, grille, nfold = 5, nrounds_max = 500,
                      early_stopping = 30, seed = seed_ref) {
  m <- en_matrice(train)
  dtrain <- xgb.DMatrix(m$X, label = m$y)
  res_grille <- lapply(seq_len(nrow(grille)), function(i) {
    p <- modifyList(params_defaut, as.list(grille[i, ]))
    set.seed(seed)
    cv <- xgb.cv(params = p, data = dtrain, nrounds = nrounds_max, nfold = nfold,
                 stratified = TRUE, early_stopping_rounds = early_stopping, verbose = 0)
    best <- cv$best_iteration
    data.frame(grille[i, ], nrounds = best,
               auc_cv = cv$evaluation_log$test_auc_mean[best],
               auc_cv_sd = cv$evaluation_log$test_auc_std[best])
  })
  res_grille <- bind_rows(res_grille) %>% arrange(desc(auc_cv), nrounds)
  meilleur <- res_grille[1, ]
  params_opt <- modifyList(params_defaut, as.list(meilleur[, names(grille)]))
  list(params = params_opt, nrounds = meilleur$nrounds, grille = res_grille)
}

construire_indicateur <- function(poids) {
  w <- setNames(poids$weight, poids$var)
  as.numeric(as.matrix(df_zscore[, poids$var]) %*% w[poids$var])
}

# Hyperparametres effectivement utilises (version-agnostique : on lit la liste
# de params passee a xgb.train plutot que l'objet xgboost)
hyperparams_xgb <- function(res, nom) {
  p <- res$params
  data.frame(
    modele    = nom,
    parametre = c(names(p), "nrounds", "importance", "nb variables"),
    valeur    = c(sapply(p, function(v) if (is.numeric(v)) as.character(round(v, 4)) else as.character(v)),
                  as.character(res$nrounds), res$importance, as.character(length(vars_X))),
    stringsAsFactors = FALSE)
}

enregistrer_xgb <- function(nom, description, res) {
  list(nom = nom, description = description, poids = res$poids, perf = res$perf,
       indicateur = construire_indicateur(res$poids), res = res)
}

resume <- function(r) {
  cat("\n==== ", r$nom, " : ", r$description, "\n", sep = "")
  print(r$perf)
  print(head(r$poids %>% select(var, weight), 8))
}

# Split de reference (identique au Tresor-Eco)
set.seed(seed_ref)
idx_ref <- sample(c(TRUE, FALSE), n_obs, replace = TRUE, prob = c(prop_train, 1 - prop_train))
train_ref <- df_model[idx_ref, ]
test_ref  <- df_model[!idx_ref, ]

# ============================================================================ #
#  X0. XGBoost, hyperparametres par defaut, split de reference, importance gain
# ============================================================================ #
# res <- estimer_xgb(train_ref, test_ref, importance = "gain")
# resultats_xgb[["X0_defaut_gain"]] <- enregistrer_xgb("X0_defaut_gain",
#   "XGBoost params par defaut, split de reference (seed 123), importance gain", res)
# resume(resultats_xgb[["X0_defaut_gain"]])

# ============================================================================ #
#  X1. Idem X0 mais importance par permutation (comparable au RF)
# ============================================================================ #
# res <- estimer_xgb(train_ref, test_ref, importance = "permutation")
# resultats_xgb[["X1_defaut_permutation"]] <- enregistrer_xgb("X1_defaut_permutation",
#   "XGBoost params par defaut, split de reference, importance par permutation (AUC test)", res)
# resume(resultats_xgb[["X1_defaut_permutation"]])

# ============================================================================ #
#  X2. Hyperparametres calibres par validation croisee (5 folds stratifies)
#  sur le train de reference, puis estimation avec ces parametres
# ============================================================================ #
# grille <- expand.grid(
#   eta              = c(0.05, 0.1, 0.3),
#   max_depth        = c(2, 3, 4, 6),
#   min_child_weight = c(1, 3, 5),
#   subsample        = c(0.7, 1),
#   colsample_bytree = c(0.7, 1),
#   KEEP.OUT.ATTRS = FALSE)
# cat("Nombre de combinaisons :", nrow(grille), "\n")
# tune <- tuner_xgb(train_ref, grille)
# print(head(tune$grille, 10))
# res <- estimer_xgb(train_ref, test_ref, params = tune$params, nrounds = tune$nrounds,
#                    importance = "gain")
# resultats_xgb[["X2_tune_gain"]] <- enregistrer_xgb("X2_tune_gain",
#   "XGBoost hyperparametres calibres par CV stratifiee (5 folds) sur le train, importance gain", res)
# resultats_xgb[["X2_tune_gain"]]$grille <- tune$grille
# resume(resultats_xgb[["X2_tune_gain"]])
# 
# # Variante permutation avec les memes hyperparametres
# res <- estimer_xgb(train_ref, test_ref, params = tune$params, nrounds = tune$nrounds,
#                    importance = "permutation")
# resultats_xgb[["X2b_tune_permutation"]] <- enregistrer_xgb("X2b_tune_permutation",
#   "XGBoost hyperparametres calibres, importance par permutation", res)
# resume(resultats_xgb[["X2b_tune_permutation"]])

# ============================================================================ #
#  X3. Hyperparametres calibres, split chronologique 70/30
# ============================================================================ #
# n_train_chrono <- floor(prop_train * n_obs)
# idx_chrono <- seq_len(n_obs) <= n_train_chrono
# if (!exists("tune")) stop("Faire tourner le bloc X2 d'abord (ou definir `tune`).")
# res <- estimer_xgb(df_model[idx_chrono, ], df_model[!idx_chrono, ],
#                    params = tune$params, nrounds = tune$nrounds, importance = "gain")
# resultats_xgb[["X3_tune_chrono"]] <- enregistrer_xgb("X3_tune_chrono",
#   paste0("XGBoost hyperparametres calibres, split chronologique : train ", periodes[1], "-",
#          periodes[n_train_chrono], ", test ", periodes[n_train_chrono + 1], "-", periodes[n_obs]), res)
# resume(resultats_xgb[["X3_tune_chrono"]])

# ============================================================================ #
#  X4. Echantillon complet : nrounds choisi par CV, performance = CV moyenne,
#  poids gain sur le modele final (pas de jeu test)
# ============================================================================ #
# if (!exists("tune")) stop("Faire tourner le bloc X2 d'abord (ou definir `tune`).")
# m_full <- en_matrice(df_model)
# set.seed(seed_ref)
# cv_full <- xgb.cv(params = tune$params, data = xgb.DMatrix(m_full$X, label = m_full$y),
#                   nrounds = 500, nfold = 5, stratified = TRUE,
#                   early_stopping_rounds = 30, verbose = 0)
# set.seed(seed_ref)
# model_full <- xgb.train(params = tune$params, data = xgb.DMatrix(m_full$X, label = m_full$y),
#                         nrounds = cv_full$best_iteration, verbose = 0)
# res <- list(model = model_full, poids = poids_gain(model_full), params = tune$params,
#             nrounds = cv_full$best_iteration, importance = "gain",
#             perf = data.frame(n_train = n_obs, n_crise_train = sum(df_model$crise == 1),
#                               n_test = NA, n_crise_test = NA, accuracy = NA, kappa = NA,
#                               sensibilite = NA, specificite = NA,
#                               auc = cv_full$evaluation_log$test_auc_mean[cv_full$best_iteration]))
# resultats_xgb[["X4_complet_CV"]] <- enregistrer_xgb("X4_complet_CV",
#   "XGBoost echantillon complet, nrounds par CV, AUC = moyenne CV 5 folds", res)
# resume(resultats_xgb[["X4_complet_CV"]])

# ============================================================================ #
#  Export Excel (fichier distinct)
# ============================================================================ #
stopifnot(length(resultats_xgb) > 0)

df_indicateurs <- df_weighted %>% select(TIME_PERIOD, Reference_publiee_RF = indicator_rf)
for (nm in names(resultats_xgb)) df_indicateurs[[nm]] <- resultats_xgb[[nm]]$indicateur
df_indicateurs <- df_indicateurs %>%
  filter(TIME_PERIOD >= date_debut_export, TIME_PERIOD != "NA-QNA") %>%
  mutate(incomplet = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)) %>%
  arrange(TIME_PERIOD)

df_poids <- imp_df %>% select(var, Reference_publiee_RF = weight)
for (nm in names(resultats_xgb))
  df_poids <- left_join(df_poids, resultats_xgb[[nm]]$poids %>% select(var, !!nm := weight), by = "var")
df_poids <- df_poids %>% arrange(desc(Reference_publiee_RF))

df_perf <- bind_rows(lapply(resultats_xgb, function(r) r$perf %>% mutate(methode = r$nom, .before = 1)))

cols_ind <- setdiff(names(df_indicateurs), c("TIME_PERIOD", "incomplet"))
mat_cor  <- cor(df_indicateurs[cols_ind], use = "pairwise.complete.obs")
df_cor   <- as.data.frame(mat_cor) %>% mutate(methode = rownames(mat_cor), .before = 1)
df_dist  <- bind_rows(lapply(names(resultats_xgb), function(nm) {
  p <- left_join(imp_df %>% select(var, w_ref = weight),
                 resultats_xgb[[nm]]$poids %>% select(var, w = weight), by = "var")
  data.frame(methode = nm,
             cor_pearson_poids  = cor(p$w_ref, p$w),
             cor_spearman_poids = cor(p$w_ref, p$w, method = "spearman"),
             ecart_abs_moyen    = mean(abs(p$w_ref - p$w)),
             ecart_abs_max      = max(abs(p$w_ref - p$w)),
             var_ecart_max      = p$var[which.max(abs(p$w_ref - p$w))],
             nb_poids_nuls      = sum(p$w == 0),
             cor_indicateur_ref = mat_cor["Reference_publiee_RF", nm])
}))

df_methodes <- bind_rows(lapply(resultats_xgb, function(r) data.frame(methode = r$nom, description = r$description)))
df_hyper    <- bind_rows(lapply(resultats_xgb, function(r) hyperparams_xgb(r$res, r$nom)))

feuilles <- list(
  "Indicateurs"     = df_indicateurs,
  "Poids"           = df_poids,
  "Performances"    = df_perf,
  "Comparaison"     = df_dist,
  "Correlations"    = df_cor,
  "Methodes"        = df_methodes,
  "Hyperparametres" = df_hyper
)
for (nm in names(resultats_xgb))
  if (!is.null(resultats_xgb[[nm]]$grille)) feuilles[[paste0("Grille_CV_", substr(nm, 1, 2))]] <- resultats_xgb[[nm]]$grille

write_xlsx(feuilles, path = chemin_export)
cat("Export termine :", chemin_export, "\n")

# Un fichier par variante au format de Donnees_RFW_ext.xlsx (dans Résultats/)
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/Tests robustesse/Export_format_RFW.R")
for (nm in names(resultats_xgb)) exporter_format_rfw(resultats_xgb[[nm]]$poids, nm)
