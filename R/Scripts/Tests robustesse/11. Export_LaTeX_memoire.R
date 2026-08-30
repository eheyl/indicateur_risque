## ---------------------------------------------------------------------------##
##   Export des tableaux du memoire en LaTeX (hyperparametres, poids, perfs)  ##
## ---------------------------------------------------------------------------##
#
# A lancer apres 9. Robustesse_splits_RF.R et/ou 10. Robustesse_XGBoost.R
# (les objets df_hyper, df_poids, df_perf, df_dist existent alors dans
# l'environnement) OU en relisant les Excel exportes (section 2).
#
# Produit des fichiers .tex contenant uniquement un environnement `tabular`
# (booktabs) : a inclure dans le memoire avec
#   \begin{table}[htbp]\centering
#     \caption{...}\label{tab:...}
#     \input{tableaux/hyperparametres_rf.tex}
#   \end{table}
# Preambule necessaire : \usepackage{booktabs}

dossier_tex <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/LaTeX/"
dir.create(dossier_tex, showWarnings = FALSE, recursive = TRUE)

# 1. Fonction generique data.frame -> tabular booktabs -------------------------
df_vers_tabular <- function(df, fichier, digits = 3, alignement = NULL,
                            entetes = NULL, ligne_gras = NULL) {
  df <- as.data.frame(df)
  # arrondi des numeriques, conversion en caractere, echappement des caracteres speciaux
  for (j in seq_along(df)) {
    x <- df[[j]]
    if (is.numeric(x)) x <- ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
    x <- as.character(x)
    x <- gsub("%", "\\\\%", x)
    x <- gsub("_", "\\\\_", x)
    x <- gsub("&", "\\\\&", x)
    df[[j]] <- x
  }
  if (is.null(entetes))    entetes <- names(df)
  entetes <- gsub("_", "\\\\_", entetes)
  if (is.null(alignement)) alignement <- paste0("l", strrep("r", ncol(df) - 1))
  lignes <- apply(df, 1, function(r) paste(r, collapse = " & "))
  if (!is.null(ligne_gras)) lignes[ligne_gras] <- paste0("\\textbf{", lignes[ligne_gras], "}")
  tex <- c(
    paste0("\\begin{tabular}{", alignement, "}"),
    "\\toprule",
    paste0(paste(entetes, collapse = " & "), " \\\\"),
    "\\midrule",
    paste0(lignes, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}")
  writeLines(tex, fichier, useBytes = TRUE)
  cat("Ecrit :", fichier, "\n")
  invisible(tex)
}

# 2. (Optionnel) relire les Excel si l'environnement est vide ------------------
# chemin_rf  <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Robustesse_splits_RF.xlsx"
# chemin_xgb <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Robustesse_XGBoost.xlsx"
# lire <- function(chemin, feuille) if (file.exists(chemin)) read_excel(chemin, sheet = feuille) else NULL
# df_hyper <- bind_rows(lire(chemin_rf, "Hyperparametres"), lire(chemin_xgb, "Hyperparametres"))
# df_poids <- lire(chemin_rf, "Poids")
# df_perf  <- lire(chemin_rf, "Performances")
# df_dist  <- lire(chemin_rf, "Comparaison")

# 3. Hyperparametres du RF de reference (sans re-estimer) ----------------------
# Ce sont les valeurs par defaut de ranger pour une classification a 19
# variables : mtry = floor(sqrt(19)) = 4, min.node.size = 1, splitrule = gini,
# tirage avec remise (sample.fraction = 1), profondeur illimitee.
hyper_rf_ref <- data.frame(
  Parametre   = c("Nombre d'arbres (num.trees)", "Variables tirees a chaque noeud (mtry)",
                  "Taille minimale des feuilles (min.node.size)", "Regle de division (splitrule)",
                  "Echantillonnage (replace / sample.fraction)", "Profondeur maximale (max.depth)",
                  "Mesure d'importance", "Separation train / test", "Graine (seed)"),
  Valeur      = c("180", "4 (= floor(sqrt(19)))", "1", "Gini", "avec remise / 1",
                  "illimitee", "permutation (OOB)", "Bernoulli 70/30 sur 106 trimestres", "123"),
  Justification = c("Stabilite des importances au-dela de ~150 arbres",
                    "Defaut ranger (classification)", "Defaut ranger (classification)",
                    "Defaut ranger", "Bootstrap classique (Breiman, 2001)",
                    "Defaut ranger", "Moins biaisee que l'impurete sur variables correlees",
                    "Voir tests de robustesse", "Reproductibilite"),
  stringsAsFactors = FALSE)
df_vers_tabular(hyper_rf_ref, file.path(dossier_tex, "hyperparametres_rf.tex"),
                alignement = "lll", entetes = c("Parametre", "Valeur", "Justification"))

# Version "extraite de l'objet ranger" si un RF vient d'etre estime dans la session
# (rf_weighted du script 5, ou resultats[["M0_reference"]]$rf du script 9)
if (exists("resultats") && !is.null(resultats[["M0_reference"]]$rf)) {
  rf <- resultats[["M0_reference"]]$rf
  hyper_extrait <- data.frame(
    Parametre = c("num.trees", "mtry", "min.node.size", "splitrule", "importance", "nb variables", "n obs"),
    Valeur    = c(rf$num.trees, rf$mtry, rf$min.node.size, rf$splitrule, rf$importance.mode,
                  rf$num.independent.variables, rf$num.samples))
  df_vers_tabular(hyper_extrait, file.path(dossier_tex, "hyperparametres_rf_extraits.tex"), alignement = "ll")
}

# 4. Hyperparametres XGBoost (si le script 10 a tourne) ------------------------
if (exists("df_hyper") && any(grepl("^X", df_hyper$modele))) {
  hyper_xgb <- df_hyper %>%
    filter(grepl("^X", modele)) %>%
    pivot_wider(names_from = modele, values_from = valeur)
  df_vers_tabular(hyper_xgb, file.path(dossier_tex, "hyperparametres_xgboost.tex"),
                  alignement = paste0("l", strrep("r", ncol(hyper_xgb) - 1)))
}

# 5. Poids par methode (si le script 9 ou 10 a tourne) -------------------------
if (exists("df_poids")) {
  df_vers_tabular(df_poids %>% mutate(across(where(is.numeric), ~ round(.x, 3))),
                  file.path(dossier_tex, "poids_robustesse.tex"), digits = 3)
}

# 6. Performances et comparaison a la reference ------------------------------
if (exists("df_perf")) {
  perf_court <- df_perf %>%
    select(any_of(c("methode", "n_train", "n_test", "n_crise_test", "accuracy", "kappa",
                    "sensibilite", "specificite", "auc", "oob_error")))
  df_vers_tabular(perf_court, file.path(dossier_tex, "performances_robustesse.tex"), digits = 3)
}
if (exists("df_dist")) {
  df_vers_tabular(df_dist %>% select(methode, cor_spearman_poids, ecart_abs_moyen, ecart_abs_max,
                                     var_ecart_max, nb_poids_nuls, cor_indicateur_ref),
                  file.path(dossier_tex, "comparaison_reference.tex"), digits = 3,
                  entetes = c("Methode", "Corr. Spearman poids", "Ecart abs. moyen", "Ecart abs. max",
                              "Variable ecart max", "Poids nuls", "Corr. indicateur"))
}
