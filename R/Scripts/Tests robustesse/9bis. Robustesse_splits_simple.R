## ---------------------------------------------------------------------------##
##   Robustesse des poids du Random Forest : methodes de split M0 a M8         ##
##   Version simplifiee : poids -> indicateur -> decomposition -> perf -> Excel ##
## ---------------------------------------------------------------------------##
#
# Principe : chaque methode M0..M8 se resume a UNE liste d'echantillons
# d'apprentissage (un seul pour M0-M4 et M8, plusieurs pour M5, M6 et M7).
# Une seule boucle estime les forets, moyenne les poids, reconstruit
# l'indicateur, sa decomposition par categorie (comme le graphique de
# reference) et les performances (AUC, sensibilite) sur l'echantillon test.
# Aucun bloc a decommenter : on lance le script en entier.
#
# Objets fournis par 5. Random_Forest.R -> 5. Data_pour_RF.R -> 3. Standardisation.R :
#   data         : liste des 5 categories (sert au mapping serie -> categorie)
#   df_zscore    : TIME_PERIOD + z-scores (serie complete)
#   df_zscore_rf : idem 1999-Q1 -> 2025-Q3 + colonne crise
#   df_model     : df_zscore_rf sans TIME_PERIOD (base d'estimation)
#   imp_df       : poids publies (Poids_variables.xlsx)
#   df_weighted  : indicateur de reference (indicator_rf)
#   trimestres_incomplets

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/5. Random_Forest.R")

# ---------------------------------------------------------------------------
# 1. Parametres
# ---------------------------------------------------------------------------
chemin_export     <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Robustesse_splits_simple.xlsx"
chemin_ccyb       <- "S:/03. Ponctuel/03. Banques/2025/CCyB across time/ts_data_ccyb_rates.xlsx"
date_debut_export <- "2009-Q1"   # meme fenetre que Donnees_RFW_ext.xlsx
n_trees           <- 180
seed_ref          <- 123
prop_train        <- 0.7
seuil_classe      <- 0.5   # seuil sur la probabilite predite (M8 / OOB)

n_obs    <- nrow(df_model)
periodes <- df_zscore_rf$TIME_PERIOD          # meme ordre que df_model
annees   <- substr(periodes, 1, 4)

if (anyNA(df_model)) warning("df_model contient des NA : ranger va planter.")

# ---------------------------------------------------------------------------
# 2. Mapping serie -> categorie et libelles (identiques a 5. Construction_graphiques.R)
# ---------------------------------------------------------------------------
categories <- names(data)

col_to_cat <- tibble(indicator = setdiff(names(df_zscore), "TIME_PERIOD")) %>%
  mutate(category = map_chr(indicator, function(cn) {
    m <- categories[str_detect(cn, paste0("^", categories))]
    if (length(m) == 0) NA_character_ else m[1]
  }))

rename_categories <- c(
  "encours_dette"         = "Encours de dette (ménages et SNF)",
  "financement_credit"    = "Financement du crédit",
  "marche_immo"           = "Marché immobilier",
  "financement_marche"    = "Financement sur les marchés",
  "marche_surrevaluation" = "Valorisation sur les marchés")

rename_indicators <- c(
  "encours_dette_CNFSI_DetteH"          = "Dette Ménages",
  "encours_dette_CNFSI_DetteSNF"        = "Dette SNF",
  "encours_dette_BSI1_TC_creditH"       = "Taux croissance crédits particuliers",
  "encours_dette_BSI1_TC_creditSNF"     = "Taux croissance crédits SNF",
  "encours_dette_DSR_DSRH"              = "Service de la dette des ménages",
  "encours_dette_DSR_DSRSNF"            = "Service de la dette des SNF",
  "financement_credit_BLS_Entreprises"  = "Durcissement conditions d'octroi - SNF",
  "financement_credit_BLS_Menages"      = "Durcissement conditions d'octroi - Ménages",
  "financement_credit_MIR_menages"      = "Taux crédits nouveaux ménages",
  "financement_credit_MIR_snf"          = "Taux crédits nouveaux SNF",
  "financement_credit_SAFE"             = "Part SNF avec contraintes de crédit",
  "marche_immo_ISPI"                    = "Surrévaluation prix immo",
  "marche_immo_OCDE"                    = "Prix immo / revenus ménages",
  "financement_marche_SPREAD_OATBD"     = "Spread OAT-Bund",
  "financement_marche_SPREADHY"         = "Spread HY",
  "marche_surrevaluation_CDS"           = "Prime de CDS moyen",
  "marche_surrevaluation_PER"           = "Price Earning Ratio")

rename_tout <- c(setNames(names(rename_categories), unname(rename_categories)),
                 setNames(names(rename_indicators), unname(rename_indicators)))

# Ordre des colonnes de Donnees_RFW_ext.xlsx (rendu identique a la reference)
ordre_colonnes <- c("TIME_PERIOD", "Date", "Indicateur de vulnérabilité", "Taux du CCyB",
                    unname(rename_categories), unname(rename_indicators), "incomplet")

# Taux du CCyB France (trimestriel, derniere valeur du trimestre) --------------
df_ccyb_fr_q <- tryCatch({
  read_excel(chemin_ccyb, sheet = "Sheet 1") %>%
    select(date, France) %>%
    mutate(date = as.Date(date), TIME_PERIOD = format(as.yearqtr(date), "%Y-Q%q")) %>%
    group_by(TIME_PERIOD) %>%
    summarise(ccyb_fr = { x <- France[!is.na(France)]
                          if (length(x) == 0) NA_real_ else dplyr::last(x) },
              .groups = "drop") %>%
    filter(TIME_PERIOD >= "2009-Q1")
}, error = function(e) {
  warning("Taux du CCyB illisible (", conditionMessage(e), ") : colonne laissee vide.")
  tibble(TIME_PERIOD = character(), ccyb_fr = numeric())
})

# ---------------------------------------------------------------------------
# 3. Les fonctions de calcul
# ---------------------------------------------------------------------------

# Performances d'un split : AUC et sensibilite d'abord, le reste en complement.
# Robuste au cas ou le test ne contient qu'une seule classe (AUC alors NA).
perf_split <- function(rf, rf_prob, test) {
  if (nrow(test) == 0) {
    # M8 : pas d'echantillon test, on utilise les predictions out-of-bag
    prob  <- rf_prob$predictions[, "1"]
    obs   <- factor(df_model$crise, levels = c("0", "1"))
    pred  <- factor(ifelse(prob > seuil_classe, 1, 0), levels = c("0", "1"))
    n_test <- NA_integer_
  } else {
    prob   <- predict(rf_prob, data = test)$predictions[, "1"]
    obs    <- factor(test$crise, levels = c("0", "1"))
    pred   <- factor(predict(rf, data = test)$predictions, levels = c("0", "1"))
    n_test <- nrow(test)
  }

  cm <- tryCatch(confusionMatrix(pred, obs, positive = "1"), error = function(e) NULL)

  tibble(
    n_test       = n_test,
    n_crise_test = sum(obs == "1"),
    auc          = tryCatch(as.numeric(auc(roc(obs, prob, levels = c("0", "1"),
                                               direction = "<", quiet = TRUE))),
                            error = function(e) NA_real_),
    sensibilite  = if (is.null(cm)) NA_real_ else unname(cm$byClass["Sensitivity"]),
    specificite  = if (is.null(cm)) NA_real_ else unname(cm$byClass["Specificity"]),
    accuracy     = if (is.null(cm)) NA_real_ else unname(cm$overall["Accuracy"]),
    kappa        = if (is.null(cm)) NA_real_ else unname(cm$overall["Kappa"]),
    oob_error    = rf$prediction.error,
    n_train      = rf$num.samples)
}

# Estimation sur un echantillon d'apprentissage `idx` :
#   - une foret de CLASSIFICATION -> importance par permutation -> poids
#     (c'est elle qui definit les poids, exactement comme dans la reference)
#   - une foret de PROBABILITE sur le meme train -> AUC
# Le test est le complement de `idx` (vide pour M8, d'ou le passage par l'OOB).
estimer <- function(idx, seed = seed_ref) {
  train <- df_model[idx, ]
  test  <- df_model[-idx, ]

  set.seed(seed)
  rf <- ranger(crise ~ ., data = train, num.trees = n_trees,
               importance = "permutation", write.forest = TRUE)
  set.seed(seed)
  rf_prob <- ranger(crise ~ ., data = train, num.trees = n_trees,
                    probability = TRUE, write.forest = TRUE)

  poids <- tibble(var    = names(rf$variable.importance),
                  imp    = rf$variable.importance,
                  weight = pmax(rf$variable.importance, 0) / sum(pmax(rf$variable.importance, 0)))

  list(poids = poids, perf = perf_split(rf, rf_prob, test))
}

# Poids d'une methode = moyenne des poids sur ses echantillons d'apprentissage
# (un seul pour M0-M4 et M8 : la moyenne est alors l'identite).
agreger_poids <- function(liste_poids) {
  bind_rows(liste_poids) %>%
    group_by(var) %>%
    summarise(imp       = mean(imp),
              sd_weight = sd(weight),
              weight    = mean(weight), .groups = "drop") %>%
    mutate(weight = weight / sum(weight)) %>%       # renormalisation
    arrange(desc(weight))
}

# Performances d'une methode = moyenne sur ses splits (+ dispersion si plusieurs).
# On calcule moyennes et dispersions separement : dans un meme summarise(),
# `auc = mean(auc)` masquerait la colonne d'origine pour les lignes suivantes
# (l'ecart-type serait alors celui d'une seule valeur, donc NA).
agreger_perf <- function(detail) {
  moyennes <- detail %>%
    summarise(across(c(auc, sensibilite, specificite, accuracy, kappa,
                       oob_error, n_train, n_test, n_crise_test),
                     ~ mean(.x, na.rm = TRUE)))
  dispersion <- detail %>%
    summarise(auc_sd         = sd(auc, na.rm = TRUE),
              sensibilite_sd = sd(sensibilite, na.rm = TRUE))

  tibble(n_splits        = nrow(detail),
         splits_sans_auc = sum(is.na(detail$auc))) %>%
    bind_cols(moyennes, dispersion) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}

# Contributions serie par serie : z-score x poids, en format long.
# La somme des contributions d'un trimestre redonne exactement l'indicateur.
contributions <- function(poids) {
  df_zscore %>%
    pivot_longer(-TIME_PERIOD, names_to = "indicator", values_to = "z") %>%
    left_join(col_to_cat, by = "indicator") %>%
    filter(!is.na(category)) %>%
    inner_join(poids %>% select(indicator = var, weight), by = "indicator") %>%
    mutate(contribution = z * weight)
}

# Indicateur = z-scores (serie complete) x poids
construire_indicateur <- function(poids) {
  w <- setNames(poids$weight, poids$var)
  as.numeric(as.matrix(df_zscore[, poids$var]) %*% w[poids$var])
}

# ---------------------------------------------------------------------------
# 4. Definition des 9 methodes : chacune = une liste d'index d'apprentissage
# ---------------------------------------------------------------------------

# M0 : split Bernoulli 70/30 seed 123 (methode de reference, Tresor-Eco)
set.seed(seed_ref)
idx_M0 <- which(sample(c(TRUE, FALSE), n_obs, replace = TRUE,
                       prob = c(prop_train, 1 - prop_train)))

# M1 : aleatoire stratifie sur crise (70/30 exact, meme part de crises)
set.seed(seed_ref)
idx_M1 <- as.integer(createDataPartition(df_model$crise, p = prop_train, list = FALSE)[, 1])

# M2 : chronologique (70 % premiers trimestres en train)
idx_M2 <- seq_len(floor(prop_train * n_obs))

# M3 : chronologique inverse (70 % derniers trimestres en train)
idx_M3 <- seq(n_obs - floor(prop_train * n_obs) + 1, n_obs)

# M4 : blocs annuels (on tire des annees entieres, pas des trimestres isoles)
set.seed(seed_ref)
annees_train <- sample(unique(annees), size = round(prop_train * length(unique(annees))))
idx_M4 <- which(annees %in% annees_train)

# M5 : leave-one-crisis-out (test = episode de crise +/- 4 trimestres)
marge  <- 4
r      <- rle(as.integer(as.character(df_model$crise)))
fin_ep <- cumsum(r$lengths)
episodes <- tibble(debut = (fin_ep - r$lengths + 1)[r$values == 1],
                   fin   = fin_ep[r$values == 1]) %>%
  mutate(libelle = paste0(periodes[debut], " -> ", periodes[fin]))
trains_M5 <- map2(episodes$debut, episodes$fin, ~ setdiff(
  seq_len(n_obs), seq(max(1, .x - marge), min(n_obs, .y + marge))))

# M6 : split de reference repete sur 200 seeds (sensibilite au tirage)
n_rep     <- 200
trains_M6 <- map(seq_len(n_rep), function(s) {
  set.seed(s)
  which(sample(c(TRUE, FALSE), n_obs, replace = TRUE, prob = c(prop_train, 1 - prop_train)))
})

# M7 : validation croisee temporelle (fenetre croissante, horizon 8 trimestres)
slices    <- createTimeSlices(seq_len(n_obs), initialWindow = floor(0.5 * n_obs),
                              horizon = 8, fixedWindow = FALSE, skip = 7)
trains_M7 <- unname(slices$train)

# M8 : echantillon complet (pas de split, l'OOB joue le role du test)
idx_M8 <- seq_len(n_obs)

methodes <- list(
  M0_reference           = list(trains = list(idx_M0), seeds = seed_ref,
    description = "Split Bernoulli 70/30 (sample avec prob), seed 123 - reference"),
  M1_stratifie           = list(trains = list(idx_M1), seeds = seed_ref,
    description = "Split aleatoire stratifie sur crise (70/30 exact), seed 123"),
  M2_chrono              = list(trains = list(idx_M2), seeds = seed_ref,
    description = paste0("Chronologique : train ", periodes[1], " - ", periodes[max(idx_M2)])),
  M3_chrono_inverse      = list(trains = list(idx_M3), seeds = seed_ref,
    description = paste0("Chronologique inverse : train ", periodes[min(idx_M3)], " - ", periodes[n_obs])),
  M4_blocs_annuels       = list(trains = list(idx_M4), seeds = seed_ref,
    description = paste0("Blocs annuels 70/30, seed 123. Annees test : ",
                         paste(sort(setdiff(unique(annees), annees_train)), collapse = " "))),
  M5_leave_one_crisis_out = list(trains = trains_M5, seeds = seed_ref,
    description = paste0("Leave-one-crisis-out (marge ", marge, " trimestres), poids moyens sur ",
                         nrow(episodes), " episodes")),
  M6_repetitions         = list(trains = trains_M6, seeds = seq_len(n_rep),
    description = paste0("Split Bernoulli 70/30 repete sur ", n_rep, " seeds, poids moyens")),
  M7_rolling_origin      = list(trains = trains_M7, seeds = seed_ref,
    description = paste0("Validation croisee temporelle (fenetre croissante, ",
                         length(trains_M7), " splits, horizon 8 trimestres), poids moyens")),
  M8_sans_split_OOB      = list(trains = list(idx_M8), seeds = seed_ref,
    description = "Echantillon complet, sans separation train / test (erreur OOB)")
)

# ---------------------------------------------------------------------------
# 5. Estimation : une seule boucle -> poids, performances, indicateur, decomposition
# ---------------------------------------------------------------------------
resultats <- imap(methodes, function(m, nom) {
  cat("Estimation", nom, ":", length(m$trains), "split(s)\n")
  seeds <- rep_len(m$seeds, length(m$trains))
  fits  <- map2(m$trains, seeds, estimer)

  p           <- agreger_poids(map(fits, "poids"))
  perf_detail <- bind_rows(map(fits, "perf"), .id = "split") %>%
    mutate(methode = nom, .before = 1)

  list(nom = nom, description = m$description, poids = p,
       perf        = agreger_perf(perf_detail),
       perf_detail = perf_detail,
       indicateur  = construire_indicateur(p),
       contrib     = contributions(p))
})

# ---------------------------------------------------------------------------
# 6. Indicateurs : un tableau large, une colonne par methode
# ---------------------------------------------------------------------------
df_indicateurs <- df_weighted %>%
  select(TIME_PERIOD, Reference_publiee = indicator_rf)
for (nom in names(resultats)) df_indicateurs[[nom]] <- resultats[[nom]]$indicateur

df_indicateurs <- df_indicateurs %>%
  filter(TIME_PERIOD >= date_debut_export, TIME_PERIOD != "NA-QNA") %>%
  arrange(TIME_PERIOD) %>%
  mutate(Date      = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
         incomplet = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)) %>%
  relocate(Date, .after = TIME_PERIOD)

cols_ind <- setdiff(names(df_indicateurs), c("TIME_PERIOD", "Date", "incomplet"))

# ---------------------------------------------------------------------------
# 7. Decomposition par categorie
#    a) format long : source unique pour tableau croise / graphique
#    b) une feuille par methode, au format exact de Donnees_RFW_ext.xlsx
# ---------------------------------------------------------------------------

# a) Long : TIME_PERIOD x methode x categorie
df_contrib_cat <- imap_dfr(resultats, function(r, nom) {
  r$contrib %>%
    group_by(TIME_PERIOD, category) %>%
    summarise(contribution = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
    mutate(methode = nom)
}) %>%
  filter(TIME_PERIOD >= date_debut_export, TIME_PERIOD != "NA-QNA") %>%
  mutate(Date      = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
         categorie = unname(rename_categories[category])) %>%
  select(TIME_PERIOD, Date, methode, categorie, contribution) %>%
  arrange(methode, TIME_PERIOD, categorie)

# Verification : la somme des 5 categories doit redonner l'indicateur
verif <- df_contrib_cat %>%
  group_by(TIME_PERIOD, methode) %>%
  summarise(somme_cat = sum(contribution), .groups = "drop") %>%
  left_join(df_indicateurs %>%
              pivot_longer(all_of(names(resultats)), names_to = "methode",
                           values_to = "indicateur") %>%
              select(TIME_PERIOD, methode, indicateur),
            by = c("TIME_PERIOD", "methode")) %>%
  mutate(ecart = abs(somme_cat - indicateur))
cat("Ecart max somme des categories vs indicateur :", max(verif$ecart, na.rm = TRUE), "\n")

# b) Une feuille par methode, mise en forme du graphique de reference
feuille_methode <- function(r) {
  cats <- r$contrib %>%
    group_by(TIME_PERIOD, category) %>%
    summarise(contribution_cat = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(id_cols = TIME_PERIOD, names_from = category, values_from = contribution_cat)

  inds <- r$contrib %>%
    pivot_wider(id_cols = TIME_PERIOD, names_from = indicator, values_from = contribution)

  tibble(TIME_PERIOD = df_zscore$TIME_PERIOD, indicator_rf = r$indicateur) %>%
    left_join(df_ccyb_fr_q, by = "TIME_PERIOD") %>%
    left_join(cats, by = "TIME_PERIOD") %>%
    left_join(inds, by = "TIME_PERIOD") %>%
    filter(TIME_PERIOD >= date_debut_export, TIME_PERIOD != "NA-QNA") %>%
    arrange(TIME_PERIOD) %>%
    mutate(Date      = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
           incomplet = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)) %>%
    rename("Indicateur de vulnérabilité" = indicator_rf,
           "Taux du CCyB" = ccyb_fr,
           any_of(rename_tout)) %>%
    select(any_of(ordre_colonnes), everything())
}

feuilles_methodes <- map(resultats, feuille_methode)

# ---------------------------------------------------------------------------
# 8. Correlations avec l'indicateur de reference
# ---------------------------------------------------------------------------
mat_cor <- cor(df_indicateurs[cols_ind], use = "pairwise.complete.obs")

df_correlations <- as.data.frame(mat_cor) %>%
  mutate(methode = rownames(mat_cor), .before = 1)

df_comparaison <- map_dfr(names(resultats), function(nom) {
  p <- left_join(imp_df %>% select(var, w_ref = weight),
                 resultats[[nom]]$poids %>% select(var, w = weight), by = "var")
  bind_cols(
    tibble(methode            = nom,
           description        = resultats[[nom]]$description,
           cor_indicateur_ref = mat_cor["Reference_publiee", nom],
           cor_poids_pearson  = cor(p$w_ref, p$w),
           cor_poids_spearman = cor(p$w_ref, p$w, method = "spearman"),
           ecart_abs_moyen    = mean(abs(p$w_ref - p$w)),
           ecart_abs_max      = max(abs(p$w_ref - p$w)),
           var_ecart_max      = p$var[which.max(abs(p$w_ref - p$w))],
           nb_poids_nuls      = sum(p$w == 0)),
    resultats[[nom]]$perf)
})

# Tableau de performances seul (AUC et sensibilite en tete)
df_performances <- map_dfr(resultats, function(r) bind_cols(tibble(methode = r$nom), r$perf)) %>%
  select(methode, n_splits, auc, auc_sd, sensibilite, sensibilite_sd,
         specificite, accuracy, kappa, oob_error, n_train, n_test,
         n_crise_test, splits_sans_auc)

# Detail split par split (utile pour M5, M6, M7)
df_perf_detail <- map_dfr(resultats, "perf_detail")

print(df_performances %>% select(methode, auc, sensibilite, specificite, oob_error))
if (any(df_performances$splits_sans_auc > 0, na.rm = TRUE))
  cat("Attention : certains splits n'ont aucune crise en test, AUC non calculable ",
      "(colonne splits_sans_auc).\n")

# Poids par categorie : ou va le poids total selon la methode ?
df_poids_cat <- map_dfr(names(resultats), function(nom) {
  resultats[[nom]]$poids %>%
    left_join(col_to_cat, by = c("var" = "indicator")) %>%
    group_by(category) %>%
    summarise(poids_total = sum(weight), .groups = "drop") %>%
    mutate(methode = nom, categorie = unname(rename_categories[category]))
}) %>%
  pivot_wider(id_cols = categorie, names_from = methode, values_from = poids_total)
print(df_poids_cat)

# ---------------------------------------------------------------------------
# 9. Poids : une colonne par methode
# ---------------------------------------------------------------------------
df_poids <- imp_df %>% select(var, Reference_publiee = weight)
for (nom in names(resultats)) {
  df_poids <- left_join(df_poids,
                        resultats[[nom]]$poids %>% select(var, !!nom := weight), by = "var")
}
df_poids <- df_poids %>%
  left_join(col_to_cat, by = c("var" = "indicator")) %>%
  mutate(categorie = unname(rename_categories[category])) %>%
  select(var, categorie, everything(), -category) %>%
  arrange(desc(Reference_publiee))

# Ecart-type des poids pour les methodes moyennees (M5, M6, M7)
df_poids_sd <- map_dfr(names(resultats), function(nom) {
  resultats[[nom]]$poids %>% transmute(methode = nom, var, weight, sd_weight)
}) %>% filter(!is.na(sd_weight))

# ---------------------------------------------------------------------------
# 10. Export Excel
#     "Indicateurs"       -> graphique superpose des 9 indicateurs
#     "Contributions_cat" -> tableau croise dynamique / barres empilees
#     "Performances"      -> AUC et sensibilite par methode
#     une feuille par methode -> graphique de reference, methode par methode
# ---------------------------------------------------------------------------
feuilles <- c(
  list("Indicateurs"       = df_indicateurs,
       "Contributions_cat" = df_contrib_cat,
       "Performances"      = df_performances,
       "Perf_detail"       = df_perf_detail,
       "Correlations"      = df_correlations,
       "Comparaison"       = df_comparaison,
       "Poids"             = df_poids,
       "Poids_categorie"   = df_poids_cat,
       "Poids_sd"          = df_poids_sd),
  feuilles_methodes                       # noms = M0_reference, M1_stratifie, ...
)
write_xlsx(feuilles, path = chemin_export)
cat("Export termine :", chemin_export, "\n")

# ---------------------------------------------------------------------------
# 11. Graphiques sous R (verification avant Excel)
# ---------------------------------------------------------------------------

# a) Les 9 indicateurs superposes
graph_superpose <- df_indicateurs %>%
  pivot_longer(all_of(cols_ind), names_to = "methode", values_to = "valeur") %>%
  mutate(reference = methode == "Reference_publiee") %>%
  ggplot(aes(Date, valeur, colour = methode, linewidth = reference)) +
  geom_line() +
  scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 1.3), guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "Indicateur de vulnerabilite selon la methode de split",
       x = NULL, y = "Indicateur (z-scores ponderes)", colour = NULL)

print(graph_superpose)

# b) Decomposition par categorie, une facette par methode (graphique de reference)
graph_decomposition <- df_contrib_cat %>%
  ggplot(aes(Date, contribution, fill = categorie)) +
  geom_col(width = 80) +
  geom_line(data = df_indicateurs %>%
              pivot_longer(all_of(names(resultats)), names_to = "methode",
                           values_to = "indicateur"),
            aes(Date, indicateur), inherit.aes = FALSE, linewidth = 0.6) +
  facet_wrap(~ methode) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Decomposition de l'indicateur par categorie, selon la methode de split",
       x = NULL, y = "Contribution", fill = NULL)

print(graph_decomposition)
