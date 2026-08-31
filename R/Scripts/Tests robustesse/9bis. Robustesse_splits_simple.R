## ---------------------------------------------------------------------------##
##   Robustesse des poids du Random Forest : methodes de split M0 a M8         ##
##   Version simplifiee : poids -> indicateur -> correlations -> export Excel   ##
## ---------------------------------------------------------------------------##
#
# Principe : chaque methode M0..M8 se resume a UNE liste d'echantillons
# d'apprentissage (un seul pour M0-M4 et M8, plusieurs pour M5, M6 et M7).
# Une seule boucle estime les forets, moyenne les poids, et reconstruit
# l'indicateur. Aucun bloc a decommenter : on lance le script en entier.
#
# Objets fournis par 5. Random_Forest.R -> 5. Data_pour_RF.R -> 3. Standardisation.R :
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
date_debut_export <- "2009-Q1"   # meme fenetre que Donnees_RFW_ext.xlsx
n_trees           <- 180
seed_ref          <- 123
prop_train        <- 0.7

n_obs    <- nrow(df_model)
periodes <- df_zscore_rf$TIME_PERIOD          # meme ordre que df_model
annees   <- substr(periodes, 1, 4)

if (anyNA(df_model)) warning("df_model contient des NA : ranger va planter.")

# ---------------------------------------------------------------------------
# 2. Trois fonctions, c'est tout
# ---------------------------------------------------------------------------

# Poids d'une foret estimee sur `idx` : importance par permutation,
# valeurs negatives ramenees a 0, puis normalisation a somme 1.
poids_rf <- function(idx, seed = seed_ref) {
  set.seed(seed)
  rf <- ranger(crise ~ ., data = df_model[idx, ], num.trees = n_trees,
               importance = "permutation", write.forest = TRUE)
  tibble(var       = names(rf$variable.importance),
         imp       = rf$variable.importance,
         weight    = pmax(rf$variable.importance, 0) / sum(pmax(rf$variable.importance, 0)),
         oob_error = rf$prediction.error)
}

# Poids d'une methode = moyenne des poids sur ses echantillons d'apprentissage
# (un seul pour M0-M4 et M8 : la moyenne est alors l'identite).
poids_methode <- function(trains, seeds = seed_ref) {
  seeds <- rep_len(seeds, length(trains))
  map2_dfr(trains, seeds, poids_rf) %>%
    group_by(var) %>%
    summarise(imp       = mean(imp),
              sd_weight = sd(weight),
              oob_error = mean(oob_error),
              weight    = mean(weight), .groups = "drop") %>%
    mutate(weight = weight / sum(weight)) %>%       # renormalisation
    arrange(desc(weight))
}

# Indicateur = z-scores (serie complete) x poids
construire_indicateur <- function(poids) {
  w <- setNames(poids$weight, poids$var)
  as.numeric(as.matrix(df_zscore[, poids$var]) %*% w[poids$var])
}

# ---------------------------------------------------------------------------
# 3. Definition des 9 methodes : chacune = une liste d'index d'apprentissage
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
# 4. Estimation : une seule boucle
# ---------------------------------------------------------------------------
resultats <- imap(methodes, function(m, nom) {
  cat("Estimation", nom, "(", length(m$trains), "foret(s) )\n")
  p <- poids_methode(m$trains, m$seeds)
  list(nom = nom, description = m$description, poids = p,
       indicateur = construire_indicateur(p))
})

# ---------------------------------------------------------------------------
# 5. Indicateurs : un tableau large, une colonne par methode
# ---------------------------------------------------------------------------
df_indicateurs <- df_weighted %>%
  select(TIME_PERIOD, Reference_publiee = indicator_rf)
for (nom in names(resultats)) df_indicateurs[[nom]] <- resultats[[nom]]$indicateur

df_indicateurs <- df_indicateurs %>%
  filter(TIME_PERIOD >= date_debut_export, TIME_PERIOD != "NA-QNA") %>%
  arrange(TIME_PERIOD) %>%
  mutate(Date       = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q")),
         incomplet  = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)) %>%
  relocate(Date, .after = TIME_PERIOD)

cols_ind <- setdiff(names(df_indicateurs), c("TIME_PERIOD", "Date", "incomplet"))

# ---------------------------------------------------------------------------
# 6. Correlations avec l'indicateur de reference
# ---------------------------------------------------------------------------
mat_cor <- cor(df_indicateurs[cols_ind], use = "pairwise.complete.obs")

df_correlations <- as.data.frame(mat_cor) %>%
  mutate(methode = rownames(mat_cor), .before = 1)

df_comparaison <- map_dfr(names(resultats), function(nom) {
  p <- left_join(imp_df %>% select(var, w_ref = weight),
                 resultats[[nom]]$poids %>% select(var, w = weight), by = "var")
  tibble(methode              = nom,
         description          = resultats[[nom]]$description,
         cor_indicateur_ref   = mat_cor["Reference_publiee", nom],
         cor_poids_pearson    = cor(p$w_ref, p$w),
         cor_poids_spearman   = cor(p$w_ref, p$w, method = "spearman"),
         ecart_abs_moyen      = mean(abs(p$w_ref - p$w)),
         ecart_abs_max        = max(abs(p$w_ref - p$w)),
         var_ecart_max        = p$var[which.max(abs(p$w_ref - p$w))],
         nb_poids_nuls        = sum(p$w == 0),
         oob_error            = mean(resultats[[nom]]$poids$oob_error))
})
print(df_comparaison %>% select(methode, cor_indicateur_ref, cor_poids_pearson, oob_error))

# ---------------------------------------------------------------------------
# 7. Poids : une colonne par methode
# ---------------------------------------------------------------------------
df_poids <- imp_df %>% select(var, Reference_publiee = weight)
for (nom in names(resultats)) {
  df_poids <- left_join(df_poids,
                        resultats[[nom]]$poids %>% select(var, !!nom := weight), by = "var")
}
df_poids <- df_poids %>% arrange(desc(Reference_publiee))

# Ecart-type des poids pour les methodes moyennees (M5, M6, M7)
df_poids_sd <- map_dfr(names(resultats), function(nom) {
  resultats[[nom]]$poids %>% transmute(methode = nom, var, weight, sd_weight)
}) %>% filter(!is.na(sd_weight))

# ---------------------------------------------------------------------------
# 8. Export Excel : la feuille "Indicateurs" est prete pour le graphique
#    (selectionner Date + les colonnes M0..M8 -> graphique en courbes)
# ---------------------------------------------------------------------------
write_xlsx(list(
  "Indicateurs"  = df_indicateurs,   # <- feuille du graphique superpose
  "Correlations" = df_correlations,
  "Comparaison"  = df_comparaison,
  "Poids"        = df_poids,
  "Poids_sd"     = df_poids_sd
), path = chemin_export)
cat("Export termine :", chemin_export, "\n")

# ---------------------------------------------------------------------------
# 9. Le meme graphique directement sous R (verification avant Excel)
# ---------------------------------------------------------------------------
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
