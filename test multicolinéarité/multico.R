## ---------------------------------------------------------------------------##
##   Diagnostic de multicolinearite des variables candidates de l'indicateur   ##
##   + export Excel des matrices de correlation pour le memoire                ##
## ---------------------------------------------------------------------------##
#
# Objet : partir des 19 variables candidates, documenter leur structure de
# correlation, montrer que deux d'entre elles (Spread IG et Taux OAT 10Y) sont
# redondantes, et produire les tableaux qui alimentent le memoire :
#   - la matrice de correlation des 17 variables finalement retenues
#     (chap. 4, "Interpretation des importances sous predicteurs correles") ;
#   - la correlation moyenne intra-categorie vs inter-categories, qui etaye
#     l'affirmation "les dix-sept variables sont fortement correlees au sein
#     des categories" ;
#   - la matrice des 19 candidates et les paires |r| > 0,8, qui justifient
#     l'ecartement de Spread IG et de Taux OAT 10Y (chap. 2, choix des variables).
#
# Sortie : Matrices_correlation_memoire.xlsx (une feuille par tableau).

library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(performance)

# 0. Chemins -----------------------------------------------------------------
dossier <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/test multicolinéarité"
if (!dir.exists(dossier)) dossier <- getwd()   # execution hors du lecteur reseau

path        <- file.path(dossier, "Donnees_RFW_ext.xlsx")
fichier_out <- file.path(dossier, "Matrices_correlation_memoire.xlsx")

data <- as.data.frame(read_excel(path, sheet = "Donnees"))
stopifnot(ncol(data) == 19)

# 1. Nomenclature : libelles et categories du memoire -------------------------
# Les noms de colonnes du fichier sont des abreviations de travail. Les tableaux
# exportes reprennent les libelles et les cinq categories du memoire (section
# "Le choix des dix-sept variables"), pour etre inserables tels quels.
meta <- data.frame(
  colonne = c("Dette Ménages", "Dette SNF",
              "Taux croissance crédits particuliers", "Taux croissance crédits SNF",
              "DSR Ménages", "DSR SNF",
              "Taux crédits nouveaux ménages", "Taux crédits nouveaux SNF",
              "Durcissement conditions d'octroi - SNF",
              "Durcissement conditions d'octroi - Ménages",
              "Part SNF avec contraintes de crédit",
              "Surrévaluation prix immo", "Prix immo / revenus ménages",
              "Spread HY", "Spread IG", "Spread OAT-Bund", "Taux OAT 10Y",
              "Prime de CDS moyen", "Price Earning Ratio"),
  libelle = c("Ratio dette/PIB des ménages", "Ratio dette/PIB des SNF",
              "Croissance des prêts aux ménages", "Croissance des prêts aux SNF",
              "Debt service ratio des ménages", "Debt service ratio des SNF",
              "Taux des nouveaux crédits aux ménages", "Taux des nouveaux crédits aux SNF",
              "Durcissement des critères d'octroi – SNF (BLS)",
              "Durcissement des critères d'octroi – ménages (BLS)",
              "Part des SNF sous contrainte de crédit (SAFE)",
              "Surévaluation des prix de l'immobilier (ISPI)",
              "Prix de l'immobilier / revenus des ménages",
              "Spread High Yield – mid swap", "Spread Investment Grade – mid swap",
              "Spread OAT–Bund", "Taux OAT 10 ans",
              "Prime de CDS moyenne (CAC 40)", "Price earning ratio"),
  categorie = c(rep("Encours de dette des ménages et des SNF", 6),
                rep("Conditions de financement du crédit", 5),
                rep("Marché immobilier", 2),
                "Conditions de financement sur les marchés", "(écartée)",
                "Conditions de financement sur les marchés", "(écartée)",
                rep("Valorisation sur les marchés", 2)),
  retenue = c(rep(TRUE, 14), FALSE, TRUE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE)

stopifnot(identical(sort(meta$colonne), sort(colnames(data))))
data <- data[, meta$colonne]              # ordre par categorie
vars_retenues <- meta$colonne[meta$retenue]   # les 17 de l'indicateur

# 2. Fonctions ----------------------------------------------------------------

# Matrice de correlation, libellee comme dans le memoire.
matrice_corr <- function(df, cols) {
  m <- cor(df[, cols], use = "pairwise.complete.obs")
  lib <- meta$libelle[match(cols, meta$colonne)]
  dimnames(m) <- list(lib, lib)
  m
}

# Paires |r| > seuil, sans doublon, triees par correlation decroissante.
paires_fortes <- function(m, seuil = 0.8) {
  idx <- which(abs(m) > seuil & upper.tri(m), arr.ind = TRUE)
  if (nrow(idx) == 0)
    return(data.frame(Variable_1 = character(), Variable_2 = character(),
                      Correlation = numeric(), stringsAsFactors = FALSE))
  data.frame(Variable_1  = rownames(m)[idx[, 1]],
             Variable_2  = colnames(m)[idx[, 2]],
             Correlation = m[idx],
             stringsAsFactors = FALSE) %>%
    arrange(desc(abs(Correlation)))
}

# VIF de chaque variable, obtenu en la regressant sur toutes les autres.
# Symetrique (toutes les variables sont couvertes), contrairement a un unique
# lm() ou la variable expliquee n'a pas de VIF.
vif_complet <- function(df, cols) {
  x <- as.data.frame(df[, cols]); names(x) <- paste0("V", seq_along(cols))
  vif <- vapply(names(x), function(v) {
    r2 <- summary(lm(reformulate(setdiff(names(x), v), v), data = x))$r.squared
    1 / (1 - r2)
  }, numeric(1))
  data.frame(Variable  = meta$libelle[match(cols, meta$colonne)],
             Categorie = meta$categorie[match(cols, meta$colonne)],
             VIF       = as.numeric(vif),
             Tolerance = 1 / as.numeric(vif),
             stringsAsFactors = FALSE) %>%
    mutate(Diagnostic = case_when(VIF >= 10 ~ "Colinéarité élevée",
                                  VIF >= 5  ~ "Colinéarité modérée",
                                  TRUE      ~ "Faible")) %>%
    arrange(desc(VIF))
}

# Correlation absolue moyenne intra-categorie et inter-categories.
# C'est le tableau qui documente l'affirmation du chapitre 4 selon laquelle
# la correlation se concentre a l'interieur des categories.
corr_par_categorie <- function(df, cols) {
  m   <- cor(df[, cols], use = "pairwise.complete.obs")
  cat_ <- meta$categorie[match(cols, meta$colonne)]
  idx <- which(upper.tri(m), arr.ind = TRUE)
  paires <- data.frame(c1 = cat_[idx[, 1]], c2 = cat_[idx[, 2]],
                       r = abs(m[idx]), stringsAsFactors = FALSE)
  intra <- paires %>% filter(c1 == c2) %>% group_by(Catégorie = c1) %>%
    summarise(`Nb de paires` = n(), `|r| moyen` = mean(r),
              `|r| médian` = median(r), `|r| max` = max(r), .groups = "drop")
  inter <- paires %>% filter(c1 != c2) %>%
    summarise(Catégorie = "Ensemble des paires inter-catégories",
              `Nb de paires` = n(), `|r| moyen` = mean(r),
              `|r| médian` = median(r), `|r| max` = max(r))
  bind_rows(intra, inter)
}

# Carte de chaleur (conservee pour le controle visuel a l'ecran).
heatmap_corr <- function(m, titre = NULL) {
  d <- melt(m)
  ggplot(d, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Corrélation") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.text = element_text(size = 9)) +
    labs(x = "", y = "", title = titre)
}

# 3. Diagnostic sur les 19 candidates ----------------------------------------
corr_19   <- matrice_corr(data, meta$colonne)
paires_19 <- paires_fortes(corr_19)
vif_19    <- vif_complet(data, meta$colonne)
print(paires_19)
print(vif_19)
print(heatmap_corr(corr_19, "19 variables candidates"))

# 4. Retraits successifs ------------------------------------------------------
# Spread IG puis Taux OAT 10Y sont les deux variables dont le VIF s'effondre
# une fois retirees : elles n'apportent pas d'information propre.
sans_ig  <- setdiff(meta$colonne, "Spread IG")
vif_18   <- vif_complet(data, sans_ig)
vif_17   <- vif_complet(data, vars_retenues)
print(vif_18); print(vif_17)

# Verification avec performance::check_collinearity sur la specification
# initiale (Dette Menages expliquee par les autres candidates).
lm_19 <- lm(reformulate(sprintf("`%s`", setdiff(meta$colonne, "Dette Ménages")),
                        "`Dette Ménages`"), data = data)
lm_17 <- lm(reformulate(sprintf("`%s`", setdiff(vars_retenues, "Dette Ménages")),
                        "`Dette Ménages`"), data = data)
print(check_collinearity(lm_19))
print(check_collinearity(lm_17))

# 5. Les 17 variables retenues ------------------------------------------------
corr_17   <- matrice_corr(data, vars_retenues)
paires_17 <- paires_fortes(corr_17)
cat_17    <- corr_par_categorie(data, vars_retenues)
print(paires_17); print(cat_17)
print(heatmap_corr(corr_17, "17 variables retenues"))

# 6. Variante : Spread HY exprime en ecart au Spread IG -----------------------
# Piste testee pour conserver l'information IG sans la colinearite. Documentee
# ici mais non retenue : l'indicateur utilise le spread HY - mid swap brut.
data_hy_ig <- data
data_hy_ig$`Spread HY` <- data$`Spread HY` - data$`Spread IG`
vif_hy_ig  <- vif_complet(data_hy_ig, setdiff(meta$colonne, "Spread IG"))

# 7. Export Excel -------------------------------------------------------------
arrondir <- function(df, n = 3) df %>% mutate(across(where(is.numeric), ~ round(.x, n)))

# Une matrice devient un data.frame dont la 1re colonne porte les libelles.
mat_vers_df <- function(m, n = 2) {
  data.frame(Variable = rownames(m), round(m, n), check.names = FALSE,
             stringsAsFactors = FALSE)
}

lecture <- data.frame(
  Feuille = c("Corr_17_retenues", "Paires_fortes_17", "Correlation_categories",
              "VIF_17_retenues", "Corr_19_candidates", "Paires_fortes_19",
              "VIF_19_candidates", "VIF_18_sans_IG", "VIF_variante_HY_moins_IG",
              "Variables"),
  Contenu = c(
    "Matrice de corrélation des 17 variables de l'indicateur",
    "Paires de variables retenues dont |r| > 0,8",
    "Corrélation absolue moyenne intra-catégorie et inter-catégories",
    "VIF de chacune des 17 variables retenues",
    "Matrice de corrélation des 19 variables candidates",
    "Paires de variables candidates dont |r| > 0,8",
    "VIF des 19 candidates, avant tout retrait",
    "VIF après retrait du seul Spread Investment Grade",
    "VIF de la variante où le spread HY est exprimé en écart au spread IG (non retenue)",
    "Correspondance nom de colonne / libellé / catégorie / variable retenue"),
  stringsAsFactors = FALSE)
lecture$Usage_memoire <- c(
  "Chap. 4 — importances sous prédicteurs corrélés ; annexe",
  "Chap. 4 — paires responsables de l'instabilité des poids",
  "Chap. 4 — étaye « fortement corrélées au sein des catégories »",
  "Chap. 2 — colinéarité résiduelle du jeu final",
  "Chap. 2 — choix des variables ; annexe",
  "Chap. 2 — justifie l'écartement de Spread IG et Taux OAT 10 ans",
  "Chap. 2 — diagnostic initial",
  "Chap. 2 — étape intermédiaire du retrait",
  "Chap. 2 — variante testée, non retenue",
  "Annexe — nomenclature")

feuilles <- list(
  "Lecture"                  = lecture,
  "Corr_17_retenues"         = mat_vers_df(corr_17),
  "Paires_fortes_17"         = arrondir(paires_17),
  "Correlation_categories"   = arrondir(cat_17),
  "VIF_17_retenues"          = arrondir(vif_17),
  "Corr_19_candidates"       = mat_vers_df(corr_19),
  "Paires_fortes_19"         = arrondir(paires_19),
  "VIF_19_candidates"        = arrondir(vif_19),
  "VIF_18_sans_IG"           = arrondir(vif_18),
  "VIF_variante_HY_moins_IG" = arrondir(vif_hy_ig),
  "Variables"                = meta %>%
    transmute(`Nom de colonne` = colonne, `Libellé` = libelle,
              `Catégorie` = categorie,
              `Retenue dans l'indicateur` = ifelse(retenue, "oui", "non")))

wb <- createWorkbook()
style_entete <- createStyle(textDecoration = "bold", halign = "center",
                            valign = "center", wrapText = TRUE,
                            fgFill = "#DDEBF7", border = "TopBottom")

for (nm in names(feuilles)) {
  df <- feuilles[[nm]]
  addWorksheet(wb, nm)
  writeData(wb, nm, df, headerStyle = style_entete)
  freezePane(wb, nm, firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, nm, cols = 1, widths = 46)
  setColWidths(wb, nm, cols = 2:(ncol(df)), widths = if (grepl("^Corr_", nm)) 7 else "auto")
  # degrade bleu-blanc-rouge sur les cellules de correlation
  if (grepl("^Corr_", nm))
    conditionalFormatting(wb, nm, cols = 2:ncol(df), rows = 2:(nrow(df) + 1),
                          type = "colourScale",
                          style = c("#2166AC", "#FFFFFF", "#B2182B"),
                          rule = c(-1, 0, 1))
  if (grepl("^VIF_", nm))
    conditionalFormatting(wb, nm, cols = 3, rows = 2:(nrow(df) + 1),
                          rule = ">=5", style = createStyle(fontColour = "#9C0006",
                                                            bgFill = "#FFC7CE"))
}
saveWorkbook(wb, fichier_out, overwrite = TRUE)
cat("Ecrit :", fichier_out, "\n")
