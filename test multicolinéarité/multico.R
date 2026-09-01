## ---------------------------------------------------------------------------##
##   Diagnostic de multicolinearite : 19 variables candidates -> 17 retenues   ##
##   Produit les tableaux et figures de la section 2.2 et de l'annexe B.1      ##
## ---------------------------------------------------------------------------##
#
# Alimente directement le memoire :
#   - tableau \label{tab:multico}  (annexe B.1) : paires |r| > 0,8 et VIF
#     avant / apres traitement  -> feuilles "Paires_correlees" et "VIF_avant_apres"
#   - figure \label{fig:multico-19} -> figures/multico/correlation_19_variables.pdf
#   - figure \label{fig:multico-17} -> figures/multico/correlation_17_variables.pdf
#   - section 2.2 "Le traitement de la multicolinearite" pour les valeurs citees
#     dans le corps du texte.
#
# Conventions reprises telles quelles du memoire (note du tableau tab:multico) :
#   - le spread High Yield est exprime NET du spread investment grade ;
#   - les VIF sont ceux de la regression auxiliaire ayant la dette des menages
#     pour variable dependante, a l'exclusion des deux taux de croissance du
#     credit. La feuille "VIF_complet" fournit en complement un VIF symetrique,
#     calcule pour toutes les variables sans exception (voir section 7).
#
# Sorties : Matrices_correlation_memoire.xlsx + deux PDF dans figures/multico/.

library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(performance)

# 0. Chemins -----------------------------------------------------------------
dossier <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/test multicolinéarité"
if (!dir.exists(dossier)) dossier <- getwd()        # execution hors du lecteur reseau
dossier_projet <- dirname(dossier)
dossier_fig    <- file.path(dossier_projet, "figures", "multico")
dir.create(dossier_fig, showWarnings = FALSE, recursive = TRUE)

path        <- file.path(dossier, "Donnees_RFW_ext.xlsx")
fichier_out <- file.path(dossier, "Matrices_correlation_memoire.xlsx")

SEUIL_CORR <- 0.80   # seuil retenu dans le memoire
SEUIL_VIF  <- 10     # seuil usuel de colinearite severe
SEUIL_AFF  <- 0.78   # seuil d'affichage, pour rendre visibles les cas limites

data_brut <- as.data.frame(read_excel(path, sheet = "Donnees"))
stopifnot(ncol(data_brut) == 19)
cat("Periode :", nrow(data_brut), "trimestres\n")

# 1. Nomenclature : libelles et categories du memoire -------------------------
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
  libelle = c("Dette ménages", "Dette SNF",
              "Croissance des crédits ménages", "Croissance des crédits SNF",
              "Service de la dette ménages", "Service de la dette SNF",
              "Taux crédits nouveaux ménages", "Taux crédits nouveaux SNF",
              "Durcissement critères d'octroi SNF", "Durcissement critères d'octroi ménages",
              "Contraintes de crédit SNF (SAFE)",
              "Surévaluation prix immobiliers", "Prix immobiliers / revenus",
              "Spread HY (net de l'IG)", "Spread IG", "Spread OAT–Bund",
              "Taux OAT 10 ans", "Prime de CDS", "Price earning ratio"),
  categorie = c(rep("Encours de dette des ménages et des SNF", 6),
                rep("Conditions de financement du crédit", 5),
                rep("Marché immobilier", 2),
                "Conditions de financement sur les marchés", "(retirée)",
                "Conditions de financement sur les marchés", "(retirée)",
                rep("Valorisation sur les marchés", 2)),
  retenue = c(rep(TRUE, 14), FALSE, TRUE, FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE)

stopifnot(identical(sort(meta$colonne), sort(colnames(data_brut))))
data_brut     <- data_brut[, meta$colonne]              # ordre par categorie
vars_retirees <- meta$colonne[!meta$retenue]            # Spread IG, Taux OAT 10Y
vars_retenues <- meta$colonne[meta$retenue]             # les 17 de l'indicateur

# Convention du memoire : spread High Yield net du spread investment grade.
data <- data_brut
data$`Spread HY` <- data_brut$`Spread HY` - data_brut$`Spread IG`

lib <- function(cols) meta$libelle[match(cols, meta$colonne)]

# 2. Matrices de correlation --------------------------------------------------
matrice_corr <- function(df, cols) {
  m <- cor(df[, cols], use = "pairwise.complete.obs")
  dimnames(m) <- list(lib(cols), lib(cols))
  m
}

corr_19 <- matrice_corr(data,      meta$colonne)
corr_17 <- matrice_corr(data,      vars_retenues)
corr_19_hy_brut <- matrice_corr(data_brut, meta$colonne)   # avant correction du HY

# 3. Paires correlees ---------------------------------------------------------
# Le traitement applique a chaque paire suit la regle du memoire : une paire est
# rompue par le retrait d'une variable lorsqu'elle fait intervenir le spread IG
# ou le taux de l'OAT, redondances de substitution ; les paires internes a la
# categorie d'endettement sont conservees, la correlation y traduisant une
# tendance commune et non une redondance de contenu.
paires_correlees <- function(m, cols, seuil = SEUIL_AFF) {
  idx <- which(abs(m) > seuil & upper.tri(m), arr.ind = TRUE)
  if (nrow(idx) == 0) return(NULL)
  retirees <- lib(vars_retirees)
  data.frame(`Variable 1` = rownames(m)[idx[, 1]],
             `Variable 2` = colnames(m)[idx[, 2]],
             Correlation  = round(m[idx], 2),
             check.names = FALSE, stringsAsFactors = FALSE) %>%
    mutate(
      # teste la valeur exacte, non la valeur arrondie : une paire a -0,7964
      # s'affiche a -0,80 sans pour autant franchir le seuil.
      `Au-dessus de 0,8` = ifelse(abs(m[idx]) >= SEUIL_CORR, "oui", "non"),
      Traitement = case_when(
        `Variable 1` %in% retirees ~ paste(`Variable 1`, "retirée"),
        `Variable 2` %in% retirees ~ paste(`Variable 2`, "retirée"),
        TRUE                       ~ "conservées (tendance commune)"),
      `Correlation exacte` = round(m[idx], 4)) %>%
    arrange(desc(abs(Correlation)))
}
paires <- paires_correlees(corr_19, meta$colonne)
print(paires)

# 4. VIF, specification du memoire --------------------------------------------
# Regression auxiliaire de la dette des menages sur les autres variables, hors
# taux de croissance du credit. C'est la specification dont sont issues les
# valeurs du tableau tab:multico.
regresseurs <- setdiff(meta$colonne,
                       c("Dette Ménages", "Taux croissance crédits particuliers",
                         "Taux croissance crédits SNF"))
vif_spec <- function(cols) {
  v <- as.data.frame(check_collinearity(
    lm(reformulate(sprintf("`%s`", cols), "`Dette Ménages`"), data = data)))
  setNames(round(v$VIF, 1), gsub("`", "", v$Term))
}
vif_19 <- vif_spec(regresseurs)
vif_18 <- vif_spec(setdiff(regresseurs, "Spread IG"))                  # retrait de l'IG
vif_17 <- vif_spec(setdiff(regresseurs, vars_retirees))                # + retrait de l'OAT

valeur <- function(v, n) ifelse(n %in% names(v), sprintf("%.1f", v[n]), "retirée")
vif_avant_apres <- data.frame(
  Variable         = lib(regresseurs),
  `Catégorie`      = meta$categorie[match(regresseurs, meta$colonne)],
  `VIF, 19 variables`     = sapply(regresseurs, valeur, v = vif_19),
  `VIF, 18 (sans IG)`     = sapply(regresseurs, valeur, v = vif_18),
  `VIF, 17 variables`     = sapply(regresseurs, valeur, v = vif_17),
  check.names = FALSE, stringsAsFactors = FALSE) %>%
  mutate(`Reporté dans le mémoire` =
           ifelse(pmax(vif_19[regresseurs],
                       ifelse(is.na(vif_17[regresseurs]), 0, vif_17[regresseurs])) >= 5,
                  "oui", "non (VIF < 5 partout)")) %>%
  arrange(desc(as.numeric(`VIF, 19 variables`)))
print(vif_avant_apres)

# 5. VIF symetrique, en complement --------------------------------------------
# La specification ci-dessus laisse la dette des menages sans VIF et ecarte les
# deux taux de croissance du credit. Ce calcul les couvre : chaque variable est
# regressee a son tour sur toutes les autres.
vif_complet <- function(cols) {
  x <- as.data.frame(data[, cols]); names(x) <- paste0("V", seq_along(cols))
  v <- vapply(names(x), function(k)
    1 / (1 - summary(lm(reformulate(setdiff(names(x), k), k), data = x))$r.squared),
    numeric(1))
  data.frame(Variable = lib(cols), `Catégorie` = meta$categorie[match(cols, meta$colonne)],
             VIF = round(as.numeric(v), 1), check.names = FALSE, stringsAsFactors = FALSE)
}
vif_sym <- full_join(
  vif_complet(meta$colonne)  %>% rename(`VIF, 19 variables` = VIF),
  vif_complet(vars_retenues) %>% rename(`VIF, 17 variables` = VIF),
  by = c("Variable", "Catégorie")) %>%
  arrange(desc(`VIF, 19 variables`))

# 6. Figures ------------------------------------------------------------------
heatmap_corr <- function(m) {
  d <- melt(m)
  d$Var1 <- factor(d$Var1, levels = rownames(m))
  d$Var2 <- factor(d$Var2, levels = rev(rownames(m)))
  ggplot(d, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sub("^(-?)0\\.", "\\1,", sprintf("%.2f", value))), size = 2.4) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                         midpoint = 0, limits = c(-1, 1), name = "Corrélation") +
    scale_x_discrete(position = "top") +
    coord_fixed() +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0),
          panel.grid = element_blank(),
          legend.position = "right", legend.key.height = unit(1.2, "cm")) +
    labs(x = NULL, y = NULL)
}
# Peripherique pdf standard, en encodage WinAnsi : il couvre les accents et le
# tiret demi-cadratin des libelles, et ne depend ni de cairo ni de X11.
# (capabilities("cairo") peut renvoyer TRUE alors que le peripherique echoue.)
sauver_pdf <- function(p, fichier, width, height) {
  grDevices::pdf(fichier, width = width, height = height, encoding = "WinAnsi")
  on.exit(grDevices::dev.off())
  print(p)
}
sauver_pdf(heatmap_corr(corr_19), file.path(dossier_fig, "correlation_19_variables.pdf"), 11, 9)
sauver_pdf(heatmap_corr(corr_17), file.path(dossier_fig, "correlation_17_variables.pdf"), 10, 8.2)
cat("Figures ecrites dans :", dossier_fig, "\n")

# 7. Export Excel -------------------------------------------------------------
mat_vers_df <- function(m) data.frame(Variable = rownames(m), round(m, 2),
                                      check.names = FALSE, stringsAsFactors = FALSE)

lecture <- data.frame(
  Feuille = c("Paires_correlees", "VIF_avant_apres", "Corr_19_candidates",
              "Corr_17_retenues", "Corr_19_HY_brut", "VIF_complet", "Variables"),
  Contenu = c(
    "Paires de variables dont |r| dépasse le seuil, et traitement appliqué",
    "VIF avant retrait, après retrait de l'IG, après retrait de l'IG et de l'OAT",
    "Matrice de corrélation des 19 candidates (spread HY net de l'IG)",
    "Matrice de corrélation des 17 variables retenues",
    "Matrice des 19 candidates avant correction du spread HY",
    "VIF symétrique, calculé pour chaque variable sans exception",
    "Correspondance nom de colonne / libellé / catégorie / variable retenue"),
  `Destination dans le mémoire` = c(
    "Tableau tab:multico, panneau supérieur (annexe B.1)",
    "Tableau tab:multico, panneau inférieur (annexe B.1)",
    "Figure fig:multico-19 (annexe B.1)",
    "Figure fig:multico-17 (annexe B.1)",
    "Section 2.2, corrélation « au-delà de 0,9 avant cette correction »",
    "Complément méthodologique, non publié en l'état",
    "Annexe, nomenclature"),
  check.names = FALSE, stringsAsFactors = FALSE)

feuilles <- list(
  "Lecture"            = lecture,
  "Paires_correlees"   = paires,
  "VIF_avant_apres"    = vif_avant_apres,
  "Corr_19_candidates" = mat_vers_df(corr_19),
  "Corr_17_retenues"   = mat_vers_df(corr_17),
  "Corr_19_HY_brut"    = mat_vers_df(corr_19_hy_brut),
  "VIF_complet"        = vif_sym,
  "Variables"          = meta %>%
    transmute(`Nom de colonne` = colonne, `Libellé` = libelle, `Catégorie` = categorie,
              `Retenue` = ifelse(retenue, "oui", "non")))

wb <- createWorkbook()
entete <- createStyle(textDecoration = "bold", halign = "center", valign = "center",
                      wrapText = TRUE, fgFill = "#DDEBF7", border = "TopBottom")
alerte <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")

for (nm in names(feuilles)) {
  df <- feuilles[[nm]]
  addWorksheet(wb, nm)
  writeData(wb, nm, df, headerStyle = entete)
  freezePane(wb, nm, firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, nm, cols = 1, widths = 40)
  setColWidths(wb, nm, cols = 2:ncol(df),
               widths = if (grepl("^Corr_", nm)) 7 else "auto")
  if (grepl("^Corr_", nm))
    conditionalFormatting(wb, nm, cols = 2:ncol(df), rows = 2:(nrow(df) + 1),
                          type = "colourScale",
                          style = c("#2166AC", "#FFFFFF", "#B2182B"), rule = c(-1, 0, 1))
  if (nm == "VIF_avant_apres")
    conditionalFormatting(wb, nm, cols = 3:5, rows = 2:(nrow(df) + 1),
                          rule = paste0(">=", SEUIL_VIF), style = alerte)
}
saveWorkbook(wb, fichier_out, overwrite = TRUE)
cat("Ecrit :", fichier_out, "\n")
