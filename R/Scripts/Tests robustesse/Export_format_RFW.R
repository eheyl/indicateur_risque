## ---------------------------------------------------------------------------##
##   Export d'un jeu de poids au format de Donnees_RFW_ext.xlsx               ##
##   (indicateur, taux du CCyB, contributions par categorie et par serie)      ##
## ---------------------------------------------------------------------------##
#
# Reprend la logique de 5. Construction_graphiques.R sous forme de fonction,
# sans le sourcer (ce script ecrase Donnees_RFW_ext.xlsx et modifie imp_df).
# Sourcer ce fichier depuis les scripts 9 et 10, puis appeler
#   exporter_format_rfw(poids, "M2_chrono")
# qui ecrit Résultats/Donnees_RFW_M2_chrono.xlsx, feuille "Donnees",
# memes colonnes et meme ordre que le fichier de l'indicateur principal.
#
# Objets attendus : data (liste des 5 categories), df_zscore, trimestres_incomplets

dossier_resultats <- "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/"

# Taux du CCyB France (trimestriel, derniere valeur du trimestre) -------------
ccyb_all <- read_excel("S:/03. Ponctuel/03. Banques/2025/CCyB across time/ts_data_ccyb_rates.xlsx", sheet = "Sheet 1")

df_ccyb_fr_q <- ccyb_all %>%
  select(date, France) %>%
  mutate(date = as.Date(date),
         TIME_PERIOD = format(as.yearqtr(date), "%Y-Q%q")) %>%
  group_by(TIME_PERIOD) %>%
  summarise(ccyb_fr = { x <- France[!is.na(France)]; if (length(x) == 0) NA_real_ else dplyr::last(x) },
            .groups = "drop") %>%
  filter(TIME_PERIOD >= "2009-Q1")

# Mapping serie -> categorie --------------------------------------------------
categories <- names(data)

col_to_cat <- tibble(indicator = names(df_zscore)) %>%
  mutate(category = case_when(
    indicator == "TIME_PERIOD" ~ NA_character_,
    TRUE ~ map_chr(indicator, \(cn) {
      m <- categories[str_detect(cn, paste0("^", categories))]
      if (length(m) == 0) NA_character_ else m[1]
    })))

# Libelles (identiques a 5. Construction_graphiques.R) -------------------------
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

# Ordre des colonnes du fichier de reference (pour un rendu strictement identique)
ordre_colonnes <- c("TIME_PERIOD", "Indicateur de vulnérabilité", "Taux du CCyB",
                    unname(rename_categories), unname(rename_indicators), "incomplet")

# Fonction d'export -----------------------------------------------------------
# poids : data.frame avec colonnes var et weight (format imp_df)
# nom   : suffixe du fichier, ex. "M2_chrono" -> Donnees_RFW_M2_chrono.xlsx
exporter_format_rfw <- function(poids, nom, date_debut = "2009-Q1",
                                dossier = dossier_resultats) {
  poids <- poids %>% select(indicator = var, weight)
  w <- setNames(poids$weight, poids$indicator)

  df_indicator <- df_zscore %>%
    mutate(indicator_rf = as.numeric(as.matrix(df_zscore[, poids$indicator]) %*% w[poids$indicator])) %>%
    select(TIME_PERIOD, indicator_rf) %>%
    filter(TIME_PERIOD >= date_debut, TIME_PERIOD != "NA-QNA")

  df_contrib_ind <- df_zscore %>%
    pivot_longer(-TIME_PERIOD, names_to = "indicator", values_to = "z") %>%
    left_join(col_to_cat, by = "indicator") %>%
    filter(!is.na(category)) %>%
    inner_join(poids, by = "indicator") %>%
    mutate(contribution = z * weight)

  df_cats_wide <- df_contrib_ind %>%
    group_by(TIME_PERIOD, category) %>%
    summarise(contribution_cat = sum(contribution, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(id_cols = TIME_PERIOD, names_from = category, values_from = contribution_cat)

  df_inds_wide <- df_contrib_ind %>%
    pivot_wider(id_cols = TIME_PERIOD, names_from = indicator, values_from = contribution)

  df_export <- df_indicator %>%
    left_join(df_ccyb_fr_q, by = "TIME_PERIOD") %>%
    left_join(df_cats_wide, by = "TIME_PERIOD") %>%
    left_join(df_inds_wide, by = "TIME_PERIOD") %>%
    arrange(TIME_PERIOD) %>%
    mutate(incomplet = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)) %>%
    rename("Indicateur de vulnérabilité" = indicator_rf,
           "Taux du CCyB" = ccyb_fr,
           any_of(rename_tout)) %>%
    select(any_of(ordre_colonnes), everything())

  chemin <- file.path(dossier, paste0("Donnees_RFW_", nom, ".xlsx"))
  write_xlsx(list("Donnees" = df_export), chemin)
  cat("Ecrit :", chemin, "\n")
  invisible(df_export)
}
