## ---------------------------------------------------------##
##    Creation graphiques de base avec poids RandomForest   ##
## ---------------------------------------------------------##

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/5. Random_Forest.R")

# Prérequis ---------------------------------------------------------
# - data      : liste avec les 5 categories
# - df_zscore : TIME_PERIOD + colonnes standardisees (z-scores), 1 colonne par indicateur

ccyb_all <- read_excel("S:/03. Ponctuel/03. Banques/2025/CCyB across time/ts_data_ccyb_rates.xlsx", sheet = "Sheet 1")

df_ccyb_fr_q <- ccyb_all %>%
  select(date, France) %>%
  mutate(
    date = as.Date(date),
    TIME_PERIOD = format(as.yearqtr(date), "%Y-Q%q")
  ) %>%
  group_by(TIME_PERIOD) %>%
  summarise(
    ccyb_fr = {
      x <- France[!is.na(France)]
      if (length(x) == 0) NA_real_ else dplyr::last(x)  
    },
    .groups = "drop"
  ) %>%
  filter(TIME_PERIOD >= "2006-Q1")

# Mapping ---------------------------------------------------------
categories <- names(data)

col_to_cat <- tibble(indicator = names(df_zscore)) %>%
  mutate(
    category = case_when(
      indicator == "TIME_PERIOD" ~ NA_character_,
      TRUE ~ map_chr(indicator, \(cn) {
        matches <- categories[str_detect(cn, paste0("^", categories))]
        if (length(matches) == 0) NA_character_ else matches[1]
      })
    )
  )

# Indicateur composite global (poids rf) ----------------------------
df_indicator <- df_weighted %>% 
  select(TIME_PERIOD, indicator_rf) 

df_indicator <- df_indicator %>% filter(TIME_PERIOD >= "2006-Q1" & TIME_PERIOD != "NA-QNA")

# Contributions par serie ---------------------------------------------
df_long <- df_zscore %>%
  pivot_longer(
    cols = -c(TIME_PERIOD),
    names_to = "indicator",
    values_to = "z"
  ) %>%
  left_join(col_to_cat, by = "indicator") %>%
  filter(!is.na(category))

imp_df <- imp_df %>% 
  rename(indicator = var) %>% 
  select(indicator, weight)

df_contrib_ind <- df_long %>%
  inner_join(imp_df, by = "indicator") %>%
  mutate(contribution = z * weight)

# Contributions par categorie (somme des series de la categorie)---------------
df_contrib_cat <- df_contrib_ind %>%
  group_by(TIME_PERIOD, category) %>%
  summarise(
    contribution_cat = sum(contribution, na.rm = TRUE),
    .groups = "drop"
  )

# Export excel pour graphiques -----------------------------------------------

df_cats_wide <- df_contrib_cat %>%
  pivot_wider(
    id_cols = TIME_PERIOD,
    names_from = category,
    values_from = contribution_cat
  )

df_inds_wide <- df_contrib_ind %>%
  pivot_wider(
    id_cols = TIME_PERIOD,
    names_from = indicator,
    values_from = contribution
  )

rename_categories <- c(
  "encours_dette"        = "Encours de dette (ménages et SNF)",
  "financement_credit"   = "Financement du crédit",
  "marche_immo"          = "Marché immobilier",
  "financement_marche"   = "Financement sur les marchés",
  "marche_surrevaluation"= "Valorisation sur les marchés"
)

rename_indicators <- c(
  "encours_dette_CNFSI_DetteH"                 = "Dette Ménages",
  "encours_dette_CNFSI_DetteSNF"               = "Dette SNF",
  "encours_dette_BSI1_TC_creditH"              = "Taux croissance crédits particuliers",
  "encours_dette_BSI1_TC_creditSNF"            = "Taux croissance crédits SNF",
  "encours_dette_DSR_DSRH"                     = "Service de la dette des ménages",
  "encours_dette_DSR_DSRSNF"                   = "Service de la dette des SNF",
  "financement_credit_BLS_Entreprises"         = "Durcissement conditions d'octroi - SNF",
  "financement_credit_BLS_Menages"             = "Durcissement conditions d'octroi - Ménages",
  "financement_credit_MIR_menages"             = "Taux crédits nouveaux ménages",
  "financement_credit_MIR_snf"                 = "Taux crédits nouveaux SNF",
  "financement_credit_SAFE"                    = "Part SNF avec contraintes de crédit",
  "marche_immo_ISPI"                           = "Surrévaluation prix immo",
  "marche_immo_OCDE"                           = "Prix immo / revenus ménages",
  "financement_marche_SPREAD_OATBD"            = "Spread OAT-Bund",
  "financement_marche_SPREADHY"                = "Spread HY", 
  "marche_surrevaluation_CDS"                  = "Prime de CDS moyen",
  "marche_surrevaluation_PER"                  = "Price Earning Ratio"
)

rename_cats <- setNames(names(rename_categories), unname(rename_categories))
rename_inds <- setNames(names(rename_indicators), unname(rename_indicators))

rename_tout <- c(rename_cats, rename_inds)

df_export_final <- df_indicator %>%
  left_join(df_ccyb_fr_q %>% select(TIME_PERIOD, ccyb_fr), by = "TIME_PERIOD") %>%
  left_join(df_cats_wide, by = "TIME_PERIOD") %>%
  left_join(df_inds_wide, by = "TIME_PERIOD") %>%
  arrange(TIME_PERIOD) %>%
  mutate(
    incomplet = if_else(TIME_PERIOD %in% trimestres_incomplets, 1000, NA_real_)
  ) %>%
  rename("Indicateur de vulnérabilité" = indicator_rf,
         "Taux du CCyB" = ccyb_fr,
         any_of(rename_tout))

write_xlsx(list("Donnees" = df_export_final), "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Donnees_RFW_ext.xlsx")
