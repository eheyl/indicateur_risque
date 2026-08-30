## ---------------------------------------------------------##
##               Graphiques avec poids égaux                ##
## ---------------------------------------------------------##

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/3. Standardisation.R")

# Pre-requis --------------------------------------------------------------
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
  filter(TIME_PERIOD >= "2009-Q1")

# Mapping : associer serie a sa categorie -----------------------------------

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

# Indicateur composite global (poids egal par serie) ------------------------

df_indicator <- df_zscore %>%
  mutate(
    indicateur_composite = rowMeans(select(., -TIME_PERIOD), na.rm = TRUE)
  ) %>%
  select(TIME_PERIOD, indicateur_composite)

df_indicator <- df_indicator %>%
  mutate(
    indicateur_composite = rowMeans(select(., -TIME_PERIOD), na.rm = TRUE)
  ) %>%
  select(TIME_PERIOD, indicateur_composite)

# Contributions par serie (sous-composant) ---------------------------------

df_long <- df_zscore %>%
  pivot_longer(
    cols = -TIME_PERIOD,
    names_to = "indicator",
    values_to = "z"
  ) %>%
  left_join(col_to_cat, by = "indicator") %>%
  filter(!is.na(category)) %>%
  group_by(TIME_PERIOD) %>%
  mutate(
    K_t = sum(!is.na(z))
  ) %>%
  ungroup()

df_contrib_ind <- df_long %>%
  filter(!is.na(z)) %>%
  mutate(
    contribution = z / K_t
  )

# Contributions par categorie (somme des series de la categorie) --------------

df_contrib_cat <- df_contrib_ind %>%
  group_by(TIME_PERIOD, category) %>%
  summarise(
    contribution_cat = sum(contribution, na.rm = TRUE),
    .groups = "drop"
  )

df_contrib_cat <- df_contrib_cat %>% filter(TIME_PERIOD >= "2009-Q1")
df_contrib_ind <- df_contrib_ind %>% filter(TIME_PERIOD >= "2009-Q1")
df_indicator <- df_indicator %>% filter(TIME_PERIOD >= "2009-Q1")

# Export excel pour graphiques  ---------------------------------------------

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
  "marche_surrevaluation"= "Surévaluation sur les marchés"
)

rename_indicators <- c(
  "encours_dette_CNFSI_DetteH"                 = "Dette Ménages",
  "encours_dette_CNFSI_DetteSNF"               = "Dette SNF",
  "encours_dette_BSI1_TC_creditH"              = "Taux croissance crédits particuliers",
  "encours_dette_BSI1_TC_creditSNF"            = "Taux croissance crédits SNF",
  "encours_dette_DSR_DSRH"                     = "DSR Ménages",
  "encours_dette_DSR_DSRSNF"                   = "DSR SNF",
  "financement_credit_BLS_Entreprises"         = "Durcissement conditions d'octroi - SNF",
  "financement_credit_BLS_Menages"             = "Durcissement conditions d'octroi - Ménages",
  "financement_credit_MIR_menages"             = "Taux crédits nouveaux ménages",
  "financement_credit_MIR_snf"                 = "Taux crédits nouveaux SNF",
  "financement_credit_SAFE"                    = "Part SNF avec contraintes de crédit",
  "marche_immo_ISPI"                           = "Surrévaluation prix immo",
  "marche_immo_OCDE"                           = "Prix immo / revenus ménages",
  "financement_marche_OAT10Y"                  = "Taux OAT 10Y",
  "financement_marche_SPREAD_OATBD"            = "Spread OAT-Bund",
  "financement_marche_SPREADHY"                = "Spread HY", 
  "financement_marche_SPREADIG"                = "Spread IG",
  "marche_surrevaluation_CDS"                  = "Prime de CDS moyen",
  "marche_surrevaluation_PER"                  = "Price Earning ratio"
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
  rename("Indicateur de vulnérabilité" = indicateur_composite,
         "Taux du CCyB" = ccyb_fr,
         any_of(rename_tout))

write_xlsx(list("Donnees" = df_export_final), "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Donnees_equalW_ext.xlsx")
