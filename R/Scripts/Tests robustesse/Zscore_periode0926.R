#Test de création de l'indicateur en standardisant uniquement sur 2009-Q - 2025-Q3 #
imp_df <- read_excel("S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Poids_variables_brut.xlsx") 
df_zscore <- read_excel("S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/data_robustness.xlsx")

w <- setNames(imp_df$weight, imp_df$var)
df_weighted <- df_zscore %>% 
  mutate(indicator_rf = as.numeric(
    as.matrix(df_zscore[, imp_df$var]) %*% w[imp_df$var]
  )) 

write_xlsx(df_weighted, "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/Data_robust.xlsx")
