## ---------------------------------------------------------##
##          Comparaison donnees brutes et zscores           ##
## ---------------------------------------------------------##
# ---------------------------------------------------------
# Liste des indicateurs 
# ---------------------------------------------------------
indicators <- df_final %>%
  select(-TIME_PERIOD) %>%
  select(where(is.numeric)) %>%
  names()

# ---------------------------------------------------------
# Fonction 
# ---------------------------------------------------------
plot_indicator <- function(ind) {
  
  data_final <- df_final %>%
    select(TIME_PERIOD, value = all_of(ind))
  
  data_z <- df_zscore %>%
    select(TIME_PERIOD, value = all_of(ind))
  
  p_final <- ggplot(data_final, aes(x = TIME_PERIOD, y = value, group = 1)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.3) +
    labs(
      title = paste0(ind, " – niveau (df_final)"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.title  = element_text(face = "bold", size = 12)
    )
  
  p_z <- ggplot(data_z, aes(x = TIME_PERIOD, y = value, group = 1)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.3) +
    labs(
      title = paste0(ind, " – z-score (df_zscore)"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.title  = element_text(face = "bold", size = 12)
    )
  
  p_final / p_z
}

# ---------------------------------------------------------
# Liste de graphes par indicateur 
# ---------------------------------------------------------
plots <- map(indicators, plot_indicator)
names(plots) <- indicators

pdf("S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/comparaison_brut_zscore.pdf", width = 10, height = 8)
for (ind in indicators) {
  print(plots[[ind]])
}
dev.off()