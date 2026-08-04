## --------------------------------------------------##
##    Prepare les donnees pour le Random Forest      ##
## --------------------------------------------------##
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/3. Standardisation.R")

# Sélection des données ------------------------------------------------------
# entre 1999-Q1 et 2025-Q2 (moment de la construction de l'indicateur où toutes données dispo)
#ancienne version : à ressayer ???
# df_final <- df_final %>% filter(TIME_PERIOD <= "2025-Q3" & TIME_PERIOD >= "1999-Q1")
# 
# # Standardisation ------------------------------------------------------------
# cols_donnees <- setdiff(names(df_final), "TIME_PERIOD")
# df_zscore_rf2 <- df_final
# df_zscore_rf2[cols_donnees] <- lapply(df_zscore_rf2[cols_donnees], function(x) as.numeric(scale(x)))

#Essai avec nouvelle version : (avoir la même base que zscore pour graphs après)
df_zscore_rf <- df_zscore %>% filter(TIME_PERIOD >= "1999-Q1" & TIME_PERIOD <= "2025-Q3") #changer ici 

# Ajout de la colonne crise (0 ou 1)------------------------------------------
df_esrb <- read_excel("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/esrb.fcdb20220120.en.xlsx") %>% 
  filter(Country == "FR")
df_syst <- read_excel("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/esrb.fcdb20220120.en.xlsx", sheet = "Residual events") %>% 
  filter(Country == "FR") 
colnames(df_esrb)[5] <- "System back \"normal\" date"
df_crisis <- rbind(df_syst,df_esrb)
df_crisis$Event <- seq_len(nrow(df_crisis))
df_crisis <- df_crisis %>% select(`Start date`,`End of crisis management date`)

df_crisis <- df_crisis %>%
  rename(
    start_date    = `Start date`,
    end_date = `End of crisis management date`
  ) %>%
  mutate(
    start_date    = ifelse(start_date    == "n.a.", NA, start_date),
    end_date = ifelse(end_date == "n.a.", NA, end_date)
  )

df_crisis2 <- df_crisis %>%
  mutate(
    start_q = as.yearqtr(as.yearmon(start_date)),
    end_q   = as.yearqtr(as.yearmon(end_date))
  )

crisis_periods <- c() 

for (i in 1:nrow(df_crisis2)) {
  
  start_i <- df_crisis2$start_q[i]
  end_i   <- df_crisis2$end_q[i]
  
  q_seq <- seq(start_i, end_i, by = 0.25)
  
  q_seq <- format(q_seq, "%Y-Q%q")
  
  crisis_periods <- c(crisis_periods, q_seq)
}

crisis_periods <- unique(crisis_periods)
# 1 crise (tout au long de la crise), 0 pas de crise 
df_zscore_rf$crise <- ifelse(df_zscore_rf$TIME_PERIOD %in% crisis_periods, 1, 0)
df_zscore_rf$crise <- factor(df_zscore_rf$crise, levels = c(0,1))

df_model <- df_zscore_rf %>%
  select(-TIME_PERIOD) 
