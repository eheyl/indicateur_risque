## ---------------------------------------------------------##
##    Creation des indicateurs taux credits menages et      ##
##       entreprises (encours et taux selon le mois)        ##
## ---------------------------------------------------------##

mir_pairs <- list(
  credit_conso   = c("Taux crédits conso",   "Flux crédits conso"),
  credit_immo    = c("Taux crédits immo",    "Flux crédits immo"),
  credit_moins1M = c("Taux crédits SNF <1m", "Flux crédits SNF <1m"),
  credit_plus1M  = c("Taux crédits SNF >1m", "Flux crédits SNF >1m")
)

create_pair_ratio <- function(mir1, mir2) {
  s1 <- data$financement_credit$MIR[[mir1]] %>% rename(Taux = OBS_VALUE)
  s2 <- data$financement_credit$MIR[[mir2]] %>% rename(Encours = OBS_VALUE)
  inner_join(s1, s2, by = "TIME_PERIOD")
}

pair_tables <- imap(mir_pairs, ~ create_pair_ratio(.x[1], .x[2]) %>% mutate(pair = .y))
pairs_df    <- bind_rows(pair_tables)

categories <- list(
  menages = c("credit_conso","credit_immo"),
  snf     = c("credit_moins1M","credit_plus1M")
)

cat_ratio <- function(cat_pairs) {
  pairs_df %>%
    filter(pair %in% cat_pairs) %>%
    group_by(TIME_PERIOD) %>%
    summarise(Encours_tot = sum(Encours, na.rm = TRUE),
              Ratio_pond  = sum(Taux * Encours, na.rm = TRUE) / Encours_tot,
              .groups = "drop") %>%
    transmute(TIME_PERIOD, OBS_VALUE = Ratio_pond)
}

data$financement_credit$MIR <- list(
  menages = cat_ratio(categories$menages),
  snf     = cat_ratio(categories$snf)
)

data$financement_credit$BLS$Menages_H <- NULL
data$financement_credit$BLS$Menages_C <- NULL