## --------------------------------------------------------##
##    Transformation de liste en df et standardisation     ##
## --------------------------------------------------------##
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/0bis. Setup.R")
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/1. Construction_data.R")

#load("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/data_long.RData")
# Transformation type -------------------------------------------------------
#data (liste) en df_final (dataframe)

# Fonction pour transformer liste en dataset
# Renvoie a seulement si a non nul, b sinon
`%||%` <- function(a, b) if (!is.null(a)) a else b

flatten_data <- function(x, parent_name = NULL) {
  if (inherits(x, c("data.frame", "tbl_df"))) {
    nm <- parent_name %||% "var"
    df <- x %>%
      rename(!!nm := OBS_VALUE)
    return(list(df))
  }
  
  if (is.list(x)) {
    out <- imap(x, function(value, name) {
      new_parent <- if (is.null(parent_name)) name else paste(parent_name, name, sep = "_")
      flatten_data(value, new_parent)
    })
    return(unlist(out, recursive = FALSE))
  }
  
  list()
}

flat_list <- flatten_data(data)

flat_list_ok <- flat_list |>
  keep(~ "TIME_PERIOD" %in% names(.x))

flat_list_ok <- flat_list_ok |>
  map(\(df) {
    df |>
      group_by(TIME_PERIOD) |>
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop")
  })

df_final <- reduce(flat_list_ok, full_join, by = "TIME_PERIOD") |>
  relocate(TIME_PERIOD) |>
  arrange(TIME_PERIOD)

# Quand les banques sont accomodantes pour accorder du crédit aux SNF = baisse du risque 
df_final <- df_final %>% 
  mutate(financement_credit_SAFE = financement_credit_SAFE*-1)

# Savoir quand commence les donnees
# df_final %>% 
#   filter(if_all(-TIME_PERIOD, ~ !is.na(.))) %>% 
#   pull(TIME_PERIOD)

df_final_filtre <- df_final %>%
  filter(TIME_PERIOD >= "1999-Q1",
         TIME_PERIOD != "NA-QNA") %>%
  arrange(TIME_PERIOD)

trimestres_incomplets <- df_final_filtre %>%
  filter(if_any(-TIME_PERIOD, is.na)) %>%
  pull(TIME_PERIOD)

# df_final <- df_final_filtre %>%
#   fill(-TIME_PERIOD, .direction = "down") #compléter quand données manquantes par dernière valeur

# Standardisation -------------------------------------------------------------

cols_donnees <- setdiff(names(df_final), "TIME_PERIOD")
df_zscore <- df_final
df_zscore[cols_donnees] <- lapply(df_zscore[cols_donnees], function(x) as.numeric(scale(x)))

df_zscore <- df_zscore %>%
  fill(-TIME_PERIOD, .direction = "down") #compléter quand données manquantes par dernière valeur APRES le zscore sinon biais

# Tests de robustesse --------------------------------------------------------------
#(ne pas faire tourner sauf si besoin)

# #Mediane et ecart inter-quartile
# df_zscore <- df_final %>%
#   mutate(across(
#     .cols = where(is.numeric) & !matches("TIME_PERIOD"),
#     .fns  = ~ ( . - median(., na.rm = TRUE) ) / IQR(., na.rm = TRUE)
#   ))
# 
# # Mediane et ecart-type
# df_zscore <- df_final %>%
#   mutate(across(
#     .cols = where(is.numeric) & !matches("TIME_PERIOD"),
#     .fns  = ~ ( . - median(., na.rm = TRUE) ) / sd(., na.rm = TRUE)
#   ))
# 
# # Fenetre glissante
# window <- 8         #En trimestres : essais avec 4 et 8
# 
# df_zscore <- df_final %>%
#   mutate(across(
#     .cols = where(is.numeric) & !matches("TIME_PERIOD"),
#     .fns  = ~ {
#       m  <- slide_dbl(., mean, .before = window - 1, .complete = TRUE)
#       s  <- slide_dbl(., sd,   .before = window - 1, .complete = TRUE)
#       (. - m) / s
#     }
#   ))
