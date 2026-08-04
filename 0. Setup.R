## ------------------------------------------------------------------------------##
##        Charger les packages, proxy, les fonctions de Morgan pour              ##
##  recuperer les donnees : ne pas ouvrir, seulement appele dans un autre script ##
## ------------------------------------------------------------------------------##

# ------------------------------------------------------------------
# Library 
# ------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readxl)
library(httr)
library(slider)
library(patchwork)  
library(zoo)
library(caret)
library(xgboost)
library(tidyverse)
library(glmnet)
library(ranger)
library(pROC)

# ------------------------------------------------------------------
# Proxy 
# ------------------------------------------------------------------
Sys.setenv(http_proxy = "http://172.22.52.40:3128")
Sys.setenv(https_proxy = "http://172.22.52.40:3128")

headers <- add_headers(
  Authorization = "Apikey b143d89aabc4df638941cb274767dbf3631c55b5173bfe0e2aaf2fd7",
  accept = "application/json"
)

# ------------------------------------------------------------------
# Fonctions 
# ------------------------------------------------------------------
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/Codes API.R")

rename_to_obs_value <- function(df, time_col = "TIME_PERIOD") {
  num_cols <- names(df)[sapply(df, is.numeric)]
  num_cols <- setdiff(num_cols, time_col)
  
  if ("OBS_VALUE" %in% names(df) && length(num_cols) == 1 && num_cols == "OBS_VALUE") {
    return(df)
  }
  
  if (length(num_cols) == 0) {
    stop("Aucune colonne numérique à renommer.")
  }
  if (length(num_cols) > 1) {
    warning("Plusieurs colonnes numériques trouvées, seule la première est renommée en OBS_VALUE.")
  }
  
  df$OBS_VALUE <- df[[num_cols[1]]]
  df[num_cols[1]] <- NULL
  df
}

monthly_to_quarterly_mean <- function(df) {
  df <- rename_to_obs_value(df)               
  df$TIME_PERIOD <- as.Date(paste0(df$TIME_PERIOD, "-01"))
  df$TIME_PERIOD <- paste0(year(df$TIME_PERIOD), "-Q", quarter(df$TIME_PERIOD))
  
  df %>%
    group_by(TIME_PERIOD) %>%
    summarise(OBS_VALUE = mean(OBS_VALUE, na.rm = TRUE), .groups = "drop")
}

semestrial_to_quarterly_duplicate <- function(df) {
  df <- rename_to_obs_value(df)  # standardise la colonne de valeur
  
  df_expanded <- df %>%
    mutate(
      year = substr(TIME_PERIOD, 1, 4),
      sem  = substr(TIME_PERIOD, 6, 7)
    ) %>%
    rowwise() %>%
    do({
      year <- .$year
      sem  <- .$sem
      val  <- .$OBS_VALUE
      
      if (sem == "S1") {
        data.frame(
          TIME_PERIOD = c(paste0(year, "-Q1"), paste0(year, "-Q2")),
          OBS_VALUE   = c(val, val)
        )
      } else if (sem == "S2") {
        data.frame(
          TIME_PERIOD = c(paste0(year, "-Q3"), paste0(year, "-Q4")),
          OBS_VALUE   = c(val, val)
        )
      } else {
        stop(paste("Semestre inconnu dans TIME_PERIOD :", sem))
      }
    }) %>%
    ungroup()
  
  df_expanded
}