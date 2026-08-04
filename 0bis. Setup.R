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
library(writexl)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ranger)

# ------------------------------------------------------------------
# Proxy 
# ------------------------------------------------------------------
Sys.setenv(http_proxy = "http://172.22.52.40:3128")
Sys.setenv(https_proxy = "http://172.22.52.40:3128")

headers <- add_headers(
  Authorization = "Apikey b143d89aabc4df638941cb274767dbf3631c55b5173bfe0e2aaf2fd7",
  accept = "application/json"
)