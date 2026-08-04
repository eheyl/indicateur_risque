## -----------------------------------------------##
##    Random Forest et exportation des poids      ##
## -----------------------------------------------##
source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/5. Data_pour_RF.R")

# Transformer crise en facteur - normalement pas besoin mais au cas où  
# df_model$crise<-as.factor(df_model$crise)
# lapply(df_model, class) 
# 
# # Random Forest --------------------------------------------------------
# (ne pas refaire tourner à chaque fois, pour voir d'où viennent les pondérations)
# set.seed(123)
# sample <- sample(c(TRUE, FALSE), nrow(df_model), replace=TRUE, prob=c(0.7,0.3))
# train  <- df_model[sample, ]
# test   <- df_model[!sample, ]
# dim(train)
# dim(test)
# summary(train$crise)
# summary(test$crise)
# set.seed(123)
# 
# rf_weighted <- ranger(
#   formula       = crise ~ ., 
#   data          = train, 
#   num.trees     = 180, 
#   importance    = "permutation",   
#   write.forest  = TRUE
# 
# )
# 
# print(rf_weighted)
# 
# imp_df <- data.frame(
#   var = names(rf_weighted$variable.importance),
#   imp = rf_weighted$variable.importance
# ) %>%
#   mutate(imp_pos = pmax(imp, 0)) %>%
#   mutate(weight = imp_pos / sum(imp_pos)) %>%
#   arrange(desc(weight))

imp_df <- read_xlsx("S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Poids_variables.xlsx", sheet = "Sheet1")

w <- setNames(imp_df$weight, imp_df$var)

# Exporter les poids-------------------------------------------------------
# une fois seulement sinon écrase ancienne version
# write_xlsx(
#   imp_df,
#  path = "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Résultats/Poids_variables.xlsx"
# )

# Mettre les poids dans les donnees------------------------------------------ 
# Quick verif qu'on a les mêmes noms
identical(colnames(df_zscore[, imp_df$var]), imp_df$var)
identical(names(w[imp_df$var]), imp_df$var)

df_weighted <- df_zscore %>% 
  mutate(indicator_rf = as.numeric(
    as.matrix(df_zscore[, imp_df$var]) %*% w[imp_df$var]
  )) 

df_weighted$crise <- NULL
