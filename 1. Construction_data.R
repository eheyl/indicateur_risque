## ---------------------------------------------------------##
##    Creation base de donnees avec tous les indicateurs    ##
##             de l'apparition a derniere periode           ##
## ---------------------------------------------------------##

# Note : dernières données au 04/05/2026 figées

source("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/0. Setup.R")

# ---------------------------------------------------------
# BLS (ECB)
# ---------------------------------------------------------
bls_keys <- c(
  "BLS.Q.FR.ALL.O.E.Z.B3.ST.S.FNET", 
  "BLS.Q.FR.ALL.Z.H.H.B3.ST.S.FNET",
  "BLS.Q.FR.ALL.Z.H.C.B3.ST.S.FNET"
)

BLS <- lapply(bls_keys, function(k) {
  get_ecb(k) %>%
    mutate(across(-TIME_PERIOD, as.numeric)) %>%
    rename_to_obs_value()
})

names(BLS) <- c("Entreprises", "Menages_H", "Menages_C")

BLS$Menages <- 
  BLS$Menages_H %>%
  rename(H = OBS_VALUE) %>%
  left_join(BLS$Menages_C %>% rename(C = OBS_VALUE), by = "TIME_PERIOD") %>%
  mutate(OBS_VALUE = (H + C) / 2) %>%
  select(TIME_PERIOD, OBS_VALUE)

# ---------------------------------------------------------
# SAFE (ECB)
# ---------------------------------------------------------
safe_keys <- "SAFE.H.FR.ALL.A.0.0.0.Q11.11F.NN.FL.WP"

SAFE <- get_ecb(safe_keys) %>%
  mutate(across(-TIME_PERIOD, as.numeric)) %>%
  semestrial_to_quarterly_duplicate()

# ---------------------------------------------------------
# MIR (Webstat)
# ---------------------------------------------------------
mir_keys <- c(
  "MIR1.M.FR.B.A2B.A.R.A.2254U6.EUR.N",
  "MIR1.M.FR.B.A2B.A.5.A.2254U6.EUR.N",
  "MIR1.M.FR.B.A22HR.A.R.A.2254U6.EUR.N",
  "MIR1.M.FR.B.A22HR.A.5.A.2254U6.EUR.N",
  "MIR1.M.FR.B.A20.A.5.0.2240U6.EUR.N",
  "MIR1.M.FR.B.A20.A.R.0.2240U6.EUR.N",
  "MIR1.M.FR.B.A20.A.5.1.2240U6.EUR.N",
  "MIR1.M.FR.B.A20.A.R.1.2240U6.EUR.N"
)

MIR <- lapply(mir_keys, function(k) {
  get_webstat(k, headers = headers) %>%
    monthly_to_quarterly_mean()
})

names(MIR) <- c("Taux crédits conso", "Flux crédits conso", "Taux crédits immo", "Flux crédits immo", "Flux crédits SNF <1m", "Taux crédits SNF <1m", "Flux crédits SNF >1m", "Taux crédits SNF >1m")

# ---------------------------------------------------------
# BSI1 (Webstat)
# ---------------------------------------------------------
bsi1_keys <- c(
  "BSI1.M.FR.N.R.A26.A.I.U6.2254FR.Z01.A",
  "BSI1.M.FR.N.R.A26.A.I.U6.2240.Z01.A"
)

BSI1 <- lapply(bsi1_keys, function(k) {
  get_webstat(k, headers = headers) %>%
    monthly_to_quarterly_mean()
})

names(BSI1) <- c("TC_creditH", "TC_creditSNF")

# ---------------------------------------------------------
# CNFSI (Webstat) 
# ---------------------------------------------------------
cnfsi_keys <- c(
  "CNFSI.Q.S.FR.W0.S1M.S1.N.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T",
  "CNFSI.Q.S.FR.W0.S11.S1.C.L.LE.DETT.T._Z.XDC_R_B1GQ_CY._T.S.V.N._T"
)

CNFSI <- lapply(cnfsi_keys, function(k) {
  get_webstat(k, headers = headers) 
})

names(CNFSI) <- c("DetteH", "DetteSNF")

# ---------------------------------------------------------
# DSR (BIS) 
# ---------------------------------------------------------
dsr_urls <- list(
  Menages = "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DSR/1.0/Q.FR.H?format=csv",
  SNF     = "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_DSR/1.0/Q.FR.N?format=csv"
)

DSR <- lapply(dsr_urls, function(u) {
  read.csv(u) %>%
    select(TIME_PERIOD, OBS_VALUE) 
})

names(DSR) <- c("DSRH", "DSRSNF")

# ---------------------------------------------------------
# Prix immo (OCDE)
# ---------------------------------------------------------
ocde_url <- "https://sdmx.oecd.org/public/rest/data/OECD.ECO.MPD,DSD_AN_HOUSE_PRICES@DF_HOUSE_PRICES,1.0/FRA.Q.HPI_YDH.?startPeriod=1999-Q1&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
OCDE <- read.csv(ocde_url) %>%
  select(TIME_PERIOD, OBS_VALUE) 

# ---------------------------------------------------------
# ISPI (local Tresor)
# ---------------------------------------------------------
ISPI <- read_xlsx(
  "//P-FS19-DGT.si.local/PAESF/03. Ponctuel/02. Ménages/3. Immobilier/10. Prix immobilier/ispi/data/ISPI_results.xlsx"
) %>%
  select(time, SUREVAL) %>%
  rename_to_obs_value() %>% 
  rename(TIME_PERIOD = time)  

# ---------------------------------------------------------
# Bloomberg 
# ---------------------------------------------------------
BLOOM <- read_xlsx(
  "//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/Bloomberg_data_v2.xlsx",
  sheet = "Données"
) %>%
  select(
    TIME_PERIOD,
    SPREADOB,
    # OAT10Y,
    BUND10Y,
    MEANCDS
  ) %>%
  mutate(
    TIME_PERIOD = suppressWarnings(
      as.Date(as.numeric(TIME_PERIOD), origin = "1899-12-30")
    ),
    across(-TIME_PERIOD, ~ suppressWarnings(as.numeric(.x)))
  )

BLOOM_SPREADOATBD <- BLOOM %>% select(TIME_PERIOD, SPREADOB)    %>% rename_to_obs_value()
# BLOOM_OAT10Y      <- BLOOM %>% select(TIME_PERIOD, OAT10Y)      %>% rename_to_obs_value()
BLOOM_CDS         <- BLOOM %>% select(TIME_PERIOD, MEANCDS)     %>% rename_to_obs_value()

PER <- read_xlsx(
  "//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/Bloomberg_data_v2.xlsx",
  sheet = "PER CAC 40"
) %>% rename_to_obs_value()


#Nouvelles donnees HY et IG recup par FBL
# IG <- read_xlsx("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/HY_IG.xlsx", 
#                      sheet = "Données",
#                      skip = 7,
#                      col_names = FALSE)
# IG <- IG[, 1:2]
# colnames(IG) <- c("TIME_PERIOD", "OBS_VALUE")

HY <- read_xlsx("//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/HY_IG.xlsx", 
                sheet = "Données",
                skip = 7,
                col_names = FALSE)

HY <- HY[, c(1,3)]
colnames(HY) <- c("TIME_PERIOD", "OBS_VALUE")

# ---------------------------------------------------------
# Liste finale 
# ---------------------------------------------------------
data <- list(
  
  # Encours de la dette : CNFSI + BSI1 
  encours_dette = list(
    CNFSI = CNFSI,
    BSI1  = BSI1,
    DSR = DSR
  ),
  
  # Financement et crédit : DSR, MIR, BLS, SAFE
  financement_credit = list(
    MIR  = MIR,
    BLS  = BLS,
    SAFE = SAFE
  ),
  
  # Marché immobilier : ISPI + OCDE
  marche_immo = list(
    ISPI = ISPI,
    OCDE = OCDE
  ),
  
  # Financement de marché : spreads HY/IG/OAT-Bund + OAT10Y
  financement_marche = list(
    SPREADHY     = HY,
    # SPREADIG     = IG,
    SPREAD_OATBD = BLOOM_SPREADOATBD
    # OAT10Y       = BLOOM_OAT10Y
  ),
  
  # Marché surévaluation : CDS, PER, CAC Index
  marche_surrevaluation = list(
    CDS      = BLOOM_CDS,
    PER      = PER
  )
)

# ---------------------------------------------------------
# Harmoniser le format TIME_PERIOD
# ---------------------------------------------------------
data$marche_immo$ISPI$TIME_PERIOD <- paste0(
  year(as.Date(data$marche_immo$ISPI$TIME_PERIOD)),
  "-Q",
  quarter(as.Date(data$marche_immo$ISPI$TIME_PERIOD))
)

for (x in c("marche_surrevaluation", "financement_marche")) {
  data[[x]] <- lapply(data[[x]], \(d) {
    if ("TIME_PERIOD" %in% names(d))
      d$TIME_PERIOD <- paste0(year(as.Date(d$TIME_PERIOD)), "-Q", quarter(as.Date(d$TIME_PERIOD)))
    d
  })
}

data$marche_surrevaluation$PER$TIME_PERIOD[1] <- "2005-Q1"

rm(list = setdiff(ls(), "data"))

# ---------------------------------------------------------
# Faire la moyenne par trimestre pour les taux crédits
# ---------------------------------------------------------
source(file = "S:/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Scripts/2. Indicateurs_credit.R")

save(data, file = "//P-FS19-DGT.si.local/PAESF/03. Ponctuel/03. Banques/2025/Indicateur risque CCyB/R/Donnees/data_long.RData")
