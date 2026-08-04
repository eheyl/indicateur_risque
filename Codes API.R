###################################################################################################
###################                  0. Import des fonctions                    ###################
###################################################################################################


#library(XML)
library(xml2)
library(httr)
library(dplyr)
library(jsonlite)
library(tidyr)


###################################################################################################
###################           1. Obtention de séries individuelles              ###################
###################################################################################################



get_insee_uni <- function(id){
  ## Cette fonction récupère une série temporelle de l'INSEE à partir de son identifiant
  ##
  # Sortie: "serie_chrono", tableau de deux colonnes (1e colonne: TIME_PERIOD / 2e colonne: valeurs de la série)
  #
  # Arguments:
  #   "id": character string contenant l'identifiant de la série INSEE

  url_INSEE <- sprintf("https://bdm.insee.fr/series/sdmx/data/SERIES_BDM/%s?", id)
  response_INSEE <- GET(url_INSEE)
  
  #Récupère les données de l'url sous forme d'un xml_document
  xmlData <- read_xml(content(response_INSEE,type="text", encoding = "UTF-8")) 
  
  #Donne un nodeset de taille nobs, chaque node étant une observation
  xmlObs <- xml_children(xml_children(xml_children(xmlData))) 
  
  #Transforme ce nodeset en dataframe
  df_obs <- bind_rows(lapply(xml_attrs(xmlObs), function(x) data.frame(as.list(x))))
  
  #Garde seulement la période et la valeur (les deux données qui nous intéressent)
  serie_chrono <- df_obs[c('TIME_PERIOD', 'OBS_VALUE')]
  names(serie_chrono)[2] <- id 
  serie_chrono <- serie_chrono[!is.na(serie_chrono$TIME_PERIOD),]
  
  return (serie_chrono)
}



#Comment déterminer le code d'une série sur Eurostat (par exemple, NASQ_10_NF_TR/Q.CP_MEUR.PAID.S14_S15.B6G.SCA.FR)

#Pour cela, il faut d'abord déterminer la "key-series template" (cad quels attributs sont à spécifier, et dans quel ordre)
#Cette template est spécifique au dataset sur lequel on travaille; ici, prenons l'exemple du dataset ISOC_CI_ID_H 
#(exemple utilisé dans la documentation de l'API) 

#Ouvrir "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/ISOC_CI_ID_H"
#En remplaçant la référence du dataset (le "code des données en ligne")

#Ca va ouvrir un xml: aller voir les variables appelées "Dimension", et prendre dans l'ordre (il y a une indication appelée "position, qui
#indique dans quel ordre doivent se placer les indicateurs)

#Par exemple, pour ISOC_CI_ID_H, on a "id=freq position = 1", "id=indic_is position=2", etc.

#Si on rassemble tout ça ensemble, on obtient une "series-key template" du type: FREQ.INDIC_IS.UNIT.HHTYP.GEO (pour ISOC_CI_ID_H)
#A noter que la dernière position est (normalement) "TIME_PERIOD", et est donc optionnelle (et à ne pas spécifier si on veut toutes les valeurs)


get_eurostat_uni <- function(id){
  ## Cette fonction récupère une série temporelle depuis Eurostat à partir de son identifiant
  ##
  # Sortie: "serie_chrono", tableau de deux colonnes (1e colonne: TIME_PERIOD / 2e colonne: valeurs de la série)
  #
  # Arguments:
  #   "id": character string contenant l'identifiant de la série INSEE

  url_eurostat <- sprintf("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/%s", id)
  response_eurostat <- GET(url_eurostat)
  
  #Récupère les données de l'url sous forme d'un xml_document
  xmlData <- read_xml(content(response_eurostat,type="text", encoding = "UTF-8"))
  
  #Récupère les observations (période + valeur)
  xmlObs <- xml_children(xml_children(xml_children(xmlData)))
  xmlObs <- xml_find_all(xmlObs, '//g:Obs') 
  
  #Récupère les périodes et met dans un dataframe (de 1 colonne)
  dim <- xml_attrs(xml_find_all(xmlObs, '//g:ObsDimension')) #Récupère les dimensions (ie périodes) sous forme de liste
  df_dim <- bind_rows(lapply(dim, function(x) data.frame(as.list(x)))) #Met tout dans un dataframe (de 1 colonne)
  names(df_dim)[1] <- 'TIME_PERIOD'
  
  #Récupère les valeurs et met dans un dataframe (de 1 colonne)
  val <- xml_attrs(xml_find_all(xmlObs, '//g:ObsValue'))
  df_val <- bind_rows(lapply(val, function(x) data.frame(as.list(x))))
  names(df_val)[1] <- id 
  
  #Merge les deux séries
  serie_chrono <- cbind(df_dim, df_val)
  
  return (serie_chrono)
}



get_ecb_uni <- function(id){
  ## Cette fonction récupère une série temporelle de l'API de l'ECB à partir de son identifiant
  ##
  # Sortie: "serie_chrono", tableau de deux colonnes (1e colonne: TIME_PERIOD / 2e colonne: valeurs de la série)
  #
  # Arguments:
  #   "id": character string contenant l'identifiant de la série INSEE

  id_slash <- sub('\\.', '/', id) #Il faut remplacer le premier point par un / pour utiliser l'API de l'ECB
  
  url_BCE <- sprintf("https://data-api.ecb.europa.eu/service/data/%s", id_slash)
  response_BCE <- GET(url_BCE)
  
  #Récupère les données de l'url sous forme d'un xml_document
  xmlData <- read_xml(content(response_BCE,type="text", encoding = "UTF-8")) 
  
  
  xmlObs <- xml_children(xml_children(xml_children(xmlData)))
  xmlObs <- xml_find_all(xmlObs, '//generic:Obs') 
  
  #Récupère les périodes et met dans un dataframe (de 1 colonne)
  dim <- xml_attrs(xml_find_all(xmlObs, '//generic:ObsDimension')) #Récupère les dimensions (ie périodes) sous forme de liste
  df_dim <- bind_rows(lapply(dim, function(x) data.frame(as.list(x)))) #Met tout dans un dataframe (de 1 colonne)
  names(df_dim)[1] <- 'TIME_PERIOD'
  
  #Récupère les valeurs et met dans un dataframe (de 1 colonne)
  val <- xml_attrs(xml_find_all(xmlObs, '//generic:ObsValue'))
  df_val <- bind_rows(lapply(val, function(x) data.frame(as.list(x))))
  names(df_val)[1] <- id 
  
  #Merge les deux séries
  serie_chrono <- cbind(df_dim, df_val)
  
  return (serie_chrono)
}


get_webstat_uni <- function(id, headers){
  url_webstat <- paste0(
    "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json",
    "?order_by=time_period_start&refine=series_key:", id
  )
  
  response_webstat <- httr::RETRY(
    "GET",
    url_webstat,
    headers,
    httr::config(ssl_verifypeer = FALSE),
    times = 5,
    terminate_on = c(400, 401, 403, 404)
  )
  
  if (httr::status_code(response_webstat) != 200) {
    stop(sprintf("Webstat a renvoyé HTTP %s pour %s",
                 httr::status_code(response_webstat), id))
  }
  
  txt  <- httr::content(response_webstat, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(txt, flatten = TRUE)
  
  if (!all(c("time_period","obs_value") %in% names(data))) {
    stop("Colonnes inattendues : j’attends 'time_period' et 'obs_value'.")
  }
  
  serie_chrono <- as.data.frame(data[c("time_period","obs_value")])
  names(serie_chrono) <- c("TIME_PERIOD","OBS_VALUE")
  serie_chrono <- serie_chrono[!is.na(serie_chrono$TIME_PERIOD), ]
  
  return(serie_chrono)
}



get_fred_uni <- function(id, api_key){
  ## Cette fonction récupère une série temporelle de la Federal Reserve Bank of St Louis (FRED) à partir de son identifiant
  ##
  # Sortie: "serie_chrono", tableau de deux colonnes (1e colonne: TIME_PERIOD / 2e colonne: valeurs de la série)
  #
  # Arguments:
  #   "id": character string contenant l'identifiant de la série FRED
  #    "api_key": Clé API nécessaire pour accéder à l'API de FRED. A obtenir avec la fonction add_headers, après s'être créé une clé individuelle sur le site de FRED
  
  url_FRED <- sprintf('https://api.stlouisfed.org/fred/series/observations?series_id=%s&api_key=%s', id, api_key)
  response_FRED <- GET(url_FRED)
  
  #Récupère les données de l'url sous forme d'un xml_document
  xmlData <- read_xml(content(response_FRED,type="text", encoding = "UTF-8")) 
  
  #Donne un nodeset de taille nobs, chaque node étant une observation
  xmlObs <- xml_children(xmlData)
  
  #Transforme ce nodeset en dataframe
  df_obs <- bind_rows(lapply(xml_attrs(xmlObs), function(x) data.frame(as.list(x))))
  
  #Garde seulement la période et la valeur (les deux données qui nous intéressent)
  serie_chrono <- df_obs[c('date', 'value')]
  names(serie_chrono) <- c('TIME_PERIOD', id) 
  serie_chrono <- serie_chrono[!is.na(serie_chrono$TIME_PERIOD),]
  
  return (serie_chrono)
}



get_bis_uni <- function(id){
  ## Cette fonction récupère une série temporelle de la BIS (Bank of International Settlements) à partir de son identifiant
  ##
  # Sortie: "serie_chrono", tableau de deux colonnes (1e colonne: TIME_PERIOD / 2e colonne: valeurs de la série)
  #
  # Arguments:
  #   "id": character string contenant l'identifiant de la série BIS: de la forme "WS_TC/Q.FR.H.A.M.XDC.A". La première partie correspond au "flux" de la série
  #         (ie sa base de données spécifique au sein des stats de la BIS). Sur les pages statistiques de la BIS, elle est présentée sous la forme "BIS,WS_TC,2.0"
  
  url_BIS <- sprintf("https://stats.bis.org/api/v1/data/%s?", id)
  response_BIS <- GET(url_BIS)
  
  #Récupère les données de l'url sous forme d'un xml_document
  xmlData <- read_xml(content(response_BIS,type="text", encoding = "UTF-8")) 
  
  #Donne un nodeset de taille nobs, chaque node étant une observation
  xmlObs <- xml_children(xml_children(xml_children(xmlData))) 
  
  #Transforme ce nodeset en dataframe
  df_obs <- bind_rows(lapply(xml_attrs(xmlObs), function(x) data.frame(as.list(x))))
  
  #Garde seulement la période et la valeur (les deux données qui nous intéressent)
  serie_chrono <- df_obs[c('TIME_PERIOD', 'OBS_VALUE')]
  names(serie_chrono)[2] <- id 
  serie_chrono <- serie_chrono[!is.na(serie_chrono$TIME_PERIOD),]
  
  return (serie_chrono)
}


###################################################################################################
###################            2. Obtention de plusieurs séries                 ###################
###################################################################################################


######## ATTENTION: pour toutes les fonctions suivantes, il est à la charge de l'utilisateur de vérifier que les dates de chaque série temporelle sont du même 
######## format (ie mensuel, trimestriel, quotidien, annuel...), et que les séries ont au moins une date en commun.



merge_API <- function(df1, df2){
  #Cette fonction merge deux jeux de données (contenant une ou des séries temporelles provenant des API précédentes), en gardant l'ordre des dates (quelque soit
  #leur format), et en remplissant de NA les variables où il manque des données pour certaines dates.
  #
  #Sortie: "df_wide": un dataframe (en format wide) dont la première colonne contient les dates des observations, et la deuxième les valeurs
  
  col <- c('TIME_PERIOD', names(df1)[-1], names(df2)[-1]) #On enregistre l'ordre des colonnes, pour les sortir dans le même sens
  
  if (typeof(df1[[2]]) != typeof(df2[[2]])){
    stop ("Erreur: les types internes des colonnes (voir avec typeof(df[[2]]) ne sont pas égaux")
  }
  
  #On passe les dataframes en format long
  df1 <- df1 %>% pivot_longer(cols=colnames(df1[-1]),
                              names_to = 'id',
                              values_to = 'obs')
  
  df2 <- df2 %>% pivot_longer(cols=colnames(df2[-1]),
                              names_to = 'id',
                              values_to = 'obs')
  
  if (df2[1,1] %in% df1$TIME_PERIOD){ #ie si df2 commence après df1
    df_wide <- bind_rows(df1, df2) %>% pivot_wider(names_from = id, values_from = obs) #Merge et repasse en format long
    
  } else if (df1[1,1] %in% df2$TIME_PERIOD){
    df_wide <- bind_rows(df2, df1) %>% pivot_wider(names_from = id, values_from = obs)
    
  } else {
    stop ("Erreur: les dates des dataframes ne correspondent pas")
    
  }
  return (df_wide[col])
}



get_insee <- function(id){
  ## Cette fonction récupère n séries temporelles de l'INSEE à partir de leurs identifiants
  ##
  # Sortie: "serie_chrono", tableau de n+1 colonnes
  #
  # Arguments:
  #   "id": character ou vecteur de character, contenant le ou les identifiants des séries INSEE
  
  #Gestion d'erreurs potentielles
  if (length(unique(id)) != length(id)){
    stop("Erreur: il y a des doublons dans la liste des codes")
  }
  

  if (length(id) == 1){ #Si un seul identifiant, on applique directement la fonction pour une seule variable
    df <- get_insee_uni(id)
    
  } else { #Si plusieurs séries de données à récupérer, on les ajoute une à une en faisant une boucle avec merge_API()
    n <- length(id)
    df1 <- get_insee_uni(id[1])
    
    for (i in 2:n){
      df2 <- get_insee_uni(id[i])
      
      if (dim(df2)[1] == 0){
        stop(paste0("La série ", id[i], " n'existe pas"))
      }
      
      df1 <- merge_API(df1, df2)
    }
    
    df <- df1[c('TIME_PERIOD',id)] #On remet les colonnes dans l'ordre de commandes
    
  }
  return (df)
}



get_eurostat <- function(id){
  ## Cette fonction récupère n séries temporelles de Eurostat à partir de leurs identifiants
  ##
  # Sortie: "serie_chrono", tableau de n+1 colonnes
  #
  # Arguments:
  #   "id": character ou vecteur de character, contenant le ou les identifiants des séries eurostat
  
  #Gestion d'erreurs potentielles
  if (length(unique(id)) != length(id)){
    stop("Erreur: il y a des doublons dans la liste des codes")
  }

  if (length(id) == 1){ #Si un seul identifiant, on applique directement la fonction pour une seule variable
    df <- get_eurostat_uni(id)
    
    if (dim(df)[1] == 0){
      stop(paste0("La série ", id, " n'existe pas"))
    }
    
  } else { #Si plusieurs séries de données à récupérer, on les ajoute une à une en faisant une boucle avec merge_API()
    n <- length(id)
    df1 <- get_eurostat_uni(id[1])
    
    if (dim(df1)[1] == 0){
      stop(paste0("La série ", id[1], " n'existe pas"))
    }
    
    for (i in 2:n){
      df2 <- get_eurostat_uni(id[i])
      
      if (dim(df2)[1] == 0){
        stop(paste0("La série ", id[i], " n'existe pas"))
      }
      
      df1 <- merge_API(df1, df2)
    }
    
    df <- df1[c('TIME_PERIOD',id)] #On remet les colonnes dans l'ordre de commandes
    
  }
  return (df)
}




get_ecb <- function(id){
  ## Cette fonction récupère n séries temporelles de la base de données de l'ECB à partir de leurs identifiants
  ##
  # Sortie: "serie_chrono", tableau de n+1 colonnes
  #
  # Arguments:
  #   "id": character ou vecteur de character, contenant le ou les identifiants des séries ecb
  
  #Gestion d'erreurs potentielles
  if (length(unique(id)) != length(id)){
    stop("Erreur: il y a des doublons dans la liste des codes")
  }

  if (length(id) == 1){ #Si un seul identifiant, on applique directement la fonction pour une seule variable
    df <- get_ecb_uni(id)
    
  } else { #Si plusieurs séries de données à récupérer, on les ajoute une à une en faisant une boucle avec merge_API()
    n <- length(id)
    df1 <- get_ecb_uni(id[1])
    
    for (i in 2:n){
      df2 <- get_ecb_uni(id[i])
      
      if (dim(df2)[1] == 0){
        stop(paste0("La série ", id[i], " n'existe pas"))
      }
      
      df1 <- merge_API(df1, df2)
    }
    
    df <- df1[c('TIME_PERIOD',id)] #On remet les colonnes dans l'ordre de commandes
    
  }
  return (df)
}


get_webstat <- function(id, headers){
  if (length(unique(id)) != length(id)){
    stop("Erreur: il y a des doublons dans la liste des codes")
  }
  
  if (length(id) == 1){
    df <- get_webstat_uni(id, headers)
  } else {
    n   <- length(id)
    df1 <- get_webstat_uni(id[1], headers)
    
    for (i in 2:n){
      df2 <- get_webstat_uni(id[i], headers)
      
      if (nrow(df2) == 0){
        stop(paste0("La série ", id[i], " n'existe pas"))
      }
      
      df2 <- df2 %>% rename(!!id[i] := OBS_VALUE)
      df1 <- df1 %>% rename(!!id[1] := OBS_VALUE)
      
      df1 <- merge(df1, df2, by = "TIME_PERIOD", all = TRUE)
    }
    
    df <- df1
  }
  return(df)
}


get_fred <- function(id, api_key){
  ## Cette fonction récupère n séries temporelles de la Federal Reserve Bank of St Louis à partir de leurs identifiants
  ##
  # Sortie: "serie_chrono", tableau de n+1 colonnes
  #
  # Arguments:
  #   "id": character ou vecteur de character, contenant le ou les identifiants des séries FRED
  #    "api_key": Clé API nécessaire pour accéder à l'API de FRED. A obtenir avec la fonction add_headers, après s'être créé une clé individuelle sur le site de FRED
  
  #Gestion d'erreurs potentielles
  if (length(unique(id)) != length(id)){
    stop("Erreur: il y a des doublons dans la liste des codes")
  }
  
  if (length(id) == 1){ #Si un seul identifiant, on applique directement la fonction pour une seule variable
    df <- get_fred_uni(id, api_key)
    
  } else { #Si plusieurs séries de données à récupérer, on les ajoute une à une en faisant une boucle avec merge_API()
    n <- length(id)
    df1 <- get_fred_uni(id[1], api_key)
    
    for (i in 2:n){
      df2 <- get_fred_uni(id[i], api_key)
      
      if (dim(df2)[1] == 0){
        stop(paste0("La série ", id[i], " n'existe pas"))
      }
      
      df1 <- merge_API(df1, df2)
    }
    
    df <- df1[c('TIME_PERIOD',id)] #On remet les colonnes dans l'ordre de commandes
    
  }
  return (df)
}



get_bis <- function(id){
  ## Cette fonction récupère n séries temporelles de la BIS à partir de leurs identifiants
  ##
  # Sortie: "serie_chrono", tableau de n+1 colonnes
  #
  # Arguments:
  #   "id": character ou vecteur de character, contenant le ou les identifiants des séries FRED
  
  #Gestion d'erreurs potentielles
  if (length(unique(id)) != length(id)){
    stop("Erreur: il y a des doublons dans la liste des codes")
  }

  if (length(id) == 1){ #Si un seul identifiant, on applique directement la fonction pour une seule variable
    df <- get_bis_uni(id)
    
  } else { #Si plusieurs séries de données à récupérer, on les ajoute une à une en faisant une boucle avec merge_API()
    n <- length(id)
    df1 <- get_bis_uni(id[1])
    
    for (i in 2:n){
      df2 <- get_bis_uni(id[i])
      
      if (dim(df2)[1] == 0){
        stop(paste0("La série ", id[i], " n'existe pas"))
      }
      
      df1 <- merge_API(df1, df2)
    }
    
    df <- df1[c('TIME_PERIOD',id)] #On remet les colonnes dans l'ordre de commandes
    
  }
  return (df)
}





