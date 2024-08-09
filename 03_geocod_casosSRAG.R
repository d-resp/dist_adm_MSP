library(plyr)
library(tidyverse)
library(microdatasus)
library(tidygeocoder)
texto_simples <- function(x){
  var = stringr::str_to_upper(x) #capital letters
  var = gsub("Á","A", var) #replacing all punctuation
  var = gsub("Â","A", var)
  var = gsub("Ã","A", var)
  var = gsub("À","A", var)
  var = gsub("É","E", var)
  var = gsub("Ê","E", var)
  var = gsub("Ê","E", var)
  var = gsub("Í","I", var)
  var = gsub("Ó","O", var)
  var = gsub("Õ","O", var)
  var = gsub("Ô","O", var)
  var = gsub("Ú","U", var)
  var = gsub("Ü","U", var)
  var = gsub("Ç","C", var)
  var = gsub("´","", var)
  var = gsub(" ","", var)
  var = gsub("[[:punct:]]" , "", var)  #taking out special characters
  return(var)
}
# referência de cnes e nome fantasia dos hospitais existentes entre 2022 e 2023
df_refcnes <- read_csv("G:/CCD/CVE/RESPIRATORIAS/CNES_2022_2023/dados_CNES_2223_MSP.csv")
df_refcnes <- df_refcnes %>% select(MUNICIPIO,CNES,LOGRADOURO:CEP,`NOME FANTASIA`) %>% 
  rename_with(.,tolower)
# dados usados no artigo
df_srag <- list.files("./dados/base_padronizada/",pattern = ".rds",full.names = T) %>% 
  lapply(.,readRDS) %>% do.call("rbind",.)
df_srag <- df_srag %>% distinct() %>% filter(!duplicated(chave)) %>% 
  rename(cnes = co_uni_not)
df_sragMSP <- df_srag %>% filter(co_mun_res=="355030")
# merge das bases
df_geocod <- left_join(df_sragMSP,df_refcnes)
# preparação Reduce
f_paste <- paste
formals(f_paste)$sep=", "
v_cols <- c("logradouro","numero","bairro","cep","municipio")
# endereço para geocod:
df_geocod$endereco <- Reduce("f_paste",df_geocod[,v_cols])
# df sem endereço:
df_tocomplete0 <- df_geocod %>% filter(grepl("NA, NA",endereco))
# com endereço para geocod:
df_geocod <- df_geocod %>% filter(!grepl("NA, NA",endereco))
#
###############################################################
########## completar manualmente o endereço dos CNES ##########
###############################################################
#
# base filtrada com os cnes que não há informação disponível
# df_tocomplete <- df_tocomplete0 %>% 
#   select(id_unidade:cnes) %>% distinct() %>% as.data.frame()
# # exemplo de obtenção do endereço completo manualmente:
# for(i in 1:nrow(df_tocomplete0)){
#   df_tocomplete0$endereco[1] <- readline(prompt = paste("endereco completo de",
#                                                         df_tocomplete0[i,"cnes"], ": "))
# }
# rbind com df_geocod:

###############################################################
#
#######################################################################
########################### Geocod per se #############################
#######################################################################
#
if(!file.exists("./resultados/parciais/df_geocod.csv")){
  # chave da plataforma HERE
  Sys.setenv(HERE_API_KEY = "satAmDy2WRSF25bYxXicCaM8olNeM8GB5YA15wAwAUM")
  # geocode
  df_geocod <- df_geocod %>%  # a função já pega apenas os casos distintos para geocodificar!
    geocode(address = endereco, method="here")
  # algum sem coord?
  df_aud <- df_geocod %>% filter(if_any(matches("lat|long"),is.na)) %>% select(endereco,lat:long) %>% distinct()
  if(nrow(df_aud)>0){
    for(i in 1:nrow(df_aud)){
      df_aud$lat_long_gmaps[i] <- readline(prompt = paste("lat, long do cnes", df_aud[i,"endereco"], ": "))
    }  
    df_aud <- df_aud %>% 
      separate(lat_long_gmaps, into = c("lat2", "long2"), sep = ", ") %>% 
      select(-lat,-long) %>% 
      mutate(across(matches("lat|long"),as.numeric))
    df_geocod <- left_join(df_geocod,df_aud,by="endereco")
    df_geocod[is.na(df_geocod$lat),c("lat","long")] <- df_geocod[is.na(df_geocod$lat),c("lat2","long2")]
    df_geocod <- df_geocod %>% select(-matches("2$"))
  }
  write_csv(df_geocod,file="./resultados/parciais/df_geocod.csv")
}else{
  df_geocod <- read_csv(file="./resultados/parciais/df_geocod.csv")
}
#
#######################################################################
################### Classificação segundo o cir 2######################
#######################################################################
#
################### Classificação MSP

library(sf)
sf_CIR2 <- read_sf("./resultados/sf_CIR2.shp")
sf_MSP <- sf_CIR2 %>% filter(NM_MUN=="SAO PAULO")
#
df_geocod <- df_geocod %>% st_as_sf(.,coords = c("long", "lat"), crs = st_crs(sf_MSP))
df_geocod <- st_transform(df_geocod, crs = st_crs(sf_MSP))
df_geocod <- st_join(df_geocod,sf_MSP)

#
################## Classificação fora de MSP
cir<-readxl::read_excel("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/00_dados_auxiliares/DRS_GVS_CEREST_RRAS_CIR_GVE_MUNICIPIO_MATRIZ.xls")
cir<- cir |> dplyr::select(Cod6, RS_CIR, COD_CIR)|>
  dplyr::rename(co_mun_res=Cod6,
                cod_cir=COD_CIR)


df_srag_nMSP <- df_srag %>% filter(co_mun_res!="355030")

