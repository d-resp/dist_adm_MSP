f_casewhen <- \(vstring){
  v_return <- vstring %>% tolower(.) %>% 
    gsub("í","i",.) %>%
    gsub("ã","a",.) %>%
    gsub("â","a",.) %>%
    gsub("á|Á","a",.) %>%
    gsub("ó|Ó","o",.) %>%
    gsub("ô","o",.) %>%
    gsub("õ|Ô","o",.) %>%
    gsub("é","e",.) %>%
    gsub("ç","c",.) %>%
    gsub("ê","e",.) %>%
    gsub("ú","u",.) %>%
    gsub("ó","o",.) %>%
    gsub("ô","o",.) %>%
    gsub("í|Í","i",.) %>%
    toupper(.)
  v_return <- case_when(
    v_return=="MOGI MIRIM" ~ "MOJI MIRIM",
    v_return=="BIRITIBA MIRIM" ~ "BIRITIBA-MIRIM",
    v_return=="SAO LUIZ DO PARAITINGA" ~ "SAO LUIS DO PARAITINGA",
    v_return=="FLORINEA" ~ "FLORINIA",
    TRUE ~ v_return)
  v_return %>% 
    gsub("APARECIDA D'OESTE","APARECIDA D OESTE",.) %>% 
    gsub("ARCO-IRIS","ARCO IRIS",.) %>% 
    gsub("BIRITIBA-MIRIM","BIRITIBA MIRIM",.) %>% 
    gsub("EMBU-GUACU","EMBU GUACU",.) %>% 
    gsub("ESTRELA D'OESTE","ESTRELA D OESTE",.) %>% 
    gsub("FLORINIA","FLORINEA",.) %>% 
    gsub("GUARANI D'OESTE","GUARANI D OESTE",.) %>% 
    gsub("MOJI MIRIM","MOGI MIRIM",.) %>% 
    gsub("PALMEIRA D'OESTE","PALMEIRA D OESTE",.) %>% 
    gsub("PARIQUERA-ACU","PARIQUERA ACU",.) %>% 
    gsub("SANTA BARBARA D'OESTE","SANTA BARBARA D OESTE",.) %>% 
    gsub("SANTA CLARA D'OESTE","SANTA CLARA D OESTE",.) %>% 
    gsub("SANTA RITA D'OESTE","SANTA RITA D OESTE",.) %>% 
    gsub("SAO JOAO DO PAU D'ALHO","SAO JOAO DO PAU D ALHO",.) %>% 
    gsub("SAO LUIS DO PARAITINGA","SAO LUIZ DO PARAITINGA",.)
}

# f_geocod_regMSP
f_geocod_regMSP <- \(df_novoscasos,method_geocod="here",api_key=NULL){
  if(!any(grepl("Logradouro_Completo",names(df_novoscasos)))){
    stop("Logradouro_Completo deve ser no formato:\n rua, número, bairro, município, cep, estado, pais")
  }
  if(nrow(df_novoscasos)>1000){
    stop("máximo de 1000 solicitações por dia no método 'here',\n reduza o número de pedidos")
  }
  if(method_geocod=="here"&is.null(api_key)){
    Sys.setenv(HERE_API_KEY = "satAmDy2WRSF25bYxXicCaM8olNeM8GB5YA15wAwAUM")
  }
  if(!is.null(api_key)){
    Sys.setenv(HERE_API_KEY = api_key)
  }
  # geocodificação per se
  
  #
  sf_regMSP
  sf_joint_pp <- st_join(sf_pontos,sf_regMSP)
  return(sf_joint_pp)
}