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
