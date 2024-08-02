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
#
f_gsub <- \(vstring){
  vstring %>% 
    gsub("Ç",'C',.) %>% 
    gsub("Â",'A',.) %>% 
    gsub("É",'E',.) %>% 
    gsub("Ê","E",.) %>% 
    gsub("Á",'A',.) %>% 
    gsub("Ã",'A',.) %>% 
    gsub("Í",'I',.) %>% 
    gsub("Ó",'O',.) %>% 
    gsub("Õ","O",.) %>% 
    gsub("Ú",'U',.) %>% 
    gsub("Ô",'O',.) %>% 
    gsub("-"," ",.) %>%
    gsub("'"," ",.) %>%
    gsub("CIDADE","CID",.) %>% 
    gsub("JD","JARDIM",.) %>% 
    gsub("FLORINIA","FLORINEA",.) %>% 
    gsub("SAO LUIS DO PARAITINGA","SAO LUIZ DO PARAITINGA",.) %>% 
    gsub("MOJI MIRIM","MOGI MIRIM",.)
}
#
f_FxEt_to_1y <- \(v_i, df_f, cols_to_return, v_patsplit = " a "){
  # amplitude da faixa etária
  v_range <- names(df_f)[v_i] %>% 
    str_split_1(.,v_patsplit) %>% as.numeric()
  # dados da faixa etária
  v_data <- df_f[,v_i]/length(v_range[1]:v_range[2])
  # while
  df_return <- data.frame(v_data)
  names(df_return) <- v_range[1]
  v_start <- v_range[1]
  while(v_start<v_range[2]){
    v_start <- v_start + 1
    df_while <- data.frame(v_data)
    names(df_while) <- v_start
    df_return <- cbind(df_return,df_while)
  }
  cbind(df_f[,cols_to_return],df_return)
}
f_casewhen_fxet <- \(dffull, cname, v_fxet, cnameout){
  # objetos comuns
  v_idademais <- v_fxet[str_detect(v_fxet,"\\+")]
  v_outras_fxet <- v_fxet[!str_detect(v_fxet,"\\+")]
  here <- environment()
  # preparação dos objetos para o metaprogramming
  v_base <- paste0(cname," %in% eval(parse(text=v_outras_fxet[i])) ~ v_outras_fxet[i]")
  v_1 <- sapply(1:length(v_outras_fxet),\(x){
    gsub("\\[i\\]",paste0("[",x,"]"),v_base)
  }) %>% paste(.,collapse = ",")
  v_2 <- paste0("TRUE ~ '",v_idademais,"'")
  v_final <- paste0("case_when(",v_1,",",v_2,")")
  assign(cname,value = dffull[[cname]],envir = here)
  # execução das strings
  v_return <- eval(expr = parse(text = v_final))
  dffull[[cnameout]] <- factor(v_return,levels = unique(v_return))
  return(dffull)
}
