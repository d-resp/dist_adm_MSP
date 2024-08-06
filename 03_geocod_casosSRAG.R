#
# referência de cnes e nome fantasia dos hospitais existentes entre 2022 e 2023




df_refcnes <- read_csv("G:/CCD/CVE/RESPIRATORIAS/CNES_2022_2023/dados_CNES_2223_MSP.csv")
df_refcnes <- df_refcnes %>% select(MUNICIPIO,CNES,LOGRADOURO:CEP,`NOME FANTASIA`) %>% 
  rename_with(.,tolower)
#
# geocodificação dos casos usados pelo número de CNES notificante
l_csv <- list.files("C:/Users/tzenker/Documents/bases_antiga_forescan",
                    full.names = TRUE,
                    pattern = "EXTRACAO_ROBO_SIVEP_FULL_")
# df_srag <- ldply(l_csv,\(i) vroom::vroom(i, col_types = cols(.default = "c")))
v_cols <- c("ID_MN_RESI","CO_MUN_RES","ID_MUNICIP","CO_MUN_NOT","ID_UNIDADE",
            "CO_UNI_NOT","DT_NOTIFIC","NU_NOTIFIC") %>% tolower()
teste0 <- vroom::vroom(l_csv[1], col_types = cols(.default = "c"))
teste <- teste0 %>% 
  mutate(across(starts_with("dt_"),lubridate::mdy)) %>% 
  filter(between(dt_sin_pri,
                 as.Date("2022-01-01",format="%Y-%m-%d"),
                 as.Date("2023-12-31",format="%Y-%m-%d")),
         id_mn_resi=="SAO PAULO") %>% 
  select(all_of(v_cols)) %>% 
  rename(id_mn_not = id_municip,
         cnes = co_uni_not,
         nm_uni_not=id_unidade) %>% 
  filter(co_mun_not=="355030")
# 
# v_cnes <- df_geocodMSP$cnes %>% unique
# v_cnes_soltos <- setdiff(v_cnes,unique(df_refcnes$cnes))
df_geocodMSP <- left_join(
  df_geocodMSP,
  df_refcnes
) %>% relocate(municipio,.after=last_col())
df_geocodMSP %>% filter(is.na(cep)) %>% select(nm_uni_not,cnes) %>% distinct