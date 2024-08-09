library(geobr)
library(plyr)
library(sf)
library(tidyverse)
source("./source/general_tools.R")
#####################################
# tabela de referência de agregados #
#####################################
# tabela com o nome das CIR
cir<-readxl::read_excel("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/00_dados_auxiliares/DRS_GVS_CEREST_RRAS_CIR_GVE_MUNICIPIO_MATRIZ.xls")
cir<- cir |> dplyr::select(Cod6, RS_CIR, COD_CIR)|>
  dplyr::rename(co_mun_res=Cod6,
                cod_cir=COD_CIR)
# tabela com a região dos distritos do MSP
df_ref_regdist <- read_tsv(file="./documentos_gerais/ref_reg_DistAdm_MSP.txt",locale = locale(encoding = 'latin1'))
df_ref_regdist <- ddply(df_ref_regdist,"regiao",\(dfi){
  v_dist <- strsplit(dfi$distritos,split = ", ") %>% 
    unlist() %>% strsplit(dfi$distritos,split = ",") %>% unlist()
  data.frame(regiao=dfi$regiao[1],
             dist_adm = v_dist)
}) %>% mutate(dist_adm = f_casewhen(dist_adm) %>% 
                gsub("CIDADE","CID",.) %>% 
                gsub("JARDIM","JD",.))
# proposta de fragmentação do CIR de campinas
df_divREGMETCAMP <- read_csv2(file="./dados/proposta_div_RMCampinas.csv") %>% 
  mutate(municipios = toupper(municipios) %>% 
           gsub("SANTA BARBARA D OESTE","SANTA BARBARA D'OESTE",.),
         eixo = factor(eixo,
                       levels=c(1:4,6,8),
                       labels=c(1:6))) %>% 
  rename(NM_MUN=municipios) %>% 
  filter(!is.na(eixo))
#
#####################################
###### shapefiles de trabalho #######
#####################################
# sf com os poligonos dos municípios do ESP
sf_municipios <- geobr::read_municipality(code_muni = "SP",
                                          year = 2022,
                                          showProgress = FALSE) %>%
  select(code_muni,name_muni,geom) %>%
  rename(co_mun_res=code_muni,
         NM_MUN=name_muni) %>%
  mutate(NM_MUN = gsub("í","i",NM_MUN) %>%
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
           toupper(.),
         NM_MUN = dplyr::case_when(
           NM_MUN=="MOGI MIRIM" ~ "MOJI MIRIM",
           NM_MUN=="BIRITIBA MIRIM" ~ "BIRITIBA-MIRIM",
           NM_MUN=="SAO LUIZ DO PARAITINGA" ~ "SAO LUIS DO PARAITINGA",
           NM_MUN=="FLORINEA" ~ "FLORINIA",
           TRUE ~ NM_MUN),
         co_mun_res = str_sub(co_mun_res, end = -2))
st_geometry(sf_municipios) <- "geometry"
# sf com o poligonos das distritros administrativos do MSP
sf_distMSP <- read_sf("./shp_files/LAYER_DISTRITO/DEINFO_DISTRITO.shp") %>% 
  select(NOME_DIST) %>% 
  mutate(NM_MUN="SAO PAULO") %>% 
  relocate(NM_MUN) %>% 
  rename(NM_DIST=NOME_DIST)
#
#####################################
############## ROTINAS ##############
#####################################
# 1) padronização do CRS dos dois shapefiles: 
if(st_crs(sf_municipios) != st_crs(sf_distMSP)){
  sf_distMSP <- st_transform(sf_distMSP, st_crs(sf_municipios))
}
# 2) agregação dos poligonos dos distritos de MSP segundo 
sf_MSP <- inner_join(
  sf_distMSP,
  df_ref_regdist,
  by=c("NM_DIST"="dist_adm")
)
sf_MSP <- sf_MSP %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  group_by(regiao) %>% 
  summarise(geometry = st_union(st_buffer(geometry,45)),
            NM_MUN = head(NM_MUN,n=1)) %>% 
  mutate(RS_CIR = paste0("MSP_",regiao)) %>% 
  select(-regiao) %>% 
  relocate(NM_MUN,RS_CIR)
# 3) rbind regiões de MSP e outros municipios do ESP
sf_munESP <- rbind(
  sf_municipios %>% filter(NM_MUN!="SAO PAULO") %>% mutate(RS_CIR=NA),
  sf_MSP %>% mutate(co_mun_res="353050")
)
# 4 agregação por CIR2
## 4.1 merge da tabela de CIR com a nova proposta de CIR da Reg. Met. Camp.
cir <- left_join(cir,
                 sf_municipios %>% as.data.frame() %>% select(co_mun_res:NM_MUN)) %>% 
  relocate(NM_MUN)
df_cir_sMSP <- left_join(cir,df_divREGMETCAMP) %>% 
  mutate(RS_CIR=ifelse(grepl("CAMPINAS",RS_CIR),paste0("CAMP_",eixo),RS_CIR)) %>% 
  select(-eixo) %>% 
  filter(co_mun_res!="355030")
## 4.2 merge de df_cir_sMSP
sf_munESP[is.na(sf_munESP$RS_CIR),] <- inner_join(
  x=sf_munESP[is.na(sf_munESP$RS_CIR),] %>% select(-RS_CIR),
  y=df_cir_sMSP %>% select(NM_MUN,RS_CIR),
  by="NM_MUN"
)
## 4.3 fusão dos poligonos de acordo com o CIR2
sf_munESP <- st_make_valid(sf_munESP)
sf_CIR2 <- sf_munESP %>% 
  mutate(geometry = st_buffer(geometry, 1e-9)) %>% 
  group_by(RS_CIR) %>% 
  summarise(geometry = st_union(st_buffer(geometry,45)),
            NM_MUN = list(unique(NM_MUN))) %>% 
  relocate(geometry,.after=last_col())
sf_unnest <- sf_CIR2 %>% unnest(c(NM_MUN,geometry))
write_sf(sf_unnest,"./resultados/sf_CIR2.shp")
# saveRDS(sf_CIR2,file="./resultados/sf_CIR2.rds")