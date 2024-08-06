library(sf)
library(plyr)
library(tidyverse)
source("./source/general_tools.R")
# objetos comuns
v_fxet <- formals(f_adequa_fxet_pop)$vfxet
v_anoref <- c(2022,2023)
##### bases #####
# cir
cir<-readxl::read_excel("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/00_dados_auxiliares/DRS_GVS_CEREST_RRAS_CIR_GVE_MUNICIPIO_MATRIZ.xls") %>% dplyr::select(Cod6, RS_CIR, COD_CIR,MUNICIPIO...15) %>% 
  dplyr::rename(co_mun_res=Cod6,
                cod_cir=COD_CIR,
                nm_mn_resi=MUNICIPIO...15) %>% 
  mutate(nm_mn_resi = toupper(nm_mn_resi) %>% f_gsub) %>% 
  rename(NM_MUN = nm_mn_resi)
# dist adm MSP
df_ref_regdist <- read_tsv(file="./documentos_gerais/ref_reg_DistAdm_MSP.txt",
                           locale = locale(encoding = 'latin1'))
df_ref_regdist <- ddply(df_ref_regdist,"regiao",\(dfi){
  v_dist <- strsplit(dfi$distritos,split = ", ") %>% 
    unlist() %>% strsplit(dfi$distritos,split = ",") %>% unlist()
  data.frame(regiao=dfi$regiao[1],
             dist_adm = v_dist)
}) %>% mutate(dist_adm = f_casewhen(dist_adm) %>% 
                gsub("CIDADE","CID",.) %>% 
                gsub("JARDIM","JD",.))
##### rotinas #####
# leitura e padronização da base pop dos dist adm de MSP:
l_csv <- list.files(path="./base_pop_tabnetMSP",pattern=".csv",full.names = TRUE)
df_pop_distadmMSP <- ldply(l_csv,f_readcsv) %>% mutate(across(matches("^[0-9]"),as.numeric))
df_pop_distadmMSP <- f_adequa_fxet_pop(dfwider = df_pop_distadmMSP,
                                       idcols = c("ano","dist_admMSP"))
df_pop_distadmMSP <- df_pop_distadmMSP %>% 
  mutate(dist_admMSP = toupper(dist_admMSP) %>% f_gsub %>% gsub("JARDIM","JD",.))
# v_pop <- teste$dist_admMSP
# v_ref <- df_ref_regdist$dist_adm
# setdiff(v_pop,v_ref)
df_popMSP <- inner_join(
  df_pop_distadmMSP,
  df_ref_regdist,
  by=c("dist_admMSP"="dist_adm")
) %>% 
  group_by(ano,fxet,regiao) %>% 
  summarise(pop=sum(pop)) %>% 
  filter(ano%in%2022:2023) %>% 
  mutate(RS_CIR = paste0("MSP_",regiao),
         MUN = "SAO PAULO") %>% 
  select(-regiao)
#
#############################################################
### padronização da base pop dos outros municipios do ESP ###
#############################################################
df_popESP <- readRDS("../../../11_MAPAS/pop_ESPserietemp_mun_fxet_sexo/pop_ESPserietemp_mun_fxet_sexo.rds")
df_popESP <- df_popESP %>%
  # seleção dos anos de estudo, remoção do MSP e da faixa etária total
  filter(FX_ETARIA!="Total",ANO%in%2022:2023,ID_MUN!=355030) %>% 
  # adequação, transformação das classes abaixo de 1 ano em '0 ano'
  mutate(across(-POP,as.character),
         FX_ETARIA = ifelse(grepl("meses",FX_ETARIA),"0",FX_ETARIA)) %>% 
  # soma das faixa etárias com 0 ano e dos sexos binários
  group_by(MUN,ANO,FX_ETARIA) %>% 
  summarise(pop = sum(POP),.groups = "drop") %>% 
  # padronização para a função que quebra as faixa etárias
  mutate(FX_ETARIA = as.character(FX_ETARIA) %>% gsub(" anos","",.))
# padronizaçaõ para a função de adequação das faixa etárias
df_popESP <- df_popESP %>% 
  group_by(MUN,ANO) %>% 
  pivot_wider(names_from=FX_ETARIA,values_from=pop)
df_popESP_adeq <- f_adequa_fxet_pop(dfwider = df_popESP,idcols=c("MUN","ANO"))
# união das bases no formato de sf_cir2
## leitura dos CIR2
sf_CIR2 <- readRDS(file="./resultados/sf_CIR2.rds")
df_cir2 <- sf_CIR2 %>% unnest(NM_MUN) %>% as.data.frame %>% select(-geometry) %>% 
  mutate(NM_MUN = f_gsub(NM_MUN))
# merge populacao do estado por mun e mun por CIRs
df_popCIR2 <- inner_join(
  df_popESP_adeq,
  df_cir2, by=c("MUN"="NM_MUN")
) %>% group_by(ANO,fxet,RS_CIR) %>% 
  summarise(pop = sum(pop)) %>% 
  rbind(.,df_popMSP %>% rename(ANO=ano) %>% select(-MUN)) %>% 
  mutate(fxet=factor(fxet,levels=c("0:9","10:18","19:60","60+"))) 
#
# salvamento
saveRDS(df_popCIR2,file="./resultados/df_popCIR2_2022e2023.rds")