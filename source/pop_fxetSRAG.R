library(sf)
library(plyr)
library(tidyverse)
source("../source/general_tools.R")
# objetos comuns
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
  filter(ano%in%2022:2023)
# padronização da base pop dos outros municipios do ESP
df_popESP <- readRDS("../../../11_MAPAS/pop_ESPserietemp_mun_fxet_sexo/pop_ESPserietemp_mun_fxet_sexo.rds")
df_popESP <- df_popESP %>%
  filter(FX_ETARIA!="Total",ANO%in%2022:2023,ID_MUN!=355030) %>% 
  mutate(across(-POP,as.character),
         FX_ETARIA = ifelse(grepl("meses",FX_ETARIA),"0",FX_ETARIA)) %>% 
  group_by(MUN,ANO,FX_ETARIA) %>% 
  summarise(pop = sum(POP),.groups = "drop") %>% 
  mutate(FX_ETARIA = as.character(FX_ETARIA) %>% gsub(" anos","",.))
df_popESP <- df_popESP %>% 
  group_by(MUN,ANO) %>% 
  pivot_wider(names_from=FX_ETARIA,values_from=pop)
df_popESP_adeq <- f_adequa_fxet_pop(dfwider = df_popESP,idcols=c("MUN","ANO"))
# união das bases no formato de sf_cir2
sf_cir2_poptotal <- readRDS("./resultados/sf_cir2_poptotal.rds") 
df_cir <- sf_cir2_poptotal %>% 
  unnest(cols=c(NM_MUN,geometry)) %>% 
  as.data.frame() %>% select(RS_CIR,NM_MUN)
## MSP
df_popMSP <- df_popMSP %>% 
  mutate(RS_CIR=paste0("MSP_",regiao),
         NM_MUN="SAO PAULO") %>% 
  select(-regiao)
## ESP
df_popESP_adeq <- df_popESP_adeq %>% 
  rename(NM_MUN=MUN,ano=ANO) %>% 
  inner_join(.,df_cir)
## merge 1
df_pop <- 
  rbind(df_popMSP,df_popESP_adeq) %>%
  pivot_wider(values_from = "pop", names_from = "ano",names_prefix = "pop") %>% 
  group_by(fxet,RS_CIR) %>% 
  summarise(across(starts_with("pop"),sum))
## merge 2
sf_cir2_fxet <- inner_join(
  sf_cir2_poptotal %>% select(RS_CIR,geometry),
  df_pop
) %>% 
  relocate(geometry,.after=last_col())
#
# auditoria
teste <- inner_join(
  x=sf_cir2_poptotal %>% as.data.frame() %>% select(RS_CIR,pop2022),
  y=sf_cir2_fxet %>% as.data.frame() %>% select(RS_CIR,pop2022) %>% group_by(RS_CIR) %>% summarise(pop2022=sum(pop2022)),
  by="RS_CIR"
)
teste %>% 
  ggplot(aes(x=pop2022.x,y=pop2022.y)) + geom_point() + geom_abline(intercept = 0,slope=1)