library(plyr)
library(sf)
library(tidyverse)
# tabela com o nome das CIR
cir<-readxl::read_excel("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA/00_dados_auxiliares/DRS_GVS_CEREST_RRAS_CIR_GVE_MUNICIPIO_MATRIZ.xls")
###Renomeando a base CIR por municipio
cir<- cir |> dplyr::select(Cod6, RS_CIR, COD_CIR)|>
  dplyr::rename(co_mun_res=Cod6,
                cod_cir=COD_CIR)
