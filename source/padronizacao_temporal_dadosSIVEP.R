rm(list = ls())
gc()

#library(readr)
#if (!require("rmarkdown")) install.packages("rmarkdown"); library(rmarkdown)

if (!require("vroom")) install.packages("vroom"); library(vroom)
if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
#if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')
if(!require ('readxl')) {install.packages('readxl')};library('readxl')
if(!require ('sf')) {install.packages('sf')};library('sf')
#if(!require ('rgdal')) {install.packages('rgdal')};library('rgdal')
if(!require ('spdep')) {install.packages('spdep')};library('spdep')
if(!require ('rgeos')) {install.packages('rgeos')};library('rgeos')
if(!require ('INLA')) {install.packages('INLA')};library('INLA')


library(tidyverse)
library(vroom)
library(aweek)
library(plyr)
# library(nowcaster)


###Funções
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
setwd("G:/CCD/CVE/RESPIRATORIAS/05_SRAG_SISTEMA_DE_ALERTA")
source("03_script/fct/nowcasting.summary_cir.R")
##Baixando cir
cir<-read_excel("00_dados_auxiliares/DRS_GVS_CEREST_RRAS_CIR_GVE_MUNICIPIO_MATRIZ.xls")
###Renomeando a base CIR por municipio
cir<- cir |> dplyr::select(Cod6, RS_CIR, COD_CIR)|>
  dplyr::rename(co_mun_res=Cod6,
                cod_cir=COD_CIR)
temp = list.files(path="C:/Users/dpmori/Desktop/bases_paper_SATSCAN",
                  pattern="EXTRACAO_ROBO",full.names = TRUE)
v_endereco <- c("NM_LOGRADO","NU_NUMERO","NM_BAIRRO","NU_CEP") %>% 
  tolower()
f_padronizacao <- \(vstring){
  # leitura
  dados<-vroom(vstring)
  dados$co_mun_res<-as.character(dados$co_mun_res)
  ##Juntando sivep com a base CIR
  dado2<- dplyr::inner_join(dados, cir, by="co_mun_res")
  dado0 <- dado2 |>
    dplyr::mutate(obt = ifelse(evolucao == '2', 1, 0)) |>
    dplyr::select(nu_notific, nm_pacient, co_mun_res, sg_uf,
                  obt,dt_sin_pri, dt_digita, dt_pcr, pcr_sars2, dt_notific,
                  an_sars2, classi_fin, dt_nasc, nu_idade_n, tp_idade, cod_cir,
                  all_of(v_endereco)) |>
    dplyr:: mutate(ID_MN_RESI=substr(co_mun_res,1,6)) |>
    dplyr:: mutate(ID_MN_RESI=as.double(ID_MN_RESI))
  names(dado0) <- tolower(names(dado0))
  dado0 <- dado0  |>
    dplyr::mutate_at(vars(starts_with('dt_')), lubridate::mdy)
  dado0 <- dado0 |>
    dplyr:: filter(sg_uf == 'SP') |>
    dplyr::mutate(dt_report = pmax (dt_digita, dt_notific, na.rm=TRUE),
                  idade = dplyr::case_when(
                    is.na(dt_nasc) & tp_idade == 3 ~ nu_idade_n,
                    is.na(dt_nasc) & tp_idade  < 3 ~ 0,
                    !is.na(dt_nasc)                ~ trunc(as.numeric
                                                           (difftime(dt_sin_pri,
                                                                     dt_nasc,
                                                                     units = "days"))/365.25)
                  )
    )
  #### Deduplicação
  dado0 <- dado0 |>
    dplyr::arrange(desc(obt), dt_digita, dt_sin_pri, nu_notific) |>
    dplyr::mutate(chave = paste0(nm_pacient, dt_nasc, dt_sin_pri)) |>
    dplyr::mutate(chave = texto_simples(chave))
  dado <- dado0 |>
    dplyr::filter(!duplicated(nu_notific)) |>
    dplyr::filter(!duplicated(chave))
  # seleção das variáveis de importância para a a análise de validação
  dado <- dado |>
    dplyr:: mutate(date_report=dt_report,
                   date_onset=dt_sin_pri)
  #
  K<-1
  Dmax <- 10
  wdw <- 10
  ## Filtering out cases without report date
  dados <- dado |>
    tidyr::drop_na(date_report)
  ##Data máxima rport
  DT_max <- max(dados$date_report) + 7*K ###forecasting
  ##Data máxima do dia da semana
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
  data_w <- dados |>
    dplyr::filter(date_report <= DT_max ) |>
    dplyr::mutate(
      ## Altering the date for the first day of the week
      date_onset = date_onset -
        as.integer(format(date_onset, "%w")) -
        (6-DT_max_diadasemana),
      date_report = date_report -
        as.integer(format(date_report, "%w")) -
        (6-DT_max_diadasemana),
      Delay = as.numeric(date_report - date_onset) / 7) |>
    dplyr::filter(Delay >= 0,
                  co_mun_res=="355030")
  Tmax <- max(data_w |> dplyr::pull(var = date_onset))
  data.inla <- data_w  |>
    dplyr::filter(date_onset >= Tmax - 7 * wdw,
                  Delay <= Dmax)
  return(data.inla)
}
df_padronizado <- adply(temp,1,f_padronizacao,.id = NULL)
df_towrite <- df_padronizado %>% select(-co_mun_res,sg_uf) %>% distinct()
saveRDS(df_towrite,file="../_EQUIPE/DaniloPereiraMori/dist_adm_MSP/dados/casosSRAG_dt_padronizado.rds")
