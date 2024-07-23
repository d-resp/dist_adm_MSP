# Tratamento e projeção das populações dos distritos administrativos do MSP
#
library(sf)
library(gganimate)
library(transformr)
library(plyr)
library(tidyverse)
library(mgcv)
# leitura e padronização das bases:
l_csv <- list.files(path=".",pattern=".csv")
f_readcsv <- \(csvpath){
  # leitura
  year_data <- str_extract(readLines(csvpath, n = 3)[3], "\\d+")
  df_return <- read_csv2(csvpath,skip = 3, locale(encoding = "Latin1"),)
  # padronização
  names(df_return) <- df_return[1,]
  df_return <- df_return[-c(1,nrow(df_return)),]
  df_return$`Dist Adm (DA)` <- iconv(df_return$`Dist Adm (DA)`,from="Latin1",to="")
  names(df_return)[1] <- "dist_admMSP"
  # return
  df_return %>% 
    mutate(ano = year_data) %>% 
    relocate(ano) %>% 
    filter(dist_admMSP!="Total") %>% 
    select(-Total)
}
df_pop_distadmMSP <- ldply(l_csv,f_readcsv)
df_pop <- df_pop_distadmMSP %>% pivot_longer(-c(ano,dist_admMSP),values_to = "pop", names_to = "fxet") %>% 
  mutate(across(c(ano,pop),as.numeric))
# projeção para 2024
# v_distadm <- df_pop$dist_admMSP %>% unique
# dfi <- df_pop %>% filter(dist_admMSP==sample(v_distadm,1),fxet=="00 a 04")
# dfi %>% 
#   ggplot(aes(x=ano,y=pop)) +
#   geom_line() +
#   geom_point() +
#   geom_smooth(method="gam")
f_gam <- \(dfi){
  md <- gam(pop ~ s(ano,bs="cr"),data=dfi,method = "REML")
  df_return <- data.frame(ano=year(Sys.Date()))
  df_return$pop <- predict.gam(md,newdata=df_return)
  rbind(
    md$model,
    df_return  
  )
}
df_fxet <- ddply(df_pop,c("dist_admMSP","fxet"),f_gam) 
write_csv(df_fxet,
          file="G:/CCD/CVE/RESPIRATORIAS/11_MAPAS/MSP_dist_adm/df_pop_fxet_1996-2024.csv")
df_fxet <- read_csv("G:/CCD/CVE/RESPIRATORIAS/11_MAPAS/MSP_dist_adm/df_pop_fxet_1996-2024.csv")
######## aud ########
df_total <- ddply(df_pop %>%
                    group_by(dist_admMSP,ano) %>%
                    summarise(pop = sum(pop)),
                  "dist_admMSP",f_gam)
f_gsub <- \(vstring){
  vstring %>% 
    gsub("Ç",'C',.) %>% 
    gsub("Â",'A',.) %>% 
    gsub("É",'E',.) %>% 
    gsub("Á",'A',.) %>% 
    gsub("Ã",'A',.) %>% 
    gsub("Í",'I',.) %>% 
    gsub("Ó",'O',.) %>% 
    gsub("Ú",'U',.) %>% 
    gsub("Ô",'O',.) %>% 
    gsub("CIDADE","CID",.) %>% 
    gsub("JARDIM","JD",.)
}
df_pop_distMSP <- df_fxet %>% group_by(dist_admMSP,ano) %>%
  filter(ano>=2000) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(dist_admMSP = str_to_upper(dist_admMSP) %>% f_gsub) %>% 
  pivot_wider(names_from="ano",values_from="pop") %>% 
  rename(NM_DIST=dist_admMSP) %>% 
  mutate(NM_MUN="SAO PAULO") %>% 
  relocate(NM_MUN)
df_pop_ESP <- readRDS("G:/CCD/CVE/RESPIRATORIAS/11_MAPAS/pop_ESPserietemp_mun_fxet_sexo/pop_ESPserietemp_mun_fxet_sexo.rds")
df_pop_ESP <- df_pop_ESP %>% 
  group_by(MUN,ANO) %>% 
  summarise(POP=sum(POP)) %>% 
  rename(NM_MUN=MUN) %>% 
  mutate(NM_DIST=NA) %>% 
  relocate(NM_DIST,.after = "NM_MUN") %>% 
  pivot_wider(names_from="ANO",values_from="POP")
df_pop_ESP_distMSP <- rbind(
  df_pop_ESP %>% filter(NM_MUN!="SAO PAULO"),
  df_pop_distMSP
)
# df_aud <- inner_join(x=df_aud0,y=df_total,by=c("dist_admMSP","ano"))
# df_aud %>%
#   ggplot(aes(x=pop.x,y=pop.y)) +
#   geom_point() +
#   geom_smooth(method="lm")
######## avaliação do shapefile ########
df_distMSP <- read_sf("./LAYER_DISTRITO/DEINFO_DISTRITO.shp") %>% 
  select(NOME_DIST) %>% 
  mutate(NM_MUN="SAO PAULO") %>% 
  relocate(NM_MUN) %>% 
  rename(NM_DIST=NOME_DIST)
# linkage das bases
df_municipios <- geobr::read_municipality(code_muni = "SP",
                                      year = 2022,
                                      showProgress = FALSE) %>%
  select(name_muni,geom) %>%
  rename(NM_MUN=name_muni) %>%
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
         NM_DIST=NA) %>% relocate(NM_DIST,.after = "NM_MUN")
st_geometry(df_municipios) <- "geometry"
if (st_crs(df_municipios) != st_crs(df_distMSP)) {
  df_municipios <- st_transform(df_municipios, st_crs(df_distMSP))
}
df_munESP_distMSP <- rbind(
  df_municipios %>% filter(NM_MUN!="SAO PAULO"),
  df_distMSP
) %>% 
  mutate(NM_MUN = gsub("APARECIDA D'OESTE","APARECIDA D OESTE",NM_MUN) %>% 
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
           gsub("SAO LUIS DO PARAITINGA","SAO LUIZ DO PARAITINGA",.))
#
v_mapa <- df_munESP_distMSP$NM_MUN %>% unique
v_pop <- df_pop_ESP_distMSP$NM_MUN %>% unique
setdiff(v_mapa,v_pop)
setdiff(v_pop,v_mapa)
sf_pop_ESP_distMSP <- left_join(
  df_munESP_distMSP,
  df_pop_ESP_distMSP,
  by=c("NM_MUN","NM_DIST"))
saveRDS(sf_pop_ESP_distMSP,
        file="G:/CCD/CVE/RESPIRATORIAS/11_MAPAS/MSP_dist_adm/sf_pop_ESP_distMSP_2000-2024.rds")
sf_pop_ESP_distMSP <- readRDS("G:/CCD/CVE/RESPIRATORIAS/11_MAPAS/MSP_dist_adm/sf_pop_ESP_distMSP_2000-2024.rds")

sf_pop_ESP_distMSP %>% 
  filter(NM_MUN=="SAO PAULO") %>% 
  ggplot(aes(fill=`2024`)) +
  geom_sf(linewidth=0.001)
aud <- teste %>% select(starts_with("NM"),`2024`)
teste %>% filter(is.na(`2024`))

df_munESP %>% 
  ggplot() + geom_sf()
##########################################
# v_site_pop <- df_aud0$dist_admMSP %>% unique
# v_site_sf <- df_sf_MSP$NOME_DIST %>% unique
# setdiff(v_site_pop,v_site_sf)
# df_map <- inner_join(df_sf_MSP,df_aud0,by=c("NOME_DIST"="dist_admMSP")) %>%
#   relocate(geometry,.after=last_col())
# df_map %>%
#   filter(ano==2024) %>%
#   ggplot(aes(fill = pop)) +
#   geom_sf(linewidth=0.001) +
#   scale_fill_gradient(
#     # Make legend title more readable
#     name = "População",
#     #set the color gradient
#     low = "lightblue",
#     high = "red") +
#   labs(title="Ano: 2024") +
#   theme_minimal()
####
# teste <- df_mun %>% 
#   filter(NM_MUN!="São Paulo") %>% 
#   select(CD_MUN,NM_MUN,geometry)
# if (st_crs(teste) != st_crs(df_map)) {
#   teste <- st_transform(teste, st_crs(df_map))
# }
# 
# 
# teste <- rbind(teste,df_map)
# 
# teste %>% ggplot() + geom_sf()
# 
# 
# 
# oname <- load("shp_mun.RData")
# df_mun <- get(oname)
# l_p <- list()
# l_p$from_mun <- df_mun %>% 
#   filter(NM_MUN=="São Paulo") %>% 
#   ggplot() +
#   geom_sf()
# l_p$from_distadm <- df_map %>% 
#   ggplot() +
#   geom_sf()
# gridExtra::grid.arrange(grobs=l_p,ncol=2)
# 
# 
# 
# map <- df_map %>% 
#   ggplot(aes(fill = pop)) +
#   geom_sf(linewidth=0.001) +
#   scale_fill_gradient(
#     # Make legend title more readable
#     name = "População",
#     #set the color gradient
#     low = "lightblue",
#     high = "red") +
#   theme_minimal() +
#   transition_states(ano,
#                     transition_length = 1,
#                     state_length = 1) +
#   ggtitle('Ano {closest_state}')
# 
# 
# 
# map_with_animation <- map +
#   transition_time(ano) +
#   ggtitle('Ano: {frame_time}')
# num_years <- max(df_map$ano) - min(df_map$ano) + 1
# gganimate::animate(map_with_animation, 
#                    #7 years - we want 7 frames
#                    nframes = num_years,
#                    #This tell it how fast to go
#                    fps = 2)
