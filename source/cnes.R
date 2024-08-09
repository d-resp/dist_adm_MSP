library(plyr)
library(tidyverse)
library(microdatasus)
#
df_cnesESP_2022e2023 <- fetch_datasus(year_start = 2022, month_start = 01,
                                      year_end = 2023, month_end = 12,
                                      uf = "SP",
                                      information_system = "CNES-ST")
df_cnesESP_2022e2023b <- fetch_datasus(year_start = 2022, month_start = 01,
                                      year_end = 2022, month_end = 01,
                                      uf = "SP",
                                      information_system = "CNES-DC")
v_suf <- c("LT","ST","EQ","SR","HB","PF","EP","RC","IN","EE","EF","GM","DC")
l_df <- lapply(v_suf,\(vs){
  fetch_datasus(year_start = 2022, month_start = 01,
                year_end = 2022, month_end = 01,
                uf = "SP",
                information_system = paste0("CNES-",vs))
})
df_cnes <- df_cnesESP_2022e2023 %>% select(CNES:COD_CEP,MICR_REG:DISTRSAN)


