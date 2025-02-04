rm(list=ls())

library(pacman)

p_load(readr, readxl, dplyr, stringr, ggplot2, forcats,ggthemes, scales, 
       tidyverse, ggrepel, lubridate, grid, extrafont, sf, scico, rgdal, naniar, srvyr,
       kableExtra)

options(scipen=999)

names <- read_excel("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/cdnames.xlsx")
datos1 <- read_excel("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/cdart.xlsx")
IDM22 <- vroom::vroom("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/IDM_NM_ene22.csv",
                    skip = 0, locale = vroom::locale(encoding = "CP1252"))

# names$CD <- as.numeric(names$CD)
# 
# datos2 <- datos1 %>% 
#   merge(names, by = "CD")

#La Laguna contempla cuatro municipios - Recalcar 
#Los Mochis = Ahome (partial)
#Villahermosa = Municipio de Centro 
#Ixtapa-Zihuatanejo = Zihuatanejo de Azueta
#Chetumal = Othón P. Blanco
#Cd. Obregón = Cajeme

names(IDM22)[4] <- "clave"

IDM23 <- IDM22 %>% 
  filter(Año == 2021) %>% 
  select(c(3:5, 8, 16:21)) %>% 
  filter(`Subtipo de delito` %in% c("Abuso sexual", "Acoso sexual",
                                    "Hostigamiento sexual",
                                    "Violación simple",
                                    "Violación equiparada"))
IDM24 <- IDM23 %>% 
  mutate(valuesT = rowSums(IDM23[ , c(5:10)], na.rm=TRUE)) %>% 
  select(c(2, 4, 11)) %>% 
  group_by(clave)

# IDM24$valuesT <- as.numeric(IDM24$valuesT)
# IDM24$clave <- as.numeric(IDM24$clave)
# IDM_sep <- IDM24 %>% 
#   select(-(4))
# IDM_dic <- IDM24 %>% 
#   select(-(3))

IDM_1 <- IDM24 %>% 
  pivot_wider(names_from = "Subtipo de delito", values_from = "valuesT")

IDM_2 <- IDM_1 %>% 
  mutate(viola_intS = sum(`Violación equiparada` + `Violación simple`))

names(IDM_2)[2] <- "abu_sexS"
names(IDM_2)[3] <- "aco_hosS"
names(IDM_2)[4] <- "host_sexS"

IDM_3 <- IDM_2 %>% 
  select(c(1:4, 7)) %>% 
  merge(names, by = "clave")
IDM_4 <- IDM_3 %>% 
  group_by(CD, ciudad) %>% 
  summarise(across(everything(), sum))

IDM_5 <- as.data.frame(IDM_4) %>% 
  mutate(totalS = rowSums(IDM_4[ , c(4:7)], na.rm=TRUE)) %>% 
  select(-(3))





write.csv(ensec,'resultados_vs.csv')

write.csv(IDM28,'resultados_sesnsp.csv')

write.csv(tabfin1,'resultados_sesnsp_ensuF.csv')

# datos1_ <- datos1 %>% 
#   select(-(1))

datos1$abu_sexE <- as.numeric(datos1$abu_sexE)
datos1$aco_hosE <- as.numeric(datos1$aco_hosE)
datos1$viola_intE <- as.numeric(datos1$viola_intE)

datos1_ <- datos1 %>% 
  select(-(1)) %>% 
  group_by(CD) %>% 
  summarise(across(everything(), sum))

tabfin1 <- IDM_5 %>% 
  merge(datos1_, by = "CD")



## ENSU 2021 Tercer trimestre

require(tidyverse, srvyr)

## Bases de datos

df <- read_csv("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/ENCUESTA_CHIDA/ensu_bd_diciembre_2021_csv/ENSU_CB_1221.csv")

## Diseño de encuesta

df_binaria <- df %>% 
  mutate(BP4_1_1 = ifelse(BP4_1_1 == "1", 1, 0),
         BP4_1_2 = ifelse(BP4_1_2 == "1", 1, 0),
         BP4_1_3 = ifelse(BP4_1_3 == "1", 1, 0),
         BP4_1_4 = ifelse(BP4_1_4 == "1", 1, 0),
         BP4_1_5 = ifelse(BP4_1_5 == "1", 1, 0),
         BP4_1_6 = ifelse(BP4_1_6 == "1", 1, 0),
         BP4_1_7 = ifelse(BP4_1_7 == "1", 1, 0),
         BP4_1_8 = ifelse(BP4_1_8 == "1", 1, 0),
         BP4_1_9 = ifelse(BP4_1_9 == "1", 1, 0),
         int_sex = (BP4_1_1 + BP4_1_4 +BP4_1_8),
         abu_sex = (BP4_1_6 + BP4_1_7 +BP4_1_9),
         aco_hos = (BP4_1_3),
         viola_int = (BP4_1_2+ BP4_1_5)) %>% 
  mutate(int_sex = ifelse(int_sex >= "1", 1, 0),
         abu_sex = ifelse(abu_sex >= "1", 1, 0),
         aco_hos = ifelse(aco_hos >= "1", 1, 0),
         viola_int = ifelse(viola_int >= "1", 1, 0),
         CD = as.numeric(CD))


df_encuesta <- df_binaria %>%
  as_survey_design(ids=UPM_DIS,
                   strata=EST_DIS,
                   weights=FAC_SEL)


df_int_sex <- df_encuesta %>% 
  group_by(CD) %>% 
  survey_count(int_sex) %>% 
  filter(int_sex  == 1) %>% 
  summarise(n_int_sex = n,
            CD) 

df_abu_sex <- df_encuesta %>% 
  group_by(CD) %>% 
  survey_count(abu_sex) %>% 
  filter(abu_sex  == 1) %>% 
  summarise(CD,
            n_abu_sex = n)

df_aco_hos <- df_encuesta %>% 
  group_by(CD) %>% 
  survey_count(aco_hos) %>% 
  filter(aco_hos  == 1) %>% 
  summarise(CD,
            n_aco_hos = n)


df_viola_int <- df_encuesta %>% 
  group_by(CD) %>% 
  survey_count(viola_int) %>% 
  filter(viola_int  == 1) %>% 
  summarise(CD,
            n_viola_int = n)
df_intentover <- df_aco_hos %>% 
  summarise(n = sum(n_aco_hos))

df_final <- df_int_sex %>% 
  merge(df_abu_sex, by = "CD", all = T) %>% 
  merge(df_viola_int, by = "CD", all = T) %>% 
  merge(df_aco_hos, by = "CD", all = T) 

df_finalt <- df_final %>% 
  mutate(n_total = rowSums(df_final[ , c(2:5)], na.rm=TRUE))



ensec <- IDM_5 %>% 
  merge(df_finalt, by= "CD") %>% 
  mutate(tasa_abu = 1-(abu_sexS/n_abu_sex),
         tasa_aco = 1-(aco_hosS/n_aco_hos),
         tasa_hos = 1-(host_sexS/n_int_sex),
         tasa_vio = 1-(viola_intS/n_viola_int),
         tasa_tot = 1-(totalS/n_total)
  )


write.csv(df_finalt,'vic_ensu_dic.csv')


