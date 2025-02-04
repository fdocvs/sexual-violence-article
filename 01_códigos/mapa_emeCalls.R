##Mapas para artículo 8M - Llamadas de emergencia 

rm(list=ls())

library(pacman)

p_load(readr, readxl, dplyr, stringr, ggplot2, forcats,ggthemes, scales, 
       tidyverse, ggrepel, lubridate, grid, extrafont, sf, scico, rgdal)

options(scipen=999) # Evita notación científica

pob_mun <- read_excel("/Users/santiago/Downloads/pob_mun_2015-2030.xls") 


municipios_shp <- sf::st_read(dsn="/Users/santiago/Downloads/MG_2020_Integrado/conjunto_de_datos", layer = "00mun",
                              stringsAsFactors = FALSE) %>%
  mutate(cvemunicipio = as.numeric(CVEGEO))


estados_shp <- sf::st_read(dsn="/Users/santiago/Downloads/MG_2020_Integrado/conjunto_de_datos", layer = "00ent",
                           stringsAsFactors = FALSE)

eme_calls <- read_xlsx("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/3. cngmd2021_seg_publica.xlsx", 
                       sheet = 7,
                       skip = 4)

names(eme_calls)[1] <- "cvedo"
names(eme_calls)[3] <- "cvmun"
names(eme_calls)[4] <- "mun"

eme_calls1 <- eme_calls %>%
  mutate(
    cvemunicipio = paste(eme_calls$cvedo, eme_calls$cvmun, sep = "")
  )
names(eme_calls1)[5] <- "csub"
eme_calls1$csub <- as.numeric(eme_calls1$csub)


emcal <- eme_calls1 %>% 
  drop_na(mun) 

emcal2 <- (emcal) %>% 
  filter(csub %in% c(30701, 30702, 30707)) %>% 
  summarise(cvemunicipio, csub, Total) %>% 
  group_by(cvemunicipio) %>% 
  pivot_wider(names_from = "csub", values_from = "Total")

emcal2$'30701' <- as.numeric(emcal2$'30701')
emcal2$'30702' <- as.numeric(emcal2$'30702')
emcal2$'30707' <- as.numeric(emcal2$'30707')

emcal2 <- emcal2 %>% 
  drop_na()

emcal3 <- as.data.frame(emcal2) %>% 
  mutate(total = rowSums(emcal2[ , c(2:4)], na.rm=TRUE))

emcal3$cvemunicipio <- as.numeric(emcal3$cvemunicipio)
emcal3 <- emcal3 %>% 
  summarise(cvemunicipio, total)





pob_mun <- pob_mun %>% 
  select(c(1, 2, 4:6)) %>% 
  group_by(ao, cvemunicipio) %>% 
  filter(ao == 2020)


emecal_pob <- emcal3 %>% 
  merge(pob_mun, by = "cvemunicipio") %>% 
  summarise(cvemunicipio, total, pob_estim, municipio)


emecal_tasas <- emecal_pob %>% 
  mutate(tasa = (total/pob_estim*100000))

emecaltas_shp <- municipios_shp %>% 
  merge(emecal_tasas, by = "cvemunicipio")




write.csv(emecal_tasas, "/Users/santiago/Downloads/tasa_emecalf.csv", row.names = FALSE)




# 
# categories10 <- emecal_tasas %>% 
#   mutate(ctg = cut(emcal3$total, breaks = c(-1, 100, 1000, 5000), 
#                    labels = c("0 - 100", 
#                               "101 - 1000",
#                               "1001 - 4852"), right = T)) #El número de rangos (breaks) debe ser igual al número de labels. La función right es T cuando el rango es (], r = F cuando (). 
# 
# categories10$ctg <- as.character(categories10$ctg) #Se transforma a caracter para poder cambiar el nombre de las variables de ser necesario. 
# categories$ctg[is.na(categories$ctg)] <- "n/a***"


# ggsave(rurales.estado, filename ="rurales.estado.png",
#        width = 7, height = 5, units = "in")




# 
# 
# emecal_shp <- municipios_shp %>% 
#   merge(categories10, by = "cvemunicipio")
# 
# emecal_shp$ctg <- factor(emecal_shp$ctg,                 # Relevel group factor
#                          levels = c("0 - 100", 
#                                     "101 - 1000",
#                                     "1001 - 4852"))
# 


# nc3 <- emecal_shp[c(158, 54, 80, 48, 406, 404, 186, 351, 47, 561, 69), ] #Selección de municipios a resaltar en el plot
# 
# nc3_points <- sf::st_point_on_surface(nc3) #Obtención de puntos geográficos
# 
# nc3_coords <- as.data.frame(sf::st_coordinates(nc3_points)) #Obtención de coordenadas 
# 
# nc3_coords$name <- nc3$NOMGEO
# 
# nc3_coords
# 



emecall_map <- ggplot() +
  geom_sf(data=estados_shp, size = 0.8, color = "#bdbdbd") + 
  geom_sf(data = emecaltas_shp, aes(fill = tasa), size = 0) + 
  theme_void() + 
  labs(title = "Llamadas de emergencia por violencia sexual* a nivel municipal",
       subtitle = "2020",
       fill=NULL,
       caption = "\nElaboración de Fernando Cuevas con datos del Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales\nde la Ciudad de México 2020 (INEGI), marco geoestadístico 2020 del INEGI y proyecciones poblacionales del CONAPO.\n*Reportes de abuso sexual, acoso u hostigamiento sexual y violación.\n\n") +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(color="black", size=12, 
                                     face="bold", vjust = 1, hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8),
        legend.key.size = unit(1, 'lines')) +
  theme(text = element_text(family = "CircularStd-Book"),
        legend.position = c(0.87, 0.8)
        ) + 
  labs(fill = "\nNúmero de llamadas\npor cada 100 mil habs.\n") 

emecall_map33 <- emecall_map +
  scale_fill_continuous(low = "#fa9fb5", high = "#7a0177")


# emecall_map3 <- emecall_map + 
#   geom_label_repel(data = nc3_coords, aes(X, Y, label = name), colour = "black",
#                   nudge_x  = c(1, -1.5, 2, 15, -1, 1,
#                                -1.5, 2, 10, -1, 1),
#                   nudge_y = c(0.25, -1, 0.5, 9, -0.5,
#                               3, -0.25, 0.5, 0.5, -9, 0.25),
#                   family = "CircularStd-Book",
#                   box.padding = 1, max.overlaps = Inf, force = 3, size = 3.5,
#                   label.size = 0.3
#                   )
# 

ggsave(emecall_map33, filename ="emecatasas.png",
       width = 7, height = 5, units = "in")


p <- emecal_tasas %>%
  ggplot( aes(x=tasa)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef") +
  theme_classic()
p
