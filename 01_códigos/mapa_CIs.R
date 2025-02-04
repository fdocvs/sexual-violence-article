##Mapas para artículo 8M - Carpetas de investigación

rm(list=ls())

library(pacman)


p_load(readr, readxl, dplyr, stringr, ggplot2, forcats,ggthemes, scales, 
       tidyverse, ggrepel, lubridate, grid, extrafont, sf, scico, rgdal)


options(scipen=999) # Evita notación científica

df <- read.csv("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/resultados_vs.csv")
cvemun <- read_xlsx("/Users/santiago/Documentos OFF/México Evalúa/Magda's Article/cdnames.xlsx")

municipios_shp <- sf::st_read(dsn="/Users/santiago/Downloads/MG_2020_Integrado/conjunto_de_datos", layer = "00mun",
                              stringsAsFactors = FALSE) %>%
  mutate(cvemunicipio = as.numeric(CVEGEO))


estados_shp <- sf::st_read(dsn="/Users/santiago/Downloads/MG_2020_Integrado/conjunto_de_datos", layer = "00ent",
                           stringsAsFactors = FALSE)




cvemun$CD <- as.numeric(cvemun$CD)

df <- df %>% 
  merge(cvemun, by = "CD")

myData <- df[-c(8:10), ] %>% 
  summarise(CD, ciudad.x, tasa_tot, clave) %>% 
  mutate(tasa_tot = 100*tasa_tot)

names(myData)[4] <- "cvemunicipio"


categories <- myData %>% 
  mutate(ctg = cut(myData$tasa_tot, breaks = c(98.2, 99, 99.89, 100), 
                   labels = c("98.4%", 
                              "99-99.8%",
                              "99.9%"), right = T)) #El número de rangos (breaks) debe ser igual al número de labels. La función right es T cuando el rango es (], r = F cuando (). 

categories$ctg <- as.character(categories$ctg) #Se transforma a caracter para poder cambiar el nombre de las variables de ser necesario. 
# categories$ctg[is.na(categories$ctg)] <- "n/a***"


# ggsave(rurales.estado, filename ="rurales.estado.png",
#        width = 7, height = 5, units = "in")






VS_shp_cat <- municipios_shp %>% 
  merge(categories, by = "cvemunicipio")

VS_shp_cat$ctg <- factor(VS_shp_cat$ctg,                 # Relevel group factor
                         levels = c("98.4%", 
                                    "99-99.8%",
                                    "99.9%"))


nc3 <- VS_shp_cat[c(82, 88, 84), ] #Selección de municipios a resaltar en el plot

nc3_points <- sf::st_point_on_surface(nc3) #Obtención de puntos geográficos

nc3_coords <- as.data.frame(sf::st_coordinates(nc3_points)) #Obtención de coordenadas 

nc3_coords$CDname <- nc3$ciudad.x

nc3_coords


VS_munmap <- ggplot(data = VS_shp_cat, fill = ctg) +
  geom_sf(data=estados_shp, size = 0.1, color = "#bdbdbd", fill = "#feebe2") + 
  # geom_sf(data = VS_shp_cat, aes(fill = ctg)) + No es usado para no graficar líneas municipales
  geom_point(
    aes(geometry = geometry, size = as.factor(ctg), color = as.factor(ctg)),
    stat = "sf_coordinates",
    stroke = 0.6,
    alpha = 0.7
    ) +
  # scale_shape_manual(values = c(20, 16, 17)) + Para cambiar el tipo de shape
  scale_color_manual(values=c("#f768a1", "#9e9ac8", "#7a0177"))+
  theme_void() + 
  labs(title = "Porcentaje de cifra negra de carpetas de investigación sobre\ndelitos de violencia sexual contra las mujeres",
       subtitle = "Julio-Septiembre 2021",
       fill=NULL,
       caption = "\nElaboración de Fernando Cuevas con datos del SESNSP, ENSU (2021) y marco geoestadístico 2020 del INEGI.\n") +
  scale_fill_manual(values = c("#8B85D6", "#A15ABF", "#9045B0")) +
  theme(plot.title = element_text(color="black", size=14, 
                                  face="bold", vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(color="black", size=12, 
                                     face="bold", vjust = 1, hjust = 0.5),
        plot.caption = element_text(hjust = 0, vjust = -1, size = 8),
        legend.key.size = unit(1, 'lines')) +
  theme(text = element_text(family = "CircularStd-Book"),
        legend.position = c(0.87, 0.8),
        legend.title = element_blank()
        ) 

VS_munmap2 <- VS_munmap +
  geom_text_repel(data = nc3_coords, aes(X, Y, label = CDname), colour = "black",
                  position=position_jitter(width=1,height=2),
                  family = "CircularStd-Book",
                  box.padding = 2.6, max.overlaps = Inf, force = 5, size = 3)



suppressWarnings(ggsave(VS_munmap2, filename ="VSmap1.png",
       width = 7, height = 5, units = "in"))












