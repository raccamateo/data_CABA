library(tidyverse)
library(ggmap)
library(lubridate)
#vamos a cargar la libreria gganimate para animar gráficos que vamos a hacer con ggplot
library(gganimate)
#estas librerías nos sirven para acomodar nuestro gráfico
library(transformr)
library(viridis)
library(hrbrthemes)
#llamamos a las librerías que vamos a usar para trabajar con datos geoespaciales
library(sf)
library(tidyverse)

options(scipen = 999)




#comunas_geo
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')
comunas <- rename(comunas, comuna = comunas)

#movilidad google
movilidad_google <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=56adbfa96dfa23c3")
movilidad_google <- movilidad_google %>% filter(country_region_code == "AR")

#vemos qué hay dentro de la variable sub_region_1
levels(movilidad_google$sub_region_1)

#filtramos solo la data correspondiente a Buenos Aires
movilidad_google_buenos_aires <- movilidad_google %>% filter(sub_region_1 == "Buenos Aires")
summary(movilidad_google_buenos_aires)

#dejamos fuera las siguientes columnas
movilidad_google_buenos_aires <- select(movilidad_google_buenos_aires, -metro_area, -iso_3166_2_code, -census_fips_code)

#como las comunas en nuestro dataset "comunas" aparecen con números y en el dataset de Google con comuna + número, vamos a renombrar.
comunas$comuna <- (comunas$comuna = case_when(comunas$comuna == 1 ~ "Comuna 1",
                                              comunas$comuna == 2 ~ "Comuna 2",
                                              comunas$comuna == 3 ~ "Comuna 3",
                                              comunas$comuna == 4 ~ "Comuna 4",
                                              comunas$comuna == 5 ~ "Comuna 5",
                                              comunas$comuna == 6 ~ "Comuna 6",
                                              comunas$comuna == 7 ~ "Comuna 7",
                                              comunas$comuna == 8 ~ "Comuna 8",
                                              comunas$comuna == 9 ~ "Comuna 9",
                                              comunas$comuna == 10 ~ "Comuna 10",
                                              comunas$comuna == 11 ~ "Comuna 11",
                                              comunas$comuna == 12 ~ "Comuna 12",
                                              comunas$comuna == 13 ~ "Comuna 13",
                                              comunas$comuna == 14 ~ "Comuna 14",
                                              comunas$comuna == 15 ~ "Comuna 15"))

#dejamos fuera columnas que no nos interesan
comunas <- select(comunas, -barrios, -perimetro, -area)

#renombramos la columna que contiene las comunas para poder crear un nuevo dataset
movilidad_google_buenos_aires <- movilidad_google_buenos_aires %>% rename(comuna = sub_region_2)

#unimos el dataset con la información de movilidad de google con el de comunas para darle atributos geoespaciales
movilidad_comunas <- inner_join(movilidad_google_buenos_aires, comunas, by = "comuna")
