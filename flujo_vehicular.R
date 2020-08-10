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


#vamos a descargar la data sobre radios censales que está publicada en el githun de bitsandbricks
radios <- st_read("https://bitsandbricks.github.io/data/CABA_rc.geojson")


#ahora vamos a trabajar con datos relacionados al flujo vehicular
flujo_vehicular <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/flujo-vehicular-anillo-digital/dataset_flujo_vehicular.csv')
write_csv(flujo_vehicular, "flujo_vehicular.csv")
head(flujo_vehicular)

#vamos a ver qué tipo de variables tenemos
str(flujo_vehicular)
