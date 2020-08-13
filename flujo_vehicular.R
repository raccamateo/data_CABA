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

options(scipen = 999)


#ahora vamos a trabajar con datos relacionados al flujo vehicular
flujo_vehicular <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/flujo-vehicular-anillo-digital/dataset_flujo_vehicular.csv')
write_csv(flujo_vehicular, "flujo_vehicular.csv")
head(flujo_vehicular)

#vamos a ver qué tipo de variables tenemos
str(flujo_vehicular)


#transformamos el formato de las fechas a día-mes-año-horas:minutos:segundos
flujo_vehicular <- flujo_vehicular %>% mutate(FECHA_HORA = dmy_hms(FECHA_HORA))

#ahora, extraemos la hora de los registros (se cierran por hora y tienen info de ingresos, egresos y circulación interna)
flujo_vehicular$HORA <- hour(flujo_vehicular$FECHA_HORA)

#animación sobre el flujo vehícular. transiciones por día
ggplot(flujo_vehicular, aes(x = FECHA_HORA, y = CANTIDAD, color = SENTIDO)) +
  geom_area() +
  transition_reveal(FECHA_HORA) +
  labs(title = "Flujo vehicular",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "",
       y = "Vehículos") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")

anim_save("flujo_vehicular.gif")



#vamos a descargar un archivo .geojson con la info sobre las comunas en CABA
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')


#chequeamos si hay valores perdidos en el dataset de flujo vehicular
anyNA(flujo_vehicular)

#como los hay, vamos a descartar las filas que tengan valores NA
flujo_vehicular <- na.omit(flujo_vehicular)

#Ahora, para poder hacer un mapa, vamos a tomar el LAT-LON y transformarlo en sf
flujo_vehicular <- st_as_sf(flujo_vehicular, coords = c('LON','LAT'), crs = 4326)

#chequeamos que los datasets estén codificados iguales
st_crs(flujo_vehicular)
st_crs(comunas)

#hacemos un mapa animado con los puntos en los que están los radares o sistemas de medición del flujo de vehículos
#y vemos la evolución (promedio de flujo por semana del año).
ggplot() +
  geom_sf(data = comunas) +
  geom_sf(data = flujo_vehicular, mapping = aes(col = CODIGO_LOCACION, size = CANTIDAD)) +
  transition_time(SEMANA) +
  scale_color_viridis(discrete=TRUE) +
  labs(title = "Flujo vehicular en CABA",
       subtitle = "Semana {as.integer(frame_time)}",
       x = "",
       y = "") +
  theme_minimal()

anim_save("flujo_vehicular_mapa_semana.gif")
