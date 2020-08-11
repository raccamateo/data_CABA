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
