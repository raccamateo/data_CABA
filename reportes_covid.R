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

#datos sobre los reportes diarios de COVID-19 del Ministerio de Salud
reporte_covid <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/reporte-covid/dataset_reporte_covid_sitio_gobierno.csv')

#vemos el dataset
head(reporte_covid)

#como FECHA_PROCESO e ID_CARGA no nos interesan, seleccionamos el resto de las variables
reporte_covid <- select(reporte_covid, -FECHA_PROCESO, - ID_CARGA)


#cambiamos el formato de la fecha para poder trabajar como serie temporal
reporte_covid <- reporte_covid %>% 
  mutate(FECHA = dmy_hms(FECHA))


#gráfico de líneas animado con la cantidad de usuarixs de cada tipo de transporte durante la pandemia
ggplot(reporte_covid, aes(x = FECHA, y = VALOR, color = TIPO_DATO)) +
  geom_line() +
  geom_point(aes(group = seq_along(FECHA))) +
  labs(title = "Reportes diarios",
       subtitle = "Covid-19",
       x = "",
       y = "N") +
  scale_color_viridis(discrete=TRUE) +
  transition_reveal(FECHA) +
  theme_minimal() +
  theme(legend.position = "bottom")


reporte_covid_camas <- filter(reporte_covid, TIPO_DATO == "ocupacion_de_camas_sistema_publico" | TIPO_DATO == "total_de_camas_sistema_publico")

#gráfico de líneas animado con la cantidad de usuarixs de cada tipo de transporte durante la pandemia
ggplot(reporte_covid_camas, aes(x = FECHA, y = VALOR, color = TIPO_DATO)) +
  geom_line() +
  geom_point(aes(group = seq_along(FECHA))) +
  labs(title = "Reportes diarios",
       subtitle = "Covid-19",
       x = "",
       y = "N") +
  scale_color_viridis(discrete=TRUE) +
  transition_reveal(FECHA) +
  theme_minimal() +
  theme(legend.position = "bottom")
