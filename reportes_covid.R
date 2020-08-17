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
