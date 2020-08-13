library(tidyverse)
library(lubridate)
#vamos a cargar la libreria gganimate para animar gráficos que vamos a hacer con ggplot
library(gganimate)
#estas librerías nos sirven para acomodar nuestro gráfico
library(transformr)
library(viridis)
library(hrbrthemes)


options(scipen = 999)



#datos sobre llamados al 107
llamados_107 <- read_csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/llamados-107-covid/llamados_107_covid.csv')

#vamos a ver qué datos hay en el dataset sobre llamados al 107
head(llamados_107)

#los datos que tenemos son: FECHA, COVID_LLAMADOS, CASOS_SOSPECHOSOS, CASOS_DESCARTADOS_COVID, CASOS_TRASLADOS y CASOS_DERIVADOS
str(llamados_107)

#transformamos el formato de las fechas a día-mes-año-horas:minutos:segundos
llamados_107 <- llamados_107 %>% mutate(FECHA = dmy_hms(FECHA))

#tomamos las fechas y las transformamos en una nueva variable. vamos a llamarla DIA, y vamos a asignarle un número según qué día de la semana es cada una de esas fechas
llamados_107$DIA <- wday(llamados_107$FECHA)

#vamos a asignar a crear una columna llamada SEMANA, que va a tener la información de a qué semana corresponde cada día
llamados_107$SEMANA <- week(llamados_107$FECHA)



