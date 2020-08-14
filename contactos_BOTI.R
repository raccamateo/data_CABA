library(tidyverse)
library(lubridate)
#vamos a cargar la libreria gganimate para animar gráficos que vamos a hacer con ggplot
library(gganimate)
#estas librerías nos sirven para acomodar nuestro gráfico
library(transformr)
library(viridis)
library(hrbrthemes)


options(scipen = 999)

#contactos BOTI/hora
contactos_BOTI <- read.csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/innovacion-transformacion-digital/contactos-boti-triage-covid-19/contactos_boti_triage_covid_19.csv')


head(contactos_BOTI)

#como la hora de los contactos contiene solo hs y no minutos ni segundos, vamos a agregárselos
contactos_BOTI$hora <- str_c(contactos_BOTI$hora, "00:00", sep = ":")

#vamos a combinar las columnas fecha y hora para crrar la variable fecha_hora
contactos_BOTI$fecha_hora <- str_c(contactos_BOTI$fecha, contactos_BOTI$hora)

#transformamos el formato de las fechas a día-mes-año-horas:minutos:segundos
contactos_BOTI <- contactos_BOTI %>% mutate(fecha_hora = dmy_hms(fecha_hora))
