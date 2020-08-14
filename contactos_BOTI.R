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

contactos_BOTI$hora <- as.hms(hora)
