library(tidyverse)
library(lubridate)
#vamos a cargar la libreria gganimate para animar gráficos que vamos a hacer con ggplot
library(gganimate)
#estas librerías nos sirven para acomodar nuestro gráfico
library(transformr)
library(viridis)
library(hrbrthemes)

options(scipen = 999)


#datos sobre la SUBE
sube <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/sube/dataset_viajes_sube.csv')
write_csv(sube, "sube.csv")
#vamos a ver qué datos hay en el dataset sobre la sube
head(sube)

#los datos que tenemos son: tipo de transporte, el día, la variable parcial y la cantidad de pasajerxs
str(sube)

#las fechas (variable día) están como factor. vamos a modificar eso para poder trabajar con otras cosas en función de las fechas. para eso usamos lubridate

#transformamos el formato de las fechas a día-mes-año-horas:minutos:segundos
sube <- sube %>% mutate(DIA = dmy_hms(DIA))

#ahora vamos a cambiar los nombres para que la variable DIA (que contiene las fechas) se llame FECHA
sube$FECHA <- sube$DIA

#tomamos las fechas y las transformamos en una nueva variable. vamos a llamarla DIA, y vamos a asignarle un número según qué día de la semana es cada una de esas fechas
sube$DIA <- wday(sube$FECHA)

#vamos a asignar a crear una columna llamada SEMANA, que va a tener la información de a qué semana corresponde cada día
sube$SEMANA <- week(sube$FECHA)

#ahora transformamos los números en días, para eso:
sube$DIA <- (sube$DIA = case_when(sube$DIA == 2 ~ "LUNES",
                                  sube$DIA == 3 ~ "MARTES",
                                  sube$DIA == 4 ~ "MIÉRCOLES",
                                  sube$DIA == 5 ~ "JUEVES",
                                  sube$DIA == 6 ~ "VIERNES",
                                  sube$DIA == 7 ~ "SÁBADO",
                                  sube$DIA == 1 ~ "DOMINGO"))

#extraemos el número del mes al que corresponde cada fecha
sube$N_MES <- month(sube$FECHA)

#asignamos a cada número de mes el mes correspondiente
sube$N_MES <- (sube$N_MES = case_when(sube$N_MES == 3 ~ "MARZO",
                                      sube$N_MES == 4 ~ "ABRIL",
                                      sube$N_MES == 5 ~ "MAYO",
                                      sube$N_MES == 6 ~ "JUNIO",
                                      sube$N_MES == 7 ~ "JULIO",
                                      sube$N_MES == 8 ~ "AGOSTO",
                                      sube$N_MES == 9 ~ "SEPTIEMBRE",
                                      sube$N_MES == 10 ~ "OCTUBRE",
                                      sube$N_MES == 11 ~ "NOVIEMBRE",
                                      sube$N_MES == 12 ~ "DICIEMBRE"))

#extraemos a qué día del mes corresponde cada fecha
sube$N_DIA <- day(sube$FECHA)

#vamos a ver cómo quedo lo que hicimos
head(sube, n=10)

#como no vamos a usar la fila PARCIAL, vamos a dejarla fuera del dataset
sube <- select(sube, -PARCIAL)
head(sube, n=10)


#ahora vamos a filtrar por tipo de transporte y a crear datasets cada
sube_subte <- filter(sube, TIPO_TRANSPORTE == "Subte")
head(sube_subte, n=10)
sube_colectivo <- filter(sube, TIPO_TRANSPORTE == "Colectivo")
head(sube_colectivo, n=10)
sube_tren <- filter(sube, TIPO_TRANSPORTE == "Tren")
head(sube_tren, n=10)

write_csv(sube_tren, "data_sube_tren.csv")


ggplot(data = sube, aes(x = factor(DIA, levels=c("LUNES", "MARTES", "MIÉRCOLES", "JUEVES", "VIERNES", "SÁBADO", "DOMINGO")), y = CANTIDAD)) +
  geom_bar(stat='identity', aes(fill = TIPO_TRANSPORTE)) +
  transition_time(SEMANA) +
  labs(title = "Uso de transporte público durante la pandemia
       semana: {as.integer(frame_time)}",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "Día",
       y = "Usuarixs")+
  scale_fill_viridis_d() +
  theme_minimal()

#gráfico de líneas animado con la cantidad de usuarixs de cada tipo de transporte durante la pandemia
ggplot(sube, aes(x = FECHA, y = CANTIDAD, color = TIPO_TRANSPORTE)) +
  geom_line() +
  geom_point(aes(group = seq_along(FECHA))) +
  labs(title = "Uso de transporte público durante la pandemia",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "",
       y = "Usuarixs") +
  scale_color_viridis(discrete=TRUE) +
  transition_reveal(FECHA) +
  theme_minimal() +
  theme(legend.position = "bottom")

anim_save("sube_lin.gif")



#gráfico de líneas animado: uso de colectivos por día y coloreado por mes durante la pandemia
ggplot(sube_colectivo, aes(x = N_DIA, y = CANTIDAD, color = N_MES)) +
  geom_line() +
  geom_point() +
  labs(title = "Uso de colectivos durante la pandemia",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "",
       y = "Usuarixs") +
  scale_color_viridis(discrete=TRUE) +
  transition_reveal(FECHA) +
  theme_minimal() +
  theme(legend.position = "bottom")
anim_save("sube_colectivos_lin.gif")

#gráfico de líneas animado: uso de subtes por día y coloreado por mes durante la pandemia
ggplot(sube_subte, aes(x = N_DIA, y = CANTIDAD, color = N_MES)) +
  geom_line() +
  geom_point() +
  labs(title = "Uso de subtes durante la pandemia",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "",
       y = "Usuarixs") +
  scale_color_viridis(discrete=TRUE) +
  transition_reveal(FECHA) +
  theme_minimal() +
  theme(legend.position = "bottom")
anim_save("sube_subte_lin.gif")


#gráfico de líneas animado: uso de trenes por día y coloreado por mes durante la pandemia
ggplot(sube_tren, aes(x = N_DIA, y = CANTIDAD, color = N_MES)) +
  geom_line() +
  geom_point() +
  labs(title = "Uso de trenes durante la pandemia",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "",
       y = "Usuarixs") +
  scale_color_viridis(discrete=TRUE) +
  transition_reveal(FECHA) +
  theme_minimal() +
  theme(legend.position = "bottom")
anim_save("sube_tren_lin.gif")
