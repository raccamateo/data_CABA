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


#ahora transformamos los números en días, para eso:
llamados_107$DIA <- (llamados_107$DIA = case_when(llamados_107$DIA == 2 ~ "LUNES",
                                                  llamados_107$DIA == 3 ~ "MARTES",
                                                  llamados_107$DIA == 4 ~ "MIÉRCOLES",
                                                  llamados_107$DIA == 5 ~ "JUEVES",
                                                  llamados_107$DIA == 6 ~ "VIERNES",
                                                  llamados_107$DIA == 7 ~ "SÁBADO",
                                                  llamados_107$DIA == 1 ~ "DOMINGO"))

#extraemos el número del mes al que corresponde cada fecha
llamados_107$N_MES <- month(llamados_107$FECHA)

#asignamos a cada número de mes el mes correspondiente
llamados_107$N_MES <- (llamados_107$N_MES = case_when(llamados_107$N_MES == 3 ~ "MARZO",
                                                      llamados_107$N_MES == 4 ~ "ABRIL",
                                                      llamados_107$N_MES == 5 ~ "MAYO",
                                                      llamados_107$N_MES == 6 ~ "JUNIO",
                                                      llamados_107$N_MES == 7 ~ "JULIO",
                                                      llamados_107$N_MES == 8 ~ "AGOSTO",
                                                      llamados_107$N_MES == 9 ~ "SEPTIEMBRE",
                                                      llamados_107$N_MES == 10 ~ "OCTUBRE",
                                                      llamados_107$N_MES == 11 ~ "NOVIEMBRE",
                                                      llamados_107$N_MES == 12 ~ "DICIEMBRE"))

#extraemos a qué día del mes corresponde cada fecha
llamados_107$N_DIA <- day(llamados_107$FECHA)

#vamos a ver cómo quedo lo que hicimos
head(llamados_107, n=10)

#renombramos la siguiente columna:
llamados_107 %>%
  rename(LLAMADOS = COVID_LLAMADOS)


llamados_107 <- llamados_107 %>% filter(SEMANA > 11)

ggplot(data = llamados_107, aes(x = factor(DIA, levels=c("LUNES", "MARTES", "MIÉRCOLES", "JUEVES", "VIERNES", "SÁBADO", "DOMINGO")), y = COVID_LLAMADOS)) +
  geom_bar(stat='identity', aes(fill = DIA)) +
  transition_time(SEMANA) +
  labs(title = "Llamados al 107 en CABA",
       subtitle = "semana: {as.integer(frame_time)}",
       x = "Día",
       y = "Llamados")+
  scale_fill_viridis_d() +
  theme_minimal()

anim_save("llamados_semana_dia.gif")

