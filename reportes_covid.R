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

#datos sobre los reportes diarios de COVID-19 de la Ciudad de Buenos Aires
reporte_covid <- read.csv('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/reporte-covid/dataset_reporte_covid_sitio_gobierno.csv')

#datos sobre los reportes de COVID-19 del ministerio de salud de la nación.
reporte_nacion <- read.csv('https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv')

#filtramos solo los casos que corresponden a CABA
reporte_nacion <- filter(reporte_nacion, residencia_provincia_nombre == "CABA")
summary(reporte_nacion)

#seleccionamos solo las variables de interés
reporte_nacion <- select(reporte_nacion, -residencia_pais_nombre, -carga_provincia_nombre,
                          -sepi_apertura, -carga_provincia_id, -residencia_provincia_id,
                          - residencia_departamento_id, - ultima_actualizacion)

#creamos un nuevo dataframe que incluye solo a personas cuya edad está indicada en meses (o sea, infantes)
reporte_nacion_infantes <- filter(reporte_nacion, edad_años_meses == "Meses")
summary(reporte_nacion_infantes)

#pasamos la fecha a formato fecha
reporte_nacion_infantes$fecha_apertura <- as.Date(reporte_nacion_infantes$fecha_apertura)


#graficamos los casos en el tiempo
ggplot(reporte_nacion_infantes) +
  geom_histogram(aes(x = fecha_apertura, fill = origen_financiamiento)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(reporte_nacion_infantes) +
  geom_histogram(binwidth = 1, aes(x = edad, fill = edad)) +
  theme_minimal() +
  theme(legend.position = "bottom")


#vemos el dataset
head(reporte_covid)

#como FECHA_PROCESO e ID_CARGA no nos interesan, seleccionamos el resto de las variables
reporte_covid <- select(reporte_covid, -FECHA_PROCESO, - ID_CARGA)

#cambiamos el formato de la fecha para poder trabajar como serie temporal
reporte_covid <- reporte_covid %>%
  mutate(FECHA = dmy_hms(FECHA))

levels(reporte_covid$SUBTIPO_DATO)


#vamos a seleccionar los datos correspondientes a hisopados en los distintos barrios de la ciudad y a crear un nuevo dataset
hisopados <- reporte_covid %>%
  filter(SUBTIPO_DATO == "hisopados_barrio_1-11-14" | SUBTIPO_DATO == "hisopados_barrio_15" |
           SUBTIPO_DATO == "hisopados_barrio_20" | SUBTIPO_DATO == "hisopados_barrio_21-24" |
           SUBTIPO_DATO == "hisopados_barrio_31" | SUBTIPO_DATO == "hisopados_barrio_almagro" |
           SUBTIPO_DATO == "hisopados_barrio_balvanera" | SUBTIPO_DATO == "hisopados_barrio_barracas" |
           SUBTIPO_DATO == "hisopados_barrio_carrillo" | SUBTIPO_DATO == "hisopados_barrio_constitucion" |
           SUBTIPO_DATO == "hisopados_barrio_flores" | SUBTIPO_DATO == "hisopados_barrio_la_boca" |
           SUBTIPO_DATO == "hisopados_barrio_palermo" | SUBTIPO_DATO == "hisopados_belgrano" |
           SUBTIPO_DATO == "hisopados_boedo" | SUBTIPO_DATO == "hisopados_caballito" |
           SUBTIPO_DATO == "hisopados_chacarita" | SUBTIPO_DATO == "hisopados_coghlan" |
           SUBTIPO_DATO == "hisopados_la_paternal" | SUBTIPO_DATO == "hisopados_mataderos" |
           SUBTIPO_DATO == "hisopados_nunez" | SUBTIPO_DATO == "hisopados_parque_avellaneda"  |
           SUBTIPO_DATO == "hisopados_parque_chacabuco"  | SUBTIPO_DATO == "hisopados_parque_patricios" |
           SUBTIPO_DATO == "hisopados_pompeya" | SUBTIPO_DATO == "hisopados_recoleta" |
           SUBTIPO_DATO == "hisopados_rodrigo_bueno" | SUBTIPO_DATO == "hisopados_saavedra" |
           SUBTIPO_DATO == "hisopados_san_cristobal" | SUBTIPO_DATO == "hisopados_san_telmo" |
           SUBTIPO_DATO == "hisopados_soldati" |  SUBTIPO_DATO == "hisopados_villa_crespo" |
           SUBTIPO_DATO == "hisopados_villa_ortuzar" | SUBTIPO_DATO == "hisopados_villa_riachuelo_-_lugano" |
           SUBTIPO_DATO == "hisopados_villa_urquiza")


summary(hisopados)
#como se puede ver, los datos son a partir del primero de julio de 2020.

arrange(hisopados, desc(VALOR))

#ahora ploteamos para ver el número de hisopados (acumulados) en el lapso de tiempo en el que tenemos información
ggplot(hisopados) +
  geom_line(aes(x = FECHA, y = VALOR)) +
  facet_wrap(~SUBTIPO_DATO, ncol = 5) +
  labs(title = "Hisopados",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal()



#gráfico de líneas animado
ggplot(reporte_covid_camas, aes(x = FECHA, y = VALOR, color = SUBTIPO_DATO)) +
  geom_line() +
  geom_point() +
  labs(title = "Camas",
       subtitle = "Covid-19",
       x = "",
       y = "N",
       caption = "fuente: data.buenosaires.gob.ar") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")


camas_publicas <- filter(reporte_covid, TIPO_DATO == "ocupacion_de_camas_sistema_publico") 

ggplot(camas_publicas, aes(x = FECHA, y = VALOR, color = SUBTIPO_DATO)) +
  geom_line() +
  geom_point() +
  labs(title = "Camas publicas",
       subtitle = "Covid-19",
       x = "",
       y = "N") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")
                  

camas_publicas_graves <- filter(reporte_covid, SUBTIPO_DATO == "gaves_arm" |                                                  
                                  SUBTIPO_DATO == "graves" |                                                     
                                  SUBTIPO_DATO == "graves_no_arm" |                                              
                                  SUBTIPO_DATO == "graves_total")  



ggplot(camas_publicas_graves, aes(x = FECHA, y = VALOR, color = SUBTIPO_DATO)) +
  geom_line() +
  geom_point() +
  labs(title = "Camas publicas - pacientes graves",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "N",
       caption = "fuente: data.buenosaires.gob.ar") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")



reporte_covid_camas <- filter(reporte_covid, TIPO_DATO == "total_de_camas_sistema_publico")

#gráfico de líneas animado
ggplot(reporte_covid_camas, aes(x = FECHA, y = VALOR, color = SUBTIPO_DATO)) +
  geom_line() +
  geom_point() +
  labs(title = "Camas",
       subtitle = "Covid-19",
       x = "",
       y = "N",
       caption = "fuente: data.buenosaires.gob.ar") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")


camas_publicas <- filter(reporte_covid, TIPO_DATO == "ocupacion_de_camas_sistema_publico") 

ggplot(camas_publicas, aes(x = FECHA, y = VALOR, color = SUBTIPO_DATO)) +
  geom_line() +
  geom_point() +
  labs(title = "Camas publicas",
       subtitle = "Covid-19",
       x = "",
       y = "N",
       caption = "fuente: data.buenosaires.gob.ar") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")
                  

camas_publicas_graves <- filter(reporte_covid, SUBTIPO_DATO == "gaves_arm" |                                                  
                                  SUBTIPO_DATO == "graves" |                                                     
                                  SUBTIPO_DATO == "graves_no_arm" |                                              
                                  SUBTIPO_DATO == "graves_total")  


#filtramos datos sobre camas publicas para pacientes graves por el numero ocupacion total
camas_publicas_graves_total <- camas_publicas_graves %>% filter(SUBTIPO_DATO == "graves_total")

#filtramos datos sobre camas publicas para pacientes graves por el numero disponible
camas_publicas_graves_disponibles <- camas_publicas_graves %>% filter(SUBTIPO_DATO == "graves")

#creamos otro dataset con el numero de camas graves disponibles y usadas coincidente por fecha
ocupacion_camas <- inner_join(camas_publicas_graves_disponibles, camas_publicas_graves_total, by = "FECHA")


#calculamos el % de camas para pacientes graves ocupadas en CABA
ocupacion_camas <- ocupacion_camas %>% mutate(porcentaje_ocupacion = (VALOR.y*100)/VALOR.x)



#ahora agregamos los datos de ocupación de camas con arm
ocupacion_camas <- inner_join(ocupacion_camas, camas_publicas_graves_arm, by = "FECHA")

#calculamos el % de camas para pacientes graves con arm ocupadas en CABA
ocupacion_camas <- ocupacion_camas %>% mutate(porcentaje_ocupacion_arm = (VALOR*100)/VALOR.x)


#ahora agregamos los datos de ocupación de camas sin arm
ocupacion_camas <- inner_join(ocupacion_camas, camas_publicas_graves_no_arm, by = "FECHA")

#calculamos el % de camas para pacientes graves con arm ocupadas en CABA
ocupacion_camas <- ocupacion_camas %>% mutate(porcentaje_ocupacion_no_arm = (VALOR.y.y*100)/VALOR.x)


#hacemos un plot con el % de ocupación de camas totales, arm y no arm
ggplot(ocupacion_camas) +
  geom_line(aes(x = FECHA, y = porcentaje_ocupacion, color = SUBTIPO_DATO.y)) +
  geom_point(aes(x = FECHA, y = porcentaje_ocupacion, color = SUBTIPO_DATO.y)) +
  geom_line(aes(x = FECHA, y = porcentaje_ocupacion_arm, color = SUBTIPO_DATO.x.x)) +
  geom_point(aes(x = FECHA, y = porcentaje_ocupacion_arm, color = SUBTIPO_DATO.x.x)) +
  geom_line(aes(x = FECHA, y = porcentaje_ocupacion_no_arm, color = SUBTIPO_DATO.y.y)) +
  geom_point(aes(x = FECHA, y = porcentaje_ocupacion_no_arm, color = SUBTIPO_DATO.y.y)) +
  ylim(0, 100) +
  labs(title = "Porcentaje de ocupación: camas para pacientes graves",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "%",
       caption = "fuente: data.buenosaires.gob.ar") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")


#extraemos los datos de personas hisopadas cada cien mil habitantes en relación a las fechas
hisopados_100000 <- filter(reporte_covid, SUBTIPO_DATO == "tasa_personas_hisopadas_c_100_000_hab")

ggplot(hisopados_100000, aes(x = FECHA, y = VALOR)) +
  geom_line() +
  geom_point() +
  labs(title = "Hisopados cada 100.000 habitantes",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "Hisopados cada 100.000",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal() +
  theme(legend.position = "bottom")


#calculamos el % de hisopados en relación a las fechas
hisopados_100000 <- hisopados_100000 %>% mutate(VALOR_PORCENTUAL = (VALOR)/1000)

ggplot(hisopados_100000, aes(x = FECHA, y = VALOR_PORCENTUAL)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de personas hisopadas",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "% hisopados",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal() +
  theme(legend.position = "bottom")


#vamos a ver la evolución del % de fallecidos en personas mayores de 60 años
porcentaje_positividad_acumulada <- filter(reporte_covid, SUBTIPO_DATO == "%_positividad_personas_hisopadas_acumulada_caba")

#eje y ajustado
ggplot(porcentaje_positividad_acumulada, aes(x = FECHA, y = VALOR)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de positividad de personas hisopadas",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "% positividad",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal() +
  theme(legend.position = "bottom")

#eje y escala 0-100
ggplot(porcentaje_positividad_acumulada, aes(x = FECHA, y = VALOR)) +
  geom_line() +
  geom_point() +
  ylim(0, 100) +
  labs(title = "Porcentaje de positividad de personas hisopadas",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "% positividad",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal() +
  theme(legend.position = "bottom")




#porcentaje de letalidad
porcentaje_letalidad <- filter(reporte_covid, SUBTIPO_DATO == "%_letalidad_acumulada")

#eje y ajustado
ggplot(porcentaje_letalidad, aes(x = FECHA, y = VALOR, color = TIPO_DATO)) +
  geom_point() +
  labs(title = "Porcentaje de letalidad",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "% letalidad",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal() +
  theme(legend.position = "bottom")


#porcentaje de positividad por día
porcentaje_positividad_dia <- filter(reporte_covid, SUBTIPO_DATO =="%_positividad_personas_hisopadas_reportados_del_dia_caba")
#eje y ajustado
ggplot(porcentaje_positividad_dia, aes(x = FECHA, y = VALOR)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de positividad de personas hisopadas",
       subtitle = "CABA - Covid-19",
       x = "",
       y = "% positividad",
       caption = "fuente: data.buenosaires.gob.ar") +
  theme_minimal() +
  theme(legend.position = "bottom")



#VER
#numero de altas acumuladas
altas_acumuladas <- filter(reporte_covid, SUBTIPO_DATO =="altas_acumuladas")
altas_acumuladas <- select(altas_acumuladas, -TIPO_REPORTE, -TIPO_DATO)
#VER

#VER
#numero de altas por día
altas_dia <- filter(reporte_covid, SUBTIPO_DATO =="altas_reportados_del_dia")
altas_dia <- select(altas_dia, -TIPO_REPORTE, -TIPO_DATO)

altas <- inner_join(altas_acumuladas, altas_dia, by = "FECHA")
#VER
