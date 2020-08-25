library(tidyverse)
library(ggmap)
library(sf)


#datos sobre la ubicación de los operativos detectar en la ciudad
detectar <- st_read('https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/operativo-detectar/operativo-detectar.geojson')

#comunas_geo
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')


detectar <- detectar %>% mutate(lat = unlist(map(detectar$geometry, 1)),long=unlist(map(detectar$geometry,2)))

ggplot() +
  geom_sf(data = comunas$geometry) +
  geom_point(data = detectar, aes(x = lat, y = long, color = comuna)) +
  geom_sf_text(data=comunas, aes(label = comunas), size=2.5, colour = "black") +
  labs(title = "Mapa de puntos DetectAR",
       subtitle = "Ciudad Autónoma de Buenos Aires",
       x = "",
       y = "",
       caption = "fuente: data.buenosaires.gob.ar",
       color = "Se encuentra en:") +
  theme_minimal()

summary(detectar$comuna)
