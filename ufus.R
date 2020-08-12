library(tidyverse)
library(viridis)
library(hrbrthemes)
#llamamos a las librerías que vamos a usar para trabajar con datos geoespaciales
library(sf)
library(tidyverse)



#datos Unodades Febriles de Emergencia (UFUS)
ufus <- read_csv('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/ufus/ubicacion-ufus.csv')


bbox <- getbb("Ciudad de Buenos Aires", format_out = "sf_polygon")
bbox


leaflet(bbox) %>%
  addTiles() %>%
  addPolygons()



#vamos a descargar un archivo .geojson con la info sobre las comunas en CABA
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')

#transformamos las coordenadas lat long en sf
ufus <- st_as_sf(ufus, coords = c('long','lat'), crs = 4326)

#hacemos un gráfico con los puntos espaciales en los que están las uf de emergencia y colocamos los nombres de hospitales
ggplot() +
  geom_sf(data = comunas) +
  geom_point(data = ufus, aes(x = long, y = lat, color = nombre), show.legend = FALSE) +
  geom_label_repel(data = ufus, aes(long, lat, label = nombre), size = 2) +
  labs(title = "Unidades Febriles de Emergencia",
       subtitle = "CABA") +
  scale_color_viridis(discrete=TRUE) +
  theme_minimal()
