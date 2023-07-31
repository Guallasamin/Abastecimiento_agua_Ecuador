########################################################################
#### Proyecto: Abastecimiento de agua
#### Documento: Armaje de base de datos
#### Elaboración: Eco. Guallasamin Miño Andrea
####              Ing. Guallasamin Miño Jonathan
########################################################################

########################################################################

# Para ejecutar el código, se necesitan los formatos .CSV y .SAV de la 
# encuesta de hogares del cuarto trimestre ENEMDU 2007 - 2022.
# Los documentos se descargan desde el siguiente enlace:
# https://aplicaciones3.ecuadorencifras.gob.ec/BIINEC-war/index.xhtml;jsessionid=brACBJeqSsGMIn5l9Orl+e2k.undefined
# Se ingresa en el parámetro de búsqueda "ENEMDU".

########################################################################

# Se instalan los paquetes y librerías a utilizar en el código.

library("pacman") ; p_load("dplyr", "data.table", "tidyverse", "tmap", "tmaptools", "leaflet", "haven", "foreign", "ggplot2", "sf")

# Se carga la base de datos creada en el código "Armaje_base_de_datos"

enemdu <- read.csv("/Users/jonathanguallasamin/Desktop/1. Dateras/enemdu.csv")

# Se agrega una columna "prov" que se obtiene tomando los primeros dos caracteres de la columna 
#"upm" y luego se rellena con ceros a la izquierda para que todos tengan dos dígitos.

enemdu <- mutate(enemdu,
                 prov=as.numeric(str_sub(upm, 1,2)),
                 prov=str_pad(prov, 2, "left", pad = "0"))

# Se realiza una agregación de los datos por los factores 
# "periodo", "prov" y "vi10", sumando los valores de la columna "fexp".

enemdu <- aggregate(fexp ~ periodo + prov + vi10, enemdu, sum)

# Se renombra la columna "fexp" como "hogares".

enemdu <- enemdu %>%
  rename(hogares = fexp ) 

# Cálculo de porcentajes y reorganización de datos:

enemdu <- enemdu %>%
  group_by(periodo, prov) %>%
  mutate(total_hogares = sum(hogares))

enemdu <- enemdu %>%
  group_by(periodo, prov) %>%
  mutate(porcentaje = hogares / total_hogares)

# Se seleccionan las columnas "periodo", "vi10", "prov" y "porcentaje",
# y se realiza una transformación para que los años sean las columnas y 
# los porcentajes sean los valores.

reshaped_df <- enemdu %>%
  select(c(periodo, vi10, prov, porcentaje)) %>%
  pivot_wider(names_from = periodo, values_from = porcentaje, names_prefix = "anio_")

# Visualización de datos y generación de mapas:
# Se lee un archivo de datos geoespaciales de Ecuador en formato shapefile y se almacena en el objeto "mymap".

mymap <- st_read("/Users/jonathanguallasamin/Desktop/Ecuador_porprovincias/ecuador.shp", stringsAsFactors = FALSE)
str(mymap)

reshape_data_filtered <- reshaped_df[reshaped_df$vi10 == 2, ]
reshape_data_filtered <- reshaped_df[reshaped_df$prov != 20, ]
reshape_data_filtered <- reshape_data_filtered %>%
  select(c(prov, anio_202212))  %>%
  rename(DPA_PROVIN = prov)


plot <- left_join(reshape_data_filtered, mymap, by = c("DPA_PROVIN"))

plot <- st_as_sf(plot)

# Calcular la escala en porcentajes
enemdu$anio_202212 <- plot$anio_202212 * 100

# Se crea un mapa usando la librería ggplot2 y los datos geoespaciales en "enemdu", 
# donde se visualizan los porcentajes de hogares sin acceso a agua potable de la 
# red pública en cada provincia para el cuarto trimestre de 2022.

ggplot() +
  geom_sf(data = enemdu, aes(fill = anio_202212), color = "black") +
  scale_fill_gradient(name = "Porcentaje \n de Hogares", labels = scales::percent_format(scale = 1),
                      low = "#fee5d9", high = "#a50f15", limits = c(0, 100), na.value = "black") +
  labs(title = "", subtitle = "", caption = " ") +
  theme_minimal() +
  theme_void()

# Guardar el gráfico en alta resolución (300 ppp) en formato PNG
ggsave("/Users/jonathanguallasamin/Desktop/2023.07.19ENEMDU/mapa_hogares.png", dpi = 300)
