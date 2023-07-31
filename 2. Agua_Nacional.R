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

library("pacman") ; p_load("dplyr", "data.table", "tidyverse", "tmap", "tmaptools", "leaflet", "haven", "foreign", "ggplot2", "sf", "scales")

# Se carga la base de datos creada en el código "Armaje_base_de_datos"

enemdu <- read.csv("/Users/enemdu.csv")

enemdu <- enemdu %>%
  rename(hogares = fexp ) 

# Calculate total_personas
enemdu <- enemdu %>%
  group_by(periodo) %>%
  mutate(total_hogares = sum(hogares))

# Calculate porcentaje
enemdu <- enemdu %>%
  group_by(periodo) %>%
  mutate(porcentaje = hogares / total_hogares)

enemdu <- enemdu %>%
  mutate(porcentajes = porcentaje * 100)

enemdu <- mutate(enemdu,
                 periodo=as.numeric(str_sub(periodo, 1,4)))

# Visualización Barras

ggplot(enemdu, aes(x = periodo, y = porcentajes, fill = factor(vi10))) +  # Convertir "vi10" a factor si es necesario
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#1E4A88", "#207DC2"), labels = c("Red pública", "Otro")) +
  guides(fill = guide_legend(title = "Fuente principal de \n obtención de agua")) +
  labs(title = "",
       x = " ", y = "Porcentaje de hogares", caption = "Otro incluye: Pila o llave pública, otra fuente por tubería,
       carro/triciclo repartidor, pozo, río, vertiente, acequia.") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, size = 10),  # Rotar y alinear etiquetas del eje x
        axis.text.y = element_text(size = 10),  # Rotar y alinear etiquetas del eje y
        legend.position = "top",
        panel.grid.major = element_blank(),  # Eliminar las líneas de referencia en el eje x
        panel.grid.minor = element_blank(),  # Eliminar las líneas de referencia en el eje y
        panel.border = element_blank()) +      # Eliminar el borde del panel
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_x_continuous(breaks=seq(2007,2022,1)) 

# Guardar la imagen en máxima resolución (300 DPI)
ggsave("grafico_barras.png",  width = 10, height = 6, dpi = 300)