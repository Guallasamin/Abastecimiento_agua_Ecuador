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

# Limpiamos todas las variables existentes y configuramos la cantidad de dígitos a mostrar.

rm(list=ls())
options(digits = 10)

# Se instalan los paquetes y librerías a utilizar en el código.

library("pacman") ; p_load("dplyr", "data.table", "tidyverse", "gdata", "srvyr", "bit64", "devtools", "haven", "foreign")

# Definimos un bucle for para cargar las bases de datos ENEMDU HOGARES 2007 - 2018 cuarto trimestre.
for (i in 7:18) {
  number_str <- sprintf("%02d", i)
  filename <- paste("/Users/2023.07.19ENEMDU/ENEMDU_BDD_20", number_str, "_12/ENEMDU_VIV_HOG_20", number_str, "_12_hom/ENEMDU_VIV_HOG_20", number_str, "_12_hom.sav", sep="")
  # Leemos el archivo SPSS y lo almacenamos en un objeto con el nombre "enemduXX".
  assign(paste("enemdu", number_str, sep=""), read_spss(filename))
}

# Se cargan las bases de datos ENEMDU HOGARES 2019 - 2022 cuarto trimestre.

enemdu19 <- read_spss("/Users/2023.07.19ENEMDU/BDD_ENEMDU_2019_12_SPSS/enemdu_viv_hog_201912.sav")
enemdu20 <- read_spss("/Users/2023.07.19ENEMDU/1_BDD_ENEMDU_2020_12_SPSS_RECALCULADO/enemdu_vivienda_hogar_2020_12.sav")
enemdu21 <- read_spss("/Users/2023.07.19ENEMDU/1_BDD_ENEMDU_2021_12_SPSS/enemdu_vivienda_hogar_2021_12.sav")
enemdu22 <- read_spss("/Users/2023.07.19ENEMDU/1_BDD_ENEMDU_2022_12_SPSS/enemdu_vivienda_hogar_2022_12.sav")

# El INEC en su encuesta ENEMDU define 1 como hogares con acceso a agua potable de la red pública.
# Las demás nomenclaturas corresponden a otras fuentes de acceso a agua como:
# Red pública
# Pila o llave pública
# Otra fuente por tubería
# Carro repartidor, triciclo
# Pozo
# Río, vertiente, acequia
# Otro

# Realizamos manipulaciones en los datos para cada año.

enemdu07 <- enemdu07 %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==8, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==9, 2, viv10)) %>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu08 <- enemdu08 %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==8, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==9, 2, viv10)) %>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu09 <- enemdu09 %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==8, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==9, 2, viv10)) %>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu10 <- enemdu10 %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==8, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==9, 2, viv10)) %>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu11 <- enemdu11 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu12 <- enemdu12 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu13 <- enemdu13 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu14 <- enemdu14 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu15 <- enemdu15 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu16 <- enemdu16 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu17 <- enemdu17 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu18 <- enemdu18 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu19 <- enemdu19 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu20 <- enemdu20 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(periodo = 202012) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu21 <- enemdu21 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(periodo = 202112) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

enemdu22 <- enemdu22 %>%
  mutate(vi10 = as.numeric(as.factor(vi10))) %>%
  mutate(periodo = 202212) %>%
  mutate(viv10 = NA,
         viv10 = ifelse(vi10==1, 1, viv10), # Posee agua de la red pública de agua
         viv10 = ifelse(vi10==2, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==3, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==4, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==5, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==6, 2, viv10), # Posee agua de otra fuente de abastecimiento
         viv10 = ifelse(vi10==7, 2, viv10))%>%
  select(c(fexp, periodo, viv10, upm)) %>%
  rename(vi10 = viv10)

# Concatenamos los datos de los diferentes años en un único dataframe llamado "enemdu"

file_paths <- c(
  "enemdu07",
  "enemdu08",
  "enemdu09",
  "enemdu10",
  "enemdu11",
  "enemdu12",
  "enemdu13",
  "enemdu14",
  "enemdu15",
  "enemdu16",
  "enemdu17",
  "enemdu18",
  "enemdu19",
  "enemdu20",
  "enemdu21",
  "enemdu22"
)

# Cargar el primer dataframe
enemdu <- get(file_paths[1])

# Concatenar los demás dataframes
for (i in 2:length(file_paths)) {
  enemdu <- rbindlist(list(enemdu, get(file_paths[i])))
}   

# Guardar todas las ENEMDU desde 2007 - 2022 con información
# sobre abastecimiento de agua en una sola base de datos para realizar cálculos.

fwrite(enemdu, "/Users/1. Dateras/enemdu.csv")
