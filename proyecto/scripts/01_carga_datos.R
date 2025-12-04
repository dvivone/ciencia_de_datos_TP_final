# ===========================================================
#                PREPARACION DE DATOS
# ============================================================

# ============================================================
# 1 CARGA DE LIBRERIAS
# ============================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(knitr)
library(kableExtra)

# ============================================================
# 2 LIMPIEZA DE ENTORNO Y CONFIGURACION GLOBAL 
# ============================================================

# 2.1 Limpieza de entorno
rm(list = ls())

# 2.2 Configurar opciones globales
options(stringsAsFactors = FALSE)
options(scipen = 999)  

# ============================================================
# 3 SETEAR DIRECTORIO DE TRABAJO 
# ============================================================

# 3.1 Setear directorio de trabajo
setwd('~/GitHub/ciencia_de_datos_TP_final/proyecto')

# 3.2 Setear subdirectorios
data_clean<-file.path(r'(data/clean)')
data_raw<-file.path(r'(data/raw)')
data_processed<-file.path(r'(data/processed)')
output_tables<-file.path(r'(output/tables)')
output_figures<-file.path(r'(output/figures)')

# 3.3 Setear path de files
path_inflacion<-file.path(data_raw,archivo_datos_inflacion)
path_gdp<-file.path(data_raw,archivo_datos_gdp)
path_desempleo<-file.path(data_raw,archivo_datos_desempleo)

# 3.4 Setear nombres de files
archivo_datos_inflacion<-"API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_130173.csv"
archivo_datos_gdp<-"API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_130026.csv"
archivo_datos_desempleo<-"API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_130165.csv"



# ============================================================
# 4 CARGA DE DATOS
# ============================================================

# 4.1 Se cargan datos de inflación
inflacion <- read.csv(path_inflacion,
                      skip = 4,
                      header = TRUE)

# 4.2 Se cargan datos de gdp
gdp <- read.csv(path_gdp,
                skip=4,
                header=TRUE)

# 4.3 Se cargan datos de desempleo
desempleo <- read.csv(path_desempleo,
                  skip=4,
                  header=TRUE)


# ============================================================
# 5 LIPIEZA DE DATOS
# ============================================================

# ============================================================
# 5.1 Inflación
# ============================================================

# 5.1.1. Estructura de los datos
glimpse(inflacion)

# 5.1.2. Eliminar columnas vacías
inflacion <- inflacion[, 1:69] 

# 5.1.3. cambiar nombres de columnas
colnames(inflacion) <- gsub("^X", "", colnames(inflacion))

# 5.1.4. Pivotear
inflacion_largos  <- inflacion %>% 
  pivot_longer(cols= 5:69,
               names_to = "anio",               # Nombre de la nueva columna
               values_to = "inflacion",        # Nombre de la columna de valores
               names_transform = list(anio = as.numeric)  # Convertir años a numérico
)
# 5.1.5. Seleccionar variables 
inflacion_largos <- inflacion_largos %>% 
  select('Country.Name','anio','inflacion')

# ============================================================
# 5.2 Gdp
# ============================================================

# 5.2.1. Estructura de los datos
glimpse(gdp)

# 5.2.2. Eliminar columnas vacías
gdp <- gdp[, 1:69] 

# 5.2.3. cambiar nombres de columnas
colnames(gdp) <- gsub("^X", "", colnames(gdp))

# 5.2.4. Pivotear
gdp_largos  <- gdp %>% 
  pivot_longer(cols= 5:69 ,
               names_to = "anio",               # Nombre de la nueva columna
               values_to = "gdp",        # Nombre de la columna de valores
               names_transform = list(anio = as.numeric)  # Convertir años a numérico
)
# 5.2.5. Seleccionar variables 
gdp_largos <- gdp_largos %>% 
  select('Country.Name','anio','gdp')


# ============================================================
# 5.3. Desempleo
# ============================================================

# 5.3.1 Estructura de los datos
glimpse(desempleo)

# 5.3.2 Eliminar columnas vacías
desempleo <- desempleo[, 1:69] 

# 5.3.3 cambiar nombres de columnas
colnames(desempleo) <- gsub("^X", "", colnames(desempleo))

# 5.3.4 Pivotear
desempleo_largos  <- desempleo %>% 
  pivot_longer(cols= 5:69,
               names_to = "anio",               # Nombre de la nueva columna
               values_to = "desempleo",        # Nombre de la columna de valores
               names_transform = list(anio = as.numeric)  # Convertir años a numérico
)
# 5.3.5 seleccionar variables 
desempleo_largos <- desempleo_largos %>% 
  select('Country.Name','anio','desempleo')


# ============================================================
# 6 JOINEAR DATASETS
# ============================================================

# 6.1 Join de datasets
data <- inflacion_largos %>% 
  left_join(gdp_largos, by = c("Country.Name", "anio")) %>%
  left_join(desempleo_largos, by = c("Country.Name", "anio"))%>%
  rename("pais"="Country.Name")

# 6.2 Dimensiones del dataset - observaciones disponibles
glimpse(data)

# 6.3 Detección inicial de patrones o anomalías
head(data,10)

# ============================================================
# 7 LIMPIEZA DATASET
# ============================================================

# 7.1 Excluir datos de no paises
regiones_a_excluir <- c(
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "Arab World",
  "Caribbean small states",
  "Central Europe and the Baltics",
  "East Asia & Pacific (excluding high income)",
  "East Asia & Pacific (IDA & IBRD countries)",
  "East Asia & Pacific",
  "Euro area",
  "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Europe & Central Asia",
  "European Union",
  "Fragile and conflict affected situations",
  "Heavily indebted poor countries (HIPC)",
  "High income",
  "IDA & IBRD total",
  "IDA blend",
  "IDA total",
  "IDA only",
  "IBRD only",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean (IDA & IBRD countries)",
  "Latin America & Caribbean",
  "Least developed countries: UN classification",
  "Low & middle income",
  "Low income",
  "Lower middle income",
  "Middle East & North Africa",
  "Middle East & North Africa (IDA & IBRD countries)",
  "Middle income",
  "North America",
  "Not classified",
  "OECD members",
  "Other small states",
  "Pacific island small states",
  "Post-demographic dividend",
  "Pre-demographic dividend",
  "Small states",
  "South Asia",
  "South Asia (IDA & IBRD)",
  "Sub-Saharan Africa (excluding high income)",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "Sub-Saharan Africa",
  "Upper middle income",
  "World"
)

# 7.2 filtar datos de no paises y se filtran datos para tres años
data_paises <- data %>%
  filter(!pais %in% regiones_a_excluir)%>%
  filter(anio %in% c(2011,2016,2022))

# ============================================================
# 8 ESTRUCTURA DEL DATA FRAME 
# ============================================================

resumen_estructura <- data.frame(
  "Variable" = colnames(data_paises), # Nombres de las columnas
  "Tipo_Dato" = sapply(data_paises, class)  # Tipo de dato de cada columna
)

resumen_columnas <- data.frame(
  "Variable" = colnames(data_paises),
  "Tipo_Dato" = sapply(data_paises, class)
)

# 8.3 Calculamos las dimensiones
num_filas <- nrow(data_paises)
num_columnas <- ncol(data_paises)

# 8.4 Creamos la cadena de texto para la fila de resumen
info_dimensiones <- paste(num_filas, "Filas (Observaciones) x", num_columnas, "Columnas (Variables)")

# 8.5 Añadimos la nueva fila al resumen
resumen_completo <- resumen_columnas %>%
  tibble::add_row(Variable = "DIMENSIONES TOTALES", Tipo_Dato = info_dimensiones)

# 8.6  
rownames(resumen_completo) <- NULL

# 8.7 Generar la tabla
tabla_final_completa <- resumen_completo %>%
  select(Variable, Tipo_Dato) %>%
  kbl(caption = "Análisis Estructural Completo de data_paises",
      raw.names = FALSE,
      col.names = c("Variable", "Tipo de Dato"),
      align = c('l', 'l')
  ) %>%
  kable_classic_2(full_width = F,
                  html_font = "Cambria") %>%
  add_header_above(c(" " = 1,
                     "Estructura del Data Frame" = 1)) %>%
  column_spec(1, bold = T,
              border_right = T) %>%
  column_spec(2, italic = T)

# 8.8 Imprimir tabla
print(tabla_final_completa)

# ============================================================
# 9 SE GUARDAN ESTADISTICAS EN NUEVO FILE
# ============================================================

path_data_paises<-file.path(data_clean,"data_paises.csv")
write.csv(data_paises,path_data_paises,row.names = FALSE)

#####################datos para la serie de tiempo ##############

data_serie <- inflacion_largos %>%
  left_join(desempleo_largos, by = c('Country.Name', "anio"))

serie_inflacion <- data_serie %>%
  filter(!'Country.Name' %in% regiones_a_excluir)

# DATOS PARA PLOTEAR LA SERIE 

df_promedios_globales <- serie_inflacion %>%
  # 1. Agrupar el dataframe por la variable 'anio' (año)
  group_by(anio) %>%
  # 2. Calcular el promedio de 'desempleo' e 'inflacion' para cada año
  summarise(
    promedio_desempleo = mean(desempleo, na.rm = TRUE),
    promedio_inflacion = mean(inflacion, na.rm = TRUE)
  ) %>%
  # 3. Desagrupar para que el dataframe sea un dataframe normal
  ungroup() %>% filter(anio >= 1998)

# Mostrar el resultado (los primeros 6 años)
head(df_promedios_globales)

#Se guardan datos en un nuevo file
path_data_serie<-file.path(data_clean,"data_serie.csv")
write.csv(df_promedios_globales,path_data_serie,row.names = FALSE)


