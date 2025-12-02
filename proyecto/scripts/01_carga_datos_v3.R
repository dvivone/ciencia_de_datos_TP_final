# Librerías del proyecto
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(knitr)

# Limpiar entorno
rm(list = ls())

# Configurar opciones globales
options(stringsAsFactors = FALSE)
options(scipen = 999)  

#Setear directorio de trabajo
setwd(r'(E:\Diego\Mis Documentos\GitHub\ciencia_de_datos_TP_final\proyecto)')

data_clean<-file.path(r'(data/clean)')
data_row<-file.path(r'(data/row)')
data_processed<-file.path(r'(data/processed)')

output_tables<-file.path(r'(output/tables)')
output_figures<-file.path(r'(output/figures)')

archivo_datos_inflation<-"API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_130173.csv"
archivo_datos_gdp<-"API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_130026.csv"
archivo_datos_unemp<-"API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_130165.csv"

path_inflation<-file.path(data_clean,archivo_datos_inflation)
path_gdp<-file.path(data_clean,archivo_datos_gdp)
path_unemp<-file.path(data_clean,archivo_datos_unemp)

######CARGA DATOS######

inflation <- read.csv(path_inflation,
                      skip = 4,
                      header = TRUE)

gdp <- read.csv(path_gdp,
                skip=4,
                header=TRUE)

unemp <- read.csv(path_unemp,
                  skip=4,
                  header=TRUE)


########LIMPIEZA DATOS#########

######INFLATION########

#Estructura de los datos
glimpse(inflation)

#eliminar columnas vacías
inflation <- inflation[, 1:69] 

#cambiar nombres de columnas
colnames(inflation) <- gsub("^X", "", colnames(inflation))

#pivotear

inflation_largos  <- inflation %>% 
  pivot_longer(cols= 5:69,
               names_to = "anio",               # Nombre de la nueva columna
               values_to = "inflacion",        # Nombre de la columna de valores
               names_transform = list(anio = as.numeric)  # Convertir años a numérico
)
#seleccionar variables 
inflation_largos <- inflation_largos %>% 
  select('Country.Name','anio','inflacion')

#######GDP#########

#Estructura de los datos
glimpse(gdp)

#eliminar columnas vacías
gdp <- gdp[, 1:69] 

#cambiar nombres de columnas
colnames(gdp) <- gsub("^X", "", colnames(gdp))

#pivotear

gdp_largos  <- gdp %>% 
  pivot_longer(cols= 5:69 ,
               names_to = "anio",               # Nombre de la nueva columna
               values_to = "gdp",        # Nombre de la columna de valores
               names_transform = list(anio = as.numeric)  # Convertir años a numérico
)
#seleccionar variables 
gdp_largos <- gdp_largos %>% 
  select('Country.Name','anio','gdp')


#######UNEMPLOYMENT##########

#Estructura de los datos
glimpse(unemp)

#eliminar columnas vacías
unemp <- unemp[, 1:69] 

#cambiar nombres de columnas
colnames(unemp) <- gsub("^X", "", colnames(unemp))

#pivotear

unemp_largos  <- unemp %>% 
  pivot_longer(cols= 5:69,
               names_to = "anio",               # Nombre de la nueva columna
               values_to = "unemp",        # Nombre de la columna de valores
               names_transform = list(anio = as.numeric)  # Convertir años a numérico
)
#seleccionar variables 
unemp_largos <- unemp_largos %>% 
  select('Country.Name','anio','unemp')


####Joinear todo en un unico dataset########

data <- inflation_largos %>% 
  left_join(gdp_largos, by = c("Country.Name", "anio")) %>%
  left_join(unemp_largos, by = c("Country.Name", "anio"))%>%
  rename("pais"="Country.Name")

#Dimensiones del dataset - observaciones disponibles
glimpse(data)

#Detección inicial de patrones o anomalías
head(data,10)

###Excluir datos de no paises
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

#filtar datos de no paises y se filtran datos para tres años
data_paises <- data %>%
  filter(!pais %in% regiones_a_excluir)%>%
  filter(anio %in% c(2011,2016,2022))

#Se guardan datos en un nuevo file
path_data_paises<-file.path(data_row,"data_paises.csv")
write.csv(data_paises,path_data_paises,row.names = FALSE)

  ###########ESTRUCTURA DEL DATA FRAME########

resumen_estructura <- data.frame(
  "Variable" = colnames(data_paises), # Nombres de las columnas
  "Tipo_Dato" = sapply(data_paises, class)  # Tipo de dato de cada columna
)


resumen_columnas <- data.frame(
  "Variable" = colnames(data_paises),
  "Tipo_Dato" = sapply(data_paises, class)
)


# Calculamos las dimensiones
num_filas <- nrow(data_paises)
num_columnas <- ncol(data_paises)

# Creamos la cadena de texto para la fila de resumen
info_dimensiones <- paste(num_filas, "Filas (Observaciones) x", num_columnas, "Columnas (Variables)")

# Añadimos la nueva fila al resumen
resumen_completo <- resumen_columnas %>%
  tibble::add_row(Variable = "DIMENSIONES TOTALES", Tipo_Dato = info_dimensiones)

rownames(resumen_completo) <- NULL

# GENERAR LA TABLA KABLE FINAL

tabla_final_completa <- resumen_completo %>%
 
  select(Variable, Tipo_Dato) %>%
  
  kbl(
    caption = "Análisis Estructural Completo de data_paises",
    
    row.names = FALSE,
    col.names = c("Variable", "Tipo de Dato"),
    align = c('l', 'l')
  ) %>%
  

  kable_classic_2(full_width = F, html_font = "Cambria") %>%
 
  add_header_above(c(" " = 1, "Estructura del Data Frame" = 1)) %>%

  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, italic = T)

print(tabla_final_completa)

#####################datos para la serie de tiempo ##############


data_serie <- inflation_largos %>%
  left_join(unemp_largos, by = c("pais", "anio"))

serie_inflacion <- data_serie %>%
  filter(!pais %in% regiones_a_excluir)




# DATOS PARA PLOTEAR LA SERIE 

df_promedios_globales <- serie_inflacion %>%
  # 1. Agrupar el dataframe por la variable 'anio' (año)
  group_by(anio) %>%
  # 2. Calcular el promedio de 'unemp' e 'inflacion' para cada año
  summarise(
    promedio_unemp = mean(unemp, na.rm = TRUE),
    promedio_inflacion = mean(inflacion, na.rm = TRUE)
  ) %>%
  # 3. Desagrupar para que el dataframe sea un dataframe normal
  ungroup() %>% filter(anio >= 1998)

# Mostrar el resultado (los primeros 6 años)
head(df_promedios_globales)

#Se guardan datos en un nuevo file
path_data_serie<-file.path(outstub,"data_serie.csv")
write.csv(df_promedios_globales,path_data_serie,row.names = FALSE)


