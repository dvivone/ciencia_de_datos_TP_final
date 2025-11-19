library(naniar)
library(patchwork)
library(kableExtra)
library(mice)

#Calcular estadísticas de faltantes

resumen_faltantes <- data_paises %>%
  miss_var_summary() %>%
  filter(n_miss > 0)

# Gráfico de barras de porcentaje faltante

p1 <- gg_miss_var(data_paises, show_pct = TRUE) +
  labs(title = "Porcentaje de valores faltantes por variable",
       y = "Variables") +
  theme(axis.text.y = element_text(size = 10))

p1


# Patrón de datos faltantes (heatmap)
p2 <- vis_miss(data_paises %>% 
                select(inflacion, unemp, gdp),
               cluster = TRUE) +
  labs(title = "Patrón de datos faltantes",
       subtitle = "Negro = Faltante, Gris = Observado")
p2

# Combinar gráficos
p1 / p2

####Test MCAR

# Preparar datos para el test
datos_para_test <- data_paises %>%
  select(where(is.numeric)) %>%
  select(where(~any(is.na(.))))

# Realizar test de Little
test_mcar <- naniar::mcar_test(datos_para_test)

resultados_mcar <- data.frame(
  Estadistico = round(test_mcar$statistic, 2),
  `Grados de libertad` = test_mcar$df,
  `P-value` = format(test_mcar$p.value, scientific = TRUE),
  Conclusion = ifelse(test_mcar$p.value < 0.05, 
                      "Rechazamos H0: Datos NO son MCAR",
                      "No rechazamos H0: Datos podrían ser MCAR")
)

resultados_mcar %>%
  kable(caption = "Resultados del Test de Little para MCAR") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(4, bold = TRUE, 
              color = ifelse(test_mcar$p.value < 0.05, "red", "green"))

##### Imputación múltiple############

# 1. Asegurar que las variables clave tengan el formato correcto

data_imputar <- data_paises %>%
  mutate(pais = as.factor(pais))

# 2. Verificar la estructura de las variables
str(data_imputar)
# 2. Configurar la imputación MICE

#####################################################################
#PREPARACIÓN Y CONFIGURACIÓN MICE PARA DATOS DE PANEL
#####################################################################

# 1. Asegurar que la variable de cluster es un FACTOR y luego convertirla a INTEGER
data_imputar <- data_paises %>%
  mutate(
    # Primero aseguramos que es un factor (necesario para mantener el orden)
    pais_Factor = as.factor(pais),
    # Luego convertimos el factor a un entero.
    # Cada nivel del factor (cada país) recibirá un identificador numérico único (1, 2, 3, ...)
    pais_ID = as.integer(pais_Factor)
  ) %>%
  # Quitamos la columna original del nombre si data_paises la tenía y no la vamos a imputar
  select(-pais_Factor) # Mantenemos solo el ID para MICE


# 2. Configurar la imputación MICE

# Variables con NAs a imputar
variables_a_imputar <- c("inflacion", "gdp", "unemp")

# Generar la matriz de predicción inicial usando el nuevo dataframe
ini <- mice(data_imputar, maxit = 0)
pred_matrix <- ini$predictorMatrix
method_vector <- ini$method

# 3. Aplicar el método de imputación multinivel ('2l.pan')
for (var in variables_a_imputar) {
  # Establecer el método de imputación multinivel
  method_vector[var] <- "2l.pan"
  
  # ¡IMPORTANTE! Indicar que 'Country.Name_ID' es la variable de cluster (código -2)
  # Usamos la nueva variable integer
  pred_matrix[var, "pais_ID"] <- -2
  
  # Asegurar que el Country.Name original (si existe) no se use para predecir ni se impute
  # Si la columna Country.Name original aún existe en data_imputar, exclúyela:
  # pred_matrix[, "Country.Name"] <- 0 
  # method_vector["Country.Name"] <- ""
}

# 4. Ejecutar la imputación
m <- 5
maxit <- 10

mice_imp_panel <- mice(
  data_imputar,
  method = method_vector,
  predictorMatrix = pred_matrix,
  m = m,
  maxit = maxit,
  seed = 42
)
##### Comparación imputación #####

# 1. Preparar datos para comparación, añadiendo una columna de origen
comparacion_dist <- bind_rows(
  data_paises %>%
    select(inflacion, gdp, unemp) %>%
    mutate(Origen = "Original"),
  datos_imp_mice %>%
    select(inflacion, gdp, unemp) %>% # Asegurar que solo se incluyan las mismas columnas
    mutate(Origen = "Imputado")
)

# 2. Calcular las estadísticas por grupo (Original vs. Imputado)
comparacion_resumen <- comparacion_dist %>%
  group_by(Origen) %>% # Agrupar por Origen para ver las estadísticas separadas
  summarise(
    n = n(),
    media_inflacion = mean(inflacion, na.rm = TRUE),
    mediana_inflacion = median(inflacion, na.rm = TRUE),
    sd_inflacion = sd(inflacion, na.rm = TRUE),
    p10_inflacion = quantile(inflacion, 0.10, na.rm = TRUE),
    p90_inflacion = quantile(inflacion, 0.90, na.rm = TRUE),
    iqr_inflacion = IQR(inflacion, na.rm = TRUE),
    
    media_gdp = mean(gdp, na.rm = TRUE),
    # ... Repetir para gdp y unemp
    
    .groups = "drop"
  ) %>%
  # Redondear las columnas numéricas 
  mutate(across(where(is.numeric), ~round(., 2)))

####Generar el data frame completo

# 1. Extraer la primera imputación completa (solo incluye las variables usadas en MICE)
datos_imputados_parciales <- complete(mice_imp, 1)

# 2. Identificar las variables faltantes en la tabla imputada
#    (Reemplaza 'pais' y 'anio' con los nombres reales de tus variables)
variables_faltantes <- data_paises %>%
  select(pais, anio)

# 3. Combinar las variables faltantes con el resultado de la imputación
#    Usamos bind_cols() para añadir las columnas, asumiendo el mismo orden de filas
datos_imputados_final <- bind_cols(variables_faltantes, datos_imputados_parciales)

# 4. Verificar el resultado
head(datos_imputados_final)
str(datos_imputados_final)



############Detección de outliers

variables_numericas <- c("inflacion", "gdp", "unemp")

# ----------------------------------------------------
# 3. Detección de Outliers (Método IQR)
# ----------------------------------------------------

# La función .data[[]] permite referenciar una columna por su nombre de string
detectar_outlier_iqr <- function(data, columna) {
  Q1 <- quantile(data[[columna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[columna]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  # Límites de Outlier: 1.5 * IQR
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  
  # TRUE si es outlier, FALSE en caso contrario
  es_outlier <- (data[[columna]] < limite_inferior) | (data[[columna]] > limite_superior)
  return(es_outlier)
}

# 4. Generar variable dummy (IQR)
datos_outliers <- datos_imputados_final %>%
  mutate(
    # Aplicamos la función a cada variable y usamos OR (|) para combinar los resultados
    # Si es outlier en CUALQUIERA de las variables, se marca como 1
    is_outlier_iqr = if_else(
      detectar_outlier_iqr(., "inflacion") |
        detectar_outlier_iqr(., "gdp") |
        detectar_outlier_iqr(., "unemp"),
      1, 0
    )
  )


datos_completos <- write.csv(datos_outliers, file = "datos_completos.csv", row.names = FALSE, na = "")
