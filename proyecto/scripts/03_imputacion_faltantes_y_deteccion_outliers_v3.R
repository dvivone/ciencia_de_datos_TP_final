library(naniar)
library(patchwork)
library(kableExtra)
library(mice)

path_data_paises<-file.path(data_row,"data_paises.csv")
data_paises_estadistica<-read.csv(path_data_paises)

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

# Seleccionar variables para imputación
vars_mice <- data_paises %>%
  select(pais, anio, gdp,
         inflacion, unemp)

# Configurar y ejecutar MICE
mice_imp <- mice(vars_mice,
                 m = 5,            # 5 imputaciones
                 method = 'pmm',   # Predictive mean matching
                 seed = 2025,
                 printFlag = FALSE)

# Ver métodos utilizados
print(mice_imp$method)


# Extraer primera imputación completa
datos_paises_completa <- complete(mice_imp, 1)

############Detección de outliers

variables_numericas <- c("inflacion", "gdp", "unemp")

# ----------------------------------------------------
# 3. Detección de Outliers (Método IQR)
# ----------------------------------------------------

# La función .data[[]] permite referenciar una columna por su nombre de string
detectar_outlier_iqr <- function(datos_paises_completa, columna) {
  Q1 <- quantile(data[[columna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[columna]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  # Límites de Outlier: 1.5 * IQR
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  
  # TRUE si es outlier, FALSE en caso contrario
  es_outlier <- (datos_paises_completa[[columna]] < limite_inferior) | (datos_paises_completa[[columna]] > limite_superior)
  return(es_outlier)
}

# 4. Generar variable dummy (IQR)
datos_outliers <- datos_paises_completa %>%
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


path_data_paises<-file.path(data_row,"data_paises_completos.csv")
write.csv(datos_outliers,path_data_paises,row.names = FALSE)