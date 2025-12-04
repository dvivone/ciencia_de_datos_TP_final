# ===========================================================
#        DETECCION DE OUTLIERS E IMUTACION DE FALTANTES
# ============================================================

# ============================================================
#  1 CARGA DE LIBRERIAS
# ============================================================

library(naniar)
library(patchwork)
library(kableExtra)
library(mice)

# ============================================================
# 2 CARGA DE DATOS
# ============================================================

path_data_paises<-file.path(data_clean,
                            "data_paises.csv")
data_paises_estadistica<-read.csv(path_data_paises)

# ============================================================
# 3 DETECCION DE OUTLIERS METODO IQR
# ============================================================

data_paises_outliers <- data_paises %>%
  mutate(across(c(inflacion, desempleo, gdp),
                ~ {
                  Q1 <- quantile(., 0.25, na.rm = TRUE)
                  Q3 <- quantile(., 0.75, na.rm = TRUE)
                  IQR_val <- Q3 - Q1
                  
                  # Definición de límites
                  limite_inferior <- Q1 - 1.5 * IQR_val
                  limite_superior <- Q3 + 1.5 * IQR_val
                  
                  # TRUE si es un outlier
                  . < limite_inferior | . > limite_superior
                },
                .names = "{.col}_outlier"))

data_paises_outliers %>%
  summarise(
    outliers_inflacion = sum(inflacion_outlier, na.rm = TRUE),
    outliers_desempleo = sum(desempleo_outlier, na.rm = TRUE),
    outliers_gdp = sum(gdp_outlier, na.rm = TRUE)
  )

# ============================================================
# 4 ELIMINAR LOS OUTLIERS
# ============================================================

data_paises_sin_outliers <- data_paises_outliers %>%
  filter((inflacion_outlier == FALSE |  is.na(inflacion_outlier)),
    (desempleo_outlier == FALSE  | is.na(desempleo_outlier)),
    (gdp_outlier == FALSE | is.na(gdp_outlier))
)

# ============================================================
# 5 DATOS FALTANTES
# ============================================================

# 5.1 Calcular estadísticas de faltantes
resumen_faltantes <- data_paises_sin_outliers %>%
  miss_var_summary() %>%
  filter(n_miss > 0)

# 5.2 Gráfico de barras de porcentaje faltante
p1 <- gg_miss_var(data_paises_sin_outliers, show_pct = TRUE) +
  labs(title = "Porcentaje de valores faltantes por variable",
       y = "Variables") +
  theme(axis.text.y = element_text(size = 10))

# 5.3 Mostrar grafico de barras
p1


# 5.4 Patrón de datos faltantes (heatmap)
p2 <- vis_miss(data_paises_sin_outliers %>% 
                 select(inflacion, desempleo, gdp),
               cluster = TRUE) +
  labs(title = "Patrón de datos faltantes",
       subtitle = "Negro = Faltante, Gris = Observado")

# 5.5 Mostrar grafico heatmap
p2

# Combinar gráficos
p1 / p2

# ============================================================
# 6 IMPUTACION DATOS FALTANTES
# ============================================================

# 6.1 Para la variable gdp el porcentaje de faltantes es menor al 5%
## se usa imputación simple
data_paises_imp1 <- data_paises_sin_outliers %>%
  mutate(gdp = ifelse(is.na(gdp), mean(gdp, na.rm = TRUE), gdp))


# 6.2 Para inflation y desempleo realizamos el test de Little para ver si los datos son MCAR
datos_para_test <- data_paises_sin_outliers %>% 
  select(inflacion,desempleo)

test_mcar <- naniar::mcar_test(datos_para_test)
                        
# 6.3 Crear dataframe con resultados
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

# 6.4 Como los datos no son MCAR hacemos imputación múltiple

vars_mice <- data_paises_imp1 %>% select(inflacion,desempleo)

# 6.5 Configurar y ejecutar MICE
mice_imp <- mice(vars_mice,
                 m = 5,            # 5 imputaciones
                 method = 'pmm',   # Predictive mean matching
                 seed = 2025,
                 printFlag = FALSE)

# 6.6 Ver métodos utilizados
print(mice_imp$method)

datos_imp_mice <- complete(mice_imp, 1)

data_paises_completos <- data_paises_imp1 %>%
  mutate(
    inflacion = datos_imp_mice$inflacion,
    desempleo = datos_imp_mice$desempleo
  ) %>% select(-inflacion_outlier,-gdp_outlier,-desempleo_outlier)


# ============================================================
# 7 SE GUARDAN ESTADISTICAS EN NUEVO FILE
# ============================================================

write.csv(data_paises_completos,
          file.path(data_processed, "data_paises_completos.csv"),
          row.names = FALSE)
