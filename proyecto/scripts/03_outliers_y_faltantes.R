#Imputación faltantes y detección de outliers

#Librerías

library(naniar)
library(patchwork)
library(kableExtra)
library(mice)

#carga datos

path_data_paises<-file.path(data_row,"data_paises.csv")
data_paises_estadistica<-read.csv(path_data_paises)

###DETECCION DE OUTLIERS METODO IQR

data_paises_outliers <- data_paises %>%
  mutate(across(c(inflacion, unemp, gdp),
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
    outliers_unemp = sum(unemp_outlier, na.rm = TRUE),
    outliers_gdp = sum(gdp_outlier, na.rm = TRUE)
  )

#Eliminar los outliers
data_paises_sin_outliers <- data_paises_outliers %>%
  filter ( (inflacion_outlier == FALSE | is.na(inflacion_outlier)),
    (unemp_outlier == FALSE | is.na(unemp_outlier)),
    (gdp_outlier == FALSE | is.na(gdp_outlier))
)
###########DATOS FALTANTES################


#Calcular estadísticas de faltantes

resumen_faltantes <- data_paises_sin_outliers %>%
  miss_var_summary() %>%
  filter(n_miss > 0)

# Gráfico de barras de porcentaje faltante

p1 <- gg_miss_var(data_paises_sin_outliers, show_pct = TRUE) +
  labs(title = "Porcentaje de valores faltantes por variable",
       y = "Variables") +
  theme(axis.text.y = element_text(size = 10))

p1


# Patrón de datos faltantes (heatmap)
p2 <- vis_miss(data_paises_sin_outliers %>% 
                 select(inflacion, unemp, gdp),
               cluster = TRUE) +
  labs(title = "Patrón de datos faltantes",
       subtitle = "Negro = Faltante, Gris = Observado")
p2

# Combinar gráficos
p1 / p2

##Para la variable gdp el porcentaje de faltantes es menor al 5%
## se usa imputación simple

data_paises_imp1 <- data_paises_sin_outliers %>% mutate(gdp = ifelse(is.na(gdp), mean(gdp, na.rm = TRUE), gdp))


#### Para inflation y unemployment realizamos el test de Little para ver si los datos son MCAR

datos_para_test <- data_paises_sin_outliers %>% select(inflacion,unemp)

test_mcar <- naniar::mcar_test(datos_para_test)
                        
# Crear dataframe con resultados
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

###Como los datos no son MCAR hacemos imputación múltiple

vars_mice <- data_paises_imp1 %>% select(inflacion,unemp)
# Configurar y ejecutar MICE
mice_imp <- mice(vars_mice,
                 m = 5,            # 5 imputaciones
                 method = 'pmm',   # Predictive mean matching
                 seed = 2025,
                 printFlag = FALSE)

# Ver métodos utilizados
print(mice_imp$method)

datos_imp_mice <- complete(mice_imp, 1)

data_paises_completo <- data_paises_imp1 %>%
  mutate(
    inflacion = datos_imp_mice$inflacion,
    unemp = datos_imp_mice$unemp
  ) %>% select(-inflacion_outlier,-gdp_outlier,-unemp_outlier)


########Guardamos los datos con los valores imputados

write.csv(data_paises_completo,
          file.path(data_row, "data_paises_completos.csv"),
          row.names = FALSE)
