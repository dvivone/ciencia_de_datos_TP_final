library(naniar)
library(patchwork)
library(kableExtra)
library(mice)

path_data_paises<-file.path(data_row,"data_paises.csv")
data_paises_estadistica<-read.csv(path_data_paises)

Detección de outliers

variables_numericas <- c("inflacion", "gdp", "unemp")

# 4. DETECCIÓN DE OUTLIERS ####

cat("\n\n=== 4. DETECCIÓN DE OUTLIERS ===\n")

# A) Método IQR (Rango Intercuartílico)
Q1 <- quantile(datos_retail$ventas_nominal, 0.25, na.rm = TRUE)
Q3 <- quantile(datos_retail$ventas_nominal, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

datos_retail <- datos_retail %>%
  mutate(
    outlier_iqr = !is.na(ventas_nominal) &
      (ventas_nominal < limite_inferior | ventas_nominal > limite_superior)
  )

cat("Método IQR:\n")
cat("  Q1:", round(Q1), "| Q3:", round(Q3), "| IQR:", round(IQR_val), "\n")
cat("  Límites: [", round(limite_inferior), ",", round(limite_superior), "]\n")
cat("  Outliers detectados:", sum(datos_retail$outlier_iqr, na.rm = TRUE), "\n")

# B) Método Z-score
datos_retail <- datos_retail %>%
  mutate(
    z_ventas = scale(ventas_nominal)[,1],
    outlier_zscore = !is.na(z_ventas) & abs(z_ventas) > 3
  )

cat("\nMétodo Z-score (|Z| > 3):\n")
cat("  Outliers detectados:", sum(datos_retail$outlier_zscore, na.rm = TRUE), "\n")

# Visualización de outliers
datos_retail %>%
  filter(!is.na(ventas_nominal)) %>%
  ggplot(aes(x = mes, y = ventas_nominal, color = outlier_iqr)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = c(limite_inferior, limite_superior),
             linetype = "dashed", color = "red") +
  labs(title = "Detección de Outliers - Método IQR",
       subtitle = "Ventas mensuales por sucursal",
       x = "Mes", y = "Ventas Nominales",
       color = "Outlier") +
  scale_color_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"))

# 5. TRATAMIENTO DE OUTLIERS ####

cat("\n\n=== 5. TRATAMIENTO DE OUTLIERS ===\n")

# A) Winsorización (recomendado)
datos_retail <- datos_retail %>%
  dplyr::mutate(
    ventas_winsorized = Winsorize(ventas_nominal, quantile(ventas_nominal,probs = c(0.01, 0.99), na.rm = TRUE))
  )

cat("Winsorización (percentiles 1% y 99%):\n")
cat("  Ventas originales - Rango: [", round(min(datos_retail$ventas_nominal, na.rm = TRUE)), ",",
    round(max(datos_retail$ventas_nominal, na.rm = TRUE)), "]\n")
cat("  Ventas winsorized - Rango: [", round(min(datos_retail$ventas_winsorized, na.rm = TRUE)), ",",
    round(max(datos_retail$ventas_winsorized, na.rm = TRUE)), "]\n")

# B) Crear dummy para outliers (mantener información)
datos_retail <- datos_retail %>%
  mutate(
    dummy_outlier = as.numeric(outlier_iqr)
  )

cat("\nDummy para outliers creada (para usar en regresiones)\n")

# Veamos como cambia el promedio luego de la winsorizacion
t1 <- datos_retail %>% 
  group_by(sucursal_id) %>% 
  summarize(promedio_ventas = mean(ventas_nominal,na.rm=T),
            promedio_ventas_win = mean(ventas_winsorized,na.rm=T),
            desvio_ventas = sd(ventas_nominal,na.rm=T),
            desvio_ventas_win = sd(ventas_winsorized,na.rm=T),
            coeficiente_dispersion = sd(ventas_nominal,na.rm=T) / mean(ventas_nominal,na.rm=T),
            coeficiente_dispersion_win = sd(ventas_winsorized,na.rm=T) / mean(ventas_winsorized,na.rm=T))
View(t1)

# Veamos que hay sucursales que no se vieron afectadas. 
# ¿Deberiamos hacer el recorte por sucursal o en general? Depende



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
