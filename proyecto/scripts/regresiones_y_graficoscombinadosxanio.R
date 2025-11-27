# ============================================================
# GRAFICOS Y REGRESIONES PARA 1994, 2007 y 2019
# ============================================================

#Filtro paises

data_graficos <- data %>%
  filter(!pais %in% regiones_a_excluir) %>%  
  filter(anio %in% c(1993, 1994, 2006, 2007, 2018, 2019)) %>%  
  group_by(pais) %>%
  arrange(pais, anio) %>%
  mutate(
    tasa_unemp = (unemp - lag(unemp)) / lag(unemp) * 100
  ) %>%
  ungroup() %>%
  filter(anio %in% c(1994, 2007, 2019))

# ------------------------------------------------------------
# Eliminamos outliers de inflación (método IQR) para los gráficos y regresiones
# ------------------------------------------------------------
Q1 <- quantile(data_graficos$inflacion, 0.25, na.rm = TRUE)
Q3 <- quantile(data_graficos$inflacion, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR_val
limite_superior <- Q3 + 1.5 * IQR_val

data_graficos <- data_graficos %>%
  filter(!is.na(inflacion)) %>%
  mutate(
    outlier_iqr = inflacion < limite_inferior | inflacion > limite_superior
  ) %>%
  filter(outlier_iqr == FALSE)


# Separar datasets por año
datos_1994 <- data_graficos %>% filter(anio == 1994)
datos_2007 <- data_graficos %>% filter(anio == 2007)
datos_2019 <- data_graficos %>% filter(anio == 2019)

nrow(datos_1994); nrow(datos_2007); nrow(datos_2019)

# --------REGRESIONES: inflación ~ variación del desempleo + gdp-------

# Nos quedamos con observaciones completas
base_reg <- data_graficos %>%
  filter(
    !is.na(tasa_unemp),
    !is.na(inflacion),
    !is.na(gdp)
  )

# Separar por año para las regresiones
datos_1994_reg <- base_reg %>% filter(anio == 1994)
datos_2007_reg <- base_reg %>% filter(anio == 2007)
datos_2019_reg <- base_reg %>% filter(anio == 2019)

nrow(datos_1994_reg); nrow(datos_2007_reg); nrow(datos_2019_reg)

# Modelo 1994
modelo_1994 <- lm(inflacion ~ tasa_unemp + gdp, data = datos_1994_reg)
summary(modelo_1994)

# Modelo 2007
modelo_2007 <- lm(inflacion ~ tasa_unemp + gdp, data = datos_2007_reg)
summary(modelo_2007)

# Modelo 2019
modelo_2019 <- lm(inflacion ~ tasa_unemp + gdp, data = datos_2019_reg)
summary(modelo_2019)


# ------------------------------------------------------------
# GRAFICOS COMBINADOS POR AÑO (1994, 2007, 2019)
# Inflación vs variación del desempleo  +  Inflación vs crecimiento del PBI
# ============================================================

# Usamos la misma base_reg que ya armamos para las regresiones
# (ya tiene fuera de outliers y sin NA en tasa_unemp, inflacion, gdp)

# ---------- 1994 ----------
p1994_unemp <- ggplot(datos_1994_reg, aes(x = tasa_unemp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs variación del desempleo (1994)",
    x = "Variación del desempleo (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p1994_gdp <- ggplot(datos_1994_reg, aes(x = gdp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "Inflación vs crecimiento del PBI (1994)",
    x = "Crecimiento del PBI (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

# Ventana con los dos gráficos de 1994 apilados
p1994_unemp / p1994_gdp


# ---------- 2007 ----------
p2007_unemp <- ggplot(datos_2007_reg, aes(x = tasa_unemp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs variación del desempleo (2007)",
    x = "Variación del desempleo (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2007_gdp <- ggplot(datos_2007_reg, aes(x = gdp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "Inflación vs crecimiento del PBI (2007)",
    x = "Crecimiento del PBI (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2007_unemp / p2007_gdp


# ---------- 2019 ----------
p2019_unemp <- ggplot(datos_2019_reg, aes(x = tasa_unemp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs variación del desempleo (2019)",
    x = "Variación del desempleo (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2019_gdp <- ggplot(datos_2019_reg, aes(x = gdp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "Inflación vs crecimiento del PBI (2019)",
    x = "Crecimiento del PBI (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2019_unemp / p2019_gdp



