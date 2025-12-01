library(car)

# ============================================================
# GRAFICOS Y REGRESIONES PARA 2011, 2016 y 2022
# ============================================================

path_data_paises<-file.path(data_row,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

glimpse(data_paises_completos)

variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,unemp)

cor(variables_numericas)

# Separar datasets por año
datos_2011 <- data_paises_completos %>% 
  filter(anio == 2011)
datos_2016 <- data_paises_completos %>% 
  filter(anio == 2016)
datos_2022 <- data_paises_completos %>% 
  filter(anio == 2022)

nrow(datos_2011); nrow(datos_2016); nrow(datos_2022)

wilcox.test(datos_2016$inflacion,datos_2022$inflacion,paired=TRUE)

# --------REGRESIONES: inflación ~ variación del desempleo + gdp-------

# Modelo 2011
modelo_2011 <- lm(inflacion ~ unemp + gdp, data = datos_2011)
summary(modelo_2011)

# Modelo 2007
modelo_2016 <- lm(inflacion ~ unemp + gdp, data = datos_2016)
summary(modelo_2016)

# Modelo 2022
modelo_2022 <- lm(inflacion ~ unemp + gdp, data = datos_2022)
summary(modelo_2022)

vif(modelo_2011)
bptest(modelo_2011)
plot(modelo_2011)

# ------------------------------------------------------------
# GRAFICOS COMBINADOS POR AÑO (2011, 2007, 2022)
# Inflación vs variación del desempleo  +  Inflación vs crecimiento del PBI
# ============================================================

# Usamos la misma base_reg que ya armamos para las regresiones
# (ya tiene fuera de outliers y sin NA en tasa_unemp, inflacion, gdp)

# ---------- 2011 ----------
p2011_unemp <- ggplot(datos_2011, aes(x = unemp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs variación del desempleo (2011)",
    x = "Variación del desempleo (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2011_gdp <- ggplot(datos_2011, aes(x = gdp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "Inflación vs crecimiento del PBI (2011)",
    x = "Crecimiento del PBI (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

# Ventana con los dos gráficos de 2011 apilados
p2011_unemp / p2011_gdp


# ---------- 2016 ----------
p2016_unemp <- ggplot(datos_2016, aes(x = unemp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs variación del desempleo (2016)",
    x = "Variación del desempleo (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2016_gdp <- ggplot(datos_2016, aes(x = gdp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "Inflación vs crecimiento del PBI (2016)",
    x = "Crecimiento del PBI (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2016_unemp / p2016_gdp


# ---------- 2022 ----------
p2022_unemp <- ggplot(datos_2022, aes(x = unemp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs variación del desempleo (2022)",
    x = "Variación del desempleo (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2022_gdp <- ggplot(datos_2022, aes(x = gdp, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(
    title = "Inflación vs crecimiento del PBI (2022)",
    x = "Crecimiento del PBI (%)",
    y = "Inflación (%)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 11)
  )

p2022_unemp / p2022_gdp



