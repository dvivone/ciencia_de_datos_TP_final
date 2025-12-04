# ===========================================================
#             REGRESION PARA AÑOS 2011, 2016 y 2022
# ============================================================

# ============================================================
# 1 CARGA DE LIBRERIAS
# ============================================================

library(tidyverse)
library(car)
library(lmtest)
library(sandwich)

# ============================================================
# 2 CARGA DE DATOS
# ============================================================

path_data_paises<-file.path(data_processed,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

# ============================================================
# 3.1 REGRESION 2011    
# ============================================================

# 3.1.1 Separar dataset para año 2011
datos_2011 <- data_paises_completos %>% 
  filter(anio == 2011)

# 3.1.2 Calculamos la matriz de correlaciones para descartar Multicolinealidad
variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,desempleo)
cor(variables_numericas)

# 3.1.3 Generamos los plots para verificar la dispersion
p2011_desempleo <- ggplot(datos_2011, aes(x = desempleo, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs Desempleo (2011)",
    x = "Desempleo (%)",
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

p2011_desempleo / p2011_gdp

# 3.1.4 Generamos el modelo para el año 2011
modelo_2011 <- lm(inflacion ~ desempleo + gdp, data = datos_2011)
summary(modelo_2011)

# 3.1.5 Residuos vs valores ajustados
plot(modelo_2011, which =1, pch=19)

# 3.1.6 Verificamos multicolinealidad
vif(modelo_2011)

# 3.1.7 Verificamos Heteroscedasticidad
bptest(modelo_2011)

# 3.1.8 Errores robustos usando sandwich
coeftest(modelo_2011, vcov = vcovHC(modelo_2011, type ="HC1"))

# 3.1.9 Verificar especificacion del modelo
resettest(modelo_2011, power =2:3)


# ============================================================
# 3.2 REGRESION 2016    
# ============================================================

# 3.2.1 Separar dataset para año 2016
datos_2016 <- data_paises_completos %>% 
  filter(anio == 2016)

# 3.2.2 Calculamos la matriz de correlaciones para descartar Multicolinealidad
variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,desempleo)
cor(variables_numericas)

# 3.2.3 Generamos los plots para verificar la dispersion
p2016_desempleo <- ggplot(datos_2016, aes(x = desempleo, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs Desempleo (2016)",
    x = "Desempleo (%)",
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

p2016_desempleo / p2016_gdp

# 3.2.4 Generamos el modelo para el año 2016
modelo_2016 <- lm(inflacion ~ desempleo + gdp, data = datos_2016)
summary(modelo_2016)

# 3.2.5 Residuos vs valores ajustados
plot(modelo_2016, which =1, pch=19)

# 3.2.6 Verificamos multicolinealidad
vif(modelo_2016)

# 3.2.7 Verificamos Heteroscedasticidad
bptest(modelo_2016)

# 3.2.8 Errores robustos usando sandwich
coeftest(modelo_2016, vcov = vcovHC(modelo_2016, type ="HC1"))

# 3.2.9 Verificar especificacion del modelo
resettest(modelo_2016, power =2:3)

# ============================================================
# 3.3 REGRESION 2022    
# ============================================================

# 3.3.1 Separar dataset para año 2022
datos_2022 <- data_paises_completos %>% 
  filter(anio == 2022)

# 3.3.2 Calculamos la matriz de correlaciones para descartar Multicolinealidad
variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,desempleo)
cor(variables_numericas)

# 3.3.3 Generamos los plots para verificar la dispersion
p2022_desempleo <- ggplot(datos_2022, aes(x = desempleo, y = inflacion)) +
  geom_point(alpha = 0.8, size = 2.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Inflación vs Desempleo (2022)",
    x = "Desempleo (%)",
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

p2022_desempleo / p2022_gdp

# 3.3.4 Generamos el modelo para el año 2022
modelo_2022 <- lm(inflacion ~ desempleo + gdp, data = datos_2022)
summary(modelo_2022)

# 3.3.5 Residuos vs valores ajustados
plot(modelo_2022, which =1, pch=19)

# 3.3.6 Verificamos multicolinealidad
vif(modelo_2022)

# 3.3.7 Verificamos Heteroscedasticidad
bptest(modelo_2022)

# 3.3.8 Errores robustos usando sandwich
coeftest(modelo_2022, vcov = vcovHC(modelo_2022, type ="HC1"))

# 3.3.9 Verificar especificacion del modelo
resettest(modelo_2022, power =2:3)
