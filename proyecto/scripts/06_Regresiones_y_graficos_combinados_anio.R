# ===========================================================
#             REGRESION PARA AÑOS 2011, 2016 y 2022
# ============================================================

# ============================================================
# CARGA DE LIBRERIAS
# ============================================================

library(car)
library(lmtest)
library(sandwich)

# ============================================================
# CARGA DE DATOS
# ============================================================

path_data_paises<-file.path(data_row,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

# ============================================================
# REGRESION 2011    
# ============================================================

#Separar dataset para año 2011
datos_2011 <- data_paises_completos %>% 
  filter(anio == 2011)

# Calculamos la matriz de correlaciones para descartar Multicolinealidad
variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,desempleo)
cor(variables_numericas)

# Generamos los plots para verificar la dispersion
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

#Generamos el modelo para el año 2011
modelo_2011 <- lm(inflacion ~ desempleo + gdp, data = datos_2011)
summary(modelo_2011)

#Residuos vs valores ajustados
plot(modelo_2011, which =1, pch=19)

#Verificamos multicolinealidad
vif(modelo_2011)

#Verificamos Heteroscedasticidad
bptest(modelo)

#Errores robustos usando sandwich
coeftest(modelo_2011, vcov = vcovHC(modelo_2011, type ="HC1"))

#Verificar especificacion del modelo
resettest(modelo_2011, power =2:3)


# ============================================================
# REGRESION 2016    
# ============================================================

#Separar dataset para año 2016
datos_2016 <- data_paises_completos %>% 
  filter(anio == 2016)

# Calculamos la matriz de correlaciones para descartar Multicolinealidad
variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,desempleo)
cor(variables_numericas)

# Generamos los plots para verificar la dispersion
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

#Generamos el modelo para el año 2016
modelo_2016 <- lm(inflacion ~ desempleo + gdp, data = datos_2016)
summary(modelo_2016)

#Residuos vs valores ajustados
plot(modelo_2016, which =1, pch=19)

#Verificamos multicolinealidad
vif(modelo_2016)

#Verificamos Heteroscedasticidad
bptest(modelo)

#Errores robustos usando sandwich
coeftest(modelo_2016, vcov = vcovHC(modelo_2016, type ="HC1"))

#Verificar especificacion del modelo
resettest(modelo_2016, power =2:3)

# ============================================================
# REGRESION 2022    
# ============================================================

#Separar dataset para año 2022
datos_2022 <- data_paises_completos %>% 
  filter(anio == 2022)

# Calculamos la matriz de correlaciones para descartar Multicolinealidad
variables_numericas<-data_paises_completos%>%
  select(gdp,inflacion,desempleo)
cor(variables_numericas)

# Generamos los plots para verificar la dispersion
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

#Generamos el modelo para el año 2022
modelo_2022 <- lm(inflacion ~ desempleo + gdp, data = datos_2022)
summary(modelo_2022)

#Residuos vs valores ajustados
plot(modelo_2022, which =1, pch=19)

#Verificamos multicolinealidad
vif(modelo_2022)

#Verificamos Heteroscedasticidad
bptest(modelo)

#Errores robustos usando sandwich
coeftest(modelo_2022, vcov = vcovHC(modelo_2022, type ="HC1"))

#Verificar especificacion del modelo
resettest(modelo_2022, power =2:3)
