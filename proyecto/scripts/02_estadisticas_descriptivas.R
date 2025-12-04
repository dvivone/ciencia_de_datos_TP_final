# ===========================================================
#            ANALISIS EXPLORATORIO DE DATOS
# ============================================================

# ============================================================
# 1 CARGA DE LIBRERIAS
# ============================================================

library(tidyverse)
library(patchwork)

# ============================================================
# 2 CALCULO ESTADISTICAS
# ============================================================

# 2.1 Definir las estadísticas a calcular
path_data_paises<-file.path(data_clean,
                            "data_paises.csv")
data_paises_estadistica<-read.csv(path_data_paises)

# 2.2 pivotear a lo largo
data_paises_estadistica_largo<-data_paises_estadistica%>%
  pivot_longer(cols = c(gdp,inflacion,desempleo),
               names_to = "variable")

# 2.3 Calcular estadisticas
estadisticas_paises_largo<-data_paises_estadistica_largo%>%
  group_by(anio,variable)%>%
  summarise(
  media = mean(value, na.rm = TRUE),
  mediana = median(value, na.rm = TRUE),
  sd = sd(value, na.rm = TRUE),
  min = min(value, na.rm = TRUE),
  max = max(value, na.rm = TRUE),
  q25 = quantile(value, 0.25,na.rm = TRUE),
  q75 = quantile(value, 0.75,na.rm = TRUE),
  .groups = "drop"
)

# 2.4 Mostrar el resultado final
print("📊 Resumen Descriptivo en formato largo")
estadisticas_paises_largo<- estadisticas_paises_largo%>%
  arrange(variable,anio)
estadisticas_paises_largo

# ============================================================
# 3 HISTOGRAMAS PARA VERIFICAR DISTRIBUCION DE VARIABLES
# ============================================================

# 3.1 Histograma de inflación
histo_infla<-ggplot(data_paises_estadistica, aes(x = inflacion, fill = anio)) +
  geom_histogram(bins = 30, alpha = 0.6,fill="#feb686", color="#e9ecef") +
  scale_x_continuous(labels = percent_format(scale=1), limits = c(0, 30)) +
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "Distribucion de las variables",
    x = "Inflacion",
    y = "Frecuencia"
  ) +
  theme_minimal()

# 3.2 Histograma de gdp
histo_gdp<-ggplot(data_paises_estadistica, aes(x = gdp, fill = anio)) +
  geom_histogram(bins = 30, alpha = 0.6,fill="#69b3a2", color="#e9ecef") +
  scale_x_continuous(labels = percent_format(scale=1), limits = c(0, 30)) +
  scale_y_continuous(limits = c(0,100))+
  ylab(NULL)+
  labs(
      x = "gdp"
  ) +
  theme_minimal()

# 3.3 Histograma de desempleo
histo_desempleo<-ggplot(data_paises_estadistica, aes(x = desempleo, fill = anio)) +
  geom_histogram(bins = 30, alpha = 0.6,fill="#cda0c9", color="#e9ecef") +
  scale_x_continuous(labels = percent_format(scale=1), limits = c(0, 30)) +
  scale_y_continuous(limits = c(0,100))+
  ylab(NULL)+
  labs(
      x = "desempleo"
  ) +
  theme_minimal()

# 3.4 Se muestran los tres histogramas juntos
histo_infla | histo_gdp | histo_desempleo

# ============================================================
# 5 SE GUARDAN ESTADISTICAS EN NUEVO FILE
# ============================================================

path_data_paises<-file.path(data_processed,"data_estadisticas_paises.csv")
write.csv(estadisticas_paises_largo,path_data_paises,row.names = FALSE)