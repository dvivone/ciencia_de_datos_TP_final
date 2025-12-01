# 1. Definir las estadísticas a calcular

path_data_paises<-file.path(data_row,"data_paises.csv")
data_paises_estadistica<-read.csv(path_data_paises)

#pivotear a lo largo
data_paises_estadistica_largo<-data_paises_estadistica%>%
  pivot_longer(cols = c(gdp,inflacion,unemp),
               names_to = "variable")

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

# 3. Mostrar el resultado final
print("📊 Resumen Descriptivo en formato largo")
estadisticas_paises_largo<- estadisticas_paises_largo%>%
  arrange(variable,anio)
estadisticas_paises_largo

shapiro.test(data_paises_estadistica$gdp)
qqnorm(data_paises_estadistica$inflacion)

# Histogramas por género
ggplot(data_paises_estadistica, aes(x = gdp, fill = anio)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_x_continuous(labels = scales::comma, limits = c(0, 20)) +
  labs(
    title = "Distribución de ingresos por género - EPH 1T 2025",
    x = "Ingreso de la ocupación principal",
    y = "Frecuencia",
    fill = "Género"
  ) +
  theme_minimal() +
  facet_wrap(~anio, ncol = 1)

# Boxplots comparativos
ggplot(data_paises_estadistica, aes(x = anio, y=gdp, fill = gdp)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 4, fill = "red") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 15)) +
  labs(
    title = "Comparación de ingresos por género",
    subtitle = "Rombo rojo = media | Línea horizontal = mediana",
    y = "Ingreso",
    x = "Género"
  ) +
  theme_minimal() +
  theme(legend.position = "none")+
  facet_wrap(~anio, ncol = 1)
  
#Se guardan datos en un nuevo file
path_data_paises<-file.path(data_row,"data_estadisticas_paises.csv")
write.csv(estadisticas_paises_largo,path_data_paises,row.names = FALSE)