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
estadisticas_paises_largo

#Se guardan datos en un nuevo file
path_data_paises<-file.path(data_row,"data_estadisticas_paises.csv")
write.csv(estadisticas_paises_largo,path_data_paises,row.names = FALSE)