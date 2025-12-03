#librerias
library(tidyverse)
library(ggthemes)

#rutas
instub <- 'data/input'
outstub <- 'data/output'



path_data_paises<-file.path(data_processed,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

data_paises_bloxpot<-data_paises_completos%>%
  pivot_longer(cols = c(gdp,inflacion,unemp),
               names_to = "variable")

data_paises_bloxpot<-data_paises_bloxpot%>%
  mutate(variable=factor(levels = c(inflation,gdp,unemp)))

plot_1 <- ggplot(data_paises_bloxpot) + 
  geom_boxplot(aes(y=value, fill=variable))+
  facet_wrap(~anio)+
  scale_y_continuous(labels = percent_format(scale=1))+
  xlab(NULL)+
  scale_fill_manual(values = c("#69b3a2","#feb686","#cda0c9"))


  plot_1

###############################################################

  ################## PLOT SERIE DE TIEMPO###################

##############################################################


####DATOS####
path_data_serie<-file.path(instub,"data_serie.csv")
data_serie <-  read.csv(path_data_serie,
                             header = TRUE)


df_long <- data_serie%>%
  pivot_longer(
    cols = starts_with("promedio_"), # Selecciona las columnas a pivotar: promedio_unemp y promedio_inflacion
    names_to = "Variable",            # Crea una nueva columna llamada 'Variable'
    values_to = "Promedio_Global"     # Mueve los valores a una nueva columna llamada 'Promedio_Global'
  )

#### EL PLOT#####
plot_serie <- ggplot(df_long, aes(x = anio, y = Promedio_Global, color = Variable)) +
  geom_line(linewidth = 1) +  # Usamos el color mapeado en el aes general
  scale_color_manual(
    name = NULL,
    values = c("promedio_inflacion" = "darkolivegreen3", "promedio_unemp" = "hotpink1"),
    labels = c("Inflación", "Desempleo") # Nombres más amigables para la leyenda
  ) +
  labs(
    title = "Evolución de los Promedios Globales",
    x = "",
    y = "Promedio Global (%)",
    caption= "Fuente: World Bank"
  ) +
  theme_few() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )


###Guardar
ruta_serie <- paste0(outstub, "/evolucion_promedios_globales.jpg")
ggsave(
  filename = ruta_serie, 
  plot = plot_serie,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)
