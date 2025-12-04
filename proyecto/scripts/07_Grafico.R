# ===========================================================
#               PLOT SERIE DE TIEMPO
# ============================================================

# ============================================================
# 1 CARGA DE LIBRERIAS
# ============================================================
library(tidyverse)
library(ggthemes)

# ============================================================
# 2 CARGA DE DATOS
# ============================================================

path_data_serie<-file.path(data_clean,"data_serie.csv")
data_serie <-  read.csv(path_data_serie,
                             header = TRUE)

# ============================================================
# 3 PIVOT DE DATOS
# ============================================================

df_long <- data_serie%>%
  pivot_longer(
    cols = starts_with("promedio_"), # Selecciona las columnas a pivotar: promedio_unemp y promedio_inflacion
    names_to = "Variable",            # Crea una nueva columna llamada 'Variable'
    values_to = "Promedio_Global"     # Mueve los valores a una nueva columna llamada 'Promedio_Global'
  )

# ============================================================
# 4 GENERAR PLOT
# ============================================================

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


# ============================================================
# 5 GUARDAR PLOT
# ============================================================ 

ruta_serie <- paste0(output_figures, "/evolucion_promedios_globales.jpg")
ggsave(
  filename = ruta_serie, 
  plot = plot_serie,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)
