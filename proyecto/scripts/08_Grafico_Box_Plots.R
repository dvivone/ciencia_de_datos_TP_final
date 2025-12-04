# ===========================================================
#        PLOT -  COMPARACION DE VARIABLES EN 3 PERIODOS
# ============================================================

# ============================================================
# 1 CARGA DE LIBRERIAS
# ============================================================

library(tidyverse)
library(cowplot)

# ============================================================
# 2 CARGA DE DATOS
# ============================================================

path_data_paises<-file.path(data_processed,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

# ============================================================
# 3 SE PIVOTEAN DATOS
# ============================================================

data_paises_bloxpot<-data_paises_completos%>%
  pivot_longer(cols = c(gdp,inflacion,desempleo),
               names_to = "variable")

data_paises_bloxpot %>%
  summarise(n_distinct(pais))

# 3.2 Se modifica variable para que sea factor
data_paises_bloxpot$variable <- factor(data_paises_bloxpot$variable,
                                      levels = c("inflacion", "gdp", "desempleo"))

# ============================================================
# 4 SE GENERA PLOT
# ============================================================

plot_box <- ggplot(data_paises_bloxpot) + 
  geom_boxplot(aes(x=variable,y=value, fill=variable)) +
  facet_wrap(~anio,
             strip.position = "bottom",
             ncol=3)+
  scale_y_continuous(labels = percent_format(scale=1), breaks = c(-5,0,5,10,15,20))+
  scale_x_discrete(labels=NULL)+
  scale_fill_manual(values=c(inflacion="#feb686",gdp="#69b3a2",desempleo="#cda0c9"))+
  ylab(NULL)+
  xlab(NULL)+
  labs(title = "Inflacion, crecimiento del PBI y desempleo",
       subtitle = "Comparacion de las distribiciones de inflacion, crecimiento de PBI y desempleo entre los años 2011, 2016 y 2022. Datos correspondientes a 216 paises",
       caption = "Fuente de datos: World Bank")+
 theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA),
    panel.spacing = unit(0.5, "lines")
    )

plot_box_sep <- plot_box +
  geom_text(
    data = subset(data_paises_bloxpot, anio == 2022 & variable == "inflacion"),
    aes(x = variable, y = 20, label = "Inflación mas elevada en post-pandemia"),
    color = "red",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE,
    nudge_x = 0.5
  )

plot_box_sep


# ============================================================
# 5 GUARDAR PLOT
# ============================================================ 

ruta_boxplot <- paste0(output_figures, "/comparacion_variables.jpg")
ggsave(
  filename = ruta_boxplot, 
  plot = plot_box_sep,
  width = 18,
  height = 10,
  units = "in",
  dpi = 300,
  limitsize = FALSE
)
  