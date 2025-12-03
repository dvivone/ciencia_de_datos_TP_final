# ===========================================================
#             REGRESION PARA AÑOS 2011, 2016 y 2022
# ============================================================

# ============================================================
# CARGA DE LIBRERIAS
# ============================================================
library(cowplot)
# ============================================================
# CARGA DE DATOS
# ============================================================

path_data_paises<-file.path(data_row,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

# ============================================================
# SE PIVOTEAN DATOS
# ============================================================

data_paises_bloxpot<-data_paises_completos%>%
  pivot_longer(cols = c(gdp,inflacion,desempleo),
               names_to = "variable")

#Se modifica variable para que sea factor
data_paises_bloxpot$variable <- factor(data_paises_bloxpot$variable,
                                      levels = c("inflacion", "gdp", "desempleo"))

# ============================================================
# SE GENERA GRAFICO
# ============================================================

plot_1 <- ggplot(data_paises_bloxpot) + 
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
       subtitle = "Comparacion de la inflacion, el crecimiento del PBI y el desempleo entre los años 2011, 2016 y 2022")+
 theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA),
    panel.spacing = unit(0.5, "lines")                       
    )

#plot_1_sep <- ggdraw(plot_1) +
 #   draw_line(
  #    x = c(63/100, 63/100), y = c(0, 0.9),
   #   linetype = "dashed",
    #  color = "red",
     # size = 0.8
    #)


plot_1_sep <- plot_1 +
  geom_text(
    data = subset(data_paises_bloxpot, anio == 2022 & variable == "inflacion"),
    aes(x = variable, y = 20, label = "Inflación mas elevada en post-pandemia"),
    color = "red",
    fontface = "bold",
    size = 4,
    inherit.aes = FALSE,
    nudge_x = 0.5
  )

plot_1_sep
  