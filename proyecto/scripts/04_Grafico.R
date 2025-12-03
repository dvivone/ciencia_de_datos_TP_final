# ===========================================================
#             REGRESION PARA AÑOS 2011, 2016 y 2022
# ============================================================

# ============================================================
# CARGA DE LIBRERIAS
# ============================================================

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
  geom_boxplot(aes(y=value, fill=variable)) +
  facet_wrap(~anio,
             strip.position = "bottom")+
  scale_y_continuous(labels = percent_format(scale=1), breaks = c(-5,0,5,10,15,20))+
  scale_x_continuous(labels=NULL)+
  scale_fill_manual(values=c(inflacion="#feb686",gdp="#69b3a2",desempleo="#cda0c9"))+
  ylab(NULL)+
  labs(title = "Inflacion, crecimiento del PBI y desempleo",
       subtitle = "Comparacion de la inflacion, el crecimiento del PBI y el desempleo entre los años 2011, 2016 y 2022")+
 theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA),
    panel.spacing = unit(0.5, "lines")                       
    )

plot_1


  