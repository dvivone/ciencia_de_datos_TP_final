path_data_paises<-file.path(data_row,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

data_paises_bloxpot<-data_paises_completos%>%
  pivot_longer(cols = c(gdp,inflacion,unemp),
               names_to = "variable")


data_paises_bloxpot$variable <- factor(data_paises_bloxpot$variable,
                                      levels = c("inflacion", "gdp", "unemp"))

plot_1 <- ggplot(data_paises_bloxpot) + 
  geom_boxplot(aes(y=value, fill=variable)) +
  facet_wrap(~anio,strip.position = "bottom")+
  scale_y_continuous(labels = percent_format(scale=1), breaks = c(-5,0,5,10,15,20))+
  scale_x_continuous(labels = NULL)+
  scale_fill_manual(values=c(inflacion="#feb686",gdp="#69b3a2",unemp="#cda0c9"))+
  ylab(NULL)+
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA),
    panel.spacing = unit(0.5, "lines")                       
    )+
  geom_text(label = "Dato importante",
    aes(y = value, label = label),
    inherit.aes = FALSE,
    color = "red",
    fontface = "bold"
  )

plot_1


  