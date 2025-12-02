path_data_paises<-file.path(data_row,"data_paises_completos.csv")
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


  