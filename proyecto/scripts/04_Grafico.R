path_data_paises<-file.path(data_row,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

data_paises_bloxpot<-data_paises_completos%>%
  pivot_longer(cols = c(gdp,inflacion,unemp),
               names_to = "variable")

plot_1 <- ggplot(data_paises_bloxpot) + 
  geom_boxplot(aes(y=value, fill=variable)) +
  facet_wrap(~anio)+
  scale_y_continuous(labels = percent_format(scale=1))+
  scale_x_continuous(labels = NULL)
plot_1

p2 <- ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot() +
  facet_wrap(~variety, scale="free")+
  