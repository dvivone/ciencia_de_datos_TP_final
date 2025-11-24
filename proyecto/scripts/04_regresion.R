path_data_paises<-file.path(data_row,"data_paises_completos.csv")
data_paises_regresion<-read.csv(path_data_paises)

data_paises_regresion_2010<-data_paises_regresion %>%
  filter(is_outlier_iqr==0,anio==2010)

regresion_paises_2010<-lm(data=data_paises_regresion, inflacion~gdp+unemp)
summary(regresion_paises_2010)

data_paises_regresion_2017<-data_paises_regresion %>%
  filter(is_outlier_iqr==0,anio==2017)

regresion_paises_2017<-lm(data=data_paises_regresion, inflacion~gdp+unemp)
summary(regresion_paises_2017)

data_paises_regresion_2023<-data_paises_regresion %>%
  filter(is_outlier_iqr==0,anio==2023)

regresion_paises_2023<-lm(data=data_paises_regresion, inflacion~gdp+unemp)
summary(regresion_paises_2023)