# ===========================================================
#           TEST PAREADO PARA COMPARAR 2016 Y 2022
# ============================================================

# ============================================================
# 1 CARGA DE LIBRERIAS
# ============================================================

library(tidyverse)

# ============================================================
# 2 CARGA DE DATOS
# ============================================================

path_data_paises<-file.path(data_processed,"data_paises_completos.csv")
data_paises_completos<-read.csv(path_data_paises)

# ============================================================
# 3 FILTRADO DE PAISES
# ============================================================

# 3.1 Se filtran paises que estan presentes en 2016 y 2022
datos_paises_tres_anios<-data_paises_completos %>%
  filter(anio %in% c(2016,2022))%>%
  group_by(pais)%>%
  summarise(cant_anios=n())%>%
  filter(cant_anios==2)%>%
  select(pais)

datos_paises_test<- data_paises_completos%>%
  filter(pais %in% datos_paises_tres_anios$pais)

# 3.2 Se crean subsets para cada año
datos_paises_test_2016<-datos_paises_test %>%
  filter(anio == 2016)
datos_paises_test_2022<-datos_paises_test %>%
  filter(anio == 2022)

# ============================================================
# 4 TEST PAREADO
# ============================================================

# H0: No hay diferencia (μ_diferencia = 0)
# H1: Hay aumento (μ_diferencia > 0)

test_pareado <- t.test(datos_paises_test_2022$inflacion, datos_paises_test_2016$inflacion, paired = TRUE, alternative = "greater")
print(test_pareado)