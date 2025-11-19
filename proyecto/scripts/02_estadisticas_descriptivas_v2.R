# 1. Definir las estadísticas a calcular
calcular_estadisticas <- list(
  media = ~mean(., na.rm = TRUE),
  mediana = ~median(., na.rm = TRUE),
  sd = ~sd(., na.rm = TRUE),
  min = ~min(., na.rm = TRUE),
  max = ~max(., na.rm = TRUE),
  q25 = ~quantile(., 0.25, na.rm = TRUE),
  q75 = ~quantile(., 0.75, na.rm = TRUE)
)

# 2. Generar el resumen agrupado en formato ANCHO
resumen_descriptivo_ancho <- data_paises %>%
  select(where(is.numeric),-anio,) %>%
  summarise(
    across(
      .cols = everything(), 
      .fns = calcular_estadisticas,
      .names = "{.col}_{.fn}" # Nombrar la columna como variable_estadistica
    ),
    .groups = "drop" # Quitar la agrupación después de resumir
  )

# 2. Generar el resumen agrupado en formato LARGO
resumen_descriptivo_largo<-resumen_descriptivo_ancho %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "valor")

# 3. Mostrar el resultado final
print("📊 Resumen Descriptivo en formato largo")
resumen_descriptivo_largo
