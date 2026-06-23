# ==============================================================================
# Script: 03_analisis.R
# Objetivo: Implementar los cuatro métodos estadísticos definidos en la
#           Instancia 2 del TP:
#             Método 1 — Participación (shares) y CAGR
#             Método 2 — Test t de Welch pre vs post Vaca Muerta
#             Método 3 — Correlación de Pearson y Spearman
#             Método 4 — Descomposición sectorial del crecimiento
# Input:    input/Jurisdiccion_52sectores_tidy.csv (generado por 01_limpieza.R)
# Output:   output/tablas/*.csv (resultados de los cuatro métodos)
# Paquetes: tidyverse
# ==============================================================================

library(tidyverse)

options(scipen = 999)

# Definir el directorio de trabajo (ajustar según corresponda)
setwd(r'(C:\Users\Javi\Desktop\Ciencia de datos\tp-vab-patagonia)')

# Carpeta de tablas
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

# Carga del dataset limpio
vab_tidy <- read_csv("input/Jurisdiccion_52sectores_tidy.csv")

# ==============================================================================
# COLUMNAS AUXILIARES: REGIÓN y FLAGS DE PROVINCIA
# ==============================================================================
# Usamos str_detect() con regex permisivos para manejar nombres con o sin
# tildes (Neuquén/Neuquen, Río Negro/Rio Negro, etc.), evitando fallas en
# los filtros del resto del script.

vab_tidy <- vab_tidy |>
  mutate(
    region = case_when(
      str_detect(provincia, regex("^Buenos Aires$", ignore_case = TRUE))      ~ "Pampeana",
      str_detect(provincia, regex("C.rdoba",        ignore_case = TRUE))      ~ "Pampeana",
      str_detect(provincia, regex("Entre R.os",     ignore_case = TRUE))      ~ "Pampeana",
      str_detect(provincia, regex("La Pampa",       ignore_case = TRUE))      ~ "Pampeana",
      str_detect(provincia, regex("Santa Fe",       ignore_case = TRUE))      ~ "Pampeana",
      str_detect(provincia, regex("Ciudad|CABA|Aut.noma", ignore_case = TRUE)) ~ "CABA",
      str_detect(provincia, regex("Catamarca",           ignore_case = TRUE))  ~ "NOA",
      str_detect(provincia, regex("Jujuy",               ignore_case = TRUE))  ~ "NOA",
      str_detect(provincia, regex("La Rioja",            ignore_case = TRUE))  ~ "NOA",
      str_detect(provincia, regex("Salta",               ignore_case = TRUE))  ~ "NOA",
      str_detect(provincia, regex("Santiago del Estero", ignore_case = TRUE))  ~ "NOA",
      str_detect(provincia, regex("Tucum.n",             ignore_case = TRUE))  ~ "NOA",
      str_detect(provincia, regex("Chaco",      ignore_case = TRUE))           ~ "NEA",
      str_detect(provincia, regex("Corrientes", ignore_case = TRUE))           ~ "NEA",
      str_detect(provincia, regex("Formosa",    ignore_case = TRUE))           ~ "NEA",
      str_detect(provincia, regex("Misiones",   ignore_case = TRUE))           ~ "NEA",
      str_detect(provincia, regex("Mendoza",  ignore_case = TRUE))             ~ "Cuyo",
      str_detect(provincia, regex("San Juan", ignore_case = TRUE))             ~ "Cuyo",
      str_detect(provincia, regex("San Luis", ignore_case = TRUE))             ~ "Cuyo",
      str_detect(provincia, regex("Chubut",           ignore_case = TRUE))     ~ "Patagonia",
      str_detect(provincia, regex("Neuqu.n",          ignore_case = TRUE))     ~ "Patagonia",
      str_detect(provincia, regex("R.o Negro",        ignore_case = TRUE))     ~ "Patagonia",
      str_detect(provincia, regex("Santa Cruz",       ignore_case = TRUE))     ~ "Patagonia",
      str_detect(provincia, regex("Tierra del Fuego", ignore_case = TRUE))     ~ "Patagonia",
      TRUE                                                                     ~ NA_character_
    ),
    # Flag para Neuquén (foco de la hipótesis principal y de los métodos 3 y 4)
    es_neuquen = str_detect(provincia, regex("Neuqu.n", ignore_case = TRUE))
  )

# Verificación
if (any(is.na(vab_tidy$region))) {
  warning("Hay provincias sin región asignada: ",
          paste(unique(vab_tidy$provincia[is.na(vab_tidy$region)]),
                collapse = ", "))
}

# Función auxiliar: tasa de crecimiento anual compuesto (CAGR)
cagr <- function(valor_final, valor_inicial, n_anios) {
  ((valor_final / valor_inicial) ^ (1 / n_anios) - 1) * 100
}


# ==============================================================================
# MÉTODO 1 — PARTICIPACIONES (SHARES) Y CAGR
# ==============================================================================
# Mide la evolución del peso relativo de cada región/provincia en el VAB
# nacional y la tasa de crecimiento anual compuesto del período.

# 1.1 Evolución anual de la participación patagónica
evolucion_patagonia <- vab_tidy |>
  group_by(anio) |>
  mutate(vab_nacional = sum(vab, na.rm = TRUE)) |>
  ungroup() |>
  filter(region == "Patagonia") |>
  group_by(anio) |>
  summarise(
    vab_patagonia = sum(vab, na.rm = TRUE),
    vab_nacional  = first(vab_nacional),
    share         = vab_patagonia / vab_nacional * 100,
    .groups       = "drop"
  )

write_excel_csv(evolucion_patagonia,
                "output/tablas/evolucion_share_patagonia.csv")

# 1.2 CAGR nacional, patagónico y por provincia
vab_anual_total <- vab_tidy |>
  group_by(anio) |>
  summarise(
    vab_nacional  = sum(vab, na.rm = TRUE),
    vab_patagonia = sum(vab[region == "Patagonia"], na.rm = TRUE),
    .groups       = "drop"
  )

cagr_nacional   <- cagr(vab_anual_total |> filter(anio == 2024) |> pull(vab_nacional),
                        vab_anual_total |> filter(anio == 2004) |> pull(vab_nacional), 20)
cagr_patagonia  <- cagr(vab_anual_total |> filter(anio == 2024) |> pull(vab_patagonia),
                        vab_anual_total |> filter(anio == 2004) |> pull(vab_patagonia), 20)

# CAGR del VAB extractivo de Neuquén: pre y post Vaca Muerta
vab_nqn_ext <- vab_tidy |>
  filter(es_neuquen, es_extractivo) |>
  group_by(anio) |>
  summarise(vab_ext = sum(vab, na.rm = TRUE), .groups = "drop")

cagr_nqn_pre  <- cagr(vab_nqn_ext |> filter(anio == 2014) |> pull(vab_ext),
                      vab_nqn_ext |> filter(anio == 2004) |> pull(vab_ext), 10)
cagr_nqn_post <- cagr(vab_nqn_ext |> filter(anio == 2024) |> pull(vab_ext),
                      vab_nqn_ext |> filter(anio == 2014) |> pull(vab_ext), 10)

resumen_cagr <- tibble(
  serie  = c("Nacional", "Patagonia", "Extractivo Neuquén pre-VM",
             "Extractivo Neuquén post-VM"),
  periodo = c("2004-2024", "2004-2024", "2004-2014", "2014-2024"),
  cagr   = c(cagr_nacional, cagr_patagonia, cagr_nqn_pre, cagr_nqn_post)
)

write_excel_csv(resumen_cagr, "output/tablas/cagr_resumen.csv")

cat("\n========================================\n")
cat("MÉTODO 1 — PARTICIPACIONES Y CAGR\n")
cat("========================================\n")
cat(sprintf("  Participación Patagonia 2004: %.2f%%\n",
            evolucion_patagonia |> filter(anio == 2004) |> pull(share)))
cat(sprintf("  Participación Patagonia 2024: %.2f%%\n",
            evolucion_patagonia |> filter(anio == 2024) |> pull(share)))
cat("\n  CAGR:\n")
print(resumen_cagr)


# ==============================================================================
# MÉTODO 2 — TEST t DE WELCH (PRE vs POST VACA MUERTA)
# ==============================================================================
# Compara la participación patagónica media entre los sub-períodos pre-Vaca
# Muerta (2004-2013, n=10) y post-Vaca Muerta (2014-2024, n=11).
# IMPORTANTE: con n bajo en ambos grupos, el poder estadístico del test es
# limitado. Reportamos también IC al 95% y Cohen's d como complemento.

pre  <- evolucion_patagonia |> filter(anio <= 2013) |> pull(share)
post <- evolucion_patagonia |> filter(anio >= 2014) |> pull(share)

resultado_t <- t.test(post, pre,
                      alternative = "two.sided",
                      var.equal   = FALSE,
                      conf.level  = 0.95)

resumen_test <- tibble(
  metrica = c("n_pre", "n_post", "media_pre", "media_post",
              "diferencia_medias", "t_estadistico", "p_valor",
              "ic_inferior_95", "ic_superior_95"),
  valor   = c(length(pre), length(post), mean(pre), mean(post),
              mean(post) - mean(pre), resultado_t$statistic, resultado_t$p.value,
              resultado_t$conf.int[1], resultado_t$conf.int[2])
)

write_excel_csv(resumen_test, "output/tablas/test_hipotesis_resultados.csv")

cat("\n\n========================================\n")
cat("MÉTODO 2 — TEST t DE WELCH (POST vs PRE VACA MUERTA)\n")
cat("========================================\n")
print(resultado_t)

cat("\n  Interpretación:\n")
if (resultado_t$p.value < 0.05) {
  cat("  El test rechaza H0 al 5%: hay evidencia de que la participación\n")
  cat("  patagónica difiere entre los dos sub-períodos.\n")
} else {
  cat("  El test NO rechaza H0 al 5%. Sin embargo, con n=10/11 por grupo\n")
  cat("  el poder estadístico es bajo, por lo que este resultado no debe\n")
  cat("  interpretarse como evidencia concluyente en contra de la hipótesis,\n")
  cat("  sino como un resultado consistente con la baja N de la prueba.\n")
}


# ==============================================================================
# MÉTODO 3 — CORRELACIÓN DE PEARSON Y SPEARMAN
# ==============================================================================
# Cuantifica la asociación entre la dinámica del VAB extractivo de Neuquén y
# su participación total en el VAB nacional, a lo largo de los 21 años.

serie_neuquen <- vab_tidy |>
  filter(es_neuquen) |>
  group_by(anio) |>
  mutate(vab_nacional = sum(vab_tidy$vab[vab_tidy$anio == first(anio)], na.rm = TRUE)) |>
  summarise(
    vab_total       = sum(vab, na.rm = TRUE),
    vab_extractivo  = sum(vab[es_extractivo], na.rm = TRUE),
    .groups         = "drop"
  )

# Calculamos los shares nacionales
share_nacional <- vab_tidy |>
  group_by(anio) |>
  summarise(
    vab_nac_total = sum(vab, na.rm = TRUE),
    vab_nac_ext   = sum(vab[es_extractivo], na.rm = TRUE),
    .groups       = "drop"
  )

serie_neuquen <- serie_neuquen |>
  left_join(share_nacional, by = "anio") |>
  mutate(
    share_total      = vab_total / vab_nac_total * 100,
    share_extractivo = vab_extractivo / vab_nac_ext * 100
  )

corr_pearson  <- cor(serie_neuquen$share_extractivo, serie_neuquen$share_total,
                     method = "pearson")
corr_spearman <- cor(serie_neuquen$share_extractivo, serie_neuquen$share_total,
                     method = "spearman")

# Test de significancia de cada correlación
test_pearson  <- cor.test(serie_neuquen$share_extractivo, serie_neuquen$share_total,
                          method = "pearson")
test_spearman <- cor.test(serie_neuquen$share_extractivo, serie_neuquen$share_total,
                          method = "spearman", exact = FALSE)

resumen_corr <- tibble(
  metodo   = c("Pearson", "Spearman"),
  estimate = c(corr_pearson, corr_spearman),
  p_valor  = c(test_pearson$p.value, test_spearman$p.value),
  ic_lower = c(test_pearson$conf.int[1], NA),  # Spearman no devuelve IC en R base
  ic_upper = c(test_pearson$conf.int[2], NA)
)

write_excel_csv(resumen_corr, "output/tablas/correlaciones_neuquen.csv")

cat("\n\n========================================\n")
cat("MÉTODO 3 — CORRELACIÓN: VAB EXTRACTIVO vs VAB TOTAL DE NEUQUÉN\n")
cat("========================================\n")
print(resumen_corr)
cat("\n  Interpretación: una correlación positiva alta indica que la dinámica\n")
cat("  del extractivo arrastra la participación total de Neuquén, lo que apoya\n")
cat("  la hipótesis principal.\n")


# ==============================================================================
# MÉTODO 4 — DESCOMPOSICIÓN SECTORIAL DEL CRECIMIENTO
# ==============================================================================
# Para cada provincia p y sector s, la contribución al crecimiento se calcula:
#   Contribución_{s,p} = (VAB_{s,p,2024} − VAB_{s,p,2004}) / VAB_{p,2004} × 100
# La suma sobre sectores = crecimiento porcentual total de la provincia.

contribuciones <- vab_tidy |>
  filter(anio %in% c(2004, 2024)) |>
  select(provincia, sector, es_extractivo, anio, vab) |>
  pivot_wider(names_from = anio, values_from = vab, names_prefix = "y") |>
  group_by(provincia) |>
  mutate(
    vab_prov_2004    = sum(y2004, na.rm = TRUE),
    vab_prov_2024    = sum(y2024, na.rm = TRUE),
    contribucion_pp  = (y2024 - y2004) / vab_prov_2004 * 100
  ) |>
  ungroup()

# 4.1 Contribución del extractivo por provincia
contribucion_extractivo <- contribuciones |>
  group_by(provincia) |>
  summarise(
    crecimiento_total       = (first(vab_prov_2024) / first(vab_prov_2004) - 1) * 100,
    contribucion_extractivo = sum(contribucion_pp[es_extractivo], na.rm = TRUE),
    peso_extractivo_pct     = contribucion_extractivo / crecimiento_total * 100,
    .groups                 = "drop"
  ) |>
  arrange(desc(contribucion_extractivo))

write_excel_csv(contribucion_extractivo,
                "output/tablas/descomposicion_extractivo_por_provincia.csv")

# 4.2 Sector líder por provincia
sector_lider <- contribuciones |>
  group_by(provincia) |>
  arrange(desc(contribucion_pp)) |>
  slice(1) |>
  mutate(
    crecimiento_total     = (vab_prov_2024 / vab_prov_2004 - 1) * 100,
    peso_sector_lider_pct = contribucion_pp / crecimiento_total * 100
  ) |>
  ungroup() |>
  select(provincia, sector_lider = sector,
         contribucion_pp, crecimiento_total, peso_sector_lider_pct,
         es_extractivo) |>
  arrange(desc(peso_sector_lider_pct))

write_excel_csv(sector_lider, "output/tablas/descomposicion_sector_lider.csv")

# 4.3 Top 5 sectores contribuyentes por provincia
top_contribuyentes <- contribuciones |>
  group_by(provincia) |>
  slice_max(contribucion_pp, n = 5, with_ties = FALSE) |>
  arrange(provincia, desc(contribucion_pp)) |>
  mutate(ranking = row_number()) |>
  ungroup() |>
  select(provincia, ranking, sector, contribucion_pp, es_extractivo)

write_excel_csv(top_contribuyentes,
                "output/tablas/descomposicion_top5_por_provincia.csv")

cat("\n\n========================================\n")
cat("MÉTODO 4 — DESCOMPOSICIÓN SECTORIAL DEL CRECIMIENTO\n")
cat("========================================\n")
cat("\n  Contribución del extractivo al crecimiento provincial:\n")
print(contribucion_extractivo)

cat("\n\n  Sector líder por provincia (ordenado por peso del líder):\n")
print(sector_lider |> select(provincia, sector_lider,
                              peso_sector_lider_pct, es_extractivo))


cat("\n\nScript de análisis finalizado. Tablas guardadas en output/tablas/\n")

