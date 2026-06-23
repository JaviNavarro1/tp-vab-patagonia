# ==============================================================================
# Script: 02_exploratorio.R
# Objetivo: Análisis descriptivo inicial de la base. Cubre tres bloques:
#           (1) Estadísticas globales del VAB y su distribución.
#           (2) Diagnóstico de calidad de datos (NAs, ceros, outliers).
#           (3) Agregados descriptivos por región y por provincia.
# Input:    input/Jurisdiccion_52sectores_tidy.csv (generado por 01_limpieza.R)
# Output:   output/tablas/*.csv (tablas descriptivas)
# Paquetes: tidyverse, e1071 (asimetría)
# ==============================================================================

library(tidyverse)
library(e1071)   # para skewness()

options(scipen = 999)

# Definir el directorio de trabajo (ajustar según corresponda)
setwd(r'(C:\Users\Javi\Desktop\Ciencia de datos\tp-vab-patagonia)')

# Carpeta de tablas
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

# Carga del dataset limpio
vab_tidy <- read_csv("input/Jurisdiccion_52sectores_tidy.csv")

glimpse(vab_tidy)


# ==============================================================================
# 1. DEFINICIÓN DE REGIONES (estándar INDEC, con CABA por separado)
# ==============================================================================
# Usamos str_detect() con regex permisivos para cubrir variantes en los
# nombres (con o sin tildes, abreviaturas, etc.). Esto es más robusto que un
# left_join() con nombres exactos.

vab_tidy <- vab_tidy |>
  mutate(region = case_when(
    # Pampeana
    str_detect(provincia, regex("^Buenos Aires$", ignore_case = TRUE))      ~ "Pampeana",
    str_detect(provincia, regex("C.rdoba",        ignore_case = TRUE))      ~ "Pampeana",
    str_detect(provincia, regex("Entre R.os",     ignore_case = TRUE))      ~ "Pampeana",
    str_detect(provincia, regex("La Pampa",       ignore_case = TRUE))      ~ "Pampeana",
    str_detect(provincia, regex("Santa Fe",       ignore_case = TRUE))      ~ "Pampeana",
    # CABA (jurisdicción separada por su perfil productivo distinto)
    str_detect(provincia, regex("Ciudad|CABA|Aut.noma", ignore_case = TRUE)) ~ "CABA",
    # NOA
    str_detect(provincia, regex("Catamarca",           ignore_case = TRUE))  ~ "NOA",
    str_detect(provincia, regex("Jujuy",               ignore_case = TRUE))  ~ "NOA",
    str_detect(provincia, regex("La Rioja",            ignore_case = TRUE))  ~ "NOA",
    str_detect(provincia, regex("Salta",               ignore_case = TRUE))  ~ "NOA",
    str_detect(provincia, regex("Santiago del Estero", ignore_case = TRUE))  ~ "NOA",
    str_detect(provincia, regex("Tucum.n",             ignore_case = TRUE))  ~ "NOA",
    # NEA
    str_detect(provincia, regex("Chaco",      ignore_case = TRUE))           ~ "NEA",
    str_detect(provincia, regex("Corrientes", ignore_case = TRUE))           ~ "NEA",
    str_detect(provincia, regex("Formosa",    ignore_case = TRUE))           ~ "NEA",
    str_detect(provincia, regex("Misiones",   ignore_case = TRUE))           ~ "NEA",
    # Cuyo
    str_detect(provincia, regex("Mendoza",  ignore_case = TRUE))             ~ "Cuyo",
    str_detect(provincia, regex("San Juan", ignore_case = TRUE))             ~ "Cuyo",
    str_detect(provincia, regex("San Luis", ignore_case = TRUE))             ~ "Cuyo",
    # Patagonia
    str_detect(provincia, regex("Chubut",           ignore_case = TRUE))     ~ "Patagonia",
    str_detect(provincia, regex("Neuqu.n",          ignore_case = TRUE))     ~ "Patagonia",
    str_detect(provincia, regex("R.o Negro",        ignore_case = TRUE))     ~ "Patagonia",
    str_detect(provincia, regex("Santa Cruz",       ignore_case = TRUE))     ~ "Patagonia",
    str_detect(provincia, regex("Tierra del Fuego", ignore_case = TRUE))     ~ "Patagonia",
    # Cualquier otro caso queda como NA (para detectar errores)
    TRUE ~ NA_character_
  ))

# Verificación: todas las jurisdicciones deberían tener región asignada
provincias_sin_region <- unique(vab_tidy$provincia[is.na(vab_tidy$region)])
if (length(provincias_sin_region) > 0) {
  warning("Provincias sin región asignada: ",
          paste(provincias_sin_region, collapse = ", "))
} else {
  cat("Asignación de regiones OK: las", n_distinct(vab_tidy$provincia),
      "jurisdicciones tienen región.\n")
}


# ==============================================================================
# 2. ESTADÍSTICAS GLOBALES DEL VAB
# ==============================================================================
# Caracterización general de la variable principal del trabajo.

stats_globales <- vab_tidy |>
  filter(!is.na(vab)) |>
  summarise(
    n          = n(),
    media      = mean(vab),
    mediana    = median(vab),
    desvio     = sd(vab),
    cv         = sd(vab) / mean(vab) * 100,
    asimetria  = skewness(vab),
    minimo     = min(vab),
    maximo     = max(vab),
    p25        = quantile(vab, 0.25),
    p75        = quantile(vab, 0.75)
  )

write_excel_csv(stats_globales, "output/tablas/distribucion_vab_global.csv")

cat("\n========================================\n")
cat("DISTRIBUCIÓN GLOBAL DEL VAB\n")
cat("========================================\n")
cat(sprintf("  N (observaciones):  %d\n",          stats_globales$n))
cat(sprintf("  Media:              %.1f mill.\n",  stats_globales$media))
cat(sprintf("  Mediana:            %.1f mill.\n",  stats_globales$mediana))
cat(sprintf("  Desvío estándar:    %.1f mill.\n",  stats_globales$desvio))
cat(sprintf("  Coef. de variación: %.1f %%\n",     stats_globales$cv))
cat(sprintf("  Asimetría (g₁):     %.2f\n",        stats_globales$asimetria))
cat("\nLa media es muy superior a la mediana y la asimetría es alta:\n")
cat("la distribución del VAB tiene cola derecha pronunciada. Esto justifica\n")
cat("trabajar con shares en lugar de niveles para la mayor parte del análisis.\n")


# ==============================================================================
# 3. DIAGNÓSTICO DE CALIDAD DE DATOS
# ==============================================================================

# 3.1 NAs
nas <- sum(is.na(vab_tidy$vab))

# 3.2 Ceros (sector ausente en una provincia)
ceros <- sum(vab_tidy$vab == 0, na.rm = TRUE)
ceros_pct <- ceros / nrow(vab_tidy) * 100

# 3.3 Outliers (regla IQR) en el año 2024
vab_2024 <- vab_tidy |> filter(anio == 2024, !is.na(vab))
q1   <- quantile(vab_2024$vab, 0.25)
q3   <- quantile(vab_2024$vab, 0.75)
iqr  <- q3 - q1
outliers <- vab_2024 |> filter(vab > q3 + 1.5 * iqr | vab < q1 - 1.5 * iqr)

# Top 10 outliers (datos genuinos: refleja la concentración productiva)
top_outliers <- outliers |>
  arrange(desc(vab)) |>
  slice_head(n = 10) |>
  mutate(vab_mill = round(vab, 0)) |>
  select(provincia, sector, vab_mill)

write_excel_csv(top_outliers, "output/tablas/top_outliers_2024.csv")

cat("\n\n========================================\n")
cat("DIAGNÓSTICO DE CALIDAD\n")
cat("========================================\n")
cat(sprintf("  NAs:                   %d\n", nas))
cat(sprintf("  Ceros (sectores ausentes): %d (%.1f%%)\n", ceros, ceros_pct))
cat(sprintf("  Outliers IQR en 2024:  %d (%.1f%% de las obs. de 2024)\n",
            nrow(outliers), nrow(outliers) / nrow(vab_2024) * 100))


# ==============================================================================
# 4. PARTICIPACIÓN POR REGIÓN EN EL VAB NACIONAL (2004 vs 2024)
# ==============================================================================

share_regional <- vab_tidy |>
  filter(anio %in% c(2004, 2024)) |>
  group_by(anio) |>
  mutate(vab_nacional = sum(vab, na.rm = TRUE)) |>
  ungroup() |>
  group_by(region, anio) |>
  summarise(
    vab_region   = sum(vab, na.rm = TRUE),
    vab_nacional = first(vab_nacional),
    share        = vab_region / vab_nacional * 100,
    .groups      = "drop"
  ) |>
  select(region, anio, share) |>
  pivot_wider(names_from = anio, values_from = share, names_prefix = "share_") |>
  mutate(delta_pp = share_2024 - share_2004) |>
  arrange(desc(share_2024))

write_excel_csv(share_regional, "output/tablas/share_regional.csv")

cat("\n\n========================================\n")
cat("PARTICIPACIÓN POR REGIÓN EN EL VAB NACIONAL\n")
cat("========================================\n")
print(share_regional)


# ==============================================================================
# 5. CAMBIO EN PARTICIPACIÓN POR PROVINCIA (TODAS, ordenadas)
# ==============================================================================

cambio_por_provincia <- vab_tidy |>
  filter(anio %in% c(2004, 2024)) |>
  group_by(anio) |>
  mutate(vab_nacional = sum(vab, na.rm = TRUE)) |>
  ungroup() |>
  group_by(provincia, region, anio) |>
  summarise(
    vab_prov     = sum(vab, na.rm = TRUE),
    vab_nacional = first(vab_nacional),
    share        = vab_prov / vab_nacional * 100,
    .groups      = "drop"
  ) |>
  select(provincia, region, anio, share) |>
  pivot_wider(names_from = anio, values_from = share, names_prefix = "share_") |>
  mutate(delta_pp = share_2024 - share_2004) |>
  arrange(desc(delta_pp))

write_excel_csv(cambio_por_provincia,
                "output/tablas/cambio_participacion_por_provincia.csv")

cat("\n\n========================================\n")
cat("TOP 5 GANADORAS Y TOP 5 PERDEDORAS (Δ pp 2004→2024)\n")
cat("========================================\n")
cat("\nGanadoras:\n")
print(cambio_por_provincia |> slice_head(n = 5))
cat("\nPerdedoras:\n")
print(cambio_por_provincia |> slice_tail(n = 5))


# ==============================================================================
# 6. SHARE DE EXTRACTIVO NACIONAL POR PROVINCIA
# ==============================================================================
# Permite ver si la concentración en Neuquén es la única historia o si otras
# provincias también ganaron/perdieron peso en el extractivo.

share_extractivo_provincial <- vab_tidy |>
  filter(es_extractivo, anio %in% c(2004, 2024)) |>
  group_by(anio) |>
  mutate(vab_ext_nac = sum(vab, na.rm = TRUE)) |>
  ungroup() |>
  group_by(provincia, region, anio) |>
  summarise(
    vab_ext     = sum(vab, na.rm = TRUE),
    vab_ext_nac = first(vab_ext_nac),
    share_ext   = vab_ext / vab_ext_nac * 100,
    .groups     = "drop"
  ) |>
  select(provincia, region, anio, share_ext) |>
  pivot_wider(names_from = anio, values_from = share_ext, names_prefix = "Y") |>
  mutate(delta_ext = Y2024 - Y2004) |>
  arrange(desc(Y2024))

write_excel_csv(share_extractivo_provincial,
                "output/tablas/share_extractivo_provincial.csv")

cat("\n\n========================================\n")
cat("SHARE EXTRACTIVO NACIONAL — PROVINCIAS RELEVANTES (>0,5%)\n")
cat("========================================\n")
print(share_extractivo_provincial |> filter(Y2024 > 0.5))


# ==============================================================================
# 7. DISTRIBUCIÓN DEL VAB POR REGIÓN (2024)
# ==============================================================================

distribucion_por_region <- vab_tidy |>
  filter(anio == 2024, !is.na(vab)) |>
  group_by(region) |>
  summarise(
    n               = n(),
    vab_total       = sum(vab),
    media           = mean(vab),
    mediana         = median(vab),
    cv              = sd(vab) / mean(vab) * 100,
    asimetria       = skewness(vab),
    .groups         = "drop"
  ) |>
  arrange(desc(vab_total))

write_excel_csv(distribucion_por_region,
                "output/tablas/distribucion_vab_por_region.csv")

cat("\n\n========================================\n")
cat("DISTRIBUCIÓN DEL VAB POR REGIÓN (2024)\n")
cat("========================================\n")
print(distribucion_por_region)


cat("\n\nScript exploratorio finalizado. Tablas guardadas en output/tablas/\n")

