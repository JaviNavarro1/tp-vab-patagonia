# ==============================================================================
# Script: 01_limpieza.R
# Objetivo: Leer, limpiar y transformar la base de CEPAL (Jurisdiccion_52sectores.xlsx)
#           en un dataframe tidy listo para el análisis.
# Input:    raw/Jurisdiccion_52sectores.xlsx
# Output:   input/Jurisdiccion_52sectores_tidy.csv
# Paquetes: tidyverse (dplyr, tidyr, purrr, readr, stringr), readxl, janitor
# Resultado esperado: 24 jurisdicciones × 52 sectores × 21 años (2004-2024)
# ==============================================================================

library(tidyverse)
library(readxl)

# install.packages(janitor)
# Definir directorio de trabajo
setwd(r'(C:\Users\Javi\Desktop\Ciencia de datos\TP)')

# Definimos la ruta al archivo original (almacenado en raw/ sin modificar)
archivo <- "raw/Jurisdiccion_52sectores.xlsx"

# ==============================================================================
# PASO 1 — Lectura y unión de hojas
# El archivo contiene una hoja por jurisdicción con los 52 sectores de actividad.
# Se leen las 24 hojas individuales (excluyendo la hoja resumen VABpb y la
# hoja No_distribuido) y se unifican en un único dataframe, agregando una
# columna que identifique la jurisdicción de origen.
# ==============================================================================

# excel_sheets() devuelve un vector con los nombres de todas las hojas
hojas <- excel_sheets(archivo)

# Filtramos las hojas que NO son jurisdicciones:
#   - "VABpb": hoja resumen con el VAB total por jurisdicción
#   - "No_distribuido": valores no asignables a ninguna jurisdicción
hojas_provincias <- hojas[!hojas %in% c("VABpb", "No_distribuido")]


# ==============================================================================
# PASO 2 — Eliminación de filas de encabezado
# Cada hoja tiene las primeras 5 filas con títulos, subtítulos y espacios
# vacíos que deben ser removidos. Usamos skip = 5 en read_excel() para que
# R empiece a leer desde la fila 6 (los encabezados reales de las columnas).
# ==============================================================================

# map_dfr() es una función de purrr (tidyverse) que:
#   1. Itera sobre cada elemento de hojas_provincias
#   2. Aplica la función que definimos (lee la hoja y agrega columna provincia)
#   3. Une todos los resultados en un único dataframe con bind_rows()
# El sufijo _dfr significa "devolver un dataframe unido por filas"
vab_raw <- map_dfr(hojas_provincias, function(hoja) {
  read_excel(archivo, sheet = hoja, skip = 5) |>  # skip = 5 salta los encabezados
    mutate(provincia = hoja)                       # agrega columna con el nombre de la hoja
})

# Verificamos cómo quedó el dataframe crudo
glimpse(vab_raw)

# ==============================================================================
# PASO 3 — Conversión de formato ancho a largo (pivot_longer)
# Actualmente cada año es una columna (2004, 2005, ..., 2024).
# Se transforma a formato tidy donde cada fila represente una observación
# única: provincia-sector-año-vab.
# ==============================================================================

# Antes de pivotar necesitamos preparar las columnas:
#   - Renombramos "Sector de actividad económica" a "sector" para mayor comodidad
# Luego pivot_longer() toma todas las columnas de años y las apila en dos columnas:
#   - "anio": con el nombre del año (originalmente era el nombre de la columna)
#   - "vab": con el valor del VAB correspondiente
# Finalmente str_extract() extrae los 4 dígitos del año (para manejar "2023 (1)")
# y as.integer() lo convierte de texto a número.
# El último filter() elimina:
#   - filas con NA en sector (vacías)
#   - la fila resumen "VAB a precios básicos" (totales de cada hoja)
#   - las notas al pie del Excel que empiezan con "(" como "(1) Datos provisorios."
vab_tidy <- vab_raw |>
  rename(sector = `Sector de actividad económica`) |>   # renombramos por nombre exacto
  pivot_longer(
    cols = -c(provincia, sector),
    names_to = "anio",
    values_to = "vab"
  ) |>
  mutate(anio = as.integer(str_extract(anio, "\\d{4}"))) |>
  janitor::clean_names() |>
  filter(anio >= 2004, anio <= 2024) |>
  mutate(provincia = str_replace_all(provincia, "_", " ")) |>
  filter(
    !is.na(sector),
    sector != "VAB a precios básicos",
    !str_starts(sector, "\\(")          # excluye filas que empiezan con "("
)

# ==============================================================================
# PASO 4 — Limpieza de nombres de columnas
# Estandarizar con janitor::clean_names() para obtener nombres en minúscula,
# sin espacios ni caracteres especiales.
# ==============================================================================

# clean_names() convierte los nombres de columnas a snake_case:
#   - Todo en minúscula
#   - Espacios reemplazados por guiones bajos
#   - Caracteres especiales eliminados
# En nuestro caso los nombres ya son simples (provincia, sector, anio, vab)
# pero es buena práctica aplicarlo siempre
vab_tidy <- vab_tidy |>
  janitor::clean_names()

# Verificamos los nombres resultantes
names(vab_tidy)  # Esperado: "provincia", "sector", "anio", "vab"

# ==============================================================================
# PASO 5 — Filtrado de período de análisis
# Conservar los años 2004 a 2024 (21 años, serie completa publicada por CEPAL).
# ==============================================================================

# filter() de dplyr: conserva solo las filas que cumplen la condición
vab_tidy <- vab_tidy |>
  filter(anio >= 2004, anio <= 2024)


# Verificamos los años disponibles
vab_tidy |> distinct(anio)  # Esperado: 21 años (2004 a 2024)
names(vab_raw)

names(vab_raw)[1:5]
# ==============================================================================
# PASO 6 — Eliminación de la hoja "No distribuido"
# Esta hoja contiene valores del VAB no asignables a ninguna jurisdicción
# y debe ser excluida del análisis provincial.
# Ya fue excluida en el Paso 1 al definir hojas_provincias, pero verificamos
# que efectivamente no haya quedado ningún registro.
# ==============================================================================

# Verificamos las jurisdicciones presentes
vab_tidy |> distinct(provincia)  # Esperado: 24 jurisdicciones (23 provincias + CABA)

# Limpiamos los nombres de provincia: los nombres de hoja usan guiones bajos
# str_replace_all() de stringr (tidyverse) reemplaza "_" por espacios
# Ej: "Ciudad_de_Buenos_Aires" -> "Ciudad de Buenos Aires"
vab_tidy <- vab_tidy |>
  mutate(provincia = str_replace_all(provincia, "_", " "))

# ==============================================================================
# PASO 7 — Creación de variables derivadas
# Construir mediante mutate() las variables:
#   - es_extractivo: flag para sectores de extracción de petróleo/gas y minería
#   - region: agrupación geográfica de las jurisdicciones
#   - participacion: share sobre VAB total nacional (%)
#   - variacion_pct: variación porcentual interanual del VAB
# ==============================================================================

# --- 7.1 es_extractivo -------------------------------------------------------
# Definimos los nombres exactos de los dos sectores extractivos según la
# clasificación CIIU Rev. 3.1 que usa la base de CEPAL.
# Deben coincidir textualmente con los valores del Excel.
sectores_extractivos <- c(
  "Extracción de carbón y lignito; extracción de turba. Extracción de petróleo crudo y gas natural; actividades de servicios relacionadas con la extracción de petróleo y gas, excepto las actividades de prospección.",
  "Extracción de minerales metalíferos. Explotación de  minas y canteras n.c.p."
)

# mutate() crea la nueva columna: TRUE si el sector está en la lista, FALSE si no
vab_tidy <- vab_tidy |>
  mutate(es_extractivo = sector %in% sectores_extractivos)

# --- 7.2 region ---------------------------------------------------------------
# case_when() de dplyr funciona como un if/else if encadenado.
# Evalúa cada condición en orden: la primera que sea TRUE asigna el valor.
# Agrupamos las 24 jurisdicciones en 6 regiones geográficas.
vab_tidy <- vab_tidy |>
  mutate(
    region = case_when(
      provincia %in% c("Neuquen", "Chubut", "Santa Cruz",
                       "Rio Negro", "Tierra del Fuego")         ~ "Patagonia",
      provincia %in% c("Buenos Aires", "Cordoba", "Santa Fe",
                       "Entre Rios", "La Pampa")                ~ "Pampeana",
      provincia %in% c("Jujuy", "Salta", "Tucuman",
                       "Santiago del Estero", "Catamarca")      ~ "NOA",
      provincia %in% c("Corrientes", "Misiones", "Chaco",
                       "Formosa")                               ~ "NEA",
      provincia %in% c("Mendoza", "San Juan", "San Luis",
                       "La Rioja")                              ~ "Cuyo",
      provincia == "Ciudad de Buenos Aires"                      ~ "CABA"
    )
  )

# --- 7.3 participacion --------------------------------------------------------
# Calculamos qué porcentaje representa cada provincia-sector sobre el
# VAB total nacional de ese año.
# group_by(anio) hace que sum(vab) sume todo el VAB del país en ese año.
# Después de mutate(), ungroup() libera el agrupamiento para no afectar
# operaciones posteriores.
vab_tidy <- vab_tidy |>
  group_by(anio) |>
  mutate(participacion = vab / sum(vab, na.rm = TRUE) * 100) |>
  ungroup()

# --- 7.4 variacion_pct --------------------------------------------------------
# Variación porcentual interanual: (valor_actual / valor_anterior - 1) * 100
# group_by(provincia, sector) asegura que lag() tome el valor del mismo
# provincia-sector del año anterior (no de otra provincia u otro sector).
# arrange(anio) ordena cronológicamente dentro de cada grupo.
# El año 2004 queda con NA porque no tiene año previo.
vab_tidy <- vab_tidy |>
  group_by(provincia, sector) |>
  arrange(anio) |>
  mutate(variacion_pct = (vab / lag(vab) - 1) * 100) |>
  ungroup()

# ==============================================================================
# PASO 8 — Eliminación de la fila resumen
# Cada hoja incluye una fila final con el total "VAB a precios básicos"
# que debe ser removida para evitar duplicar valores al calcular agregados.
# También eliminamos filas con NA en sector (notas al pie del Excel).
# Nota: este filtro ya se aplicó en el Paso 3 dentro de la cadena de pivot.
# Se mantiene acá como verificación de seguridad por si alguna fila resumen
# se hubiera colado después de la creación de las variables derivadas.
# ==============================================================================

# filter() con condiciones negadas:
#   - !is.na(sector): elimina filas donde sector es NA (filas vacías, notas al pie)
#   - sector != "VAB a precios básicos": elimina la fila de totales
vab_tidy <- vab_tidy |>
  filter(
    !is.na(sector),
    sector != "VAB a precios básicos"
  )



# ==============================================================================
# VERIFICACIONES FINALES
# Antes de guardar, verificamos que el dataframe tenga la estructura esperada.
# ==============================================================================

# Estructura general del dataframe
glimpse(vab_tidy)

# Cantidad de jurisdicciones (esperado: 24)
vab_tidy |> distinct(provincia) |> nrow()

# Cantidad de filas por provincia (esperado: 52 sectores x 21 años = 1.092)
vab_tidy |> count(provincia) |> print(n = 24)

# Cantidad de años (esperado: 21, de 2004 a 2024)
vab_tidy |> distinct(anio) |> nrow()

# Verificar asignación de regiones (ningún NA en region)
vab_tidy |> distinct(provincia, region) |> print(n = 24)

# Columnas finales del dataframe
# Esperado: provincia, sector, anio, vab, es_extractivo, region,
#           participacion, variacion_pct
names(vab_tidy)

# ==============================================================================
# GUARDADO
# Los datos procesados se guardan en input/ con write_excel_csv() de readr.
# Esta función agrega un BOM UTF-8 para que Excel en Windows lea correctamente
# las tildes y caracteres especiales (ej: "ñ", "á", "é").
# Los datos crudos permanecen intactos en raw/, siguiendo la estructura de
# carpetas requerida por el curso.
# ==============================================================================

write_excel_csv(vab_tidy, "input/Jurisdiccion_52sectores_tidy.csv")
                
##write_csv(vab_tidy, "input/Jurisdiccion_52sectores_tidy.csv")

cat("Limpieza finalizada. Dataset guardado en input/Jurisdiccion_52sectores_tidy.csv\n")
cat("Dimensiones:", nrow(vab_tidy), "filas x", ncol(vab_tidy), "columnas\n")
