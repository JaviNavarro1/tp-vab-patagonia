# ==============================================================================
# Script: 04_visualizaciones.R
# Objetivo: Generar las dos visualizaciones del TP (Instancia 3):
#           (1) GRÁFICO COMUNICACIONAL — línea de tiempo de la participación
#               patagónica en el VAB nacional, estilo OWID.
#           (2) GRÁFICO EXPLORATORIO — mapa coroplético de la variación de la
#               participación provincial entre 2004 y 2024.
# Input:    input/Jurisdiccion_52sectores_tidy.csv (generado por 01_limpieza.R)
# Output:   output/graficos/grafico_comunicacional.png
#           output/graficos/grafico_exploratorio.png
# Paquetes: tidyverse, ggtext, scales, sf, geoAr
# ==============================================================================

library(tidyverse)
library(ggtext)
library(scales)
library(sf)
library(geoAr)

options(scipen = 999)

# Definir el directorio de trabajo (ajustar según corresponda)
setwd(r'(C:\Users\Javi\Desktop\Ciencia de datos\tp-vab-patagonia)')

# Carpeta de gráficos
if (!dir.exists("output/graficos")) dir.create("output/graficos", recursive = TRUE)

# Carga del dataset limpio
vab_tidy <- read_csv("input/Jurisdiccion_52sectores_tidy.csv")


# ==============================================================================
# PALETA Y TEMA REUTILIZABLES (estilo OWID, Clase 21)
# ==============================================================================

owid_azul <- "#1F4E79"
owid_rojo <- "#B13507"
owid_gris <- "#888888"

theme_owid <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.35),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(1.0), colour = "#5b5b5b",
                                       margin = margin(b = 16)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 14)),
      axis.title    = element_blank(),
      axis.text     = element_text(colour = "#5b5b5b"),
      axis.ticks    = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "none",
      plot.margin = margin(t = 14, r = 80, b = 10, l = 16)
    )
}


# ==============================================================================
# GRÁFICO 1 — COMUNICACIONAL: LÍNEA DE TIEMPO CON STORYTELLING
# ==============================================================================
# Historia: la Patagonia recupera peso en la economía argentina tras una década
# de caída, con la intensificación de Vaca Muerta a partir de 2014.
# Claves del tipo: una sola línea principal, recuadro sombreado para el período
# de interés (Vaca Muerta), anotaciones en los puntos clave (inicio, mínimo,
# final), línea de referencia para el nivel inicial.
# ==============================================================================

patagonia_pat <- regex("Chubut|Neuqu.n|R.o Negro|Santa Cruz|Tierra del Fuego",
                       ignore_case = TRUE)

evolucion_patagonia <- vab_tidy |>
  group_by(anio) |>
  mutate(vab_nacional = sum(vab, na.rm = TRUE)) |>
  ungroup() |>
  filter(str_detect(provincia, patagonia_pat)) |>
  group_by(anio) |>
  summarise(
    vab_patagonia = sum(vab, na.rm = TRUE),
    vab_nacional  = first(vab_nacional),
    share         = vab_patagonia / vab_nacional * 100,
    .groups       = "drop"
  )

puntos_clave <- evolucion_patagonia |>
  filter(anio %in% c(2004, 2011, 2024))

val_2004 <- evolucion_patagonia |> filter(anio == 2004) |> pull(share)
val_2024 <- evolucion_patagonia |> filter(anio == 2024) |> pull(share)
val_2011 <- evolucion_patagonia |> filter(anio == 2011) |> pull(share)

# Título con palabra coloreada (markdown con ggtext)
titulo_linea <- sprintf(
  "<span style='color:%s'>**La Patagonia**</span> recupera peso en la economía argentina tras una década de caída",
  owid_rojo
)

g_linea <- ggplot(evolucion_patagonia, aes(x = anio, y = share)) +
  
  # Recuadro sombreado: período intensivo de Vaca Muerta (2014-2024)
  annotate("rect", xmin = 2014, xmax = 2024, ymin = -Inf, ymax = Inf,
           fill = owid_rojo, alpha = 0.08) +
  annotate("text", x = 2019, y = 10.1, label = "Inicio intensivo de Vaca Muerta",
           colour = owid_rojo, size = 3.8, fontface = "bold") +
  
  # Línea de referencia: nivel de 2004
  geom_hline(yintercept = val_2004, linetype = "dashed",
             colour = "#888888", linewidth = 0.4, alpha = 0.7) +
  
  # Línea principal y puntos
  geom_line(colour = owid_rojo, linewidth = 1.3) +
  geom_point(colour = owid_rojo, size = 1.5) +
  
  # Puntos clave destacados (2004, 2011, 2024)
  geom_point(data = puntos_clave, colour = owid_rojo, size = 4,
             stroke = 2, fill = "white", shape = 21) +
  
  # Anotación 2004: en la zona superior izquierda, separada de la línea descendente
  annotate("text", x = 2003.5, y = 10.45,
           label = paste0("2004: ", round(val_2004, 2), "%"),
           hjust = 0.5, size = 3.8, fontface = "bold", colour = "#1d1d1d") +
  annotate("segment", x = 2003.7, y = 10.35, xend = 2004, yend = val_2004 + 0.1,
           colour = "#888888", linewidth = 0.4, alpha = 0.5) +
  
  # Anotación 2011: mínimo histórico
  annotate("text", x = 2010, y = val_2011 - 0.4,
           label = paste0("Mínimo histórico\n2011: ", round(val_2011, 2), "%"),
           hjust = 0.5, size = 3.5, colour = "#1d1d1d", lineheight = 0.95) +
  annotate("segment", x = 2011, y = val_2011 - 0.05, xend = 2011, yend = val_2011 - 0.25,
           colour = "#555555", linewidth = 0.4) +
  
  # Anotación 2024: destacado en color
  annotate("text", x = 2022.5, y = 10.6,
           label = paste0("2024: ", round(val_2024, 2), "%\n(+",
                          round(val_2024 - val_2004, 2), " pp vs. 2004)"),
           hjust = 0.5, size = 4, fontface = "bold", colour = owid_rojo,
           lineheight = 0.95) +
  annotate("curve", x = 2023, y = 10.4, xend = 2024, yend = val_2024 + 0.05,
           curvature = -0.15, linewidth = 0.5, colour = owid_rojo,
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  
  # Etiqueta del nivel 2004 (sobre la línea de referencia, a la derecha)
  annotate("text", x = 2024.5, y = val_2004,
           label = "Nivel 2004", hjust = 0, vjust = 0.5,
           size = 3, colour = "#888888", fontface = "italic") +
  
  # Escalas
  scale_x_continuous(breaks = c(2004, 2008, 2011, 2014, 2018, 2024)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1),
                     limits = c(7.0, 11.0)) +
  coord_cartesian(clip = "off") +
  
  # Título, subtítulo, fuente
  labs(
    title    = titulo_linea,
    subtitle = "Participación de las 5 provincias patagónicas en el VAB nacional, 2004–2024",
    caption  = paste(
      "Fuente: Elaboración propia en base a MECON-CEPAL (LC/TS.2022/196) — Desagregación provincial del VAB de Argentina, base 2004.",
      "Nota: Las provincias patagónicas son Neuquén, Río Negro, Chubut, Santa Cruz y Tierra del Fuego.",
      sep = "<br>"
    )
  ) +
  theme_owid() +
  theme(plot.margin = margin(t = 14, r = 30, b = 10, l = 16))

ggsave("output/graficos/grafico_comunicacional.png", g_linea,
       width = 13, height = 7.5, dpi = 300, bg = "white")


# ==============================================================================
# GRÁFICO 2 — EXPLORATORIO: MAPA COROPLÉTICO
# ==============================================================================
# Variación de la participación provincial en el VAB nacional entre 2004 y
# 2024, con paleta divergente y anotaciones para las 3 provincias clave.
# ==============================================================================

# Calculamos la variación en puntos porcentuales por provincia
delta_provincial <- vab_tidy |>
  filter(anio %in% c(2004, 2024)) |>
  group_by(anio) |>
  mutate(vab_nacional = sum(vab, na.rm = TRUE)) |>
  ungroup() |>
  group_by(provincia, anio) |>
  summarise(
    vab_prov     = sum(vab, na.rm = TRUE),
    vab_nacional = first(vab_nacional),
    share        = vab_prov / vab_nacional * 100,
    .groups      = "drop"
  ) |>
  select(provincia, anio, share) |>
  pivot_wider(names_from = anio, values_from = share, names_prefix = "share_") |>
  mutate(delta_pp = share_2024 - share_2004)


# Cargar la geometría provincial con geoAr (descarga la primera vez)
arg <- get_geo("ARGENTINA", level = "provincia") |>
  add_geo_codes() |>
  st_make_valid()

# Recortar para excluir la Antártida y conservar continente + Malvinas
arg <- st_crop(arg, st_bbox(c(xmin = -74, xmax = -52, ymin = -56, ymax = -21),
                            crs = st_crs(arg)))

# Join robusto con str_detect (cubre variantes en el nombre, en particular
# Tierra del Fuego que tiene la coletilla "Antártida e Islas del Atlántico Sur")
mapa_datos <- arg |>
  mutate(
    provincia_datos = case_when(
      str_detect(name_iso, "Tierra del Fuego")                                  ~ "Tierra del Fuego",
      str_detect(name_iso, regex("Aut.noma|Ciudad de Buenos", ignore_case = TRUE)) ~ "Ciudad de Buenos Aires",
      str_detect(name_iso, "C.rdoba")                                           ~ "Cordoba",
      str_detect(name_iso, "Entre R.os")                                        ~ "Entre Rios",
      str_detect(name_iso, "Neuqu.n")                                           ~ "Neuquen",
      str_detect(name_iso, "R.o Negro")                                         ~ "Rio Negro",
      str_detect(name_iso, "Tucum.n")                                           ~ "Tucuman",
      TRUE                                                                       ~ name_iso
    )
  ) |>
  left_join(delta_provincial, by = c("provincia_datos" = "provincia"))

# Verificación: ninguna provincia debería quedar sin delta_pp
provincias_sin_match <- mapa_datos |>
  filter(is.na(delta_pp)) |>
  pull(name_iso) |>
  unique()
if (length(provincias_sin_match) > 0) {
  warning("Provincias sin match en el join: ",
          paste(provincias_sin_match, collapse = ", "))
}


# Anotaciones para las 3 provincias clave
centroides <- mapa_datos |>
  filter(provincia_datos %in% c("Neuquen", "Ciudad de Buenos Aires", "Mendoza")) |>
  st_centroid() |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  select(provincia_datos, delta_pp, lon, lat)

# Tabla de anotaciones con tres posiciones: texto, inicio de flecha, fin
etiquetas_anot <- centroides |>
  mutate(
    label = paste0(
      recode(provincia_datos,
             "Neuquen" = "Neuquén",
             "Ciudad de Buenos Aires" = "CABA"),
      "\n",
      ifelse(delta_pp > 0, "+", "−"),
      formatC(abs(delta_pp), format = "f", digits = 2, decimal.mark = ","),
      " pp"
    ),
    label_x = case_when(
      provincia_datos == "Neuquen"                ~ -76,
      provincia_datos == "Ciudad de Buenos Aires" ~ -54,
      provincia_datos == "Mendoza"                ~ -76
    ),
    label_y = case_when(
      provincia_datos == "Neuquen"                ~ -43,
      provincia_datos == "Ciudad de Buenos Aires" ~ -31,
      provincia_datos == "Mendoza"                ~ -33
    ),
    arrow_x = case_when(
      provincia_datos == "Neuquen"                ~ -73.5,
      provincia_datos == "Ciudad de Buenos Aires" ~ -56,
      provincia_datos == "Mendoza"                ~ -73.5
    ),
    arrow_y = case_when(
      provincia_datos == "Neuquen"                ~ -42,
      provincia_datos == "Ciudad de Buenos Aires" ~ -32,
      provincia_datos == "Mendoza"                ~ -33.5
    ),
    color = ifelse(delta_pp > 0, owid_azul, owid_rojo)
  )


g_mapa <- ggplot(mapa_datos) +
  
  # Coropletas con paleta divergente (azul = ganó, rojo = perdió)
  geom_sf(aes(fill = delta_pp), colour = "white", linewidth = 0.3) +
  
  # Flechas curvas, una capa por provincia (geom_curve solo acepta un valor de
  # curvature por capa, por eso se separa)
  geom_curve(
    data = etiquetas_anot |> filter(provincia_datos == "Neuquen"),
    aes(x = arrow_x, y = arrow_y, xend = lon, yend = lat, colour = color),
    linewidth = 0.5, curvature = -0.15,
    arrow = arrow(length = unit(2.2, "mm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = etiquetas_anot |> filter(provincia_datos == "Ciudad de Buenos Aires"),
    aes(x = arrow_x, y = arrow_y, xend = lon, yend = lat, colour = color),
    linewidth = 0.5, curvature = -0.15,
    arrow = arrow(length = unit(2.2, "mm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = etiquetas_anot |> filter(provincia_datos == "Mendoza"),
    aes(x = arrow_x, y = arrow_y, xend = lon, yend = lat, colour = color),
    linewidth = 0.5, curvature = 0.15,
    arrow = arrow(length = unit(2.2, "mm"), type = "closed"),
    inherit.aes = FALSE
  ) +
  
  # Etiquetas de las tres provincias clave
  geom_text(
    data = etiquetas_anot,
    aes(x = label_x, y = label_y, label = label, colour = color),
    size = 3.3, fontface = "bold", lineheight = 0.9,
    hjust = 0.5, vjust = 0.5,
    inherit.aes = FALSE
  ) +
  
  scale_colour_identity() +
  
  scale_fill_gradient2(
    low      = owid_rojo,
    mid      = "#F5F5F5",
    high     = owid_azul,
    midpoint = 0,
    limits   = c(-1.2, 1.2),
    breaks   = seq(-1, 1, 0.5),
    name     = "Variación de la participación en el VAB nacional (puntos porcentuales)"
  ) +
  
  coord_sf(xlim = c(-77, -52), ylim = c(-56, -21), expand = FALSE, clip = "off") +
  
  labs(
    title    = "Variación de la participación provincial en el VAB nacional (2004 vs. 2024)",
    subtitle = "Diferencia en puntos porcentuales entre 2024 y 2004. Azul = ganó peso; Rojo = perdió peso.",
    caption  = paste(
      "Fuente: Elaboración propia en base a MECON-CEPAL (LC/TS.2022/196).",
      "Nota: la participación se calcula como VAB provincial / VAB nacional × 100, para cada año.",
      sep = "<br>"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = rel(1.3),
                                 colour = "#1d1d1d", margin = margin(b = 4)),
    plot.subtitle = element_text(size = rel(0.98), colour = "#5b5b5b",
                                 margin = margin(b = 14)),
    plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                     colour = "#8a8a8a", margin = margin(t = 12)),
    legend.position    = "bottom",
    legend.title       = element_text(size = rel(0.8), colour = "#5b5b5b"),
    legend.text        = element_text(size = rel(0.72), colour = "#5b5b5b"),
    legend.key.width   = unit(2.5, "cm"),
    legend.key.height  = unit(0.35, "cm"),
    plot.margin        = margin(20, 30, 15, 30)
  ) +
  guides(
    fill   = guide_colorbar(title.position = "top", title.hjust = 0),
    colour = "none"
  )

ggsave("output/graficos/grafico_exploratorio.png", g_mapa,
       width = 10, height = 13, dpi = 300, bg = "white")


cat("\nGráficos guardados en output/graficos/:\n")
cat("  - grafico_comunicacional.png\n")
cat("  - grafico_exploratorio.png\n")

