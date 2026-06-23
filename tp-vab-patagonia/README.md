# Participación patagónica en el VAB nacional argentino (2004–2024)

**Trabajo Práctico Final — Ciencia de Datos para Economía y Negocios**
Facultad de Ciencias Económicas, Universidad de Buenos Aires.

## Integrantes


- Javier Navarro - Nro. de registro: 886569

## Objetivo

Analizar la evolución de la participación de las provincias patagónicas (Neuquén, Chubut, Santa Cruz, Río Negro y Tierra del Fuego) en el Valor Agregado Bruto (VAB) nacional argentino durante el período 2004–2024, con foco en el rol del sector extractivo (extracción de petróleo crudo y gas natural) y, en particular, en el efecto del desarrollo intensivo del yacimiento no convencional de Vaca Muerta (Neuquén) a partir de 2014.

**Hipótesis principal:** las provincias patagónicas aumentaron su participación en el VAB nacional entre 2004 y 2024, impulsadas principalmente por el crecimiento del sector extractivo, en particular en Neuquén a partir del desarrollo de Vaca Muerta.

**Hipótesis complementaria:** el desarrollo intensivo de Vaca Muerta marca un punto de quiebre: el crecimiento del VAB extractivo de Neuquén es significativamente distinto en el período posterior a 2014 respecto del período previo.

## Datos

- **Fuente principal:** [Desagregación provincial del valor agregado bruto de la Argentina, base 2004 — CEPAL y Ministerio de Economía (MECON)](https://www.cepal.org/es/publicaciones/47900-desagregacion-provincial-valor-agregado-bruto-la-argentina-base-2004). Documento LC/TS.2022/196.
- **Período:** 2004–2024 (21 años).
- **Unidad de análisis:** par provincia–sector, con cobertura de las 24 jurisdicciones argentinas (23 provincias + CABA) y 52 sectores de actividad económica clasificados según CIIU Rev. 3.1.
- **Variable principal:** VAB a precios básicos en millones de pesos constantes de 2004.
- **Estructura final:** panel balanceado de 26.208 observaciones.

## Análisis realizado

1. **Limpieza** del archivo Excel original (24 hojas provinciales con headers no estructurados), conversión a formato tidy con `pivot_longer()` y filtrado de notas al pie y de la fila resumen "VAB a precios básicos".
2. **Análisis exploratorio**: distribución del VAB (media, mediana, asimetría), diagnóstico de calidad (NAs, ceros, outliers) y agregados descriptivos por región y por provincia.
3. **Cálculos principales** (los cuatro métodos definidos en la Instancia 2):
   - **Método 1 — Shares y CAGR:** evolución de la participación regional y tasas de crecimiento anual compuesto.
   - **Método 2 — Test t de Welch:** comparación de la participación patagónica entre los sub-períodos pre-Vaca Muerta (2004–2013) y post-Vaca Muerta (2014–2024). Se reporta además IC al 95% y Cohen's d, dadas las limitaciones de poder estadístico por la baja N.
   - **Método 3 — Correlación de Pearson y Spearman:** asociación entre la participación del sector extractivo de Neuquén y su participación total en el VAB nacional.
   - **Método 4 — Descomposición sectorial del crecimiento:** atribución del crecimiento provincial entre 2004 y 2024 a la contribución de cada sector.
4. **Visualizaciones** con storytelling estilo *Our World in Data*: gráfico comunicacional (línea de tiempo de la participación patagónica) y gráfico exploratorio (mapa coroplético de la variación por provincia).

## Estructura del repositorio

```
proyecto/
├── raw/                # Base original sin modificar (Excel CEPAL-MECON)
├── auxiliar/           # Bases complementarias y materiales de apoyo
├── input/              # Datos procesados, listos para análisis
├── output/
│   ├── tablas/         # Tablas de resultados generadas por los scripts
│   └── graficos/       # Visualizaciones generadas por los scripts
├── script/             # Scripts de R, uno por objetivo específico
│   ├── 01_limpieza.R
│   ├── 02_exploratorio.R
│   ├── 03_analisis.R
│   └── 04_visualizaciones.R
├── utils/              # Funciones propias (un script por función)
└── README.md
```

## Reproducción

### Paquetes necesarios

```r
install.packages(c("tidyverse", "readxl", "janitor", "ggtext",
                   "scales", "sf", "geoAr", "e1071"))
```

### Orden de ejecución

1. `script/01_limpieza.R` — Lee la base de `raw/`, la convierte a formato tidy y genera el archivo procesado en `input/`.
2. `script/02_exploratorio.R` — Análisis descriptivo inicial: estadísticas globales del VAB, diagnóstico de calidad (NAs, ceros, outliers) y agregados por región y provincia. Guarda tablas en `output/tablas/`.
3. `script/03_analisis.R` — Cálculos principales: participaciones y CAGR (Método 1), test t pre vs. post Vaca Muerta (Método 2), correlaciones de Pearson y Spearman (Método 3) y descomposición sectorial del crecimiento (Método 4). Guarda resultados en `output/tablas/`.
4. `script/04_visualizaciones.R` — Genera el gráfico comunicacional (línea de tiempo) y el gráfico exploratorio (mapa coroplético) en `output/graficos/`.

Cada script crea automáticamente las carpetas de salida que necesita, por lo que pueden ejecutarse en orden sin pasos previos manuales.

**Configuración del directorio de trabajo:** los scripts contienen una línea `setwd(...)` con una ruta personal al inicio. Antes de ejecutar, **ajustar esa línea** a la ruta donde se clonó el repositorio, o abrir el proyecto en RStudio mediante un archivo `.Rproj` que setea el directorio automáticamente.

## Conclusiones principales

- La hipótesis principal se sostiene preliminarmente: la Patagonia aumentó su participación en el VAB nacional de **9,59% en 2004 a 9,88% en 2024** (+0,29 puntos porcentuales), creciendo a un CAGR de 1,93% anual, por encima del promedio nacional (1,78%).
- El recorrido no fue lineal: la participación cayó hasta un mínimo histórico de **8,11% en 2011** y luego se recuperó sostenidamente desde 2014, coincidiendo con la intensificación de Vaca Muerta.
- El motor del cambio es **Neuquén**, cuyo VAB extractivo pasó de representar el 31,4% al **52,7% del extractivo nacional** entre 2004 y 2024, con un quiebre marcado en su CAGR extractivo: **–4,31% anual pre-VM (2004–2014) vs. +11,46% anual post-VM (2014–2024)**.
- El resto de la Patagonia (Santa Cruz, Chubut, Río Negro y Tierra del Fuego) **perdió peso** en el VAB extractivo nacional. La transformación regional es entonces de **concentración productiva en Neuquén**, no de expansión patagónica uniforme.
- A nivel inferencial, el test t entre los sub-períodos pre y post Vaca Muerta enfrenta una limitación de poder estadístico (n=10 y n=11), por lo que su interpretación se hace a la luz de la evidencia descriptiva y de la descomposición sectorial, que confirman el rol central del hidrocarburo en el crecimiento neuquino.

## Fuente

Equipo de trabajo de la CEPAL y el Ministerio de Economía de la Argentina (2022). *Desagregación provincial del valor agregado bruto de la Argentina, base 2004*. Documentos de Proyectos (LC/TS.2022/196; LC/BUE/TS.2022/9), Santiago, Comisión Económica para América Latina y el Caribe. Disponible en: https://www.cepal.org/es/publicaciones/47900-desagregacion-provincial-valor-agregado-bruto-la-argentina-base-2004
