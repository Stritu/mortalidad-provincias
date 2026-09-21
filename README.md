# Análisis de defunciones — España y Europa

[![tests](https://github.com/Stritu/mortalidad-provincias/actions/workflows/tests.yml/badge.svg)](https://github.com/Stritu/mortalidad-provincias/actions)

App Shiny de mortalidad con datos del INE y Eurostat: 52 provincias × 13 capítulos de causa × 5 años (2018–2022), exceso 2009–2024 y comparación con 35 países europeos.

**Demo:** https://stritu.shinyapps.io/mortalidad-provincias/

## Qué incluye

- **Resumen general**: portada con hero, titulares calculados y accesos directos.
- **Causas de defunción**: mapa provincial, estacionalidad, evolución, comparador de provincias y de CCAA, APVP, tasas estandarizadas (ESP-2013), edad y mes, mortalidad evitable, desigualdad territorial (Gini), alertas de atípicos e informes Excel por territorio.
- **Métricas demográficas**: pirámide, envejecimiento, brecha de género, esperanza de vida.
- **Europa**: mapa, comparador de países, comparación estadística y modelo de efectos fijos con errores cluster por país.
- **Determinantes**: renta, médicos e índice sintético por CCAA.
- **Regresiones**: entre causas con supuestos, Moran y matriz de validez.
- **Exceso de mortalidad**: P-score vs media 2015–2019, anual y mensual.
- **Multivariante**: PCA + k-means con codo, silueta y perfiles.

## Métodos

Tasas ponderadas por población; ESP-2013 para estandarizar; Gini ponderado; z-scores temporal (|z|≥2) y transversal (|z|≥2,5); k-means con semilla fija; efectos fijos con SE robustas clusterizadas (Arellano). Sin límite <75 en mortalidad evitable (los ficheros no traen edad): aproximación por capítulos documentada en la app.

## Estructura

- `app.R`: entrada (171 líneas).
- `R/global.R`: datos, cachés en `cache/*.rds` y utilidades.
- `R/ui_*` / `R/server_*`: una pestaña por fichero.
- Arranque ~4 s en caliente (cachés en disco).

## Ejecutar en local

```r
# R ≥ 4.6, trabajar desde la carpeta del proyecto
shiny::runApp()
```

Los CSV de origen van en la raíz (`causas_defunciones.csv`, `funciones.csv`, `europa.csv`, …). La primera ejecución genera `cache/`.

> `funciones.csv` (70 MB) y `defunciones_edad_provincia.csv` (146 MB) superan el límite de GitHub y no viajan en el repo: la app arranca igualmente desde `cache/*.rds` (incluidos). Solo hacen falta si quieres regenerar las cachés desde cero (fuente: INE).

## Tests

```sh
Rscript tests/run.R
```

19 comprobaciones (grano de datos, KPIs 2022, Gini, modelos) sin dependencias extra. También corren en CI con GitHub Actions.
