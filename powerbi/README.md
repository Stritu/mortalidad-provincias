# Datos para Power BI (derivados de la app Shiny)

Tablas curadas con el mismo pipeline de `R/global.R`: los números coinciden
con la app. Todo en **UTF-8, punto decimal, sin separador de miles**.
Regenerar con `Rscript powerbi/exportar.R` desde la raíz.

## Ficheros y grano

| Fichero | Grano | Filas | Notas |
|---|---|---|---|
| `causas_provinciales.csv` | Provincia × Comunidad × Sexo × Año × Causa | 6.760 | Sin filas "Ambos" (sumar H+M). `Poblacion` repetida por causa: no sumarla tal cual |
| `europa.csv` | País × Sexo × Año × Causa | 9.318 | 35 países, con `Poblacion` y `Tasa_100k` ya calculadas |
| `renta_ccaa.csv` | Comunidad × Año × Indicador | 1.530 | Renta por persona y hogar (2018–2022) |
| `medicos_ccaa.csv` | Comunidad × Año | 95 | Colegiados no jubilados por 100k |
| `apvp_ccaa.csv` | Comunidad × Causa × Indicador × Sexo | 2.308 | Sin desglose provincial (el INE no lo publica) |
| `meses_causa.csv` | Causa × Mes × Sexo × Año | 2.040 | Estacionalidad |

Relaciones sugeridas: `causas_provinciales[Comunidad]` → `renta_ccaa[Comunidad]`,
`causas_provinciales[Provincia]` → tabla de provincias para el mapa (códigos INE).

## Medidas DAX de ejemplo

```dax
Fallecidos = SUM ( causas_provinciales[Fallecidos] )

-- Población SIN duplicar (viene repetida por causa):
Poblacion = SUMX (
    SUMMARIZE ( causas_provinciales, causas_provinciales[Provincia], causas_provinciales[Sexo], causas_provinciales[Anio] ),
    CALCULATE ( MAX ( causas_provinciales[Poblacion] ) )
)

Tasa 100k = DIVIDE ( [Fallecidos], [Poblacion] ) * 100000

% Evitable aprox =
VAR Sensibles = CALCULATE ( [Fallecidos],
    causas_provinciales[Causa] IN {
        "Causas externas de mortalidad",
        "Enfermedades infecciosas y parasitarias",
        "Enfermedades del sistema circulatorio",
        "Enfermedades del sistema genitourinario",
        "Tumores",
        "Enfermedades del sistema respiratorio",
        "Enfermedades del sistema digestivo",
        "Enfermedades endocrinas, nutricionales y metabólicas" } )
RETURN DIVIDE ( Sensibles, [Fallecidos] )
```

> Las cestas evitables son aproximación por capítulos (sin límite <75:
> los ficheros no traen edad), igual que en la app.
