# Exporta tablas curadas para Power BI (UTF-8, punto decimal, sin miles).
# Uso desde la raíz:  Rscript powerbi/exportar.R
# Salen de los mismos objetos que usa la app Shiny: los números coinciden.
suppressPackageStartupMessages({library(dplyr)})
invisible(capture.output(source("R/global.R")))
dir.create("powerbi", showWarnings = FALSE)

# 1. Causas por provincia (grano: Provincia x Comunidad x Sexo x Año x Causa).
#    OJO: sin filas "Ambos" (H+M separados); Poblacion repetida por causa.
causas <- causas_provinciales %>%
  transmute(Provincia, Comunidad, Sexo, Anio = Año_Num, Causa = Defunción,
            Fallecidos, Poblacion, Tasa_100k = Tasa)
utils::write.csv(causas, "powerbi/causas_provinciales.csv", row.names = FALSE,
                 fileEncoding = "UTF-8")

# 2. Europa (grano: Pais x Sexo x Año x Grupo de causa; con población y tasa).
eur <- europa_agrupada %>%
  transmute(Pais = pais, Sexo = sexo, Anio = anio, Causa = causa_grupo,
            Defunciones = defunciones, Poblacion = poblacion, Tasa_100k = tasa_100k)
utils::write.csv(eur, "powerbi/europa.csv", row.names = FALSE,
                 fileEncoding = "UTF-8")

# 3. Renta por CCAA (tal cual, ya limpia en global.R si existe).
if (exists("renta_data")) {
  utils::write.csv(renta_data, "powerbi/renta_ccaa.csv", row.names = FALSE,
                   fileEncoding = "UTF-8")
}

# 4. Médicos por CCAA y año (combinado 2018-2022).
if (exists("medicos_data")) {
  utils::write.csv(medicos_data, "powerbi/medicos_ccaa.csv", row.names = FALSE,
                   fileEncoding = "UTF-8")
}

# 5. APVP por CCAA (grano: Comunidad x Causa x Indicador x Sexo; sin provincias).
if (exists("apvp_data")) {
  utils::write.csv(apvp_data, "powerbi/apvp_ccaa.csv", row.names = FALSE,
                   fileEncoding = "UTF-8")
}

# 6. Estacionalidad mensual por causa (grano: Causa x Mes x Sexo x Año).
if (exists("meses_data")) {
  utils::write.csv(meses_data, "powerbi/meses_causa.csv", row.names = FALSE,
                   fileEncoding = "UTF-8")
}

fi <- file.info(list.files("powerbi", pattern = "[.]csv$", full.names = TRUE))
print(data.frame(fichero = rownames(fi), MB = round(fi$size / 1024^2, 2),
                 filas = vapply(rownames(fi), function(f) length(readLines(f, warn = FALSE)) - 1,
                                numeric(1)), row.names = NULL))
