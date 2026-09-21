# Grano y cobertura de los datasets (números fijados por los ficheros fuente).
check("causas: 52 prov x 13 causas x 5 años x 2 sexos",
      nrow(causas_provinciales) == 52 * 13 * 5 * 2)
check("causas: 13 capítulos", length(unique(causas_provinciales$Defunción)) == 13)
check("causas: sin Ambos (H+M separados)",
      !any(causas_provinciales$Sexo == "Ambos"))
check("causas: 0 provincias sin comunidad",
      sum(causas_provinciales$Comunidad == "Sin asignar") == 0)
check("funciones: filas y columnas", nrow(copia_func) > 100000 &&
  all(c("Provincia", "Sexo", "Edad", "Funciones", "Año", "Total") %in% names(copia_func)))
check("europa: 35 países", length(unique(europa_agrupada$pais)) == 35)
check("apvp: 19 CCAA", length(unique(apvp_data$Comunidad)) == 19)
check("ccaa_corto cubre las 19",
      all(!is.na(unname(ccaa_corto[normalizar_ccaa(unique(apvp_data$Comunidad))]))))
check("tasas causas finitas", all(is.finite(causas_provinciales$Tasa)))
check("sin intermedios de lectura en memoria",
      !any(c("df_causas_raw", "df_pob_raw", "df_apvp_raw", "df_meses_raw",
             "df_edad_com_raw") %in% ls()))
