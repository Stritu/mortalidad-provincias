server_resumen <- function(input, output, session) {

  # Accesos directos de la portada a las pestañas principales.
  observeEvent(input$por_ir_causas, {
    bslib::nav_select("nav_principal", selected = "Causas de defunción", session = session)
  })
  observeEvent(input$por_ir_europa, {
    bslib::nav_select("nav_principal", selected = "Europa", session = session)
  })
  observeEvent(input$por_ir_exceso, {
    bslib::nav_select("nav_principal", selected = "Exceso de mortalidad", session = session)
  })
  observeEvent(input$por_ir_determinantes, {
    bslib::nav_select("nav_principal", selected = "Determinantes", session = session)
  })
  observeEvent(input$por_ir_multi, {
    bslib::nav_select("nav_principal", selected = "Análisis multivariante", session = session)
  })

  # Índice de la portada: cada enlace salta a su pestaña y subpestaña.
  destinos_indice <- list(
    list("ir_c_prov", "Causas de defunción", "nav_causas", "Análisis provincial"),
    list("ir_c_est", "Causas de defunción", "nav_causas", "Estacionalidad"),
    list("ir_c_evo", "Causas de defunción", "nav_causas", "Evolución temporal"),
    list("ir_c_comp", "Causas de defunción", "nav_causas", "Comparador de Provincias"),
    list("ir_c_apvp", "Causas de defunción", "nav_causas", "Mortalidad prematura (APVP)"),
    list("ir_c_std", "Causas de defunción", "nav_causas", "Tasas estandarizadas por edad"),
    list("ir_c_edad", "Causas de defunción", "nav_causas", "Edad y mes"),
    list("ir_c_evit", "Causas de defunción", "nav_causas", "Mortalidad evitable"),
    list("ir_c_des", "Causas de defunción", "nav_causas", "Desigualdad territorial"),
    list("ir_c_al", "Causas de defunción", "nav_causas", "Alertas de atípicos"),
    list("ir_c_cc", "Causas de defunción", "nav_causas", "Comparador de CCAA"),
    list("ir_c_inf", "Causas de defunción", "nav_causas", "Informes"),
    list("ir_m_ana", "Métricas demográficas", "nav_metricas", "Análisis demográfico"),
    list("ir_m_comp", "Métricas demográficas", "nav_metricas", "Comparador demográfico"),
    list("ir_m_pir", "Métricas demográficas", "nav_metricas", "Pirámide de población"),
    list("ir_m_env", "Métricas demográficas", "nav_metricas", "Envejecimiento y dependencia"),
    list("ir_m_bre", "Métricas demográficas", "nav_metricas", "Brecha de género"),
    list("ir_m_evo", "Métricas demográficas", "nav_metricas", "Evolución temporal"),
    list("ir_m_ev", "Métricas demográficas", "nav_metricas", "Esperanza de vida"),
    list("ir_e_mapa", "Europa", "nav_europa", "Mapa europeo"),
    list("ir_e_comp", "Europa", "nav_europa", "Comparador de países europeos"),
    list("ir_e_mod", "Europa", "nav_europa", "Comparación estadística europea"),
    list("ir_e_ef", "Europa", "nav_europa", "Modelo europeo: efectos fijos"),
    list("ir_d_renta", "Determinantes", "nav_determinantes", "Renta y médicos"),
    list("ir_d_idx", "Determinantes", "nav_determinantes", "Índice sintético"),
    list("ir_r_causas", "Regresiones lineales", "nav_regresiones", "Regresión entre causas"),
    list("ir_ex", "Exceso de mortalidad", NA, NA),
    list("ir_mult", "Análisis multivariante", NA, NA)
  )
  for (d in destinos_indice) {
    local({
      dd <- d
      observeEvent(input[[dd[[1]]]], {
        bslib::nav_select("nav_principal", selected = dd[[2]], session = session)
        if (!is.na(dd[[3]])) bslib::nav_select(dd[[3]], selected = dd[[4]], session = session)
      }, ignoreInit = TRUE)
    })
  }

  # Titulares calculados con los datos (2022, todas las causas, ambos sexos).
  output$por_titulares <- renderUI({
    c22 <- causas_provinciales %>% filter(Año_Num == 2022)
    if (!nrow(c22)) return(HTML("<p>Sin datos de 2022.</p>"))
    tot <- sum(c22$Fallecidos, na.rm = TRUE)
    pob <- c22 %>% distinct(Provincia, Sexo, Poblacion) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>% pull(P)
    tasa <- if (length(pob) && pob > 0) tot / pob * 100000 else NA_real_
    top_c <- c22 %>% group_by(Defunción) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(F)) %>% slice(1)
    pob_p <- c22 %>% distinct(Provincia, Sexo, Poblacion) %>%
      group_by(Provincia) %>% summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    top_p <- c22 %>% group_by(Provincia) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob_p, by = "Provincia") %>%
      mutate(T = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(T)) %>% arrange(desc(T)) %>% slice(1)
    ev <- tryCatch(
      funciones_provinciales %>%
        filter(Funciones == "Esperanza de vida", Año == "2022", Sexo == "Ambos",
               as.character(Edad) == "0") %>%
        arrange(desc(Valor)) %>% slice(1),
      error = function(e) NULL
    )
    fmt <- function(x, d = 1) format(round(x, d), big.mark = ".", decimal.mark = ",")
    items <- c(
      sprintf("<li>En 2022 fallecieron <b>%s personas</b> (tasa de %s por 100k).</li>",
              fmt(tot, 0), fmt(tasa)),
      if (nrow(top_c)) sprintf("<li>Primera causa: <b>%s</b> (%s defunciones, %s %% del total).</li>",
                               htmltools::htmlEscape(top_c$Defunción[1]),
                               fmt(top_c$F[1], 0), fmt(top_c$F[1] / tot * 100)) else NULL,
      if (nrow(top_p)) sprintf("<li>Mayor tasa provincial: <b>%s</b> (%s por 100k).</li>",
                               htmltools::htmlEscape(top_p$Provincia[1]), fmt(top_p$T[1])) else NULL,
      if (!is.null(ev) && nrow(ev)) sprintf("<li>Mayor esperanza de vida al nacer: <b>%s</b> (%s años).</li>",
                                            htmltools::htmlEscape(ev$Provincia[1]),
                                            fmt(ev$Valor[1])) else NULL
    )
    HTML(paste0("<ul class='mb-0'>", paste(items, collapse = ""), "</ul>"))
  })

  # --- RESUMEN GENERAL SERVER ---
  # FIX: causas_provinciales solo trae Hombres/Mujeres (sin filas "Ambos"),
  # así que el total se obtiene sumando ambos sexos en lugar de filtrar
  # Sexo == "Ambos" (el filtro devolvía 0 filas y los KPI mostraban "-").
  output$res_kpi_tasa <- renderText({
    # FIX: la población se repite en cada causa; se suma una sola vez
    # (antes el denominador contaba 13 veces y la tasa salía ~13× menor).
    df <- causas_provinciales %>% filter(Año_Num == 2022)
    if (nrow(df) == 0) return("-")
    pob <- df %>% distinct(Provincia, Sexo, Año, Poblacion) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      pull(P)
    if (!length(pob) || !is.finite(pob) || pob <= 0) return("-")
    paste0(format(round(sum(df$Fallecidos, na.rm = TRUE) / pob * 100000, 2), decimal.mark = ","), " / 100k")
  })

  output$res_kpi_causa <- renderText({
    df <- causas_provinciales %>% filter(Año_Num == 2022, Defunción != "Total") %>%
      group_by(Defunción) %>% summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Fallecidos)) %>% slice(1)
    if (nrow(df) == 0) return("-")
    as.character(df$Defunción)
  })

  output$res_kpi_provincia <- renderText({
    # FIX: igual que res_kpi_tasa, la población se suma una sola vez.
    df <- causas_provinciales %>% filter(Año_Num == 2022)
    if (nrow(df) == 0) return("-")
    pob <- df %>% distinct(Provincia, Sexo, Año, Poblacion) %>%
      group_by(Provincia) %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    df <- df %>%
      group_by(Provincia) %>%
      summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = "Provincia") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      arrange(desc(Tasa)) %>%
      slice(1)
    if (nrow(df) == 0) return("-")
    paste0(df$Provincia, " (", format(round(df$Tasa, 2), decimal.mark = ","), " / 100k)")
  })

  output$res_kpi_europa <- renderText({
    tryCatch({
      df <- europa_agrupada %>% filter(anio == max(anio, na.rm = TRUE), sexo == "Ambos", causa_grupo == "Total") %>%
        group_by(pais) %>% summarise(Tasa = sum(defunciones, na.rm = TRUE) / sum(poblacion, na.rm = TRUE) * 100000, .groups = "drop") %>%
        filter(is.finite(Tasa))
      # FIX: la columna pais está en inglés ("Spain", no "España").
      esp <- df %>% filter(pais == "Spain") %>% pull(Tasa)
      if (length(esp) == 0) return("Disponible en Comparación europea")
      paste0("España: ", format(round(esp[1], 2), decimal.mark = ","), " / 100k")
    }, error = function(e) "Disponible en Comparación europea")
  })

  output$res_apvp <- renderPlotly({
    tryCatch({
      # FIX: apvp_data solo trae Hombres/Mujeres; se suman ambos sexos.
      df <- apvp_data %>% filter(Indicador == "Nº de APVP") %>%
        group_by(Causa) %>%
        summarise(APVP = sum(Valor, na.rm = TRUE), .groups = "drop") %>% arrange(desc(APVP)) %>% slice_head(n = 8) %>%
        mutate(Causa = stringr::str_wrap(Causa, width = 32))
      plot_ly(df, x = ~APVP, y = ~reorder(Causa, APVP), type = "bar", orientation = "h",
              marker = list(color = ~APVP, colorscale = "Greens", showscale = FALSE,
                            line = list(color = "rgba(0,0,0,0.15)", width = 1)),
              text = ~format(round(APVP), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE),
              textposition = "outside",
              hovertemplate = "<b>%{y}</b><br>APVP: %{x:,.0f}<extra></extra>") %>%
        layout(xaxis = list(title = "APVP"), yaxis = list(title = "", automargin = TRUE),
               hoverlabel = list(bgcolor = "white"),
               margin = list(l = 220, r = 60, t = 20, b = 50))
    }, error = function(e) plotly_empty())
  })
  
  output$res_europa <- renderPlotly({
    tryCatch({
      df <- europa_agrupada %>% filter(anio == max(anio, na.rm = TRUE), sexo == "Ambos", causa_grupo == "Total") %>%
        group_by(pais) %>% summarise(Tasa = sum(defunciones, na.rm = TRUE) / sum(poblacion, na.rm = TRUE) * 100000, .groups = "drop") %>%
        filter(is.finite(Tasa)) %>% arrange(desc(Tasa)) %>% slice_head(n = 10)
      plot_ly(df, x = ~Tasa, y = ~reorder(pais, Tasa), type = "bar", orientation = "h",
              marker = list(color = ~Tasa, colorscale = "Blues", showscale = FALSE,
                            line = list(color = "rgba(0,0,0,0.15)", width = 1)),
              hovertemplate = "<b>%{y}</b><br>Tasa de mortalidad: %{x:.1f} por 100.000 hab.<extra></extra>") %>%
        layout(xaxis = list(title = "Tasa de mortalidad por 100.000"), yaxis = list(title = "", automargin = TRUE),
               margin = list(l = 150, r = 30, t = 20, b = 50))
    }, error = function(e) plotly_empty())
  })
  
  # --- DISTRIBUCIÓN POR EDAD Y SEXO (RESUMEN GENERAL) SERVER ---
  output$ped_edad_sexo <- renderPlotly({
    # Pirámide fija con ambos sexos: hombres a la izquierda, mujeres a la derecha.
    # La fuente agrega todo el periodo 2018-2022 (sin desglose anual).
    df <- edad_com_data %>%
      group_by(Edad, Sexo) %>%
      summarise(Valor = sum(Defunciones, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Valor), Sexo %in% c("Hombres", "Mujeres"))
    req(nrow(df) > 0)
    df$Edad <- factor(df$Edad, levels = orden_edad_com)
    df_h <- df %>% filter(Sexo == "Hombres")
    df_m <- df %>% filter(Sexo == "Mujeres")
    max_v <- max(df$Valor, na.rm = TRUE) * 1.1
    ticks <- seq(-round(max_v), round(max_v), length.out = 7)
    plot_ly() %>%
      add_trace(
        data = df_h, x = ~-Valor, y = ~Edad,
        type = "bar", orientation = "h",
        name = "Hombres", marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = "<b>Edad: %{y}</b><br>Hombres: %{customdata:,.0f}<extra></extra>"
      ) %>%
      add_trace(
        data = df_m, x = ~Valor, y = ~Edad,
        type = "bar", orientation = "h",
        name = "Mujeres", marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
        hovertemplate = "<b>Edad: %{y}</b><br>Mujeres: %{x:,.0f}<extra></extra>"
      ) %>%
      layout(
        barmode = "overlay",
        bargap = 0.1,
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(
          title = "Defunciones",
          range = c(-max_v, max_v),
          tickmode = "array",
          tickvals = ticks,
          ticktext = format(abs(round(ticks)), big.mark = ".", decimal.mark = ",", trim = TRUE)
        ),
        yaxis = list(title = "Tramo de edad", categoryorder = "array", categoryarray = orden_edad_com),
        legend = list(orientation = "h", x = 0.35, y = 1.05),
        margin = list(l = 100, r = 30, t = 30, b = 60)
      )
  })
}
