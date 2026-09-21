server_europa <- function(input, output, session) {

  # --- PESTAÑA 4: EUROPA SERVER ---
  observe({
    req(nrow(europa_agrupada) > 0)
    
    anios_eur <- sort(unique(europa_agrupada$anio), decreasing = TRUE)
    causas_eur <- causas_europa_raw
    sexos_eur <- sexos_europa_raw
    paises_eur <- paises_europa_raw
    
    updateSelectInput(
      session, "eur_anio",
      choices = as.character(anios_eur),
      selected = if (length(anios_eur)) as.character(anios_eur[1]) else NULL
    )
    updateSelectInput(
      session, "eur_causa",
      choices = causas_europa_es,
      selected = if (length(causas_eur)) causas_eur[1] else character(0)
    )
    updateRadioButtons(
      session, "eur_sexo",
      choices = sexos_europa_es,
      selected = if ("Ambos" %in% sexos_eur) "Ambos" else sexos_eur[1]
    )
    
    updateSelectInput(
      session, "eur_comp_anio",
      choices = as.character(anios_eur),
      selected = if (length(anios_eur)) as.character(anios_eur[1]) else NULL
    )
    updateSelectInput(
      session, "eur_comp_causa",
      choices = causas_europa_es,
      selected = if (length(causas_eur)) causas_eur[1] else character(0)
    )
    updateRadioButtons(
      session, "eur_comp_sexo",
      choices = sexos_europa_es,
      selected = if ("Ambos" %in% sexos_eur) "Ambos" else sexos_eur[1]
    )
    pais_a_inicial <- if ("Spain" %in% paises_eur) "Spain" else paises_eur[1]
    pais_b_inicial <- if ("France" %in% paises_eur) "France" else if (length(paises_eur) > 1) paises_eur[2] else paises_eur[1]
    
    updateSelectInput(
      session, "eur_comp_pais_a",
      label = paste0("País: ", traducir_pais(pais_a_inicial)),
      choices = paises_europa_es,
      selected = pais_a_inicial
    )
    updateSelectInput(
      session, "eur_comp_pais_b",
      label = paste0("País: ", traducir_pais(pais_b_inicial)),
      choices = paises_europa_es,
      selected = pais_b_inicial
    )
  })
  
  observeEvent(input$eur_comp_pais_a, {
    req(input$eur_comp_pais_a)
    updateSelectInput(session, "eur_comp_pais_a",
                      label = paste0("País: ", traducir_pais(input$eur_comp_pais_a)))
  }, ignoreInit = TRUE)
  
  observeEvent(input$eur_comp_pais_b, {
    req(input$eur_comp_pais_b)
    updateSelectInput(session, "eur_comp_pais_b",
                      label = paste0("País: ", traducir_pais(input$eur_comp_pais_b)))
  }, ignoreInit = TRUE)
  
  datos_europa_filtrados <- reactive({
    req(input$eur_causa, input$eur_sexo, input$eur_anio)
    
    europa_agrupada %>%
      filter(
        causa_grupo == input$eur_causa,
        sexo == input$eur_sexo,
        anio == as.numeric(input$eur_anio)
      )
  }) %>% bindCache(input$eur_causa, input$eur_sexo, input$eur_anio)
  
  output$eur_filtro_info <- renderUI({
    req(input$eur_anio, input$eur_causa, input$eur_sexo)
    anio_visible <- input$eur_anio
    div(
      class = "filter-help",
      HTML(paste0(
        "<b>Selección:</b> ", htmltools::htmlEscape(input$eur_sexo),
        " · ", htmltools::htmlEscape(input$eur_causa),
        " · ", htmltools::htmlEscape(anio_visible),
        ". Las categorías agregadas priorizan la categoría madre de Eurostat cuando está disponible para evitar doble conteo."
      ))
    )
  })
  
  output$eur_kpi_total <- renderText({
    total <- sum(datos_europa_filtrados()$defunciones, na.rm = TRUE)
    if (!is.finite(total)) return("-")
    format(round(total), big.mark = ".", decimal.mark = ",")
  })
  
  output$eur_kpi_max <- renderText({
    top_pais <- datos_europa_filtrados() %>%
      group_by(pais) %>%
      summarise(tasa = sum(defunciones, na.rm = TRUE) / sum(poblacion, na.rm = TRUE) * 100000,
                .groups = "drop") %>%
      filter(is.finite(tasa)) %>%
      slice_max(tasa, n = 1, with_ties = FALSE)
    if (nrow(top_pais)) paste0(traducir_pais(top_pais$pais), " (", format(round(top_pais$tasa, 1), decimal.mark = ","), ")") else "Sin datos"
  })
  
  output$eur_kpi_sexo <- renderText({ input$eur_sexo })
  
  output$eur_plot_barras <- renderPlotly({
    req(input$eur_causa, input$eur_sexo, input$eur_anio)
    
    df_base <- europa_agrupada %>%
      filter(causa_grupo == input$eur_causa, sexo == input$eur_sexo) %>%
      group_by(anio, pais) %>%
      summarise(
        defunciones = sum(defunciones, na.rm = TRUE),
        poblacion = max(poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(is.finite(defunciones), is.finite(poblacion), poblacion > 0) %>%
      mutate(tasa_100k = defunciones / poblacion * 100000)
    
    validate(need(nrow(df_base) > 0, "No hay datos de población para la selección actual."))
    
    anio_ref <- as.numeric(input$eur_anio)
    
    ranking_ref <- df_base %>%
      filter(anio == anio_ref) %>%
      arrange(desc(tasa_100k))
    
    validate(need(nrow(ranking_ref) > 0, "No hay países con población y defunciones para el año seleccionado."))
    
    n_grupo <- min(5, floor(nrow(ranking_ref) / 2))
    validate(need(n_grupo >= 1, "No hay suficientes países para construir la comparación."))
    
    top5 <- ranking_ref %>% slice_head(n = n_grupo) %>% mutate(grupo = "5 países más afectados")
    bottom5 <- ranking_ref %>% slice_tail(n = n_grupo) %>% mutate(grupo = "5 países menos afectados")
    seleccion <- bind_rows(top5, bottom5)
    
    df_evol <- df_base %>%
      filter(pais %in% seleccion$pais) %>%
      mutate(
        grupo = if_else(pais %in% top5$pais, "5 países más afectados", "5 países menos afectados"),
        pais_es = traducir_pais(pais)
      ) %>%
      left_join(seleccion %>% select(pais, tasa_ref = tasa_100k), by = "pais") %>%
      mutate(
        tooltip = paste0(
          "<b>", pais_es, "</b>",
          "<br>Año: ", anio,
          "<br>Tasa: ", format(round(tasa_100k, 1), decimal.mark = ","), " por 100.000 hab.",
          "<br>Defunciones: ", format(round(defunciones), big.mark = ".", decimal.mark = ","),
          "<br>Población: ", format(round(poblacion), big.mark = ".", decimal.mark = ",")
        )
      )
    
    # Media europea ponderada por población: defunciones totales / población total.
    media_europea <- df_base %>%
      group_by(anio) %>%
      summarise(
        defunciones = sum(defunciones, na.rm = TRUE),
        poblacion = sum(poblacion, na.rm = TRUE),
        tasa_100k = if_else(poblacion > 0, defunciones / poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(tasa_100k)) %>%
      mutate(
        pais_es = "Media europea",
        grupo = "Media europea",
        tooltip = paste0(
          "<b>Media europea</b>",
          "<br>Año: ", anio,
          "<br>Tasa: ", format(round(tasa_100k, 1), decimal.mark = ","), " por 100.000 hab."
        )
      )
    
    colores_base <- grDevices::colorRampPalette(c("#440154", "#31688E", "#35B779", "#FDE725"))(256)
    val_min <- min(seleccion$tasa_100k, na.rm = TRUE)
    val_max <- max(seleccion$tasa_100k, na.rm = TRUE)
    if (!is.finite(val_min) || !is.finite(val_max) || val_min == val_max) {
      val_min <- 0
      val_max <- max(1, val_max)
    }
    idx <- floor((seleccion$tasa_100k - val_min) / (val_max - val_min) * 255) + 1
    idx <- pmax(1, pmin(256, idx))
    colores_paises <- setNames(colores_base[idx], traducir_pais(seleccion$pais))
    
    # La media se dibuja en gris oscuro para distinguirla de los países.
    colores <- c(colores_paises, "Media europea" = "#2c3e50")
    
    df_evol$pais_es <- factor(df_evol$pais_es, levels = c(rev(traducir_pais(top5$pais)), rev(traducir_pais(bottom5$pais))))
    
    plot_ly() %>%
      add_trace(
        data = df_evol,
        x = ~anio, y = ~tasa_100k,
        color = ~pais_es, colors = colores,
        type = "scatter", mode = "lines+markers",
        line = list(width = 2.2), marker = list(size = 6),
        text = ~tooltip, hovertemplate = "%{text}<extra></extra>"
      ) %>%
      add_trace(
        data = media_europea,
        x = ~anio, y = ~tasa_100k,
        name = "Media europea",
        type = "scatter", mode = "lines+markers",
        line = list(color = "#2c3e50", width = 3, dash = "dash"),
        marker = list(color = "#2c3e50", size = 7),
        text = ~tooltip, hovertemplate = "%{text}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
        yaxis = list(title = "Tasa de defunción por 100.000 habitantes"),
        hovermode = "x unified",
        legend = list(title = list(text = "País"), orientation = "v", x = 1.02, y = 1),
        margin = list(l = 70, r = 175, t = 35, b = 45),
        annotations = list(list(
          x = 0, y = 1.08, xref = "paper", yref = "paper",
          text = paste0("5 más afectadas + 5 menos afectadas · Media europea · referencia: ", anio_ref),
          showarrow = FALSE, xanchor = "left"
        ))
      )
  })
  
  output$eur_mapa <- renderLeaflet({
    datos_valores <- datos_europa_filtrados() %>%
      group_by(pais) %>%
      summarise(
        defunciones = sum(defunciones, na.rm = TRUE),
        poblacion = max(poblacion, na.rm = TRUE),
        tasa_100k = if_else(poblacion > 0, defunciones / poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      left_join(pais_eurostat_mapa, by = "pais") %>%
      filter(!is.na(nombre_mapa), is.finite(tasa_100k))
    
    datos_mapa <- mapa_europa %>%
      left_join(datos_valores %>% select(nombre_mapa, tasa_100k), by = "nombre_mapa") %>%
      mutate(pais_es = traducir_pais(coalesce(nombre_mapa, "Europa")))
    
    val_max <- max(datos_mapa$tasa_100k, na.rm = TRUE)
    if (!is.finite(val_max) || val_max <= 0) val_max <- 1
    
    pal <- colorNumeric(palette = "viridis", domain = c(0, val_max), na.color = "#D9D9D9")
    
    etiquetas <- sprintf(
      "<strong>%s</strong><br/>%s",
      datos_mapa$pais_es,
      ifelse(is.na(datos_mapa$tasa_100k),
             "Sin datos de población",
              paste0("Tasa de defunción: ", format(round(datos_mapa$tasa_100k, 1), decimal.mark = ","), " por 100.000 hab."))
    ) %>% lapply(htmltools::HTML)
    
    leaflet(datos_mapa) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(
        fillColor = ~pal(tasa_100k), weight = 1, color = "white", fillOpacity = 0.82,
        label = etiquetas,
        highlightOptions = highlightOptions(weight = 2, color = "#2c3e50", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~tasa_100k, opacity = 0.85,
                title = "Tasa de defunción / 100.000 hab.", position = "bottomright") %>%
      fitBounds(lng1 = -12, lat1 = 34, lng2 = 45, lat2 = 72) %>%
      control_ano(input$eur_anio)
  })
  
  # ---------------------------------------------------------------------------
  # RANKING EUROPEO POR GÉNERO
  # Siempre muestra Hombres y Mujeres para el año seleccionado en el panel.
  # ---------------------------------------------------------------------------
  datos_europa_ranking_causas <- reactive({
    req(input$eur_anio)
    
    anio_sel <- as.character(input$eur_anio)
    
    # OPT: usa sexo_norm precomputado en europa_agrupada.
    base <- europa_agrupada %>%
      mutate(Sexo = sexo_norm) %>%
      filter(Sexo %in% c("Hombres", "Mujeres"), causa_grupo != "Total")
    
    if (anio_sel != "Todos los años") {
      base <- base %>% filter(anio == as.numeric(anio_sel))
    }
    
    base %>%
      group_by(Sexo, causa_grupo) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = sum(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa), is.finite(Defunciones), is.finite(Poblacion), Tasa >= 0)
  })
  
  output$eur_3_causas_top <- renderPlotly({
    df_base <- datos_europa_ranking_causas()
    
    df <- df_base %>%
      group_by(Sexo) %>%
      arrange(desc(Tasa), .by_group = TRUE) %>%
      slice_head(n = 4) %>%
      ungroup() %>%
      mutate(
        Causa = as.character(causa_grupo),
        Genero = factor(Sexo, levels = c("Hombres", "Mujeres"))
      )
    
    validate(need(nrow(df) >= 2, "No hay datos suficientes para el ranking europeo por género."))
    
    plot_ly(
      df,
      x = ~Tasa,
      y = ~Causa,
      color = ~Genero,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "Tasa: %{x:.1f} por 100.000 habitantes<extra>%{fullData.name}</extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de defunción del grupo (por 100.000 hab.)"),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(orientation = "h", x = 0.25, y = 1.15),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 230, r = 20, t = 45, b = 45)
      )
  })
  
  output$eur_3_causas_bottom <- renderPlotly({
    df_base <- datos_europa_ranking_causas()
    
    df <- df_base %>%
      filter(Tasa > 0) %>%
      group_by(Sexo) %>%
      arrange(Tasa, .by_group = TRUE) %>%
      slice_head(n = 4) %>%
      ungroup() %>%
      mutate(
        Causa = as.character(causa_grupo),
        Genero = factor(Sexo, levels = c("Hombres", "Mujeres"))
      )
    
    validate(need(nrow(df) >= 2, "No hay suficientes datos positivos para el ranking europeo por género."))
    
    plot_ly(
      df,
      x = ~Tasa,
      y = ~Causa,
      color = ~Genero,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "Tasa: %{x:.1f} por 100.000 habitantes<extra>%{fullData.name}</extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de defunción del grupo (por 100.000 hab.)"),
        yaxis = list(title = "", automargin = TRUE),
        legend = list(orientation = "h", x = 0.25, y = 1.15),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 230, r = 20, t = 45, b = 45)
      )
  })
  
  output$eur_tabla <- DT::renderDT({
    df <- datos_europa_filtrados() %>%
      transmute(
        Sexo = sexo,
        `Causa / grupo` = causa_grupo,
        País = traducir_pais(pais),
        Año = anio,
        Población = poblacion,
        Defunciones = defunciones,
        `Tasa de defunción por 100.000 hab.` = tasa_100k
      ) %>%
      arrange(desc(Año), País)
    
    DT::datatable(
      df,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound("Tasa de defunción por 100.000 hab.", 1, dec.mark = ",", mark = ".")
  })
  
  datos_europa_comparador <- reactive({
    req(input$eur_comp_anio, input$eur_comp_causa, input$eur_comp_sexo,
        input$eur_comp_pais_a, input$eur_comp_pais_b)
    europa_agrupada %>%
      filter(
        causa_grupo == input$eur_comp_causa,
        sexo == input$eur_comp_sexo,
        pais %in% c(input$eur_comp_pais_a, input$eur_comp_pais_b)
      ) %>%
      group_by(anio, pais) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = max(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa))
  })
  
  output$eur_comp_filtro_info <- renderUI({
    req(input$eur_comp_anio, input$eur_comp_causa, input$eur_comp_sexo,
        input$eur_comp_pais_a, input$eur_comp_pais_b)
    div(
      class = "filter-help",
      HTML(paste0(
        "<b>Comparación:</b> ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_a)),
        " vs. ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_b)),
        " · ", htmltools::htmlEscape(input$eur_comp_causa),
        " · ", htmltools::htmlEscape(input$eur_comp_sexo),
        " · población: ", htmltools::htmlEscape(input$eur_comp_anio), "."
      ))
    )
  })
  
  output$eur_comp_evol_nacional <- renderPlotly({
    req(input$eur_comp_pais_a, input$eur_comp_pais_b, input$eur_comp_causa, input$eur_comp_sexo)
    
    df_paises <- europa_agrupada %>%
      filter(
        causa_grupo == input$eur_comp_causa,
        sexo == input$eur_comp_sexo,
        pais %in% c(input$eur_comp_pais_a, input$eur_comp_pais_b)
      ) %>%
      group_by(anio, pais) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = max(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa))
    
    df_media <- europa_agrupada %>%
      filter(causa_grupo == input$eur_comp_causa, sexo == input$eur_comp_sexo) %>%
      group_by(anio) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = sum(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa))
    
    validate(need(nrow(df_paises) > 0, "No hay datos disponibles para los países seleccionados."))
    
    p <- plot_ly()
    
    df_a <- df_paises %>% filter(pais == input$eur_comp_pais_a) %>% mutate(País = traducir_pais(pais))
    if (nrow(df_a) > 0) {
      p <- p %>% add_trace(
        data = df_a, x = ~anio, y = ~Tasa, name = unique(df_a$País),
        type = "scatter", mode = "lines+markers",
        line = list(color = "#1f77b4", width = 3),
        marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
        text = ~paste0("<b>", País, "</b><br>Año: ", anio,
                       "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ",")),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
    
    df_b <- df_paises %>% filter(pais == input$eur_comp_pais_b) %>% mutate(País = traducir_pais(pais))
    if (nrow(df_b) > 0) {
      p <- p %>% add_trace(
        data = df_b, x = ~anio, y = ~Tasa, name = unique(df_b$País),
        type = "scatter", mode = "lines+markers",
        line = list(color = "#ff7f0e", width = 3),
        marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
        text = ~paste0("<b>", País, "</b><br>Año: ", anio,
                       "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ",")),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
    
    p <- p %>% add_trace(
      data = df_media, x = ~anio, y = ~Tasa, name = "Tasa media europea",
      type = "scatter", mode = "lines+markers",
      line = list(color = "#2c3e50", width = 2.5, dash = "dash"),
      marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
      text = ~paste0("<b>Tasa media europea</b><br>Año: ", anio,
                     "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ",")),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
        yaxis = list(title = "Tasa de defunción del grupo (por 100k hab.)", showgrid = TRUE, gridcolor = "#E5E5E5"),
        hovermode = "x unified",
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.2, y = 1.12),
        margin = list(l = 60, r = 20, t = 15, b = 40)
      )
  })
  
  # ---------------------------------------------------------------------------
  # Radares comparativos de países europeos.
  # Se mantienen junto a los dashboards de población y la evolución temporal.
  # ---------------------------------------------------------------------------
  preparar_radar_europa_comparador <- function(pais_sel, anio_sel, sexo_sel, extremos = "top") {
    req(pais_sel, sexo_sel)
    
    df <- europa_agrupada %>%
      filter(
        pais == pais_sel,
        sexo == sexo_sel,
        causa_grupo != "Total"
      )
    
    if (!is.null(anio_sel) && !is.na(anio_sel) && anio_sel != "Todos los años") {
      df <- df %>% filter(anio == as.numeric(anio_sel))
    }
    
    df <- df %>%
      group_by(causa_grupo) %>%
      summarise(
        Defunciones = sum(defunciones, na.rm = TRUE),
        Poblacion = sum(poblacion, na.rm = TRUE),
        Tasa = if_else(Poblacion > 0, Defunciones / Poblacion * 100000, NA_real_),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa), Tasa >= 0)
    
    if (!nrow(df)) return(df)
    
    df <- df %>%
      arrange(if (extremos == "top") desc(Tasa) else Tasa) %>%
      slice_head(n = min(4, nrow(df)))
    
    # En los radares inferiores excluimos, cuando sea posible, causas con tasa 0
    # para evitar un gráfico vacío o poco informativo.
    if (extremos == "bottom") {
      positivos <- df[df$Tasa > 0, , drop = FALSE]
      if (nrow(positivos) > 0) df <- positivos
    }
    
    df
  }
  
  construir_radar_europa <- function(pais_sel, anio_sel, sexo_sel, extremos = "top", color = "#1f77b4") {
    df <- preparar_radar_europa_comparador(pais_sel, anio_sel, sexo_sel, extremos)
    validate(need(nrow(df) > 0, "No hay datos suficientes para construir el radar."))
    
    df$Causa <- as.character(df$causa_grupo)
    df <- df %>% arrange(if (extremos == "top") desc(Tasa) else Tasa)
    
    # Cerramos el polígono repitiendo exactamente la primera causa.
    df_radar <- bind_rows(df, df[1, , drop = FALSE])
    df_radar$theta <- df_radar$Causa
    df_radar$r <- df_radar$Tasa
    df_radar$hover <- paste0(
      "<b>", htmltools::htmlEscape(df_radar$Causa), "</b>",
      "<br>Tasa: ", format(round(df_radar$Tasa, 1), decimal.mark = ","),
      " por 100.000 habitantes"
    )
    
    fill_rgba <- function(hex, alpha = 0.30) {
      rgbv <- grDevices::col2rgb(hex)[, 1]
      paste0("rgba(", paste(rgbv, collapse = ","), ", ", alpha, ")")
    }
    
    plot_ly(
      df_radar,
      type = "scatterpolar",
      r = ~r,
      theta = ~theta,
      mode = "lines+markers",
      fill = "toself",
      fillcolor = fill_rgba(color, 0.30),
      line = list(color = color, width = 2.5),
      marker = list(color = color, size = 6),
      text = ~hover,
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(showline = TRUE, gridcolor = "#D9D9D9", rangemode = "tozero"),
          angularaxis = list(direction = "clockwise")
        ),
        showlegend = FALSE,
        margin = list(l = 55, r = 55, t = 20, b = 35)
      )
  }
  
  output$eur_comp_radar_title_a <- renderUI({
    req(input$eur_comp_pais_a)
    HTML(paste0("4 causas más afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_a))))
  })
  
  output$eur_comp_radar_title_b <- renderUI({
    req(input$eur_comp_pais_b)
    HTML(paste0("4 causas más afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_b))))
  })
  
  output$eur_comp_radar_bottom_title_a <- renderUI({
    req(input$eur_comp_pais_a)
    HTML(paste0("4 causas menos afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_a))))
  })
  
  output$eur_comp_radar_bottom_title_b <- renderUI({
    req(input$eur_comp_pais_b)
    HTML(paste0("4 causas menos afectadas — ", htmltools::htmlEscape(traducir_pais(input$eur_comp_pais_b))))
  })
  
  output$eur_comp_radar_a <- renderPlotly({
    construir_radar_europa(input$eur_comp_pais_a, input$eur_comp_anio, input$eur_comp_sexo, "top", "#1f77b4")
  })
  
  output$eur_comp_radar_b <- renderPlotly({
    # El segundo país usa el mismo eje radial para facilitar la comparación visual.
    construir_radar_europa(input$eur_comp_pais_b, input$eur_comp_anio, input$eur_comp_sexo, "top", "#ff7f0e")
  })
  
  output$eur_comp_radar_a_bot <- renderPlotly({
    construir_radar_europa(input$eur_comp_pais_a, input$eur_comp_anio, input$eur_comp_sexo, "bottom", "#1f77b4")
  })
  
  output$eur_comp_radar_b_bot <- renderPlotly({
    construir_radar_europa(input$eur_comp_pais_b, input$eur_comp_anio, input$eur_comp_sexo, "bottom", "#ff7f0e")
  })
  
  output$eur_comp_poblacion_titulo_a <- renderText({
    req(input$eur_comp_pais_a, input$eur_comp_anio)
    paste0("Población total — ", traducir_pais(input$eur_comp_pais_a))
  })
  
  output$eur_comp_poblacion_titulo_b <- renderText({
    req(input$eur_comp_pais_b, input$eur_comp_anio)
    paste0("Población total — ", traducir_pais(input$eur_comp_pais_b))
  })
  
  poblacion_pais_total <- function(pais_sel, anio_sel) {
    val <- poblacion_europa %>%
      filter(pais_key == normalizar_clave_pais_europa(pais_sel), anio == as.numeric(anio_sel)) %>%
      summarise(Poblacion = sum(poblacion, na.rm = TRUE), .groups = "drop") %>%
      pull(Poblacion)
    if (!length(val) || !is.finite(val)) return(NA_real_)
    val
  }
  
  output$eur_comp_poblacion_a <- renderText({
    req(input$eur_comp_pais_a, input$eur_comp_anio)
    val <- poblacion_pais_total(input$eur_comp_pais_a, input$eur_comp_anio)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$eur_comp_poblacion_b <- renderText({
    req(input$eur_comp_pais_b, input$eur_comp_anio)
    val <- poblacion_pais_total(input$eur_comp_pais_b, input$eur_comp_anio)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  # ---------------------------------------------------------------------------
  # MODELO ESTADÍSTICO EUROPEO
  # Modelo lineal: tasa ~ país + año. Permite comprobar si existen diferencias
  # entre países controlando por la evolución temporal.
  # ---------------------------------------------------------------------------
  datos_modelo_europa <- reactive({
    req(input$eur_mod_causa, input$eur_mod_sexo, input$eur_mod_anio)
    
    # OPT: usa sexo_norm precomputado en europa_agrupada.
    df <- europa_agrupada %>%
      mutate(Sexo = sexo_norm) %>%
      filter(
        causa_grupo == input$eur_mod_causa,
        Sexo == input$eur_mod_sexo,
        is.finite(tasa_100k),
        is.finite(anio)
      )
    
    if (input$eur_mod_anio != "Todos los años") {
      df <- df %>% filter(anio == as.numeric(input$eur_mod_anio))
    }
    
    df %>%
      group_by(pais, anio) %>%
      summarise(
        Tasa = if (all(!is.finite(tasa_100k))) NA_real_ else mean(tasa_100k, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(is.finite(Tasa)) %>%
      mutate(
        pais = factor(pais),
        anio = as.numeric(anio)
      )
  })
  
  modelo_europa <- reactive({
    df <- datos_modelo_europa()
    validate(need(nrow(df) >= 5, "No hay suficientes datos para el modelo europeo: se necesitan al menos 5 observaciones."))
    validate(need(dplyr::n_distinct(df$pais) >= 3, "No hay suficientes países para comparar: se necesitan al menos 3 países."))
    
    if (dplyr::n_distinct(df$anio) >= 2) {
      lm(Tasa ~ pais + anio, data = df)
    } else {
      lm(Tasa ~ pais, data = df)
    }
  })
  
  output$eur_mod_info <- renderUI({
    req(input$eur_mod_causa, input$eur_mod_sexo, input$eur_mod_anio)
    df <- datos_modelo_europa()
    HTML(paste0(
      "<b>Observaciones:</b> ", nrow(df),
      " · <b>Países:</b> ", dplyr::n_distinct(df$pais),
      " · <b>Periodo:</b> ", htmltools::htmlEscape(input$eur_mod_anio),
      " · <b>Modelo:</b> ", if (dplyr::n_distinct(df$anio) >= 2) "tasa ~ país + año" else "tasa ~ país"
    ))
  })
  
  output$eur_mod_r2 <- renderText({
    fit <- modelo_europa()
    r2 <- summary(fit)$r.squared
    paste0(round(r2, 4), " (", round(r2 * 100, 2), " % de variabilidad explicada)")
  })
  
  output$eur_mod_validez <- renderText({
    fit <- modelo_europa()
    a <- anova(fit)
    p_pais <- if ("pais" %in% rownames(a) && "Pr(>F)" %in% names(a)) a["pais", "Pr(>F)"] else NA_real_
    if (!is.finite(p_pais)) {
      "No evaluable con estos datos (ajuste perfecto o sin grados de libertad residuales)"
    } else if (p_pais < 0.05) {
      "Diferencias significativas entre países"
    } else {
      "Sin diferencias significativas entre países"
    }
  })
  
  output$eur_mod_boxplot <- renderPlotly({
    df <- datos_modelo_europa()
    validate(need(nrow(df) > 5, "No hay suficientes datos para el modelo europeo."))
    # Horizontal y ordenado por mediana: con ~30 países el eje X vertical
    # solapaba los nombres y el gráfico quedaba ilegible.
    df_plot <- df %>%
      mutate(País = traducir_pais(as.character(pais))) %>%
      mutate(País = reorder(País, Tasa, FUN = function(v) median(v, na.rm = TRUE)))
    plot_ly(df_plot, y = ~País, x = ~Tasa, type = "box", orientation = "h",
            boxpoints = "outliers",
            marker = list(color = "#2c3e50"),
            line = list(color = "#2c3e50"),
            hovertemplate = "<b>%{y}</b><br>Tasa: %{x:.1f} por 100.000 habitantes<extra></extra>") %>%
      layout(
        xaxis = list(title = "Tasa de defunción del grupo (por 100.000 hab.)"),
        yaxis = list(title = "", automargin = TRUE),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 185, r = 30, t = 15, b = 55)
      )
  })
  
  output$eur_mod_tabla <- renderTable({
    df <- datos_modelo_europa()
    fit <- modelo_europa()
    a <- anova(fit)
    tiene_anio <- dplyr::n_distinct(df$anio) >= 2
    formula_txt <- if (tiene_anio) "lm(Tasa ~ pais + anio)" else "lm(Tasa ~ pais)"
    
    p_pais <- if ("pais" %in% rownames(a) && "Pr(>F)" %in% names(a)) a["pais", "Pr(>F)"] else NA_real_
    f_pais <- if ("pais" %in% rownames(a) && "F value" %in% names(a)) a["pais", "F value"] else NA_real_
    
    f_txt <- if (is.finite(f_pais)) format(round(unname(f_pais), 4), nsmall = 4) else "No evaluable"
    p_txt <- if (is.finite(p_pais)) format.pval(p_pais, digits = 4, eps = 0.001) else "No evaluable"
    conclusion <- if (!is.finite(p_pais)) {
      "No evaluable: el ajuste es esencialmente perfecto o no quedan grados de libertad residuales para contrastar el efecto país."
    } else if (p_pais < 0.05) {
      if (tiene_anio) "Se rechaza H0: existen diferencias entre países, controlando por año." else "Se rechaza H0: existen diferencias entre países."
    } else {
      if (tiene_anio) "No se rechaza H0: no hay evidencia suficiente de diferencias entre países, controlando por año." else "No se rechaza H0: no hay evidencia suficiente de diferencias entre países."
    }
    
    data.frame(
      Elemento = c("Modelo", "F del efecto país", "p-valor país", "Conclusión"),
      Resultado = c(formula_txt, f_txt, p_txt, conclusion),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$eur_mod_summary <- renderPrint({
    summary(modelo_europa())
  })
# ---------------------------------------------------------------------------
  # PANEL EUROPEO: EFECTOS FIJOS (pais, anio, causa_grupo, sexo)
  # tasas_100k ~ efectos fijos seleccionados. Errores robustos cluster país.
  # ---------------------------------------------------------------------------
  datos_eur_reg <- reactive({
    req(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe)
    df <- europa_agrupada %>%
      filter(causa_grupo == input$eur_reg_causa, sexo == input$eur_reg_sexo) %>%
      filter(is.finite(tasa_100k), tasa_100k > 0)
    if (isTRUE(input$eur_reg_log)) {
      df <- df %>% mutate(tasa_100k = log(tasa_100k))
    }
    req(nrow(df) > 10)
    df
  })
  
  modelo_eur_reg <- reactive({
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    fe_terms <- input$eur_reg_fe
    # Mapear términos UI a nombres de columnas reales
    term_map <- c(pais = "pais", anio = "anio", causa = "causa_grupo", sexo = "sexo")
    fe_cols <- term_map[fe_terms]
    
    # Solo mantener efectos fijos con 2+ niveles en los datos filtrados
    fe_cols <- fe_cols[vapply(fe_cols, function(col) {
      nlevels(factor(df[[col]])) >= 2
    }, logical(1))]
    
    if (length(fe_cols) == 0) {
      formula_str <- "tasa_100k ~ 1"
    } else {
      formula_str <- paste("tasa_100k ~", paste(fe_cols, collapse = " + "))
    }
    lm(as.formula(formula_str), data = df)
  })
  
  # Errores robustos cluster país (Arellano, 1987) - implementación manual
  coef_cluster_pais <- function(fit, df) {
    if (!"pais" %in% names(df)) return(NULL)
    X <- model.matrix(fit)
    u <- residuals(fit)
    paises <- df$pais
    n <- nrow(X)
    k <- ncol(X)
    G <- length(unique(paises))
    
    # Calcular meat matrix: sum_g (X_g' u_g) (X_g' u_g)'
    # Para cada cluster (país), sumar X_i * u_i sobre observaciones del cluster
    uX <- u * X  # n x k matrix
    # Sumar por país usando split
    uX_by_country <- split(as.data.frame(uX), paises)
    uX_sums <- lapply(uX_by_country, colSums)
    uX_mat <- do.call(rbind, uX_sums)
    
    meat <- crossprod(uX_mat)
    bread <- solve(crossprod(X))
    # Factor de corrección de grados de libertad
    dfc <- (G / (G - 1)) * (n / (n - k))
    vcov_cl <- dfc * bread %*% meat %*% bread
    # pmax: redondeos numéricos pueden dar varianzas ligeramente negativas
    se <- sqrt(pmax(diag(vcov_cl), 0))
    coefs <- coef(fit)
    tval <- coefs / se
    pval <- 2 * pt(abs(tval), df = G - 1, lower.tail = FALSE)
    data.frame(
      term = names(coefs),
      estimate = unname(coefs),
      std.error = unname(se),
      statistic = unname(tval),
      p.value = unname(pval),
      stringsAsFactors = FALSE
    )
  }
  
  coef_table <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    coef_cluster_pais(fit, df)
  })
  
  # Diagnostics
  diag_plots <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    df$.fitted <- fitted(fit)
    df$.resid <- residuals(fit)
    df$.stdresid <- rstandard(fit)
    list(fit = fit, df = df)
  })
  
  # Predictions by country for trends plot
  pred_by_country <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    newdata <- expand.grid(
      pais = unique(df$pais),
      anio = unique(df$anio),
      causa_grupo = unique(df$causa_grupo),
      sexo = unique(df$sexo)
    )
    newdata <- newdata[newdata$causa_grupo == input$eur_reg_causa & newdata$sexo == input$eur_reg_sexo, ]
    newdata$.fitted <- predict(fit, newdata = newdata)
    newdata$pais_es <- traducir_pais(newdata$pais)
    newdata
  })
  
  output$eur_reg_r2 <- renderText({
    fit <- modelo_eur_reg()
    r2 <- summary(fit)$adj.r.squared
    formatC(r2, format = "f", digits = 3, decimal.mark = ",")
  })
  
  output$eur_reg_n <- renderText({
    df <- datos_eur_reg()
    format(nrow(df), big.mark = ".", decimal.mark = ",")
  })
  
  output$eur_reg_paises <- renderText({
    df <- datos_eur_reg()
    length(unique(df$pais))
  })
  
  output$eur_reg_summary <- renderPrint({
    summary(modelo_eur_reg())
  })
  
  output$eur_reg_coef <- DT::renderDT({
    ct <- coef_table()
    req(!is.null(ct), nrow(ct) > 0)
    DT::datatable(ct,
      options = list(pageLength = 20, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound(c("estimate", "std.error", "statistic"), 4, dec.mark = ",", mark = ".") %>%
      DT::formatRound("p.value", 4, dec.mark = ",", mark = ".")
  })
  
  output$eur_reg_diag1 <- renderPlotly({
    d <- diag_plots()
    req(!is.null(d))
    plot_ly(d$df, x = ~.fitted, y = ~.resid,
            type = "scatter", mode = "markers",
            marker = list(color = "#1f77b4", size = 5, opacity = 0.6),
            hovertemplate = "<b>%{fullData.name}</b><br>Ajustado: %{x:.2f}<br>Residuo: %{y:.2f}<extra></extra>") %>%
      add_trace(x = range(d$df$.fitted), y = c(0, 0), type = "scatter", mode = "lines",
                line = list(color = "#e74c3c", width = 2, dash = "dash"),
                showlegend = FALSE, hoverinfo = "skip") %>%
      layout(xaxis = list(title = "Valores ajustados"),
             yaxis = list(title = "Residuos"),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 20, b = 50))
  })
  
  output$eur_reg_diag2 <- renderPlotly({
    d <- diag_plots()
    req(!is.null(d))
    qq <- qqnorm(d$df$.stdresid, plot.it = FALSE)
    ok <- is.finite(qq$x) & is.finite(qq$y)
    qx <- qq$x[ok]
    qy <- qq$y[ok]
    req(length(qx) > 2)
    plot_ly(x = qx, y = qy,
            type = "scatter", mode = "markers",
            marker = list(color = "#1f77b4", size = 5, opacity = 0.6),
            hovertemplate = "Teórico: %{x:.2f}<br>Muestral: %{y:.2f}<extra></extra>") %>%
      add_trace(x = range(qx), y = range(qx), type = "scatter", mode = "lines",
                line = list(color = "#e74c3c", width = 2, dash = "dash"),
                showlegend = FALSE, hoverinfo = "skip") %>%
      layout(xaxis = list(title = "Cuantiles teóricos (Normal)"),
             yaxis = list(title = "Cuantiles muestrales"),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 20, b = 50))
  })
  
output$eur_reg_trends <- renderPlotly({
    d <- pred_by_country()
    req(nrow(d) > 0)
    npaises <- dplyr::n_distinct(d$pais_es)
    plot_ly(d, x = ~anio, y = ~.fitted, color = ~pais_es,
            colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(npaises, 1)),
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Predicho: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = "Tasa predicha (por 100k hab.)"),
             legend = list(orientation = "h", x = 0, y = -0.2),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 20, b = 100))
  })
}
