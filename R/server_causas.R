server_causas <- function(input, output, session) {
  
  # Ayudas contextuales
  output$p2_filtro_info <- renderUI({
    req(input$p2_defuncion)
    div(class = "filter-help",
        HTML(paste0("<b>Causa:</b> ", htmltools::htmlEscape(input$p2_defuncion),
                    ". La tasa de la causa es defunciones / población × 100.000.")))
  })
  
  output$p21_filtro_info <- renderUI({
    req(input$p21_prov_a, input$p21_prov_b, input$p21_defuncion, input$p21_ano)
    div(class = "filter-help",
        HTML(paste0("<b>Comparación:</b> ", htmltools::htmlEscape(input$p21_prov_a),
                    " vs. ", htmltools::htmlEscape(input$p21_prov_b),
                    ". El radar usa ", htmltools::htmlEscape(input$p21_ano), ".")))
  })
  
  # --- PESTAÑA 2 SERVER ---
  datos_p2 <- reactive({
    req(input$p2_defuncion, input$p2_ano, input$p2_sexo)
    df <- causas_provinciales %>%
      filter(Año == input$p2_ano, Defunción == input$p2_defuncion)
    if (input$p2_sexo != "Ambos") df <- df %>% filter(Sexo == input$p2_sexo)
    
    df %>%
      group_by(Provincia) %>%
      summarise(
        Total_Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion_Total = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion_Total > 0,
                            Total_Fallecidos / Poblacion_Total * 100000,
                            NA_real_)) %>%
      filter(is.finite(Tasa))
  }) %>% bindCache(input$p2_defuncion, input$p2_ano, input$p2_sexo)
  
  output$p2_kpi_total <- renderText({
    df <- datos_p2()
    if (nrow(df) == 0) return("-")
    tasa_nac <- (sum(df$Total_Fallecidos, na.rm = TRUE) / sum(df$Poblacion_Total, na.rm = TRUE)) * 100000
    paste0(format(round(tasa_nac, 2), big.mark = ".", decimal.mark = ","), " / 100k hab.")
  })
  
  output$p2_kpi_max <- renderText({
    df <- datos_p2()
    if (nrow(df) == 0) return("-")
    max_row <- df %>% arrange(desc(Tasa)) %>% slice(1)
    paste0(max_row$Provincia, " (", format(round(max_row$Tasa, 2), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$p2_kpi_min <- renderText({
    df <- datos_p2()
    if (nrow(df) == 0) return("-")
    min_row <- df %>% arrange(Tasa) %>% slice(1)
    paste0(min_row$Provincia, " (", format(round(min_row$Tasa, 2), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$p2_mapa <- renderLeaflet({
    df <- datos_p2()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    
    val_max <- max(mapa_datos$Tasa, na.rm = TRUE)
    dominio_colores <- if (is.infinite(val_max) || is.na(val_max) || val_max == 0) c(0, 1) else c(0, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio_colores, na.color = "#E0E0E0")
    
    etiquetas <- sprintf("<strong>%s</strong><br/>Tasa de la causa: %s por 100k hab.", mapa_datos$NAME_2, format(round(mapa_datos$Tasa, 2), decimal.mark = ",")) %>% lapply(htmltools::HTML)
    
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Tasa), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio_colores, opacity = 0.8, title = "Tasa de la causa / 100k hab.", position = "bottomright") %>%
      control_ano(input$p2_ano)
  })
  
  output$p2_evolucion_top_bot <- renderPlotly({
    req(input$p2_defuncion)
    
    df_evo <- causas_provinciales %>% filter(Defunción == input$p2_defuncion)
    if (input$p2_sexo != "Ambos") df_evo <- df_evo %>% filter(Sexo == input$p2_sexo)
    
    df_evo <- df_evo %>% mutate(Ano_Num = Año_Num)
    
    df_nacional <- df_evo %>%
      group_by(Ano_Num) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Provincia = "Tasa nacional") %>%
      filter(is.finite(Tasa))
    
    ultimo_ano <- max(df_evo$Ano_Num, na.rm = TRUE)
    ranking <- df_evo %>%
      filter(Ano_Num == ultimo_ano) %>%
      group_by(Provincia) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      arrange(desc(Tasa))
    
    top5 <- head(ranking$Provincia, 5)
    bot5 <- tail(ranking$Provincia, 5)
    
    # FIX: tasa ponderada por población (defunciones/población × 100k),
    # coherente con la "Tasa nacional" del mismo gráfico. Antes se usaba la
    # media simple de las tasas por sexo.
    df_top_bot <- df_evo %>%
      filter(Provincia %in% c(top5, bot5)) %>%
      group_by(Ano_Num, Provincia) %>%
      summarise(
        Fallecidos = sum(Total, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    
    colores_top <- c("#8B0000", "#B22222", "#CD5C5C", "#E53935", "#FF5252")
    colores_bot <- c("#FBC02D", "#FDD835", "#FFEE58", "#FFF176", "#FFF59D")
    
    vector_colores <- setNames(
      c(colores_top, colores_bot, "#000000"), 
      c(top5, bot5, "Tasa nacional")
    )
    
    p <- plot_ly()
    
    for (prov in top5) {
      sub_df <- df_top_bot %>% filter(Provincia == prov)
      p <- p %>% add_trace(data = sub_df, x = ~Ano_Num, y = ~Tasa, name = prov, type = "scatter", mode = "lines+markers", line = list(color = vector_colores[prov], width = 2), marker = list(color = vector_colores[prov], size = 6, line = list(color = "white", width = 1)), hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    for (prov in bot5) {
      sub_df <- df_top_bot %>% filter(Provincia == prov)
      p <- p %>% add_trace(data = sub_df, x = ~Ano_Num, y = ~Tasa, name = prov, type = "scatter", mode = "lines+markers", line = list(color = vector_colores[prov], width = 2), marker = list(color = vector_colores[prov], size = 6, line = list(color = "white", width = 1)), hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    p <- p %>% add_trace(
      data = df_nacional, x = ~Ano_Num, y = ~Tasa, name = "Tasa nacional",
      type = "scatter", mode = "lines+markers",
      line = list(color = "#000000", width = 3, dash = "dash"),
      marker = list(color = "#000000", size = 7, line = list(color = "white", width = 1.5)),
      hovertemplate = "<b>Tasa nacional</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>"
    )
    
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1),
      yaxis = list(title = "Tasa de la causa (por 100k hab.)"),
      margin = list(l = 50, r = 20, t = 10, b = 40),
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "v", x = 1.02, y = 1)
    )
  })
  
  output$p2_causas_mas_sexo <- renderPlotly({
    req(input$p2_ano)
    
    top_df <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Sexo, Defunción) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      group_by(Sexo) %>%
      slice_max(order_by = Tasa, n = 3, with_ties = FALSE) %>%
      ungroup()
    
    causas_sel <- unique(top_df$Defunción)
    
    df_plot <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres"), Defunción %in% causas_sel) %>%
      group_by(Defunción, Sexo) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Defunción_Short = stringr::str_wrap(Defunción, width = 25)) %>%
      filter(is.finite(Tasa))
    
    plot_ly(
      data = df_plot,
      x = ~Tasa,
      y = ~Defunción_Short,
      color = ~Sexo,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{y}</b><br>%{fullData.name}: %{x:.1f} /100k<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de la causa (por 100k hab.)"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "total ascending"),
        margin = list(l = 180, r = 20, t = 10, b = 40),
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.2, y = 1.15)
      )
  })
  
  output$p2_causas_menos_sexo <- renderPlotly({
    req(input$p2_ano)
    
    bot_df <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Sexo, Defunción) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa), Tasa > 0) %>%
      group_by(Sexo) %>%
      slice_min(order_by = Tasa, n = 3, with_ties = FALSE) %>%
      ungroup()
    
    causas_sel <- unique(bot_df$Defunción)
    
    df_plot <- causas_provinciales %>%
      filter(Año == input$p2_ano, Sexo %in% c("Hombres", "Mujeres"), Defunción %in% causas_sel) %>%
      group_by(Defunción, Sexo) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Defunción_Short = stringr::str_wrap(Defunción, width = 25)) %>%
      filter(is.finite(Tasa))
    
    plot_ly(
      data = df_plot,
      x = ~Tasa,
      y = ~Defunción_Short,
      color = ~Sexo,
      colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
      type = "bar",
      orientation = "h",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{y}</b><br>%{fullData.name}: %{x:.1f} /100k<extra></extra>"
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tasa de la causa (por 100k hab.)"),
        yaxis = list(title = "", automargin = TRUE, categoryorder = "total descending"),
        margin = list(l = 180, r = 20, t = 10, b = 40),
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.2, y = 1.15)
      )
  })
  
  observeEvent(input$p21_prov_a, {
    req(input$p21_prov_a)
    updateSelectInput(session, "p21_prov_a", label = paste0("Provincia: ", input$p21_prov_a))
  }, ignoreInit = TRUE)
  
  observeEvent(input$p21_prov_b, {
    req(input$p21_prov_b)
    updateSelectInput(session, "p21_prov_b", label = paste0("Provincia: ", input$p21_prov_b))
  }, ignoreInit = TRUE)
  
  output$p2_tabla_resumen <- DT::renderDT({
    df <- datos_p2() %>%
      transmute(
        Provincia = Provincia,
        Defunciones = Total_Fallecidos,
        Población = Poblacion_Total,
        `Tasa de la causa por 100.000 habitantes` = Tasa
      ) %>%
      arrange(desc(`Tasa de la causa por 100.000 habitantes`))
    
    DT::datatable(
      df,
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatRound("Tasa de la causa por 100.000 habitantes", 2, dec.mark = ",", mark = ".")
  })
  
  # --- PESTAÑA 2.1 SERVER ---
  output$p21_evol_nacional <- renderPlotly({
    req(input$p21_prov_a, input$p21_prov_b, input$p21_defuncion)
    
    df_provs <- causas_provinciales %>% 
      filter(Defunción == input$p21_defuncion, Provincia %in% c(input$p21_prov_a, input$p21_prov_b)) %>%
      group_by(Año, Provincia) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Ano_Num = as.numeric(Año)) %>%
      filter(is.finite(Tasa))
    
    df_nac <- causas_provinciales %>% 
      filter(Defunción == input$p21_defuncion) %>%
      group_by(Año) %>%
      summarise(Fallecidos = sum(Total, na.rm = TRUE),
                Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
             Provincia = "Tasa nacional",
             Ano_Num = as.numeric(Año)) %>%
      filter(is.finite(Tasa))
    
    p <- plot_ly()
    
    df_a <- df_provs %>% filter(Provincia == input$p21_prov_a)
    if(nrow(df_a) > 0) {
      p <- p %>% add_trace(data = df_a, x = ~Ano_Num, y = ~Tasa, name = input$p21_prov_a,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#1f77b4", width = 3),
                           marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    df_b <- df_provs %>% filter(Provincia == input$p21_prov_b)
    if(nrow(df_b) > 0) {
      p <- p %>% add_trace(data = df_b, x = ~Ano_Num, y = ~Tasa, name = input$p21_prov_b,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#ff7f0e", width = 3),
                           marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    }
    
    p <- p %>% add_trace(data = df_nac, x = ~Ano_Num, y = ~Tasa, name = "Tasa nacional",
                         type = "scatter", mode = "lines+markers",
                         line = list(color = "#2c3e50", width = 2.5, dash = "dash"),
                         marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                         hovertemplate = "<b>Tasa nacional</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
      yaxis = list(title = "Tasa de la causa (por 100k hab.)", showgrid = TRUE, gridcolor = "#E5E5E5"),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "h", x = 0.2, y = 1.12),
      margin = list(l = 50, r = 20, t = 10, b = 40)
    )
  })
  
  color_prov_a <- "#1f77b4"
  color_prov_b <- "#ff7f0e"
  
  construir_radar_provincia <- function(prov_sel, ano_sel, tipo = c("top", "bottom"), color = "#1f77b4") {
    tipo <- match.arg(tipo)
    req(prov_sel, ano_sel)
    
    df <- causas_provinciales %>%
      filter(Provincia == prov_sel)
    
    if (ano_sel != "Todos los años") {
      df <- df %>% filter(Año == ano_sel)
    }
    
    # FIX: tasa ponderada por población (defunciones/población × 100k).
    # Antes se usaba la media simple de las tasas por sexo y año, que no
    # equivale a la tasa del periodo (los años/sexos pesan distinto).
    df <- df %>%
      group_by(Defunción) %>%
      summarise(
        Fallecidos = sum(Total, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa), Tasa >= 0)
    
    df <- if (tipo == "top") {
      df %>% arrange(desc(Tasa)) %>% slice_head(n = 6)
    } else {
      df %>% filter(Tasa > 0) %>% arrange(Tasa) %>% slice_head(n = 6)
    }
    
    validate(need(nrow(df) >= 3, "No hay suficientes causas para construir el radar."))
    df$Defuncion_Short <- stringr::str_wrap(df$Defunción, width = 18)
    max_tasa <- max(df$Tasa, na.rm = TRUE)
    if (!is.finite(max_tasa) || max_tasa <= 0) max_tasa <- 1
    
    alpha_fill <- if (tipo == "top") "0.35" else "0.20"
    fill_rgba <- function(hex, alpha) {
      rgbv <- grDevices::col2rgb(hex)[,1]
      paste0("rgba(", paste(rgbv, collapse = ","), ", ", alpha, ")")
    }
    
    vals <- c(df$Tasa, df$Tasa[1])
    cats <- c(df$Defuncion_Short, df$Defuncion_Short[1])
    hover <- c(paste0(df$Defunción, "<br>Tasa: ", format(round(df$Tasa, 1), decimal.mark = ",")),
               paste0(df$Defunción[1], "<br>Tasa: ", format(round(df$Tasa[1], 1), decimal.mark = ",")))
    
    plot_ly(
      r = vals, theta = cats, text = hover,
      type = "scatterpolar", mode = "lines+markers",
      fill = "toself",
      fillcolor = fill_rgba(color, alpha_fill),
      line = list(color = color, width = 2),
      marker = list(color = color, size = 6),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, max_tasa * 1.1)),
          angularaxis = list(tickfont = list(size = 10))
        ),
        margin = list(l = 40, r = 40, t = 20, b = 20),
        showlegend = FALSE
      )
  }
  
  output$p21_radar_title_a <- renderUI({
    req(input$p21_prov_a)
    paste0("6 causas principales — ", input$p21_prov_a)
  })
  output$p21_radar_title_b <- renderUI({
    req(input$p21_prov_b)
    paste0("6 causas principales — ", input$p21_prov_b)
  })
  output$p21_radar_bottom_title_a <- renderUI({
    req(input$p21_prov_a)
    paste0("6 causas menos frecuentes — ", input$p21_prov_a)
  })
  output$p21_radar_bottom_title_b <- renderUI({
    req(input$p21_prov_b)
    paste0("6 causas menos frecuentes — ", input$p21_prov_b)
  })
  
  output$p21_radar_a <- renderPlotly({
    construir_radar_provincia(input$p21_prov_a, input$p21_ano, "top", color_prov_a)
  })
  output$p21_radar_b <- renderPlotly({
    construir_radar_provincia(input$p21_prov_b, input$p21_ano, "top", color_prov_b)
  })
  output$p21_radar_a_bot <- renderPlotly({
    construir_radar_provincia(input$p21_prov_a, input$p21_ano, "bottom", color_prov_a)
  })
  output$p21_radar_b_bot <- renderPlotly({
    construir_radar_provincia(input$p21_prov_b, input$p21_ano, "bottom", color_prov_b)
  })
  

  
  # --- EVOLUCIÓN TEMPORAL POR COMUNIDADES SERVER ---
  datos_pt_ccaa <- reactive({
    req(input$pt_causa, input$pt_sexo)
    df <- causas_provinciales %>%
      filter(Defunción == input$pt_causa)
    if (input$pt_sexo != "Ambos") df <- df %>% filter(Sexo == input$pt_sexo)
    
    df %>%
      group_by(Comunidad, Año) %>%
      summarise(
        Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_),
        Año_Num = suppressWarnings(as.numeric(Año))
      ) %>%
      filter(is.finite(Tasa), !is.na(Comunidad), Comunidad != "Sin asignar")
  }) %>% bindCache(input$pt_causa, input$pt_sexo)
  
  output$pt_kpi_cambio <- renderText({
    df <- datos_pt_ccaa() %>%
      select(Comunidad, Año_Num, Tasa) %>%
      filter(Año_Num %in% c(2018, 2022)) %>%
      group_by(Comunidad, Año_Num) %>%
      summarise(Tasa = mean(Tasa, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = Año_Num, values_from = Tasa)
    if (!all(c("2018", "2022") %in% names(df))) return("-")
    cambios <- df %>% mutate(Cambio = .data[["2022"]] - .data[["2018"]]) %>%
      filter(is.finite(Cambio)) %>% arrange(desc(Cambio)) %>% slice(1)
    if (nrow(cambios) == 0) return("-")
    paste0(cambios$Comunidad, " (", format(round(cambios$Cambio, 2), decimal.mark = ","), ")")
  })
  
  output$pt_kpi_defunciones <- renderText({
    df <- datos_pt_ccaa()
    if (nrow(df) == 0) return("-")
    format(sum(df$Fallecidos, na.rm = TRUE), big.mark = ".", decimal.mark = ",")
  })
  
  output$pt_kpi_ano_max <- renderText({
    df <- datos_pt_ccaa() %>%
      group_by(Año_Num) %>%
      summarise(Defunciones = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Año_Num), is.finite(Defunciones)) %>%
      arrange(desc(Defunciones))
    if (nrow(df) == 0) return("-")
    paste0(as.integer(df$Año_Num[1]), " (",
           format(round(df$Defunciones[1]), big.mark = ".", decimal.mark = ","),
           " defunciones)")
  })
  
  output$pt_evolucion_ccaa <- renderPlotly({
    df <- datos_pt_ccaa() %>% arrange(Año_Num, Comunidad)
    req(nrow(df) > 0)
    nccaa <- dplyr::n_distinct(df$Comunidad)
    plot_ly(
      data = df, x = ~Año_Num, y = ~Tasa, color = ~Comunidad,
      colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                             "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1)),
      type = "scatter", mode = "lines+markers",
      marker = list(line = list(color = "white", width = 1)),
      text = ~paste0(
        Comunidad, "<br>Año: ", Año_Num,
        "<br>Tasa: ", format(round(Tasa, 1), decimal.mark = ","), " / 100k",
        "<br>Defunciones: ", format(Fallecidos, big.mark = ".", decimal.mark = ","),
        "<br>Población: ", format(round(Poblacion), big.mark = ".", decimal.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Año", dtick = 1),
        yaxis = list(title = "Tasa de la causa seleccionada (por 100.000 hab.)"),
        legend = list(orientation = "h", x = 0, y = -0.2),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 60, r = 20, t = 10, b = 100)
      )
  })
  
  datos_pt_variacion <- reactive({
    datos_pt_ccaa() %>%
      arrange(Comunidad, Año_Num) %>%
      group_by(Comunidad) %>%
      mutate(
        Variacion_interanual = Tasa - lag(Tasa),
        Variacion_porcentual = if_else(lag(Tasa) != 0, (Tasa - lag(Tasa)) / lag(Tasa) * 100, NA_real_)
      ) %>%
      ungroup() %>%
      filter(!is.na(Variacion_interanual), is.finite(Variacion_interanual))
  })
  
  output$pt_variacion_interanual <- renderPlotly({
    df <- datos_pt_variacion() %>%
      mutate(Año = as.integer(Año_Num)) %>%
      arrange(Comunidad, Año)
    
    req(nrow(df) > 0)
    
    # Mapa de intensidad: evita el solapamiento de decenas de líneas y
    # permite localizar rápidamente aumentos y descensos interanuales.
    z <- df %>%
      select(Comunidad, Año, Variacion_interanual) %>%
      tidyr::pivot_wider(names_from = Año, values_from = Variacion_interanual) %>%
      arrange(Comunidad)
    
    anos <- sort(unique(df$Año))
    mat <- as.matrix(z[, intersect(as.character(anos), names(z)), drop = FALSE])
    rownames(mat) <- z$Comunidad
    
    # OPT: vectorizado en lugar de double loop
    finitos <- is.finite(mat)
    texto <- matrix("", nrow = nrow(mat), ncol = ncol(mat))
    if (any(finitos)) {
      idx <- which(finitos, arr.ind = TRUE)
      coms <- z$Comunidad[idx[, 1]]
      ans  <- colnames(mat)[idx[, 2]]
      vals <- mat[idx]
      texto[idx] <- paste0(
        coms, "<br>Año: ", ans,
        "<br>Variación: ",
        ifelse(vals >= 0, "+", ""),
        round(vals, 2), " puntos / 100k"
      )
    }
    
    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      text = texto,
      hoverinfo = "text",
      colorbar = list(title = "Cambio<br>/100k")
    ) %>%
      layout(
        xaxis = list(title = "Año", dtick = 1),
        yaxis = list(title = "", autorange = "reversed", automargin = TRUE),
        margin = list(l = 135, r = 70, t = 10, b = 55)
      )
  })
  
  output$pt_tabla_ccaa <- DT::renderDT({
    df <- datos_pt_variacion() %>%
      mutate(Año = as.integer(Año_Num)) %>%
      select(Comunidad, Año, Variacion_interanual) %>%
      arrange(Comunidad, Año)
    
    req(nrow(df) > 0)
    
    tabla <- df %>%
      mutate(
        Variacion = round(Variacion_interanual, 2),
        Periodo = paste0(Año - 1, "–", Año)
      ) %>%
      select(Comunidad, Periodo, Variacion) %>%
      tidyr::pivot_wider(
        names_from = Periodo,
        values_from = Variacion,
        names_prefix = "Variación "
      ) %>%
      arrange(Comunidad)
    
    DT::datatable(
      tabla,
      rownames = FALSE,
      options = list(
        pageLength = 19,
        autoWidth = TRUE,
        scrollX = TRUE,
        order = list(0, "asc")
      ),
      caption = "Variación interanual de la tasa de la causa (puntos por 100.000 habitantes)"
    ) %>%
      DT::formatRound(
        columns = grep("^Variación ", names(tabla), value = TRUE),
        digits = 2, dec.mark = ",", mark = "."
      )
  })
  
  # --- MORTALIDAD PREMATURA (APVP, EN CAUSAS DE DEFUNCIÓN) SERVER ---
  datos_apvp_filtrados <- reactive({
    req(input$papvp_causa, input$papvp_indicador, input$papvp_sexo)
    df <- apvp_data %>%
      filter(Causa == input$papvp_causa, Indicador == input$papvp_indicador)
    if (input$papvp_sexo != "Ambos") df <- df %>% filter(Sexo == input$papvp_sexo)
    df %>%
      mutate(Comunidad = unname(ccaa_corto[normalizar_ccaa(Comunidad)])) %>%
      filter(!is.na(Comunidad), is.finite(Valor))
  })

  output$papvp_ranking_title <- renderUI({
    n <- dplyr::n_distinct(datos_apvp_filtrados()$Comunidad)
    paste0("Distribución por comunidad autónoma (", n, " de 19 con dato)")
  })
  
  output$papvp_kpi_total <- renderText({
    df <- datos_apvp_filtrados()
    if (!nrow(df)) return("-")
    format(round(sum(df$Valor, na.rm = TRUE), 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$papvp_kpi_max <- renderText({
    df <- datos_apvp_filtrados()
    if (!nrow(df)) return("-")
    x <- df %>% group_by(Comunidad) %>% summarise(Valor = sum(Valor, na.rm = TRUE), .groups = "drop") %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Comunidad, " (", format(round(x$Valor, 2), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$papvp_kpi_media <- renderText({
    df <- datos_apvp_filtrados()
    if (!nrow(df)) return("-")
    format(round(mean(df$Valor, na.rm = TRUE), 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$papvp_ranking <- renderPlotly({
    df <- datos_apvp_filtrados()
    req(nrow(df) > 0)
    df <- df %>%
      group_by(Comunidad) %>%
      summarise(Valor = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(Valor) %>%
      mutate(Comunidad = factor(Comunidad, levels = Comunidad))
    dom <- dominio_seguro(df$Valor)
    plot_ly(
      df, x = ~Valor, y = ~Comunidad, type = "bar", orientation = "h",
      marker = list(color = ~Valor, cmin = dom[1], cmax = dom[2],
                    colorscale = escala_plotly(PAL_YLORRD), showscale = FALSE,
                    line = list(color = "white", width = 1)),
      text = ~format(round(Valor, 1), big.mark = ".", decimal.mark = ","),
      textposition = "outside",
      hovertemplate = "<b>%{y}</b><br>Valor: %{x:,.2f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = input$papvp_indicador),
        yaxis = list(title = "", automargin = TRUE),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 140, r = 60, b = 60, t = 20)
      )
  })

  # --- TASAS ESTANDARIZADAS POR EDAD SERVER (solo si existen los ficheros) ---
  datos_std_sel <- reactive({
    req(!is.null(datos_edad_std), input$std_ano, input$std_sexo)
    datos_edad_std$resumen %>%
      filter(Año == as.character(input$std_ano),
             Sexo == input$std_sexo) %>%
      filter(is.finite(Tasa_bruta), is.finite(Tasa_std)) %>%
      mutate(Puesto_bruta = rank(-Tasa_bruta, ties.method = "min"),
             Puesto_std = rank(-Tasa_std, ties.method = "min"),
             Cambio = Puesto_std - Puesto_bruta)
  })

  output$std_kpi_max <- renderText({
    df <- datos_std_sel()
    if (!nrow(df)) return("-")
    x <- df %>% arrange(desc(Tasa_std)) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Tasa_std, 1), decimal.mark = ","), " / 100k)")
  })

  output$std_kpi_min <- renderText({
    df <- datos_std_sel()
    if (!nrow(df)) return("-")
    x <- df %>% arrange(Tasa_std) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Tasa_std, 1), decimal.mark = ","), " / 100k)")
  })

  output$std_kpi_rank <- renderText({
    df <- datos_std_sel()
    if (!nrow(df)) return("-")
    x <- df %>% arrange(desc(abs(Cambio))) %>% slice(1)
    paste0(x$Provincia, " (bruta: ", x$Puesto_bruta, " → std: ", x$Puesto_std, ")")
  })

  output$std_mapa <- renderLeaflet({
    df <- datos_std_sel()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    val_max <- max(mapa_datos$Tasa_std, na.rm = TRUE)
    dominio <- if (!is.finite(val_max) || val_max <= 0) c(0, 1) else c(0, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>Bruta: %s<br/>Estandarizada: %s por 100k hab.",
                         mapa_datos$NAME_2,
                         format(round(mapa_datos$Tasa_bruta, 1), decimal.mark = ","),
                         format(round(mapa_datos$Tasa_std, 1), decimal.mark = ",")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Tasa_std), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = "Tasa std / 100k", position = "bottomright") %>%
      control_ano(input$std_ano)
  })

  output$std_scatter <- renderPlotly({
    df <- datos_std_sel()
    req(nrow(df) > 0)
    df <- df %>% mutate(
      etiqueta = paste0("<b>", Provincia, "</b><br>Bruta: ",
                        format(round(Tasa_bruta, 1), decimal.mark = ","),
                        "<br>Estandarizada: ",
                        format(round(Tasa_std, 1), decimal.mark = ","),
                        "<br>Puesto ", Puesto_bruta, " → ", Puesto_std)
    )
    lim <- max(c(df$Tasa_bruta, df$Tasa_std), na.rm = TRUE) * 1.05
    diag <- data.frame(x = c(0, lim), y = c(0, lim))
    plot_ly() %>%
      add_trace(data = df, x = ~Tasa_bruta, y = ~Tasa_std, text = ~etiqueta,
                type = "scatter", mode = "markers",
                marker = list(size = 9, color = "#1f77b4",
                              line = list(color = "white", width = 1.5)),
                hovertemplate = "%{text}<extra></extra>") %>%
      add_trace(data = diag, x = ~x, y = ~y, type = "scatter", mode = "lines",
                line = list(color = "#7f8c8d", width = 1.5, dash = "dash"),
                hoverinfo = "none", showlegend = FALSE) %>%
      layout(xaxis = list(title = "Tasa bruta (por 100k hab.)"),
             yaxis = list(title = "Tasa estandarizada ESP 2013 (por 100k hab.)"),
             showlegend = FALSE,
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 15, b = 50))
  })

  output$std_tabla <- DT::renderDT({
    df <- datos_std_sel() %>%
      transmute(Provincia = Provincia,
                `Tasa bruta / 100k` = Tasa_bruta,
                `Tasa estandarizada / 100k` = Tasa_std,
                `Puesto (bruta)` = Puesto_bruta,
                `Puesto (std)` = Puesto_std,
                `Cambio de puesto` = Cambio) %>%
      arrange(`Puesto (std)`)
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Tasa bruta / 100k", "Tasa estandarizada / 100k"), 1, dec.mark = ",", mark = ".")
  })

  # --- EDAD Y MES POR PROVINCIA SERVER (solo si existe el fichero) ---
  q5_etiqueta <- function(e) {
    ifelse(e >= 95, "95 y más", paste0("De ", 5 * (e %/% 5), " a ", 5 * (e %/% 5) + 4))
  }

  datos_ep_edad <- reactive({
    req(!is.null(datos_edadprov), input$ep_prov, input$ep_ano)
    df <- datos_edadprov$edad %>% filter(Año == as.character(input$ep_ano))
    if (input$ep_prov != "Todas") df <- df %>% filter(Provincia == input$ep_prov)
    df %>%
      filter(Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Edad_num, Sexo) %>%
      summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Def)) %>%
      mutate(Q5 = q5_etiqueta(Edad_num),
             Q5 = factor(Q5, levels = unique(Q5[order(Edad_num)])))
  })

  datos_ep_mes <- reactive({
    req(!is.null(datos_edadprov), input$ep_prov, input$ep_ano)
    df <- datos_edadprov$mes %>% filter(Año == as.character(input$ep_ano))
    if (input$ep_prov != "Todas") df <- df %>% filter(Provincia == input$ep_prov)
    df %>%
      filter(Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Mes, Sexo) %>%
      summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Def)) %>%
      mutate(Mes = factor(Mes, levels = intersect(orden_meses, unique(as.character(Mes)))))
  })

  datos_ep_anual <- reactive({
    req(!is.null(datos_edadprov), input$ep_prov)
    df <- datos_edadprov$anual %>% filter(Sexo == "Ambos")
    nac <- df %>%
      group_by(Año) %>%
      summarise(Nacional = sum(Def, na.rm = TRUE), .groups = "drop")
    if (input$ep_prov != "Todas") {
      df <- df %>%
        filter(Provincia == input$ep_prov) %>%
        group_by(Año) %>%
        summarise(Provincia = sum(Def, na.rm = TRUE), .groups = "drop") %>%
        left_join(nac, by = "Año")
    } else {
      df <- nac %>% transmute(Año = Año, Provincia = Nacional, Nacional = Nacional)
    }
    df %>% filter(is.finite(Provincia)) %>% arrange(suppressWarnings(as.numeric(Año)))
  })

  output$ep_kpi_mediana <- renderText({
    df <- datos_ep_edad() %>% arrange(Edad_num)
    req(nrow(df) > 0)
    tot <- sum(df$Def, na.rm = TRUE)
    med <- df$Edad_num[which(cumsum(df$Def) >= tot / 2)[1]]
    if (!is.finite(med)) return("-")
    paste0(med, " años (", format(round(tot), big.mark = ".", decimal.mark = ","), " defunciones)")
  })

  output$ep_kpi_65 <- renderText({
    df <- datos_ep_edad()
    req(nrow(df) > 0)
    p <- sum(df$Def[df$Edad_num >= 65], na.rm = TRUE) / sum(df$Def, na.rm = TRUE) * 100
    if (!is.finite(p)) return("-")
    paste0(format(round(p, 1), decimal.mark = ","), " %")
  })

  output$ep_kpi_mes <- renderText({
    df <- datos_ep_mes()
    req(nrow(df) > 0)
    x <- df %>% group_by(Mes) %>% summarise(Def = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Def)) %>% slice(1)
    paste0(as.character(x$Mes), " (", format(round(x$Def), big.mark = ".", decimal.mark = ","), ")")
  })

  output$ep_piramide <- renderPlotly({
    df <- datos_ep_edad()
    req(nrow(df) > 0)
    df_h <- df %>% filter(Sexo == "Hombres")
    df_m <- df %>% filter(Sexo == "Mujeres")
    max_v <- max(df$Def, na.rm = TRUE) * 1.1
    if (!is.finite(max_v) || max_v <= 0) return(plotly_empty())
    ticks <- seq(-round(max_v), round(max_v), length.out = 7)
    plot_ly() %>%
      add_trace(data = df_h, x = ~-Def, y = ~Q5, type = "bar", orientation = "h",
                name = "Hombres", marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
                customdata = ~Def,
                hovertemplate = "<b>%{y}</b><br>Hombres: %{customdata:,.0f}<extra></extra>") %>%
      add_trace(data = df_m, x = ~Def, y = ~Q5, type = "bar", orientation = "h",
                name = "Mujeres", marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{y}</b><br>Mujeres: %{x:,.0f}<extra></extra>") %>%
      layout(barmode = "overlay", bargap = 0.1,
             hoverlabel = list(bgcolor = "white"),
             xaxis = list(title = "Defunciones", range = c(-max_v, max_v), tickmode = "array",
                          tickvals = ticks,
                          ticktext = format(abs(round(ticks)), big.mark = ".", decimal.mark = ",", trim = TRUE)),
             yaxis = list(title = "Tramo de edad", categoryorder = "array",
                          categoryarray = levels(df$Q5)),
             legend = list(orientation = "h", x = 0.35, y = 1.05),
             margin = list(l = 90, r = 30, t = 30, b = 60))
  })

  output$ep_meses <- renderPlotly({
    df <- datos_ep_mes()
    req(nrow(df) > 0)
    plot_ly(df, x = ~Mes, y = ~Def, color = ~Sexo, type = "bar",
            colors = c("Hombres" = "#00B2A9", "Mujeres" = "#FF6F61"),
            marker = list(line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:,.0f}<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "Mes", categoryorder = "array",
                          categoryarray = levels(df$Mes)),
             yaxis = list(title = "Defunciones"),
             legend = list(orientation = "h", y = -0.2),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, t = 20, b = 80))
  })

  output$ep_evol <- renderPlotly({
    df <- datos_ep_anual()
    req(nrow(df) > 0)
    df <- df %>% mutate(Ano_Num = suppressWarnings(as.numeric(Año)))
    p <- plot_ly(data = df, x = ~Ano_Num, y = ~Provincia, name = "Selección",
                 type = "scatter", mode = "lines+markers",
                 line = list(color = "#1f77b4", width = 3),
                 marker = list(color = "#1f77b4", size = 7, line = list(color = "white", width = 1.5)),
                 hovertemplate = "<b>%{x}</b><br>Defunciones: %{y:,.0f}<extra></extra>")
    if (input$ep_prov != "Todas" && all(is.finite(df$Nacional))) {
      p <- p %>% add_trace(data = df, x = ~Ano_Num, y = ~Nacional, name = "Total nacional",
                           type = "scatter", mode = "lines",
                           line = list(color = "#7f8c8d", width = 2, dash = "dash"),
                           hovertemplate = "<b>%{x}</b><br>Nacional: %{y:,.0f}<extra></extra>")
    }
    p %>% layout(xaxis = list(title = "Año", tickmode = "linear", dtick = 2),
                 yaxis = list(title = "Defunciones"),
                 legend = list(orientation = "h", x = 0.25, y = 1.1),
                 hoverlabel = list(bgcolor = "white"),
                 margin = list(l = 70, r = 20, t = 30, b = 50))
  })

  output$ep_tabla <- DT::renderDT({
    df <- datos_ep_anual() %>%
      transmute(Año = Año,
                Defunciones = Provincia,
                `Total nacional` = Nacional) %>%
      arrange(desc(suppressWarnings(as.numeric(Año))))
    DT::datatable(df, options = list(pageLength = 16, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Defunciones", "Total nacional"), 0, dec.mark = ",", mark = ".")
  })

  # --- PESTAÑA 8: ESTACIONALIDAD SERVER ---
  datos_meses_filtrados <- reactive({
    req(input$pmes_causa, input$pmes_ano, input$pmes_sexo)
    df <- meses_data %>% filter(Causa == input$pmes_causa)
    if (input$pmes_ano != "Todos") df <- df %>% filter(Año == input$pmes_ano)
    if (input$pmes_sexo != "Ambos") df <- df %>% filter(Sexo == input$pmes_sexo)
    df
  })
  
  output$pmes_kpi_total <- renderText({
    df <- datos_meses_filtrados()
    if (!nrow(df)) return("-")
    format(round(sum(df$Defunciones, na.rm = TRUE), 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$pmes_kpi_max <- renderText({
    df <- datos_meses_filtrados() %>% group_by(Mes) %>% summarise(Defunciones = sum(Defunciones, na.rm = TRUE), .groups = "drop")
    if (!nrow(df)) return("-")
    x <- df %>% arrange(desc(Defunciones)) %>% slice(1)
    paste0(as.character(x$Mes), " (", format(round(x$Defunciones, 0), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$pmes_kpi_min <- renderText({
    df <- datos_meses_filtrados() %>% group_by(Mes) %>% summarise(Defunciones = sum(Defunciones, na.rm = TRUE), .groups = "drop")
    if (!nrow(df)) return("-")
    x <- df %>% arrange(Defunciones) %>% slice(1)
    paste0(as.character(x$Mes), " (", format(round(x$Defunciones, 0), big.mark = ".", decimal.mark = ","), ")")
  })
  
  output$pmes_evolucion <- renderPlotly({
    df <- datos_meses_filtrados()
    req(nrow(df) > 0)
    df <- df %>%
      group_by(Año, Mes) %>%
      summarise(Defunciones = sum(Defunciones, na.rm = TRUE), .groups = "drop") %>%
      mutate(Año = as.character(Año), Mes = factor(Mes, levels = orden_meses))
    p <- plot_ly(
      df, x = ~Mes, y = ~Defunciones, color = ~Año,
      type = "scatter", mode = "lines+markers",
      marker = list(line = list(color = "white", width = 1)),
      hovertemplate = "<b>%{x} %{fullData.name}</b><br>Defunciones: %{y:,.0f}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Mes", categoryorder = "array", categoryarray = orden_meses),
        yaxis = list(title = "Defunciones"),
        legend = list(orientation = "h", y = -0.15),
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 70, r = 20, b = 80, t = 20)
      )
    p
  })

  # --- MORTALIDAD EVITABLE (aproximación por capítulos, sin límite <75) ---
  cesta_evitable <- c(
    "Causas externas de mortalidad" = "Prevenible",
    "Enfermedades infecciosas y parasitarias" = "Prevenible",
    "Enfermedades del sistema circulatorio" = "Tratable",
    "Enfermedades del sistema genitourinario" = "Tratable",
    "Embarazo, parto y puerperio" = "Tratable",
    "Afecciones originadas en el periodo perinatal" = "Tratable",
    "Tumores" = "Mixto",
    "Enfermedades del sistema respiratorio" = "Mixto",
    "Enfermedades del sistema digestivo" = "Mixto",
    "Enfermedades endocrinas, nutricionales y metabólicas" = "Mixto"
  )
  cesta_palanca <- c(
    "Prevenible" = "Prevención primaria y salud pública",
    "Tratable" = "Detección precoz y sistema asistencial",
    "Mixto" = "Mezcla causas evitables y no evitables",
    "Resto" = "Sin palanca clara a este nivel de desglose"
  )
  cesta_colores <- c("Prevenible" = "#0E9F8A", "Tratable" = "#1a2f47",
                     "Mixto" = "#E8A838", "Resto" = "#BDBDBD")

  asignar_cesta <- function(defuncion) {
    cesta <- unname(cesta_evitable[as.character(defuncion)])
    cesta[is.na(cesta)] <- "Resto"
    factor(cesta, levels = c("Prevenible", "Tratable", "Mixto", "Resto"))
  }

  # FIX: la población se extrae con distinct(Provincia, Sexo) porque viene
  # repetida en cada fila de causa; sumarla por causa la multiplicaría ×17.
  datos_pev <- reactive({
    req(input$pev_ano, input$pev_sexo)
    df <- causas_provinciales %>% filter(Año == input$pev_ano)
    if (input$pev_sexo != "Ambos") df <- df %>% filter(Sexo == input$pev_sexo)
    df <- df %>% mutate(Cesta = asignar_cesta(Defunción), Sensible = Cesta != "Resto")
    pob <- df %>%
      distinct(Provincia, Sexo, Poblacion) %>%
      group_by(Provincia) %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    df %>%
      group_by(Provincia, Comunidad) %>%
      summarise(Fall_Sens = sum(Fallecidos[Sensible], na.rm = TRUE),
                Fall_Tot = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = "Provincia") %>%
      mutate(Pct = if_else(Fall_Tot > 0, Fall_Sens / Fall_Tot * 100, NA_real_),
             Tasa = if_else(Poblacion > 0, Fall_Sens / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Pct), is.finite(Tasa))
  }) %>% bindCache(input$pev_ano, input$pev_sexo)

  datos_pev_evol <- reactive({
    req(input$pev_sexo)
    df <- causas_provinciales
    if (input$pev_sexo != "Ambos") df <- df %>% filter(Sexo == input$pev_sexo)
    df %>%
      mutate(Cesta = asignar_cesta(Defunción)) %>%
      group_by(Año, Año_Num, Cesta) %>%
      summarise(Fall = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      group_by(Año, Año_Num) %>%
      mutate(Pct = Fall / sum(Fall) * 100) %>%
      ungroup() %>%
      filter(is.finite(Pct))
  }) %>% bindCache(input$pev_sexo)

  output$pev_kpi_pct <- renderText({
    df <- datos_pev()
    if (!nrow(df)) return("-")
    paste0(format(round(sum(df$Fall_Sens) / sum(df$Fall_Tot) * 100, 1),
                  big.mark = ".", decimal.mark = ","), " %")
  })

  output$pev_kpi_tasa <- renderText({
    df <- datos_pev()
    if (!nrow(df)) return("-")
    paste0(format(round(sum(df$Fall_Sens) / sum(df$Poblacion) * 100000, 1),
                  big.mark = ".", decimal.mark = ","), " / 100k hab.")
  })

  output$pev_kpi_max <- renderText({
    df <- datos_pev() %>%
      group_by(Comunidad) %>%
      summarise(Fall_Sens = sum(Fall_Sens), Fall_Tot = sum(Fall_Tot), .groups = "drop") %>%
      mutate(Pct = Fall_Sens / Fall_Tot * 100) %>%
      filter(is.finite(Pct)) %>%
      arrange(desc(Pct))
    if (!nrow(df)) return("-")
    paste0(df$Comunidad[1], " (",
           format(round(df$Pct[1], 1), big.mark = ".", decimal.mark = ","), " %)")
  })

  output$pev_mapa <- renderLeaflet({
    df <- datos_pev()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    val_max <- suppressWarnings(max(mapa_datos$Pct, na.rm = TRUE))
    dominio_colores <- if (!is.finite(val_max) || val_max == 0) c(0, 1) else c(0, val_max)
    pal <- colorNumeric(palette = PAL_YLGNBU, domain = dominio_colores, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%% sensible: %s %%", mapa_datos$NAME_2,
                         format(round(mapa_datos$Pct, 1), decimal.mark = ",")) %>% lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Pct), weight = 1, color = "white",
                  fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio_colores, opacity = 0.8,
                title = "% sensible", position = "bottomright",
                labFormat = labelFormat(suffix = " %", digits = 1))
  })

  output$pev_evol <- renderPlotly({
    df <- datos_pev_evol()
    req(nrow(df) > 0)
    p <- plot_ly()
    for (cesta in c("Prevenible", "Tratable", "Mixto", "Resto")) {
      d <- df %>% filter(Cesta == cesta) %>% arrange(Año_Num)
      if (!nrow(d)) next
      p <- add_trace(p, data = d, x = ~Año_Num, y = ~Pct, name = cesta,
                     type = "scatter", mode = "lines", stackgroup = "one",
                     line = list(width = 0.5, color = "white"),
                     fillcolor = unname(cesta_colores[cesta]),
                     hovertemplate = paste0("<b>", cesta, " %{x}</b><br>%: %{y:.1f}<extra></extra>"))
    }
    p %>% layout(xaxis = list(title = "Año", dtick = 1), yaxis = list(title = "% sobre el total"),
                 legend = list(orientation = "h", y = -0.15),
                 hoverlabel = list(bgcolor = "white"),
                 margin = list(l = 60, r = 20, b = 80, t = 20))
  })

  output$pev_ranking <- renderPlotly({
    df <- datos_pev() %>%
      group_by(Comunidad) %>%
      summarise(Fall_Sens = sum(Fall_Sens), Poblacion = sum(Poblacion), .groups = "drop") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fall_Sens / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      arrange(Tasa)
    req(nrow(df) > 0)
    pal <- colorNumeric(palette = PAL_YLGNBU, domain = dominio_seguro(df$Tasa))
    plot_ly(df, x = ~Tasa, y = ~reorder(Comunidad, Tasa), type = "bar", orientation = "h",
            marker = list(color = ~pal(Tasa), line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>Tasa: %{x:.1f} / 100k<extra></extra>") %>%
      layout(xaxis = list(title = "Tasa sensible / 100k hab."), yaxis = list(title = ""),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 140, r = 20, b = 60, t = 20))
  })

  output$pev_tabla <- DT::renderDT({
    req(input$pev_ano, input$pev_sexo)
    df <- causas_provinciales %>% filter(Año == input$pev_ano)
    if (input$pev_sexo != "Ambos") df <- df %>% filter(Sexo == input$pev_sexo)
    total <- sum(df$Fallecidos, na.rm = TRUE)
    tab <- df %>%
      mutate(Cesta = asignar_cesta(Defunción)) %>%
      group_by(Capítulo = Defunción, Cesta) %>%
      summarise(Defunciones = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      mutate(`% total` = round(Defunciones / total * 100, 1),
             Palanca = unname(cesta_palanca[as.character(Cesta)])) %>%
      arrange(desc(Defunciones))
    DT::datatable(tab, options = list(pageLength = 17, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatRound("% total", 1, dec.mark = ",", mark = ".") %>%
      DT::formatRound("Defunciones", 0, dec.mark = ",", mark = ".")
  })

  # --- DESIGUALDAD TERRITORIAL (Gini ponderado + brechas vs nacional) ---
  gini_pond <- function(x, w) {
    ok <- is.finite(x) & is.finite(w) & w > 0
    x <- x[ok]
    w <- w[ok]
    if (length(x) < 2) return(NA_real_)
    mu <- sum(w * x) / sum(w)
    if (!is.finite(mu) || mu == 0) return(NA_real_)
    sum(outer(w, w) * abs(outer(x, x, `-`))) / (2 * sum(w)^2 * mu)
  }

  # FIX: la población se extrae con distinct(Provincia, Sexo) porque viene
  # repetida en cada fila de causa; con "Todas" se suman los capítulos.
  datos_des <- reactive({
    req(input$des_ano, input$des_sexo, input$des_causa)
    df <- causas_provinciales %>% filter(Año == input$des_ano)
    if (input$des_sexo != "Ambos") df <- df %>% filter(Sexo == input$des_sexo)
    if (input$des_causa != "Todas") df <- df %>% filter(Defunción == input$des_causa)
    pob <- df %>%
      distinct(Provincia, Sexo, Poblacion) %>%
      group_by(Provincia) %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    prov <- df %>%
      group_by(Provincia) %>%
      summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = "Provincia") %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    tasa_nac <- sum(prov$Fallecidos) / sum(prov$Poblacion) * 100000
    prov %>% mutate(Ratio = Tasa / tasa_nac)
  }) %>% bindCache(input$des_ano, input$des_sexo, input$des_causa)

  datos_des_evol <- reactive({
    req(input$des_sexo, input$des_causa)
    df <- causas_provinciales
    if (input$des_sexo != "Ambos") df <- df %>% filter(Sexo == input$des_sexo)
    if (input$des_causa != "Todas") df <- df %>% filter(Defunción == input$des_causa)
    pob <- df %>%
      distinct(Año, Año_Num, Provincia, Sexo, Poblacion) %>%
      group_by(Año, Año_Num, Provincia) %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    df %>%
      group_by(Año, Año_Num, Provincia) %>%
      summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = c("Año", "Año_Num", "Provincia")) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      group_by(Año, Año_Num) %>%
      summarise(Gini = gini_pond(Tasa, Poblacion),
                P90_P10 = unname(stats::quantile(Tasa, 0.9) / stats::quantile(Tasa, 0.1)),
                Max_Min = max(Tasa) / min(Tasa[Tasa > 0]), .groups = "drop") %>%
      filter(is.finite(Gini), is.finite(P90_P10), is.finite(Max_Min)) %>%
      arrange(Año_Num)
  }) %>% bindCache(input$des_sexo, input$des_causa)

  output$des_kpi_gini <- renderText({
    df <- datos_des()
    if (!nrow(df)) return("-")
    format(round(gini_pond(df$Tasa, df$Poblacion), 3), decimal.mark = ",")
  })

  output$des_kpi_p90 <- renderText({
    df <- datos_des()
    if (!nrow(df)) return("-")
    format(round(unname(stats::quantile(df$Tasa, 0.9) / stats::quantile(df$Tasa, 0.1)), 2),
           big.mark = ".", decimal.mark = ",")
  })

  output$des_kpi_maxmin <- renderText({
    df <- datos_des()
    if (!nrow(df)) return("-")
    format(round(max(df$Tasa) / min(df$Tasa[df$Tasa > 0]), 2),
           big.mark = ".", decimal.mark = ",")
  })

  output$des_mapa <- renderLeaflet({
    df <- datos_des()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    lim <- suppressWarnings(max(abs(mapa_datos$Ratio - 1), na.rm = TRUE))
    dominio_colores <- if (!is.finite(lim) || lim == 0) c(0.9, 1.1) else c(1 - lim, 1 + lim)
    pal <- colorNumeric(palette = PAL_RDBU_REV, domain = dominio_colores, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>Ratio: %s (tasa %s / 100k)",
                         mapa_datos$NAME_2,
                         format(round(mapa_datos$Ratio, 2), decimal.mark = ","),
                         format(round(mapa_datos$Tasa, 1), decimal.mark = ",")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Ratio), weight = 1, color = "white",
                  fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio_colores, opacity = 0.8,
                title = "Ratio vs nacional", position = "bottomright",
                labFormat = labelFormat(digits = 2))
  })

  output$des_evol <- renderPlotly({
    df <- datos_des_evol()
    req(nrow(df) > 0)
    plot_ly(df, x = ~Año_Num) %>%
      add_trace(y = ~P90_P10, name = "P90/P10", type = "scatter", mode = "lines+markers",
                line = list(color = "#1a2f47"),
                marker = list(color = "#1a2f47", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>P90/P10: %{y:.2f}<extra></extra>") %>%
      add_trace(y = ~Max_Min, name = "Máx/Mín", type = "scatter", mode = "lines+markers",
                line = list(color = "#E53935"),
                marker = list(color = "#E53935", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>Máx/Mín: %{y:.2f}<extra></extra>") %>%
      add_trace(y = ~Gini, name = "Gini", type = "scatter", mode = "lines+markers", yaxis = "y2",
                line = list(color = "#0E9F8A"),
                marker = list(color = "#0E9F8A", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>Gini: %{y:.3f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1), yaxis = list(title = "Ratios"),
             yaxis2 = list(title = "Gini", overlaying = "y", side = "right", showgrid = FALSE),
             legend = list(orientation = "h", y = -0.15),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 60, b = 80, t = 20))
  })

  output$des_ranking <- renderPlotly({
    df <- datos_des() %>% arrange(Ratio)
    req(nrow(df) > 0)
    lim <- suppressWarnings(max(abs(df$Ratio - 1)))
    if (!is.finite(lim) || lim == 0) lim <- 0.1
    plot_ly(df, x = ~Ratio, y = ~reorder(Provincia, Ratio), type = "bar", orientation = "h",
            marker = list(color = ~Ratio, colorscale = escala_plotly(PAL_RDBU_REV),
                          cmin = 1 - lim, cmax = 1 + lim, showscale = TRUE,
                          colorbar = list(title = "Ratio"),
                          line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>Ratio: %{x:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Ratio frente a la media nacional"),
             yaxis = list(title = ""), hoverlabel = list(bgcolor = "white"),
             shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1,
                                yref = "paper",
                                line = list(color = "black", dash = "dash"))),
             margin = list(l = 140, r = 20, b = 60, t = 20))
  })

  output$des_tabla <- DT::renderDT({
    tab <- datos_des() %>%
      transmute(Provincia, Tasa = round(Tasa, 1), Ratio = round(Ratio, 3),
                Fallecidos) %>%
      arrange(desc(Ratio))
    DT::datatable(tab, options = list(pageLength = 17, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatRound(c("Tasa", "Ratio"), c(1, 3), dec.mark = ",", mark = ".") %>%
      DT::formatRound("Fallecidos", 0, dec.mark = ",", mark = ".")
  })

  # --- ALERTAS DE ATÍPICOS (z temporal 2022 vs 2018-2021 + z transversal) ---
  # FIX: población con distinct(Provincia, Sexo, Año) por venir repetida por causa.
  datos_al_tasas <- reactive({
    req(input$al_sexo)
    df <- causas_provinciales
    if (input$al_sexo != "Ambos") df <- df %>% filter(Sexo == input$al_sexo)
    pob <- df %>%
      distinct(Año, Año_Num, Provincia, Sexo, Poblacion) %>%
      group_by(Año, Año_Num, Provincia) %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    df %>%
      group_by(Año, Año_Num, Provincia, Defunción) %>%
      summarise(Fallecidos = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = c("Año", "Año_Num", "Provincia")) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
  }) %>% bindCache(input$al_sexo)

  datos_al <- reactive({
    t <- datos_al_tasas()
    req(nrow(t) > 0)
    hist <- t %>%
      filter(Año_Num < 2022) %>%
      group_by(Provincia, Defunción) %>%
      summarise(n = sum(is.finite(Tasa)), F_hist = sum(Fallecidos, na.rm = TRUE),
                Media = mean(Tasa, na.rm = TRUE), SD = stats::sd(Tasa, na.rm = TRUE),
                .groups = "drop")
    cur <- t %>%
      filter(Año_Num == 2022) %>%
      transmute(Provincia, Causa = Defunción, F2022 = Fallecidos, Tasa2022 = Tasa)
    niv <- cur %>%
      group_by(Causa) %>%
      mutate(Media_nac = mean(Tasa2022, na.rm = TRUE),
             SD_nac = stats::sd(Tasa2022, na.rm = TRUE)) %>%
      ungroup()
    base <- cur %>%
      left_join(hist, by = c("Provincia", "Causa" = "Defunción")) %>%
      left_join(niv %>% select(Provincia, Causa, Media_nac, SD_nac),
                by = c("Provincia", "Causa")) %>%
      mutate(
        z_temp = if_else(!is.na(SD) & SD > 0 & n >= 3 & F_hist >= 20,
                         (Tasa2022 - Media) / SD, NA_real_),
        z_nivel = if_else(!is.na(SD_nac) & SD_nac > 0 & F2022 >= 20,
                          (Tasa2022 - Media_nac) / SD_nac, NA_real_)
      )
    temp <- base %>%
      filter(is.finite(z_temp), abs(z_temp) >= 2) %>%
      transmute(Provincia, Causa, Tipo = "Cambio brusco", Tasa2022,
                Referencia = Media, z = z_temp)
    nivel <- base %>%
      filter(is.finite(z_nivel), abs(z_nivel) >= 2.5) %>%
      transmute(Provincia, Causa, Tipo = "Nivel extremo", Tasa2022,
                Referencia = Media_nac, z = z_nivel)
    bind_rows(temp, nivel) %>%
      mutate(absz = abs(z)) %>%
      arrange(desc(absz))
  }) %>% bindCache(input$al_sexo)

  datos_al_filtrados <- reactive({
    req(input$al_causa, input$al_tipo)
    df <- datos_al()
    if (input$al_causa != "Todas") df <- df %>% filter(Causa == input$al_causa)
    if (input$al_tipo != "Todas") df <- df %>% filter(Tipo == input$al_tipo)
    df
  })

  output$al_kpi_n <- renderText({
    format(nrow(datos_al()), big.mark = ".", decimal.mark = ",")
  })

  output$al_kpi_top <- renderText({
    df <- datos_al()
    if (!nrow(df)) return("-")
    paste0(df$Provincia[1], " · ", substr(as.character(df$Causa[1]), 1, 28),
           " (z=", format(round(df$z[1], 1), decimal.mark = ","), ")")
  })

  output$al_kpi_prov <- renderText({
    format(dplyr::n_distinct(datos_al()$Provincia), big.mark = ".", decimal.mark = ",")
  })

  output$al_barras <- renderPlotly({
    df <- datos_al_filtrados() %>% slice_head(n = 15) %>% arrange(absz)
    req(nrow(df) > 0)
    df <- df %>% mutate(Etiqueta = paste0(Provincia, " · ", substr(Causa, 1, 30)))
    plot_ly(df, x = ~z, y = ~reorder(Etiqueta, absz), type = "bar", orientation = "h",
            marker = list(color = ifelse(df$z >= 0, "#C0392B", "#2471A3"),
                          line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>%{customdata}<br>z: %{x:.2f}<extra></extra>",
            customdata = ~Tipo) %>%
      layout(xaxis = list(title = "z (signo = dirección)"), yaxis = list(title = ""),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 220, r = 20, b = 60, t = 20))
  })

  output$al_serie <- renderPlotly({
    df <- datos_al_filtrados()
    req(nrow(df) > 0)
    sel <- input$al_tabla_rows_selected
    fila <- if (length(sel) && sel >= 1 && sel <= nrow(df)) df[sel, ] else df[1, ]
    t <- datos_al_tasas() %>%
      filter(Provincia == fila$Provincia, Defunción == fila$Causa) %>%
      arrange(Año_Num)
    req(nrow(t) > 0)
    nac <- datos_al_tasas() %>%
      filter(Defunción == fila$Causa) %>%
      group_by(Año, Año_Num) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE),
                P = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>% arrange(Año_Num)
    plot_ly() %>%
      add_trace(data = t, x = ~Año_Num, y = ~Tasa, name = fila$Provincia,
                type = "scatter", mode = "lines+markers",
                line = list(color = "#C0392B", width = 2.5),
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>Tasa: %{y:.1f}<extra></extra>") %>%
      add_trace(data = nac, x = ~Año_Num, y = ~Tasa, name = "Nacional",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#1a2f47", dash = "dash"),
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>Nacional: %{y:.1f}<extra></extra>") %>%
      layout(title = list(text = paste0(fila$Provincia, " · ", fila$Causa,
                                        " (", fila$Tipo, ", z=",
                                        format(round(fila$z, 2), decimal.mark = ","),
                                        ")"),
                          font = list(size = 12)),
             xaxis = list(title = "Año", dtick = 1), yaxis = list(title = "Tasa / 100k"),
             legend = list(orientation = "h", y = -0.18),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, b = 80, t = 50))
  })

  output$al_tabla <- DT::renderDT({
    tab <- datos_al_filtrados() %>%
      transmute(Provincia, Causa, Tipo, `Tasa 2022` = round(Tasa2022, 1),
                Referencia = round(Referencia, 1), z = round(z, 2))
    DT::datatable(tab, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE, selection = "single") %>%
      DT::formatRound(c("Tasa 2022", "Referencia", "z"), c(1, 1, 2),
                      dec.mark = ",", mark = ".")
  })

  # --- COMPARADOR DE CCAA (una comunidad frente a la media nacional) ---
  # FIX: población con distinct(Comunidad, Provincia, Sexo) por venir repetida
  # en cada fila de causa.
  datos_cc <- reactive({
    req(input$cc_comunidad, input$cc_ano, input$cc_sexo)
    df <- causas_provinciales %>% filter(Año == input$cc_ano)
    if (input$cc_sexo != "Ambos") df <- df %>% filter(Sexo == input$cc_sexo)
    pob <- df %>%
      distinct(Comunidad, Provincia, Sexo, Poblacion) %>%
      group_by(Comunidad) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    pn <- sum(pob$P)
    nac <- df %>%
      group_by(Defunción) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa_nac = F / pn * 100000)
    cap <- df %>%
      group_by(Comunidad, Defunción) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = "Comunidad") %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    cc <- cap %>%
      filter(Comunidad == input$cc_comunidad) %>%
      transmute(Causa = Defunción, Tasa_cc = Tasa) %>%
      right_join(nac %>% transmute(Causa = Defunción, Tasa_nac), by = "Causa") %>%
      mutate(Tasa_cc = dplyr::coalesce(Tasa_cc, 0),
             Ratio = if_else(Tasa_nac > 0, Tasa_cc / Tasa_nac, NA_real_))
    tot <- cap %>%
      group_by(Comunidad) %>%
      summarise(F = sum(F, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = "Comunidad") %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>% arrange(desc(Tasa))
    list(cap = cc, tot = tot, tasa_nac = sum(nac$F, na.rm = TRUE) / pn * 100000)
  }) %>% bindCache(input$cc_comunidad, input$cc_ano, input$cc_sexo)

  datos_cc_evol <- reactive({
    req(input$cc_comunidad, input$cc_sexo)
    df <- causas_provinciales
    if (input$cc_sexo != "Ambos") df <- df %>% filter(Sexo == input$cc_sexo)
    pob <- df %>%
      distinct(Año, Año_Num, Comunidad, Provincia, Sexo, Poblacion) %>%
      group_by(Año, Año_Num, Comunidad) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    t <- df %>%
      group_by(Año, Año_Num, Comunidad) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob, by = c("Año", "Año_Num", "Comunidad")) %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    cc <- t %>%
      filter(Comunidad == input$cc_comunidad) %>%
      transmute(Año_Num, Tasa_cc = Tasa)
    nac <- t %>%
      group_by(Año, Año_Num) %>%
      summarise(F = sum(F, na.rm = TRUE), P = sum(P, na.rm = TRUE), .groups = "drop") %>%
      transmute(Año_Num, Tasa_nac = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa_nac))
    left_join(cc, nac, by = "Año_Num") %>% arrange(Año_Num)
  }) %>% bindCache(input$cc_comunidad, input$cc_sexo)

  output$cc_kpi_tasa <- renderText({
    d <- datos_cc()
    t <- d$tot %>% filter(Comunidad == input$cc_comunidad)
    if (!nrow(t)) return("-")
    paste0(format(round(t$Tasa[1], 1), big.mark = ".", decimal.mark = ","),
           " / 100k (nac: ",
           format(round(d$tasa_nac, 1), big.mark = ".", decimal.mark = ","), ")")
  })

  output$cc_kpi_brecha <- renderText({
    d <- datos_cc()
    t <- d$tot %>% filter(Comunidad == input$cc_comunidad)
    if (!nrow(t) || !is.finite(d$tasa_nac) || d$tasa_nac == 0) return("-")
    b <- (t$Tasa[1] - d$tasa_nac) / d$tasa_nac * 100
    paste0(ifelse(b >= 0, "+", ""), format(round(b, 1), decimal.mark = ","), " %")
  })

  output$cc_kpi_puesto <- renderText({
    d <- datos_cc()
    p <- match(input$cc_comunidad, d$tot$Comunidad)
    if (is.na(p)) return("-")
    paste0(p, "º de ", nrow(d$tot))
  })

  output$cc_radar <- renderPlotly({
    d <- datos_cc()$cap
    req(nrow(d) > 0)
    d <- d %>% mutate(Etiqueta = stringr::str_wrap(Causa, width = 22))
    plot_ly(type = "scatterpolar", fill = "toself", mode = "lines+markers") %>%
      add_trace(r = d$Tasa_cc, theta = d$Etiqueta, name = input$cc_comunidad,
                marker = list(color = "#0E9F8A"), fillcolor = "rgba(14,159,138,0.25)",
                hovertemplate = "<b>%{theta}</b><br>Tasa: %{r:.1f}<extra></extra>") %>%
      add_trace(r = d$Tasa_nac, theta = d$Etiqueta, name = "Nacional",
                marker = list(color = "#1a2f47"), fillcolor = "rgba(26,47,71,0.15)",
                hovertemplate = "<b>%{theta}</b><br>Nacional: %{r:.1f}<extra></extra>") %>%
      layout(polar = list(radialaxis = list(visible = TRUE)),
             legend = list(orientation = "h", y = -0.15),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 40, r = 40, b = 80, t = 20))
  })

  output$cc_evol <- renderPlotly({
    df <- datos_cc_evol()
    req(nrow(df) > 0)
    plot_ly(df, x = ~Año_Num) %>%
      add_trace(y = ~Tasa_cc, name = input$cc_comunidad, type = "scatter",
                mode = "lines+markers", line = list(color = "#0E9F8A", width = 2.5),
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>Tasa: %{y:.1f}<extra></extra>") %>%
      add_trace(y = ~Tasa_nac, name = "Nacional", type = "scatter",
                mode = "lines+markers", line = list(color = "#1a2f47", dash = "dash"),
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{x}</b><br>Nacional: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1), yaxis = list(title = "Tasa / 100k"),
             legend = list(orientation = "h", y = -0.15),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, b = 80, t = 20))
  })

  output$cc_brechas <- renderPlotly({
    df <- datos_cc()$cap %>% filter(is.finite(Ratio)) %>% arrange(Ratio)
    req(nrow(df) > 0)
    lim <- suppressWarnings(max(abs(df$Ratio - 1)))
    if (!is.finite(lim) || lim == 0) lim <- 0.1
    plot_ly(df, x = ~Ratio, y = ~reorder(Causa, Ratio), type = "bar", orientation = "h",
            marker = list(color = ~Ratio, colorscale = escala_plotly(PAL_RDBU_REV),
                          cmin = 1 - lim, cmax = 1 + lim, showscale = TRUE,
                          colorbar = list(title = "Ratio"),
                          line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>Ratio: %{x:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Ratio frente a la media nacional"),
             yaxis = list(title = "", tickfont = list(size = 10)),
             hoverlabel = list(bgcolor = "white"),
             shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1,
                                yref = "paper",
                                line = list(color = "black", dash = "dash"))),
             margin = list(l = 220, r = 20, b = 60, t = 20))
  })

  output$cc_tabla <- DT::renderDT({
    tab <- datos_cc()$cap %>%
      transmute(Capítulo = Causa, `Tasa CCAA` = round(Tasa_cc, 1),
                `Tasa nacional` = round(Tasa_nac, 1), Ratio = round(Ratio, 3)) %>%
      arrange(desc(Ratio))
    DT::datatable(tab, options = list(pageLength = 13, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatRound(c("Tasa CCAA", "Tasa nacional", "Ratio"), c(1, 1, 3),
                      dec.mark = ",", mark = ".")
  })

}
