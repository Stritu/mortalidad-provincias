server_causas <- function(input, output, session) {
  
  # Ayudas contextuales
  output$p2_filtro_info <- renderUI({
    req(input$p2_defuncion)
    div(class = "filter-help",
        HTML(paste0("<b>Causa:</b> ", htmltools::htmlEscape(input$p2_defuncion),
                    ". La tasa de la causa es defunciones / población × 100.000.")))
  })
  
  
  # --- PESTAÑA 2 SERVER ---
  # Tasa por causa: helper puro tasa_causa_prov() (global.R); aquí solo renombres.
  datos_p2 <- reactive({
    req(input$p2_defuncion, input$p2_ano, input$p2_sexo)
    tasa_causa_prov(input$p2_ano, input$p2_sexo, input$p2_defuncion) %>%
      transmute(Provincia, Total_Fallecidos = Fallecidos,
                Poblacion_Total = Poblacion, Tasa)
  }) %>% bindCache(input$p2_defuncion, input$p2_ano, input$p2_sexo, cache = "app")

  # --- MAPA ÚNICO PROVINCIAL (los 4 indicadores en una tarjeta) ---
  datos_map <- reactive({
    req(input$map_ind, input$map_ano, input$map_sexo)
    ind <- input$map_ind
    if (ind == "Tasa por causa") {
      req(input$map_causa)
      df <- tasa_causa_prov(input$map_ano, input$map_sexo, input$map_causa)
      list(df = df %>% transmute(Provincia, Valor = Tasa),
           etiqueta = "Tasa", unidad = " / 100k", unidad_leg = "",
           titulo = paste0("Tasa por causa: ", input$map_causa, " (", input$map_ano, ")"),
           pal = PAL_YLORRD, desde_cero = TRUE, fmt = 1)
    } else if (ind == "Estandarizada") {
      df <- tasa_std_prov(input$map_ano, input$map_sexo)
      req(nrow(df) > 0)
      list(df = df %>% transmute(Provincia, Valor = Tasa_std, Bruta = Tasa_bruta),
           etiqueta = "Estandarizada", unidad = " / 100k", unidad_leg = "",
           titulo = paste0("Tasa estandarizada ESP 2013 (", input$map_ano, ")"),
           pal = PAL_YLORRD, desde_cero = TRUE, fmt = 1)
    } else if (ind == "% sensible") {
      df <- tasa_sensible_prov(input$map_ano, input$map_sexo)
      req(nrow(df) > 0)
      list(df = df %>% transmute(Provincia, Valor = Pct),
           etiqueta = "% sensible", unidad = " %", unidad_leg = " %",
           titulo = paste0("% sensible a prevención/sanidad (", input$map_ano, ")"),
           pal = PAL_YLGNBU, desde_cero = TRUE, fmt = 1)
    } else {
      req(input$map_causa)
      df <- brecha_prov(input$map_ano, input$map_sexo, input$map_causa)
      req(nrow(df) > 0)
      list(df = df %>% transmute(Provincia, Valor = Ratio, Tasa),
           etiqueta = "Ratio", unidad = "", unidad_leg = "",
           titulo = paste0("Brecha frente a nacional (", input$map_ano, ")"),
           pal = PAL_RDBU_REV, desde_cero = FALSE, fmt = 2)
    }
  }) %>% bindCache(input$map_ind, input$map_ano, input$map_sexo, input$map_causa, cache = "app")

  output$map_titulo <- renderText({
    datos_map()$titulo
  })

  output$map_kpi_max <- renderText({
    m <- datos_map()
    req(nrow(m$df) > 0)
    x <- m$df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Valor, m$fmt), big.mark = ".", decimal.mark = ","), m$unidad, ")")
  })

  output$map_kpi_min <- renderText({
    m <- datos_map()
    req(nrow(m$df) > 0)
    x <- m$df %>% arrange(Valor) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Valor, m$fmt), big.mark = ".", decimal.mark = ","), m$unidad, ")")
  })

  output$map_mapa <- renderLeaflet({
    m <- datos_map()
    req(nrow(m$df) > 0)
    md <- mapa_provincias %>% left_join(m$df, by = c("NAME_2" = "Provincia"))
    dom <- if (m$desde_cero) {
      vm <- suppressWarnings(max(md$Valor, na.rm = TRUE))
      # Margen: el máximo exacto puede quedar fuera por coma flotante.
      if (!is.finite(vm) || vm == 0) c(0, 1) else c(0, vm * 1.02 + 1e-9)
    } else {
      lim <- suppressWarnings(max(abs(md$Valor - 1), na.rm = TRUE))
      if (!is.finite(lim) || lim == 0) {
        c(0.9, 1.1)
      } else {
        mg <- 0.02 * lim + 1e-9
        c(1 - lim - mg, 1 + lim + mg)
      }
    }
    pal <- colorNumeric(palette = m$pal, domain = dom, na.color = "#E0E0E0")
    fv <- function(v) format(round(v, m$fmt), decimal.mark = ",")
    extra <- if ("Bruta" %in% names(md)) paste0("<br/>Bruta: ", fv(md$Bruta), " / 100k") else ""
    extra <- if ("Tasa" %in% names(m$df) && !"Bruta" %in% names(m$df)) {
      paste0("<br/>Tasa: ", fv(md$Tasa), " / 100k")
    } else extra
    etiquetas <- sprintf("<strong>%s</strong><br/>%s: %s%s%s",
                         md$NAME_2, m$etiqueta, fv(md$Valor), m$unidad, extra) %>%
      lapply(htmltools::HTML)
    leaflet(md) %>%
      tiles_osm() %>%
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white",
                  fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dom, opacity = 0.8, title = m$etiqueta,
                position = "bottomright",
                labFormat = labelFormat(suffix = m$unidad_leg, digits = m$fmt)) %>%
      control_ano(input$map_ano)
  })

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
  
  # --- COMPARADOR TERRITORIAL (nivel Provincia/CCAA, A frente a B o nacional) ---
  observe({
    req(input$ct_nivel)
    vals <- if (input$ct_nivel == "CCAA") sort(unique(causas_provinciales$Comunidad)) else sort(unique(causas_provinciales$Provincia))
    updateSelectInput(session, "ct_a", choices = vals,
                      selected = if ("Madrid" %in% vals) "Madrid" else vals[1])
    updateSelectInput(session, "ct_b", choices = c("Media nacional", vals),
                      selected = "Media nacional")
  })

  # Si A y B coinciden (y B no es la nacional), B vuelve a la nacional.
  observe({
    req(input$ct_a, input$ct_b)
    if (input$ct_b != "Media nacional" && identical(input$ct_a, input$ct_b))
      updateSelectInput(session, "ct_b", selected = "Media nacional")
  })

  # FIX: población con distinct por venir repetida en cada fila de causa.
  datos_ct <- reactive({
    req(input$ct_nivel, input$ct_a, input$ct_b, input$ct_causa, input$ct_ano, input$ct_sexo)
    df <- causas_provinciales
    if (input$ct_sexo != "Ambos") df <- df %>% filter(Sexo == input$ct_sexo)
    base <- df %>%
      group_by(Año, Año_Num, Provincia, Comunidad, Defunción) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop")
    pob <- df %>%
      distinct(Año, Año_Num, Provincia, Sexo, Poblacion) %>%
      group_by(Año, Año_Num, Provincia) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    if (input$ct_nivel == "CCAA") {
      prov_com <- distinct(df, Provincia, Comunidad)
      base <- base %>%
        group_by(Año, Año_Num, Entidad = Comunidad, Defunción) %>%
        summarise(F = sum(F, na.rm = TRUE), .groups = "drop")
      pob <- pob %>%
        left_join(prov_com, by = "Provincia") %>%
        group_by(Año, Año_Num, Entidad = Comunidad) %>%
        summarise(P = sum(P, na.rm = TRUE), .groups = "drop")
    } else {
      base <- base %>% transmute(Año, Año_Num, Entidad = Provincia, Defunción, F)
      pob <- pob %>% transmute(Año, Año_Num, Entidad = Provincia, P)
    }
    # Capítulo-año: tasas por entidad.
    cap <- base %>%
      filter(Año == input$ct_ano) %>%
      group_by(Entidad, Defunción) %>%
      summarise(F = sum(F, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob %>% filter(Año == input$ct_ano) %>% select(Entidad, P),
                by = "Entidad") %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    pn_ano <- sum(pob$P[pob$Año == input$ct_ano], na.rm = TRUE)
    nac <- base %>%
      filter(Año == input$ct_ano) %>%
      group_by(Defunción) %>%
      summarise(Fn = sum(F, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa_nac = Fn / pn_ano * 100000)
    # Evolución de la causa (o total): A, B y nacional.
    ev_base <- base
    if (input$ct_causa != "Todas") ev_base <- ev_base %>% filter(Defunción == input$ct_causa)
    pob_ano <- pob %>%
      group_by(Año, Año_Num, Entidad) %>%
      summarise(P = sum(P, na.rm = TRUE), .groups = "drop")
    ev <- ev_base %>%
      group_by(Año, Año_Num, Entidad) %>%
      summarise(F = sum(F, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob_ano, by = c("Año", "Año_Num", "Entidad")) %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa))
    ev_nac <- ev_base %>%
      group_by(Año, Año_Num) %>%
      summarise(F = sum(F, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob %>% group_by(Año, Año_Num) %>%
                  summarise(P = sum(P, na.rm = TRUE), .groups = "drop"),
                by = c("Año", "Año_Num")) %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>%
      arrange(Año_Num)
    list(cap = cap, nac = nac, ev = ev, ev_nac = ev_nac)
  }) %>% bindCache(input$ct_nivel, input$ct_a, input$ct_b, input$ct_causa,
                   input$ct_ano, input$ct_sexo, cache = "app")

  # Referencia efectiva: B, o nacional si B es la nacional o coincide con A.
  ct_ref <- function() {
    if (input$ct_b == "Media nacional" || identical(input$ct_a, input$ct_b)) "NACIONAL" else input$ct_b
  }

  output$ct_info <- renderUI({
    req(input$ct_a, input$ct_b, input$ct_causa, input$ct_ano)
    ref <- if (ct_ref() == "NACIONAL") "la media nacional" else ct_ref()
    div(class = "filter-help",
        HTML(paste0("<b>A:</b> ", htmltools::htmlEscape(input$ct_a),
                    " · <b>Ref:</b> ", htmltools::htmlEscape(ref),
                    " · <b>Causa:</b> ", htmltools::htmlEscape(input$ct_causa),
                    " · <b>Año:</b> ", htmltools::htmlEscape(as.character(input$ct_ano)), ".")))
  })

  # Tasa de A y de referencia (causa y año) para KPIs.
  datos_ct_kpi <- function() {
    d <- datos_ct()
    a <- d$cap %>% filter(Entidad == input$ct_a)
    if (input$ct_causa != "Todas") a <- a %>% filter(Defunción == input$ct_causa)
    f_a <- sum(a$F, na.rm = TRUE)
    p_a <- if (nrow(a)) a$P[1] else NA_real_
    t_a <- if (is.finite(p_a) && p_a > 0) f_a / p_a * 100000 else NA_real_
    if (ct_ref() == "NACIONAL") {
      n <- d$nac
      if (input$ct_causa != "Todas") n <- n %>% filter(Defunción == input$ct_causa)
      f_r <- sum(n$Fn, na.rm = TRUE)
      pn <- sum(unique(d$cap$P), na.rm = TRUE)
      t_r <- if (is.finite(pn) && pn > 0) f_r / pn * 100000 else NA_real_
    } else {
      b <- d$cap %>% filter(Entidad == ct_ref())
      if (input$ct_causa != "Todas") b <- b %>% filter(Defunción == input$ct_causa)
      f_r <- sum(b$F, na.rm = TRUE)
      p_r <- if (nrow(b)) b$P[1] else NA_real_
      t_r <- if (is.finite(p_r) && p_r > 0) f_r / p_r * 100000 else NA_real_
    }
    list(t_a = t_a, t_r = t_r)
  }

  output$ct_kpi_tasa <- renderText({
    k <- datos_ct_kpi()
    if (!is.finite(k$t_a)) return("-")
    ref_nom <- if (ct_ref() == "NACIONAL") "nac" else "B"
    paste0(format(round(k$t_a, 1), big.mark = ".", decimal.mark = ","),
           " / 100k (", ref_nom, ": ",
           format(round(k$t_r, 1), big.mark = ".", decimal.mark = ","), ")")
  })

  output$ct_kpi_brecha <- renderText({
    k <- datos_ct_kpi()
    if (!is.finite(k$t_a) || !is.finite(k$t_r) || k$t_r == 0) return("-")
    b <- (k$t_a - k$t_r) / k$t_r * 100
    paste0(ifelse(b >= 0, "+", ""), format(round(b, 1), decimal.mark = ","), " %")
  })

  output$ct_kpi_top <- renderText({
    d <- datos_ct()
    a <- d$cap %>% filter(Entidad == input$ct_a) %>% select(Defunción, Tasa_a = Tasa)
    if (ct_ref() == "NACIONAL") {
      r <- a %>% left_join(d$nac %>% select(Defunción, Tasa_nac), by = "Defunción") %>%
        mutate(Ratio = if_else(Tasa_nac > 0, Tasa_a / Tasa_nac, NA_real_))
    } else {
      r <- a %>% left_join(d$cap %>% filter(Entidad == ct_ref()) %>%
                             select(Defunción, Tasa_b = Tasa), by = "Defunción") %>%
        mutate(Ratio = if_else(Tasa_b > 0, Tasa_a / Tasa_b, NA_real_))
    }
    r <- r %>% filter(is.finite(Ratio)) %>% arrange(desc(Ratio))
    if (!nrow(r)) return("-")
    paste0(substr(r$Defunción[1], 1, 28), " (×",
           format(round(r$Ratio[1], 2), decimal.mark = ","), ")")
  })

  output$ct_evol <- renderPlotly({
    d <- datos_ct()
    ev_a <- d$ev %>% filter(Entidad == input$ct_a) %>% arrange(Año_Num)
    req(nrow(ev_a) > 0)
    p <- plot_ly()
    p <- add_trace(p, data = ev_a, x = ~Año_Num, y = ~Tasa, name = input$ct_a,
                   type = "scatter", mode = "lines+markers",
                   line = list(color = "#1f77b4", width = 3),
                   marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
                   hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    if (ct_ref() != "NACIONAL") {
      ev_b <- d$ev %>% filter(Entidad == ct_ref()) %>% arrange(Año_Num)
      if (nrow(ev_b) > 0) {
        p <- add_trace(p, data = ev_b, x = ~Año_Num, y = ~Tasa, name = ct_ref(),
                       type = "scatter", mode = "lines+markers",
                       line = list(color = "#ff7f0e", width = 3),
                       marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
                       hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
      }
    }
    p <- add_trace(p, data = d$ev_nac, x = ~Año_Num, y = ~Tasa, name = "Media nacional",
                   type = "scatter", mode = "lines+markers",
                   line = list(color = "#1a2f47", width = 2.5, dash = "dash"),
                   marker = list(color = "#1a2f47", size = 6, line = list(color = "white", width = 1)),
                   hovertemplate = "<b>Media nacional</b><br>Año: %{x}<br>Tasa: %{y:.1f}<extra></extra>")
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
      yaxis = list(title = "Tasa (por 100k hab.)", showgrid = TRUE, gridcolor = "#E5E5E5"),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "h", x = 0.2, y = 1.12),
      margin = list(l = 50, r = 20, t = 10, b = 40)
    )
  })

  output$ct_radar <- renderPlotly({
    d <- datos_ct()
    a <- d$cap %>% filter(Entidad == input$ct_a) %>%
      transmute(Etiqueta = stringr::str_wrap(Defunción, width = 22), Tasa_a = Tasa) %>%
      arrange(Etiqueta)
    if (ct_ref() == "NACIONAL") {
      r <- d$nac %>% transmute(Etiqueta = stringr::str_wrap(Defunción, width = 22), Tasa_r = Tasa_nac)
      nom_r <- "Media nacional"
    } else {
      r <- d$cap %>% filter(Entidad == ct_ref()) %>%
        transmute(Etiqueta = stringr::str_wrap(Defunción, width = 22), Tasa_r = Tasa)
      nom_r <- ct_ref()
    }
    m <- a %>% left_join(r, by = "Etiqueta")
    req(nrow(m) > 0)
    plot_ly(type = "scatterpolar", fill = "toself", mode = "lines+markers") %>%
      add_trace(r = m$Tasa_a, theta = m$Etiqueta, name = input$ct_a,
                marker = list(color = "#1f77b4"), fillcolor = "rgba(31,119,180,0.25)",
                hovertemplate = "<b>%{theta}</b><br>Tasa: %{r:.1f}<extra></extra>") %>%
      add_trace(r = m$Tasa_r, theta = m$Etiqueta, name = nom_r,
                marker = list(color = "#ff7f0e"), fillcolor = "rgba(255,127,14,0.25)",
                hovertemplate = "<b>%{theta}</b><br>Tasa: %{r:.1f}<extra></extra>") %>%
      layout(polar = list(radialaxis = list(visible = TRUE)),
             legend = list(orientation = "h", y = -0.15),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 40, r = 40, b = 80, t = 20))
  })

  output$ct_brechas <- renderPlotly({
    d <- datos_ct()
    a <- d$cap %>% filter(Entidad == input$ct_a) %>% select(Defunción, Tasa_a = Tasa)
    if (ct_ref() == "NACIONAL") {
      r <- a %>% left_join(d$nac %>% select(Defunción, Tasa_nac), by = "Defunción") %>%
        mutate(Ratio = if_else(Tasa_nac > 0, Tasa_a / Tasa_nac, NA_real_))
    } else {
      r <- a %>% left_join(d$cap %>% filter(Entidad == ct_ref()) %>%
                             select(Defunción, Tasa_b = Tasa), by = "Defunción") %>%
        mutate(Ratio = if_else(Tasa_b > 0, Tasa_a / Tasa_b, NA_real_))
    }
    df <- r %>% filter(is.finite(Ratio)) %>% arrange(Ratio)
    req(nrow(df) > 0)
    lim <- suppressWarnings(max(abs(df$Ratio - 1)))
    if (!is.finite(lim) || lim == 0) lim <- 0.1
    plot_ly(df, x = ~Ratio, y = ~reorder(Defunción, Ratio), type = "bar", orientation = "h",
            marker = list(color = ~Ratio, colorscale = escala_plotly(PAL_RDBU_REV),
                          cmin = 1 - lim, cmax = 1 + lim, showscale = TRUE,
                          colorbar = list(title = "Ratio"),
                          line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>Ratio: %{x:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Ratio frente a la referencia"),
             yaxis = list(title = "", tickfont = list(size = 10)),
             hoverlabel = list(bgcolor = "white"),
             shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = 0, y1 = 1,
                                yref = "paper",
                                line = list(color = "black", dash = "dash"))),
             margin = list(l = 220, r = 20, b = 60, t = 20))
  })

  output$ct_tabla <- DT::renderDT({
    d <- datos_ct()
    a <- d$cap %>% filter(Entidad == input$ct_a) %>% select(Defunción, Tasa_a = Tasa)
    if (ct_ref() == "NACIONAL") {
      r <- a %>% left_join(d$nac %>% select(Defunción, Tasa_nac), by = "Defunción") %>%
        transmute(Capítulo = Defunción, `Tasa A` = round(Tasa_a, 1),
                  Referencia = round(Tasa_nac, 1),
                  Ratio = round(if_else(Tasa_nac > 0, Tasa_a / Tasa_nac, NA_real_), 3))
    } else {
      r <- a %>% left_join(d$cap %>% filter(Entidad == ct_ref()) %>%
                             select(Defunción, Tasa_b = Tasa), by = "Defunción") %>%
        transmute(Capítulo = Defunción, `Tasa A` = round(Tasa_a, 1),
                  Referencia = round(Tasa_b, 1),
                  Ratio = round(if_else(Tasa_b > 0, Tasa_a / Tasa_b, NA_real_), 3))
    }
    tab <- r %>% arrange(desc(Ratio))
    DT::datatable(tab, options = list(pageLength = 13, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatRound(c("Tasa A", "Referencia", "Ratio"), c(1, 1, 3),
                      dec.mark = ",", mark = ".")
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
  }) %>% bindCache(input$pt_causa, input$pt_sexo, cache = "app")
  
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
    tasa_std_prov(input$std_ano, input$std_sexo)
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
  # Cestas y asignar_cesta() viven en global.R (las usa también el mapa único).

  # FIX: la población se extrae con distinct(Provincia, Sexo) porque viene
  # repetida en cada fila de causa; sumarla por causa la multiplicaría ×17.
  # Sensible por provincia: helper puro tasa_sensible_prov() (global.R).
  datos_pev <- reactive({
    req(input$pev_ano, input$pev_sexo)
    tasa_sensible_prov(input$pev_ano, input$pev_sexo)
  }) %>% bindCache(input$pev_ano, input$pev_sexo, cache = "app")

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
  }) %>% bindCache(input$pev_sexo, cache = "app")

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
  # gini_pond() vive en global.R (también lo usan los tests).

  # FIX: la población se extrae con distinct(Provincia, Sexo) porque viene
  # repetida en cada fila de causa; con "Todas" se suman los capítulos.
  # Brechas por provincia: helper puro brecha_prov() (global.R).
  datos_des <- reactive({
    req(input$des_ano, input$des_sexo, input$des_causa)
    brecha_prov(input$des_ano, input$des_sexo, input$des_causa)
  }) %>% bindCache(input$des_ano, input$des_sexo, input$des_causa, cache = "app")

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
  }) %>% bindCache(input$des_sexo, input$des_causa, cache = "app")

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
  }) %>% bindCache(input$al_sexo, cache = "app")

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
  }) %>% bindCache(input$al_sexo, cache = "app")

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


  # --- INFORMES EXCEL POR TERRITORIO ---
  observe({
    req(input$in_nivel)
    vals <- if (input$in_nivel == "CCAA") sort(unique(causas_provinciales$Comunidad)) else sort(unique(causas_provinciales$Provincia))
    sel <- if (input$in_nivel == "CCAA") {
      if ("Madrid" %in% vals) "Madrid" else vals[1]
    } else {
      if ("Madrid" %in% vals) "Madrid" else vals[1]
    }
    updateSelectInput(session, "in_terr", choices = vals, selected = sel)
  })

  # FIX: población con distinct por venir repetida en cada fila de causa.
  datos_in <- reactive({
    req(input$in_nivel, input$in_terr, input$in_ano, input$in_sexo)
    df <- causas_provinciales %>% filter(Año == input$in_ano)
    if (input$in_sexo != "Ambos") df <- df %>% filter(Sexo == input$in_sexo)
    geo <- if (input$in_nivel == "CCAA") "Comunidad" else "Provincia"
    pob <- df %>%
      distinct(Año, Año_Num, Provincia, Comunidad, Sexo, Poblacion) %>%
      group_by(Año, Año_Num, Comunidad, Provincia) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop")
    pob_peer <- pob %>%
      group_by(.data[[geo]]) %>%
      summarise(P = sum(P, na.rm = TRUE), .groups = "drop")
    pn <- sum(pob_peer$P)
    sel <- df %>% filter(.data[[geo]] == input$in_terr)
    p_sel <- sum(pob_peer$P[pob_peer[[geo]] == input$in_terr], na.rm = TRUE)
    f_sel <- sum(sel$Fallecidos, na.rm = TRUE)
    cap <- sel %>%
      group_by(Defunción) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa = if (p_sel > 0) F / p_sel * 100000 else NA_real_,
             Pct = if (f_sel > 0) F / f_sel * 100 else NA_real_) %>%
      filter(is.finite(Tasa))
    nac <- df %>%
      group_by(Defunción) %>%
      summarise(Fn = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tasa_nac = Fn / pn * 100000)
    cap <- cap %>%
      left_join(nac %>% select(Defunción, Tasa_nac), by = "Defunción") %>%
      mutate(Ratio = if_else(Tasa_nac > 0, Tasa / Tasa_nac, NA_real_)) %>%
      arrange(desc(F))
    tot_peer <- df %>%
      group_by(.data[[geo]]) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop") %>%
      left_join(pob_peer, by = geo) %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>% arrange(desc(Tasa))
    evo_sel <- df %>%
      filter(.data[[geo]] == input$in_terr) %>%
      group_by(Año, Año_Num) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE), .groups = "drop")
    evo_pob <- df %>%
      distinct(Año, Año_Num, Provincia, Comunidad, Sexo, Poblacion) %>%
      group_by(Año, Año_Num, Comunidad, Provincia) %>%
      summarise(P = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      group_by(Año, Año_Num) %>%
      summarise(Pt = sum(P, na.rm = TRUE),
                Ps = sum(P[(if (geo == "Comunidad") Comunidad else Provincia) == input$in_terr], na.rm = TRUE),
                .groups = "drop")
    evo <- evo_sel %>%
      left_join(df %>% group_by(Año, Año_Num) %>%
                  summarise(Fn = sum(Fallecidos, na.rm = TRUE), .groups = "drop"),
                by = c("Año", "Año_Num")) %>%
      left_join(evo_pob, by = c("Año", "Año_Num")) %>%
      transmute(Año, Fallecidos = F,
                Tasa = if_else(Ps > 0, F / Ps * 100000, NA_real_),
                Tasa_nacional = if_else(Pt > 0, Fn / Pt * 100000, NA_real_)) %>%
      filter(is.finite(Tasa)) %>% arrange(Año)
    list(cap = cap, tot_peer = tot_peer, f_sel = f_sel, p_sel = p_sel,
         tasa = if (p_sel > 0) f_sel / p_sel * 100000 else NA_real_,
         tasa_nac = if (pn > 0) sum(df$Fallecidos, na.rm = TRUE) / pn * 100000 else NA_real_,
         evo = evo)
  }) %>% bindCache(input$in_nivel, input$in_terr, input$in_ano, input$in_sexo, cache = "app")

  output$in_kpi_fall <- renderText({
    d <- datos_in()
    format(round(d$f_sel, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })

  output$in_kpi_tasa <- renderText({
    d <- datos_in()
    if (!is.finite(d$tasa)) return("-")
    paste0(format(round(d$tasa, 1), big.mark = ".", decimal.mark = ","),
           " (nac: ", format(round(d$tasa_nac, 1), big.mark = ".", decimal.mark = ","), ")")
  })

  output$in_kpi_top <- renderText({
    d <- datos_in()
    if (!nrow(d$cap)) return("-")
    paste0(substr(d$cap$Defunción[1], 1, 30), " (",
           format(round(d$cap$F[1], 0), big.mark = ".", decimal.mark = ","), ")")
  })

  output$in_tabla <- DT::renderDT({
    tab <- datos_in()$cap %>%
      transmute(Capítulo = Defunción, Fallecidos = F, Tasa = round(Tasa, 1),
                `% total` = round(Pct, 1), `Ratio nacional` = round(Ratio, 3))
    DT::datatable(tab, options = list(pageLength = 13, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatRound(c("Tasa", "% total", "Ratio nacional"), c(1, 1, 3),
                      dec.mark = ",", mark = ".") %>%
      DT::formatRound("Fallecidos", 0, dec.mark = ",", mark = ".")
  })

  output$in_descargar <- downloadHandler(
    filename = function() {
      base <- stringi::stri_trans_general(input$in_terr, "Latin-ASCII")
      base <- gsub("[^A-Za-z0-9]+", "_", trimws(base))
      paste0("informe_", tolower(input$in_nivel), "_", base, "_",
             gsub("[^0-9]", "", input$in_ano), ".xlsx")
    },
    content = function(file) {
      d <- datos_in()
      puesto <- match(input$in_terr, d$tot_peer[[if (input$in_nivel == "CCAA") "Comunidad" else "Provincia"]])
      resumen <- data.frame(
        Indicador = c("Territorio", "Nivel", "Año", "Sexo", "Fallecidos", "Población",
                      "Tasa por 100k", "Tasa nacional por 100k", "Primera causa",
                      "Fallecidos primera causa", "Puesto por tasa"),
        Valor = c(input$in_terr, input$in_nivel, input$in_ano, input$in_sexo,
                  round(d$f_sel, 0), round(d$p_sel, 0),
                  round(d$tasa, 1), round(d$tasa_nac, 1),
                  if (nrow(d$cap)) d$cap$Defunción[1] else "-",
                  if (nrow(d$cap)) round(d$cap$F[1], 0) else NA_real_,
                  if (!is.na(puesto)) paste0(puesto, " de ", nrow(d$tot_peer)) else "-"),
        stringsAsFactors = FALSE
      )
      por_causa <- as.data.frame(d$cap %>% transmute(
        Capitulo = Defunción, Fallecidos = F, Tasa_100k = round(Tasa, 1),
        Pct_total = round(Pct, 1), Ratio_nacional = round(Ratio, 3)))
      evolucion <- as.data.frame(d$evo %>% transmute(
        Ano = Año, Fallecidos = F, Tasa_100k = round(Tasa, 1),
        Tasa_nacional_100k = round(Tasa_nacional, 1)))
      evolucion <- as.data.frame(d$evo %>% transmute(
        Ano = Año, Fallecidos = F, Tasa_100k = round(Tasa, 1),
        Tasa_nacional_100k = round(Tasa_nacional, 1)))
      writexl::write_xlsx(list(Resumen = resumen, Por_causa = por_causa,
                               Evolucion = evolucion), path = file)
    }
  )

  # ---- CLUSTERS ESPACIO-TEMPORALES (Local Moran I) ----
  # nb_provincias vive en global.R (reina, precomputada por arranque).

  datos_st <- reactive({
    req(input$st_causa, input$st_sexo)
    df <- causas_provinciales %>%
      filter(Defunción == input$st_causa)
    if (input$st_sexo != "Ambos") df <- df %>% filter(Sexo == input$st_sexo)
    df %>%
      group_by(Año_Num, Provincia) %>%
      summarise(Tasa = sum(Fallecidos) / sum(Poblacion) * 100000, .groups = "drop") %>%
      filter(is.finite(Tasa))
  }) %>% bindCache(input$st_causa, input$st_sexo, cache = "app")

  datos_st_clusters <- reactive({
    d <- datos_st()
    req(nrow(d) > 0)
    scan_espacio_temporal(d, nb_provincias, alpha = input$st_alpha)
  }) %>% bindCache(input$st_causa, input$st_sexo, input$st_alpha, cache = "app")

  output$st_kpi_anos <- renderText({
    d <- datos_st()
    if (!nrow(d)) return("-")
    paste0(min(d$Año_Num), "–", max(d$Año_Num), " (", length(unique(d$Año_Num)), " años)")
  })

  output$st_kpi_provs <- renderText({
    cl <- datos_st_clusters()
    if (!nrow(cl)) return("0")
    format(dplyr::n_distinct(cl$Provincia[cl$p_valor < input$st_alpha]),
           big.mark = ".", decimal.mark = ",")
  })

  output$st_kpi_hot <- renderText({
    cl <- datos_st_clusters()
    if (!nrow(cl)) return("0")
    hot <- cl %>% filter(p_valor < input$st_alpha, tipo == "alto")
    format(nrow(hot), big.mark = ".", decimal.mark = ",")
  })

  output$st_mapa_nota <- renderUI({
    cl <- datos_st_clusters()
    req(nrow(cl) > 0)
    a <- sort(unique(cl$Año_Num))
    a_sel <- suppressWarnings(as.numeric(input$st_ano))
    if (length(a_sel) != 1 || !is.finite(a_sel) || !(a_sel %in% a)) a_sel <- max(a)
    n <- sum(cl$Año_Num == a_sel & cl$p_valor < input$st_alpha, na.rm = TRUE)
    HTML(if (n == 0) paste0("<span class='text-muted'>", a_sel,
                            ": sin clusters significativos (&alpha; = ",
                            input$st_alpha, "). Prueba con otro año o causa.</span>")
         else paste0("<b>", a_sel, ":</b> ", n, " provincias con cluster."))
  })

  output$st_mapa <- renderLeaflet({
    cl <- datos_st_clusters()
    req(nrow(cl) > 0)
    a <- sort(unique(cl$Año_Num))
    a_sel <- suppressWarnings(as.numeric(input$st_ano))
    if (length(a_sel) != 1 || !is.finite(a_sel) || !(a_sel %in% a)) a_sel <- max(a)
    df_map <- cl %>% filter(Año_Num == a_sel, p_valor < input$st_alpha)
    mapa_datos <- mapa_provincias %>% left_join(df_map, by = c("NAME_2" = "Provincia"))
    pal <- colorFactor(
      palette = c("alto" = "#ef4444", "bajo" = "#3b82f6"),
      domain = c("alto", "bajo"),
      na.color = "#E0E0E0"
    )
    etiquetas <- sprintf("<strong>%s</strong><br/>Tipo: %s<br/>Z: %.2f<br/>p: %.3f",
                         mapa_datos$NAME_2,
                         ifelse(is.na(mapa_datos$tipo), "Sin cluster", mapa_datos$tipo),
                         mapa_datos$Z,
                         mapa_datos$p_valor) %>% lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      tiles_osm() %>%
      addPolygons(fillColor = ~pal(tipo), weight = 1, color = "white",
                  fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = c("alto", "bajo"), opacity = 0.8,
                title = "Cluster", position = "bottomright") %>%
      addControl(html = sprintf("<div class='map-year-badge'>%s</div>", a_sel), position = "topright") %>%
      fitBounds(lng1 = -9.5, lat1 = 35.5, lng2 = 4.5, lat2 = 44)
  })

  output$st_tabla <- DT::renderDT({
    cl <- datos_st_clusters()
    req(nrow(cl) > 0)
    tab <- cl %>%
      filter(p_valor < input$st_alpha) %>%
      mutate(Z = round(Z, 2), p_valor = round(p_valor, 4)) %>%
      arrange(Año_Num, desc(abs(Z))) %>%
      select(Año_Num, Provincia, tipo, Z, p_valor)
    DT::datatable(tab, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatRound(c("Z", "p_valor"), c(2, 4), dec.mark = ",", mark = ".")
  })

}




