server_metricas <- function(input, output, session) {
  
  # Cuadro informativo dinámico
  output$p3_cuadro_informativo_dinamico <- renderUI({
    req(input$p3_funcion)
    f_sel <- input$p3_funcion
    
    info <- switch(
      f_sel,
      "Esperanza de vida" = list(
        nombre = "Esperanza de vida / Años a vivir a la edad x",
        def = "Número medio de años adicionales que le quedan por vivir a una persona que alcanza exactamente el tramo de edad x.",
        unidad = "Años",
        recom = "**Menores de 1 año (De 0 a 0)** para la esperanza de vida al nacer, y **De 65 a 69 años** para analizar la expectativa de vida residual al alcanzar la jubilación."
      ),
      "Población estacionaria" = list(
        nombre = "Población estacionaria",
        def = "Número total de personas que estarían con vida permanentemente en un tramo de edad si nacieran continuamente 100.000 personas al año y se mantuviera constante la mortalidad.",
        unidad = "Personas (Años-Persona)",
        recom = "**De 15 a 64 años** para estimar el peso relativo del volumen poblacional en edad de trabajar en una estructura demográfica estable."
      ),
      "Promedio de años vividos el último año de vida" = list(
        nombre = "Promedio de años vividos el último año de vida",
        def = "Número medio de años vividos dentro del intervalo de edad por las personas que fallecen en dicho tramo.",
        unidad = "Años",
        recom = "**Menores de 1 año (De 0 a 0)** debido a la concentración del riesgo en los primeros días/meses de vida."
      ),
      "Riesgo de muerte" = list(
        nombre = "Riesgo de muerte",
        def = "Probabilidad de que una persona que alcanza el inicio del tramo de edad x fallezca antes de cumplir o superar la siguiente edad.",
        unidad = "Probabilidad (escala decimal entre 0 y 1)",
        recom = "**De 80 a 84 años y superiores**, tramos donde la probabilidad de fallecer se acelera exponencialmente."
      ),
      "Supervivientes" = list(
        nombre = "Supervivientes / Personas que alcanzan la edad x",
        def = "Número de personas (de los 100.000 nacidos iniciales) que sobreviven al llegar exactamente a la edad x.",
        unidad = "Personas (sobre la cohorte teórica de 100.000)",
        recom = "**De 65 a 69 años** para observar la proporción de la cohorte inicial que alcanza la edad madura, y **De 85 a 89 años** para evaluar la longevidad extrema."
      ),
      "Tasa de mortalidad" = list(
        nombre = "Tasa de mortalidad / Probabilidad de muerte",
        def = "Proporción de personas de la edad x que fallecen antes de cumplir la siguiente edad.",
        unidad = "Promil (‰)",
        recom = "**De 50 a 54 años y De 70 a 74 años** para medir el impacto de enfermedades crónicas antes y después del retiro profesional."
      ),
      "Tiempo por vivir" = list(
        nombre = "Tiempo por vivir",
        def = "Número total acumulado de años que le restan por vivir a todo el grupo de población que ha alcanzado la edad x.",
        unidad = "Años-Persona acumulados",
        recom = "**Menores de 1 año (De 0 a 0)** para evaluar la bolsa total del capital de vida potencial de la cohorte."
      ),
      list(
        nombre = f_sel,
        def = "Información estadística correspondiente a las tablas demográficas de mortalidad del INE.",
        unidad = obtener_unidad(f_sel),
        recom = "**De 0 a 0 años y De 65 a 69 años** como tramos de referencia demográfica estándar."
      )
    )
    
    bslib::card(
      card_header(class = "bg-info text-white", paste("Guía Explicativa:", info$nombre)),
      card_body(
        markdown(sprintf("
        * **Definición:** %s
        * **Unidad de Medida:** **%s**
        * **Grupos de Edad Recomendados:** %s
        ", info$def, info$unidad, info$recom))
      )
    )
  })
  
  output$p3_filtro_info <- renderUI({
    req(input$p3_funcion, input$p3_edad)
    texto <- if (input$p3_edad == "Todos los tramos")
      "El mapa resume la media provincial para todos los tramos."
    else
      paste0("Se está mostrando específicamente el tramo ", htmltools::htmlEscape(input$p3_edad), ".")
    div(class = "filter-help",
        HTML(paste0("<b>Métrica:</b> ", htmltools::htmlEscape(input$p3_funcion), ". ", texto)))
  })
  
  output$p31_filtro_info <- renderUI({
    req(input$p31_funcion, input$p31_prov_a, input$p31_prov_b)
    div(class = "filter-help",
        HTML(paste0("<b>Lectura:</b> ", htmltools::htmlEscape(input$p31_funcion),
                    " para ", htmltools::htmlEscape(input$p31_prov_a),
                    " y ", htmltools::htmlEscape(input$p31_prov_b),
                    " · población mostrada para ", htmltools::htmlEscape(input$p31_ano_radar), ".")))
  })
  
  # --- PESTAÑA 3: SERVER ---
  p3_pagina_actual <- reactiveVal(1)
  
  observeEvent(c(input$p3_funcion, input$p3_ano, input$p3_sexo, input$p3_edad), {
    p3_pagina_actual(1)
  })
  
  datos_p3 <- reactive({
    req(input$p3_funcion, input$p3_ano, input$p3_edad)
    df <- copia_func %>% filter(Año == input$p3_ano, Funciones == input$p3_funcion)

    # FIX: filtro de sexo exacto siempre. copia_func trae filas pre-agregadas
    # "Ambos"; no filtrar mezclaba Ambos+Hombres+Mujeres en la media.
    df <- df %>% filter(Sexo == input$p3_sexo)
    if (input$p3_edad != "Todos los tramos") df <- df %>% filter(Edad == input$p3_edad)
    
    df %>%
      group_by(Provincia) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Valor))
  })
  
  output$p3_kpi_media <- renderText({
    df <- datos_p3()
    if (nrow(df) == 0) return("-")
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    paste0(format(round(mean(df$Valor, na.rm = TRUE), num_dec), big.mark = ".", decimal.mark = ","), " ", obtener_unidad(input$p3_funcion))
  })
  
  output$p3_kpi_max <- renderText({
    df <- datos_p3()
    if (nrow(df) == 0) return("-")
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    max_row <- df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(max_row$Provincia, " (", format(round(max_row$Valor, num_dec), decimal.mark = ","), ")")
  })
  
  output$p3_kpi_min <- renderText({
    df <- datos_p3()
    if (nrow(df) == 0) return("-")
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    min_row <- df %>% arrange(Valor) %>% slice(1)
    paste0(min_row$Provincia, " (", format(round(min_row$Valor, num_dec), decimal.mark = ","), ")")
  })
  
  output$p3_mapa <- renderLeaflet({
    df <- datos_p3()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    
    val_max <- max(mapa_datos$Valor, na.rm = TRUE)
    val_min <- min(mapa_datos$Valor, na.rm = TRUE)
    
    dominio_colores <- if (is.na(val_max) || val_max == val_min) c(0, 1) else c(val_min, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio_colores, na.color = "#E0E0E0")
    
    num_dec <- if (grepl("Riesgo", input$p3_funcion, ignore.case = TRUE)) 4 else 2
    
    etiquetas <- sprintf("<strong>%s</strong><br/>Valor: %s %s", 
                         mapa_datos$NAME_2, 
                         format(round(mapa_datos$Valor, num_dec), decimal.mark = ","),
                         obtener_unidad(input$p3_funcion)) %>% 
      lapply(htmltools::HTML)
    
    leaflet(mapa_datos) %>% 
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>% 
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio_colores, opacity = 0.8, 
                title = paste0(input$p3_funcion, "<br/>(", obtener_unidad(input$p3_funcion), ")"), 
                position = "bottomright") %>%
      control_ano(input$p3_ano)
  })
  
  output$p3_piramide <- renderPlotly({
    req(input$p3_ano, input$p3_funcion)
    
    df_pira <- copia_func %>% 
      filter(Año == input$p3_ano, 
             Funciones == input$p3_funcion,
             Sexo %in% c("Hombres", "Mujeres")) %>%
      filter(!grepl("95", Edad, ignore.case = TRUE) & !grepl("90 y más", Edad, ignore.case = TRUE)) %>%
      group_by(Edad, Sexo) %>% 
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop")
    
    df_hombres <- df_pira %>% filter(Sexo == "Hombres")
    df_mujeres <- df_pira %>% filter(Sexo == "Mujeres")
    
    lista_edades_filtradas <- setdiff(lista_edades, c("95 y más", "95 y mas", "90 y más", "90 y mas"))
    max_val <- max(df_pira$Valor, na.rm = TRUE) * 1.1
    
    plot_ly() %>%
      add_trace(
        data = df_hombres, 
        x = ~-Valor, 
        y = ~Edad, 
        type = "bar", 
        orientation = "h",
        name = "Hombres",
        marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = "<b>Edad: %{y}</b><br>Hombres: %{customdata:,.2f}<extra></extra>"
      ) %>%
      add_trace(
        data = df_mujeres, 
        x = ~Valor, 
        y = ~Edad, 
        type = "bar", 
        orientation = "h",
        name = "Mujeres",
        marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
        hovertemplate = "<b>Edad: %{y}</b><br>Mujeres: %{x:,.2f}<extra></extra>"
      ) %>%
      layout(
        barmode = "overlay",
        bargap = 0.1,
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(
          title = paste(input$p3_funcion, "(", obtener_unidad(input$p3_funcion), ")"),
          range = c(-max_val, max_val),
          tickmode = "array",
          tickvals = seq(-round(max_val), round(max_val), length.out = 7),
          ticktext = abs(round(seq(-round(max_val), round(max_val), length.out = 7)))
        ),
        yaxis = list(
          title = "Tramo de Edad", 
          categoryorder = "array", 
          categoryarray = lista_edades_filtradas
        ),
        legend = list(orientation = "h", x = 0.35, y = 1.1),
        margin = list(l = 60, r = 20, t = 20, b = 40)
      )
  })
  
  ranking_p3_data <- reactive({
    datos_p3() %>% 
      arrange(desc(Valor)) %>% 
      mutate(Posicion = row_number())
  })
  
  p3_total_paginas <- reactive({
    ceiling(nrow(ranking_p3_data()) / 13)
  })
  
  observeEvent(input$p3_prev_page, {
    if (p3_pagina_actual() > 1) {
      p3_pagina_actual(p3_pagina_actual() - 1)
    }
  })
  
  observeEvent(input$p3_next_page, {
    if (p3_pagina_actual() < p3_total_paginas()) {
      p3_pagina_actual(p3_pagina_actual() + 1)
    }
  })
  
  output$p3_page_info <- renderUI({
    span(paste("Página", p3_pagina_actual(), "de", max(1, p3_total_paginas())), class = "fw-bold px-2")
  })
  
  output$p3_tabla_ranking <- renderTable({
    df <- ranking_p3_data()
    if (nrow(df) == 0) return(NULL)
    
    inicio <- (p3_pagina_actual() - 1) * 13 + 1
    fin <- min(p3_pagina_actual() * 13, nrow(df))
    
    df %>% 
      slice(inicio:fin) %>% 
      select(Posicion, Provincia, Valor)
  }, striped = TRUE, hover = TRUE)
  
  # --- PESTAÑA 3.1 SERVER ---
  output$p31_evol_demografica <- renderPlotly({
    req(input$p31_prov_a, input$p31_prov_b, input$p31_funcion)
    
    df_func_sub <- copia_func %>% filter(Funciones == input$p31_funcion)

    # FIX: filtro de sexo exacto siempre (ver datos_p3: existen filas "Ambos"
    # pre-agregadas y no filtrar las mezclaba con Hombres/Mujeres).
    df_func_sub <- df_func_sub %>% filter(Sexo == input$p31_sexo)
    
    df_provs <- df_func_sub %>% 
      filter(Provincia %in% c(input$p31_prov_a, input$p31_prov_b)) %>%
      group_by(Año, Provincia) %>% 
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>% 
      mutate(Ano_Num = as.numeric(as.character(Año)))
    
    df_nac <- df_func_sub %>% 
      group_by(Año) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Provincia = "Media entre provincias", Ano_Num = as.numeric(as.character(Año)))
    
    p <- plot_ly()
    
    df_a <- df_provs %>% filter(Provincia == input$p31_prov_a)
    if(nrow(df_a) > 0) {
      p <- p %>% add_trace(data = df_a, x = ~Ano_Num, y = ~Valor, name = input$p31_prov_a,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#1f77b4", width = 3),
                           marker = list(color = "#1f77b4", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>")
    }
    
    df_b <- df_provs %>% filter(Provincia == input$p31_prov_b)
    if(nrow(df_b) > 0) {
      p <- p %>% add_trace(data = df_b, x = ~Ano_Num, y = ~Valor, name = input$p31_prov_b,
                           type = "scatter", mode = "lines+markers",
                           line = list(color = "#ff7f0e", width = 3),
                           marker = list(color = "#ff7f0e", size = 8, line = list(color = "white", width = 1.5)),
                           hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>")
    }
    
    p <- p %>% add_trace(data = df_nac, x = ~Ano_Num, y = ~Valor, name = "Media entre provincias",
                         type = "scatter", mode = "lines+markers",
                         line = list(color = "#2c3e50", width = 2.5, dash = "dash"),
                         marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                         hovertemplate = "<b>Media entre provincias</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>")
    
    p %>% layout(
      xaxis = list(title = "Año", tickmode = "linear", dtick = 1, showgrid = TRUE, gridcolor = "#E5E5E5"),
      yaxis = list(title = paste(input$p31_funcion, "(", obtener_unidad(input$p31_funcion), ")"), showgrid = TRUE, gridcolor = "#E5E5E5"),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "white"),
      legend = list(orientation = "h", x = 0.2, y = 1.12),
      margin = list(l = 50, r = 20, t = 10, b = 40)
    )
  })
  
  output$p31_piramide_comparativa <- renderPlotly({
    req(input$p31_prov_a, input$p31_prov_b)
    
    # La pirámide compara las dos provincias por tramo de edad.
    # Se utiliza el sexo seleccionado; con "Ambos" se muestran los totales.
    sexo_pira <- if (is.null(input$p31_sexo) || is.na(input$p31_sexo)) "Ambos" else input$p31_sexo
    
    df_pira <- copia_func %>%
      filter(Provincia %in% c(input$p31_prov_a, input$p31_prov_b)) %>%
      filter(!grepl("^(90|95) y (más|mas)$|^(90|95) años y (más|mas)$", Edad, ignore.case = TRUE)) %>%
      { if (sexo_pira %in% c("Hombres", "Mujeres", "Ambos")) filter(., Sexo == sexo_pira) else . } %>%
      group_by(Provincia, Edad) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Valor))
    
    validate(need(nrow(df_pira) > 0, "No hay datos suficientes para construir la pirámide."))
    
    edades <- intersect(lista_edades, unique(df_pira$Edad))
    df_pira$Edad <- factor(df_pira$Edad, levels = edades)
    
    df_a <- df_pira %>% filter(Provincia == input$p31_prov_a)
    df_b <- df_pira %>% filter(Provincia == input$p31_prov_b)
    max_val <- max(c(df_a$Valor, df_b$Valor), na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1
    
    p <- plot_ly()
    
    if (nrow(df_a) > 0) {
      p <- p %>% add_trace(
        data = df_a, x = ~-Valor, y = ~Edad, type = "bar", orientation = "h",
        name = input$p31_prov_a,
        marker = list(color = "#1f77b4", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = paste0("<b>", htmltools::htmlEscape(input$p31_prov_a), "</b><br>Edad: %{y}<br>Valor: %{customdata:,.2f}<extra></extra>")
      )
    }
    if (nrow(df_b) > 0) {
      p <- p %>% add_trace(
        data = df_b, x = ~Valor, y = ~Edad, type = "bar", orientation = "h",
        name = input$p31_prov_b,
        marker = list(color = "#ff7f0e", line = list(color = "white", width = 1)),
        customdata = ~Valor,
        hovertemplate = paste0("<b>", htmltools::htmlEscape(input$p31_prov_b), "</b><br>Edad: %{y}<br>Valor: %{customdata:,.2f}<extra></extra>")
      )
    }
    
    tick_span <- pretty(c(-max_val, max_val), n = 7)
    tick_span <- tick_span[tick_span >= -max_val & tick_span <= max_val]
    p %>% layout(
      barmode = "overlay",
      bargap = 0.08,
      xaxis = list(
        title = obtener_unidad(input$p31_funcion),
        range = c(-max_val * 1.1, max_val * 1.1),
        tickmode = "array",
        tickvals = sort(unique(c(-rev(abs(tick_span)), abs(tick_span)))),
        ticktext = format(abs(sort(unique(c(-rev(abs(tick_span)), abs(tick_span))))), big.mark = ".", decimal.mark = ",", trim = TRUE)
      ),
      yaxis = list(title = "Tramo de Edad", categoryorder = "array", categoryarray = edades),
      legend = list(orientation = "h", x = 0.28, y = 1.08),
      margin = list(l = 70, r = 30, t = 35, b = 55)
    )
  })
  
  output$p31_poblacion_titulo_a <- renderText({
    req(input$p31_prov_a, input$p31_ano_radar)
    paste0("Población total — ", input$p31_prov_a)
  })
  
  output$p31_poblacion_titulo_b <- renderText({
    req(input$p31_prov_b, input$p31_ano_radar)
    paste0("Población total — ", input$p31_prov_b)
  })
  
  poblacion_provincia_total <- function(provincia, anio) {
    val <- copia_p %>%
      filter(Provincia == provincia, Año == as.character(anio), Sexo == "Ambos") %>%
      summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
      pull(Poblacion)
    if (!length(val) || !is.finite(val)) return(NA_real_)
    val
  }
  
  output$p31_poblacion_a <- renderText({
    req(input$p31_prov_a, input$p31_ano_radar)
    val <- poblacion_provincia_total(input$p31_prov_a, input$p31_ano_radar)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  
  output$p31_poblacion_b <- renderText({
    req(input$p31_prov_b, input$p31_ano_radar)
    val <- poblacion_provincia_total(input$p31_prov_b, input$p31_ano_radar)
    if (!is.finite(val)) return("Sin datos")
    format(round(val), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })

  # --- PIRÁMIDE DE POBLACIÓN SERVER ---
  datos_demp <- reactive({
    req(!is.null(padron_edad), input$demp_prov, input$demp_ano)
    df <- padron_edad %>% filter(Año == as.character(input$demp_ano))
    if (input$demp_prov != "Todas") df <- df %>% filter(Provincia == input$demp_prov)
    df %>%
      mutate(Q5 = ifelse(Edad_num >= 95, "95 y más",
                         paste0("De ", 5 * (Edad_num %/% 5), " a ", 5 * (Edad_num %/% 5) + 4))) %>%
      group_by(Q5, Sexo) %>%
      summarise(Pob = sum(Pob, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(Pob))
  })

  output$demp_piramide <- renderPlotly({
    df <- datos_demp()
    req(nrow(df) > 0)
    orden_q5 <- df %>% distinct(Q5) %>%
      mutate(ini = suppressWarnings(as.numeric(gsub("[^0-9].*", "", Q5))),
             ini = ifelse(grepl("^95", Q5), 95, ini)) %>%
      arrange(ini) %>% pull(Q5)
    df$Q5 <- factor(df$Q5, levels = orden_q5)
    df_h <- df %>% filter(Sexo == "Hombres")
    df_m <- df %>% filter(Sexo == "Mujeres")
    max_v <- max(df$Pob, na.rm = TRUE) * 1.08
    if (!is.finite(max_v) || max_v <= 0) return(plotly_empty())
    ticks <- seq(-round(max_v), round(max_v), length.out = 7)
    plot_ly() %>%
      add_trace(data = df_h, x = ~-Pob, y = ~Q5, type = "bar", orientation = "h",
                name = "Hombres", marker = list(color = "#00B2A9", line = list(color = "white", width = 1)),
                customdata = ~Pob,
                hovertemplate = "<b>%{y}</b><br>Hombres: %{customdata:,.0f}<extra></extra>") %>%
      add_trace(data = df_m, x = ~Pob, y = ~Q5, type = "bar", orientation = "h",
                name = "Mujeres", marker = list(color = "#FF6F61", line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{y}</b><br>Mujeres: %{x:,.0f}<extra></extra>") %>%
      layout(barmode = "overlay", bargap = 0.08,
             hoverlabel = list(bgcolor = "white"),
             xaxis = list(title = "Población",
                          range = c(-max_v, max_v), tickmode = "array",
                          tickvals = ticks,
                          ticktext = format(abs(round(ticks)), big.mark = ".", decimal.mark = ",", trim = TRUE)),
             yaxis = list(title = "Tramo de edad", categoryorder = "array", categoryarray = orden_q5),
             legend = list(orientation = "h", x = 0.35, y = 1.05),
             margin = list(l = 90, r = 30, t = 30, b = 60))
  })

  # --- ENVEJECIMIENTO Y DEPENDENCIA SERVER ---
  datos_deme <- reactive({
    req(!is.null(padron_edad), input$deme_ano)
    # FIX: no reutilizar el nombre Pob dentro del mismo summarise: desde dplyr
    # 1.0 las expresiones se evalúan en secuencia y Pob[...] usaría el escalar
    # ya agregado (daba P65 = 0 en todas las provincias).
    padron_edad %>%
      filter(Año == as.character(input$deme_ano)) %>%
      group_by(Provincia) %>%
      summarise(Pob_total = sum(Pob, na.rm = TRUE),
                P65 = sum(Pob[Edad_num >= 65], na.rm = TRUE),
                P015 = sum(Pob[Edad_num <= 15], na.rm = TRUE),
                P1664 = sum(Pob[Edad_num >= 16 & Edad_num <= 64], na.rm = TRUE),
                .groups = "drop") %>%
      transmute(Provincia = Provincia,
                Pob = Pob_total, P65 = P65, P015 = P015, P1664 = P1664,
                Pct65 = P65 / Pob_total * 100,
                IndEnv = if_else(P015 > 0, P65 / P015 * 100, NA_real_),
                Dep = if_else(P1664 > 0, (P015 + P65) / P1664 * 100, NA_real_)) %>%
      filter(is.finite(Pct65))
  })

  output$deme_kpi_65 <- renderText({
    df <- datos_deme()
    req(nrow(df) > 0)
    p <- sum(df$P65, na.rm = TRUE) / sum(df$Pob, na.rm = TRUE) * 100
    paste0(format(round(p, 1), decimal.mark = ","), " %")
  })

  output$deme_kpi_env <- renderText({
    df <- datos_deme()
    req(nrow(df) > 0)
    v <- sum(df$P65, na.rm = TRUE) / sum(df$P015, na.rm = TRUE) * 100
    paste0(format(round(v, 0), decimal.mark = ","), " mayores por cada 100 jóvenes")
  })

  output$deme_kpi_dep <- renderText({
    df <- datos_deme()
    req(nrow(df) > 0)
    v <- sum(df$P015 + df$P65, na.rm = TRUE) / sum(df$P1664, na.rm = TRUE) * 100
    paste0(format(round(v, 1), decimal.mark = ","), " %")
  })

  output$deme_mapa <- renderLeaflet({
    df <- datos_deme()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    pal <- colorNumeric(palette = PAL_YLORRD, domain = c(0, max(mapa_datos$Pct65, na.rm = TRUE)), na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%% de 65+: %s %%", mapa_datos$NAME_2,
                         format(round(mapa_datos$Pct65, 1), decimal.mark = ",")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Pct65), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = ~Pct65, opacity = 0.8, title = "% de 65+", position = "bottomright") %>%
      control_ano(input$deme_ano)
  })

  output$deme_ranking <- renderPlotly({
    df <- datos_deme()
    req(nrow(df) > 0)
    df <- df %>% arrange(Pct65) %>% mutate(Provincia = factor(Provincia, levels = Provincia))
    dom <- dominio_seguro(df$Pct65)
    plot_ly(df, x = ~Pct65, y = ~Provincia, type = "bar", orientation = "h",
            marker = list(color = ~Pct65, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLORRD), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~format(round(Pct65, 1), decimal.mark = ","),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>% de 65+: %{x:.1f} %<extra></extra>") %>%
      layout(xaxis = list(title = "% de población con 65 años o más"),
             yaxis = list(title = "", automargin = TRUE),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 110, r = 60, t = 20, b = 50))
  })

  output$deme_tabla <- DT::renderDT({
    df <- datos_deme() %>%
      transmute(Provincia = Provincia,
                `% de 65+` = Pct65,
                `Índice de envejecimiento` = IndEnv,
                `Tasa de dependencia` = Dep) %>%
      arrange(desc(`% de 65+`))
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("% de 65+", "Índice de envejecimiento", "Tasa de dependencia"), 1, dec.mark = ",", mark = ".")
  })

  # --- BRECHA DE GÉNERO SERVER (indicador a la edad 0) ---
  datos_demb <- reactive({
    req(input$demb_ano, input$demb_func)
    df <- copia_func %>%
      filter(Año == as.character(input$demb_ano), Funciones == input$demb_func,
             Edad == "0", Sexo %in% c("Hombres", "Mujeres")) %>%
      group_by(Provincia, Sexo) %>%
      summarise(V = mean(Total, na.rm = TRUE), .groups = "drop")
    wide <- df %>% tidyr::pivot_wider(names_from = Sexo, values_from = V)
    req(all(c("Hombres", "Mujeres") %in% names(wide)))
    wide %>%
      transmute(Provincia = as.character(Provincia),
                Hombres = Hombres, Mujeres = Mujeres,
                Brecha = Hombres - Mujeres) %>%
      filter(is.finite(Brecha))
  })

  output$demb_mapa <- renderLeaflet({
    df <- datos_demb()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    maxabs <- max(abs(mapa_datos$Brecha), na.rm = TRUE)
    if (!is.finite(maxabs) || maxabs <= 0) maxabs <- 1
    pal <- colorNumeric(palette = "RdBu", domain = c(-maxabs, maxabs), reverse = TRUE)
    unidad <- obtener_unidad(input$demb_func)
    etiquetas <- sprintf("<strong>%s</strong><br/>Brecha H-M: %s %s", mapa_datos$NAME_2,
                         format(round(mapa_datos$Brecha, 2), decimal.mark = ","),
                         unidad) %>% lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Brecha), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = ~Brecha, opacity = 0.8, title = paste0("Brecha H-M (", unidad, ")"), position = "bottomright") %>%
      control_ano(input$demb_ano)
  })

  output$demb_ranking <- renderPlotly({
    df <- datos_demb()
    req(nrow(df) > 0)
    df <- df %>% arrange(Brecha) %>% mutate(Provincia = factor(Provincia, levels = Provincia))
    maxabs <- max(abs(df$Brecha), na.rm = TRUE)
    if (!is.finite(maxabs) || maxabs <= 0) maxabs <- 1
    plot_ly(df, x = ~Brecha, y = ~Provincia, type = "bar", orientation = "h",
            marker = list(color = ~Brecha, cmin = -maxabs, cmax = maxabs,
                          colorscale = escala_plotly(PAL_RDBU_REV), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{y}</b><br>Brecha H-M: %{x:+.2f}<extra></extra>") %>%
      layout(xaxis = list(title = paste0("Brecha Hombres − Mujeres (", obtener_unidad(input$demb_func), ")"),
                          zeroline = FALSE),
             yaxis = list(title = "", automargin = TRUE),
             shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1,
                                yref = "paper", line = list(color = "#2c3e50", width = 1.5))),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 110, r = 60, t = 20, b = 50))
  })

  output$demb_tabla <- DT::renderDT({
    df <- datos_demb() %>%
      transmute(Provincia = Provincia, Hombres = Hombres, Mujeres = Mujeres, `Brecha H-M` = Brecha) %>%
      arrange(`Brecha H-M`)
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Hombres", "Mujeres", "Brecha H-M"), 2, dec.mark = ",", mark = ".")
  })

  # --- EVOLUCIÓN DEMOGRÁFICA POR COMUNIDAD SERVER ---
  datos_demv <- reactive({
    req(input$demv_func, input$demv_sexo)
    copia_func %>%
      filter(Funciones == input$demv_func, Sexo == input$demv_sexo) %>%
      mutate(Comunidad = prov_a_comunidad(Provincia)) %>%
      filter(Comunidad != "Sin asignar") %>%
      group_by(Comunidad, Año) %>%
      summarise(Valor = mean(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Ano_Num = suppressWarnings(as.numeric(Año))) %>%
      filter(is.finite(Valor), is.finite(Ano_Num))
  })

  output$demv_evol <- renderPlotly({
    df <- datos_demv() %>% arrange(Ano_Num, Comunidad)
    req(nrow(df) > 0)
    media <- df %>%
      group_by(Ano_Num) %>%
      summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Comunidad = "Media entre provincias")
    nccaa <- dplyr::n_distinct(df$Comunidad)
    pal_demv <- grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                              "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1))
    plot_ly() %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Valor, color = ~Comunidad, colors = pal_demv,
                type = "scatter", mode = "lines+markers",
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:.2f}<extra></extra>") %>%
      add_trace(data = media, x = ~Ano_Num, y = ~Valor, name = "Media entre provincias",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 3, dash = "dash"),
                marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                hovertemplate = "<b>Media entre provincias</b><br>Año: %{x}<br>Valor: %{y:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = paste0(input$demv_func, " (", obtener_unidad(input$demv_func), ")")),
             legend = list(orientation = "h", x = 0, y = -0.25),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$demv_tabla <- DT::renderDT({
    df <- datos_demv() %>%
      transmute(Comunidad = Comunidad, Año = Ano_Num, Valor = Valor) %>%
      arrange(Comunidad, Año)
    DT::datatable(df, options = list(pageLength = 19, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Valor", 2, dec.mark = ",", mark = ".")
  })
  
  # --- ESPERANZA DE VIDA SERVER ---
  datos_ev <- reactive({
    req(input$ev_ano, input$ev_edad, input$ev_sexo)
    funciones_provinciales %>%
      filter(Funciones == "Esperanza de vida", Año == as.character(input$ev_ano),
             Edad == input$ev_edad, Sexo == input$ev_sexo) %>%
      filter(is.finite(Valor)) %>%
      transmute(Provincia = as.character(Provincia), Valor = Valor)
  }) %>% bindCache(input$ev_ano, input$ev_edad, input$ev_sexo)

  output$ev_kpi_media <- renderText({
    df <- datos_ev()
    req(nrow(df) > 0)
    a <- as.character(input$ev_ano)
    m <- df %>%
      left_join(pob_prov %>% filter(Año == a), by = "Provincia") %>%
      summarise(m = sum(Valor * Pob, na.rm = TRUE) / sum(Pob, na.rm = TRUE)) %>%
      pull(m)
    if (!is.finite(m)) m <- mean(df$Valor, na.rm = TRUE)
    paste0(format(round(m, 1), decimal.mark = ","), " años")
  })

  output$ev_kpi_max <- renderText({
    df <- datos_ev()
    req(nrow(df) > 0)
    x <- df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Valor, 1), decimal.mark = ","), ")")
  })

  output$ev_kpi_min <- renderText({
    df <- datos_ev()
    req(nrow(df) > 0)
    x <- df %>% arrange(Valor) %>% slice(1)
    paste0(x$Provincia, " (", format(round(x$Valor, 1), decimal.mark = ","), ")")
  })

  output$ev_mapa <- renderLeaflet({
    df <- datos_ev()
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    dominio <- dominio_seguro(mapa_datos$Valor)
    pal <- colorNumeric(palette = PAL_YLGNBU, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%s",
                         mapa_datos$NAME_2,
                         ifelse(is.finite(mapa_datos$Valor),
                                paste0("Esperanza de vida: ",
                                       format(round(mapa_datos$Valor, 1), big.mark = ".", decimal.mark = ","),
                                       " años"),
                                "sin dato")) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = "Años de vida", position = "bottomright")
  })

  output$ev_ranking <- renderPlotly({
    df <- datos_ev()
    req(nrow(df) > 0)
    df <- df %>% arrange(Valor) %>% mutate(Provincia = factor(Provincia, levels = Provincia))
    dom <- dominio_seguro(df$Valor)
    plot_ly(df, x = ~Valor, y = ~Provincia, type = "bar", orientation = "h",
            marker = list(color = ~Valor, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLGNBU), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~format(round(Valor, 1), decimal.mark = ","),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Esperanza de vida: %{x:.1f} años<extra></extra>") %>%
      layout(xaxis = list(title = "Años de vida"),
             yaxis = list(title = "", automargin = TRUE),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 140, r = 60, t = 20, b = 50))
  })

  datos_ev_evol <- reactive({
    req(input$ev_edad, input$ev_sexo)
    funciones_provinciales %>%
      filter(Funciones == "Esperanza de vida", Edad == input$ev_edad, Sexo == input$ev_sexo) %>%
      mutate(Comunidad = prov_a_comunidad(Provincia)) %>%
      filter(Comunidad != "Sin asignar") %>%
      group_by(Comunidad, Año) %>%
      summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Ano_Num = suppressWarnings(as.numeric(Año))) %>%
      filter(is.finite(Valor), is.finite(Ano_Num))
  }) %>% bindCache(input$ev_edad, input$ev_sexo)

  output$ev_evol <- renderPlotly({
    df <- datos_ev_evol() %>% arrange(Ano_Num, Comunidad)
    req(nrow(df) > 0)
    media <- df %>%
      group_by(Ano_Num) %>%
      summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Comunidad = "Media entre provincias")
    ncolores <- dplyr::n_distinct(df$Comunidad)
    plot_ly() %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Valor, color = ~Comunidad,
                colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                      "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(ncolores, 1)),
                type = "scatter", mode = "lines+markers",
                marker = list(line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:.1f}<extra></extra>") %>%
      add_trace(data = media, x = ~Ano_Num, y = ~Valor, name = "Media entre provincias",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 3, dash = "dash"),
                marker = list(color = "#2c3e50", size = 6, line = list(color = "white", width = 1)),
                hovertemplate = "<b>Media entre provincias</b><br>Año: %{x}<br>Valor: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = "Años de vida"),
             legend = list(orientation = "h", x = 0, y = -0.25),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$ev_tabla <- DT::renderDT({
    df <- datos_ev() %>%
      transmute(Provincia = Provincia, `Esperanza de vida (años)` = round(Valor, 1)) %>%
      arrange(desc(`Esperanza de vida (años)`))
    DT::datatable(df, options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Esperanza de vida (años)", 1, dec.mark = ",", mark = ".")
  })
}
