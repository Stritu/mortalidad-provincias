server_determinantes <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # DETERMINANTES SERVER (renta/médicos por CCAA + cruce con mortalidad)
  # ---------------------------------------------------------------------------
  # Valor del determinante por CCAA y año (renta agregada ponderada por
  # población provincial; médicos directos por CCAA).
  # OPT: los datos son estáticos por sesión: cada (indicador, año) y cada
  # (mortalidad, año) se calcula una sola vez aunque lo pidan varios reactives.
  det_memo <- new.env(parent = emptyenv())
  det_ccaa_ano <- function(ind, ano) {
    key <- paste0(ind, "||", ano)
    hit <- mget(key, envir = det_memo, ifnotfound = list(NULL), inherits = FALSE)[[1]]
    if (!is.null(hit)) return(hit)
    req(ind, ano)
    a <- as.character(ano)
    val <- if (identical(ind, det_ind_medicos)) {
      req(!is.null(medicos_data))
      medicos_data %>%
        filter(Año == a) %>%
        transmute(Comunidad = as.character(Comunidad), Valor = Valor)
    } else {
      req(!is.null(renta_data))
      renta_data %>%
        filter(Indicador == ind, Año == a) %>%
        left_join(pob_prov %>% filter(Año == a) %>% select(Provincia, Pob), by = "Provincia") %>%
        group_by(Comunidad) %>%
        summarise(Valor = sum(Valor * Pob, na.rm = TRUE) / sum(Pob, na.rm = TRUE), .groups = "drop") %>%
        mutate(Comunidad = as.character(Comunidad))
    }
    assign(key, val, envir = det_memo)
    val
  }

  # Población por CCAA y año (para ponderar medias nacionales).
  pob_ccaa_ano <- function(a) {
    pob_prov %>%
      filter(Año == as.character(a)) %>%
      mutate(Comunidad = prov_a_comunidad(Provincia)) %>%
      filter(Comunidad != "Sin asignar") %>%
      group_by(Comunidad) %>%
      summarise(Pob = sum(Pob, na.rm = TRUE), .groups = "drop")
  }

  det_fmt <- function(v, ind) {
    dec <- if (grepl("Médicos", ind)) 1 else 0
    format(round(v, dec), big.mark = ".", decimal.mark = ",", nsmall = dec, scientific = FALSE, trim = TRUE)
  }

  datos_det <- reactive({
    req(input$det_ind, input$det_ano)
    det_ccaa_ano(input$det_ind, input$det_ano) %>% filter(is.finite(Valor))
  }) %>% bindCache(input$det_ind, input$det_ano)

  datos_det_evol <- reactive({
    req(input$det_ind)
    lapply(det_anos, function(a) {
      det_ccaa_ano(input$det_ind, a) %>% mutate(Año = a)
    }) %>% bind_rows()
  }) %>% bindCache(input$det_ind)

  output$det_kpi_media <- renderText({
    df <- datos_det()
    req(nrow(df) > 0)
    pob <- pob_ccaa_ano(input$det_ano)
    j <- df %>% inner_join(pob, by = "Comunidad")
    req(nrow(j) > 0)
    v <- sum(j$Valor * j$Pob, na.rm = TRUE) / sum(j$Pob, na.rm = TRUE)
    det_fmt(v, input$det_ind)
  })

  output$det_kpi_max <- renderText({
    df <- datos_det()
    req(nrow(df) > 0)
    x <- df %>% arrange(desc(Valor)) %>% slice(1)
    paste0(x$Comunidad, " (", det_fmt(x$Valor, input$det_ind), ")")
  })

  output$det_kpi_min <- renderText({
    df <- datos_det()
    req(nrow(df) > 0)
    x <- df %>% arrange(Valor) %>% slice(1)
    paste0(x$Comunidad, " (", det_fmt(x$Valor, input$det_ind), ")")
  })

  output$det_mapa <- renderLeaflet({
    df <- datos_det()
    mapa_datos <- mapa_ccaa %>%
      left_join(df, by = "Comunidad")
    val_max <- max(mapa_datos$Valor, na.rm = TRUE)
    val_min <- min(mapa_datos$Valor, na.rm = TRUE)
    dominio <- if (!all(is.finite(c(val_min, val_max))) || val_min == val_max) c(0, 1) else c(val_min, val_max)
    pal <- colorNumeric(palette = PAL_YLORRD, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%s: %s",
                         mapa_datos$Comunidad,
                         htmltools::htmlEscape(input$det_ind),
                         det_fmt(mapa_datos$Valor, input$det_ind)) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Valor), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = input$det_ind, position = "bottomright")
  })

  output$det_evol <- renderPlotly({
    df <- datos_det_evol()
    req(nrow(df) > 0)
    df <- df %>% mutate(Ano_Num = suppressWarnings(as.numeric(Año)))
    nccaa <- dplyr::n_distinct(df$Comunidad)
    plot_ly(data = df, x = ~Ano_Num, y = ~Valor, color = ~Comunidad,
            colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1)),
            type = "scatter", mode = "lines+markers",
            marker = list(line = list(color = "white", width = 1)),
            hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Valor: %{y:,.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = input$det_ind),
             legend = list(orientation = "h", x = 0, y = -0.2),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$det_ranking <- renderPlotly({
    df <- datos_det()
    req(nrow(df) > 0)
    df <- df %>% arrange(Valor) %>% mutate(Comunidad = factor(Comunidad, levels = Comunidad))
    dom <- dominio_seguro(df$Valor)
    plot_ly(df, x = ~Valor, y = ~Comunidad, type = "bar", orientation = "h",
            marker = list(color = ~Valor, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLORRD), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~det_fmt(Valor, input$det_ind), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Valor: %{x:,.2f}<extra></extra>") %>%
      layout(xaxis = list(title = input$det_ind),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 140, r = 60, t = 20, b = 50))
  })

  output$det_tabla <- DT::renderDT({
    df <- datos_det_evol() %>%
      transmute(Comunidad = Comunidad, Año = Año, Valor = Valor) %>%
      arrange(Comunidad, Año)
    DT::datatable(df, options = list(pageLength = 19, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Valor", 1, dec.mark = ",", mark = ".")
  })

  # --- Índice sintético por CCAA (renta + médicos + mortalidad) ---
  idx_ind_renta <- if ("Renta neta media por persona" %in% det_indicadores) "Renta neta media por persona" else det_indicadores[1]

  # Tasa bruta de mortalidad total (ambos sexos) por CCAA y año (memoizada).
  mort_ccaa_ano <- function(a) {
    key <- paste0("mort||", a)
    hit <- mget(key, envir = det_memo, ifnotfound = list(NULL), inherits = FALSE)[[1]]
    if (!is.null(hit)) return(hit)
    val <- causas_provinciales %>%
      filter(Año == as.character(a), Comunidad != "Sin asignar") %>%
      group_by(Comunidad) %>%
      summarise(F = sum(Fallecidos, na.rm = TRUE),
                P = sum(dplyr::distinct(dplyr::pick(Provincia, Sexo, Año, Poblacion))$Poblacion, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Tasa = if_else(P > 0, F / P * 100000, NA_real_)) %>%
      transmute(Comunidad = as.character(Comunidad), Mortalidad = Tasa)
    assign(key, val, envir = det_memo)
    val
  }

  # Rangos min-max globales 2018-2022 por componente (la evolución es comparable).
  # OPT: estáticos por sesión (no dependen de inputs) -> fuera de reactive.
  idx_rangos <- local({
    rg <- function(getter) {
      v <- unlist(lapply(det_anos, function(x) getter(as.character(x))[[2]]))
      v <- v[is.finite(v)]
      req(length(v) > 0)
      c(min(v), max(v))
    }
    list(
      renta = rg(function(x) det_ccaa_ano(idx_ind_renta, x)),
      med = rg(function(x) det_ccaa_ano(det_ind_medicos, x)),
      mort = rg(function(x) mort_ccaa_ano(x))
    )
  })

  idx_norm <- function(v, rg, invert = FALSE) {
    if (!all(is.finite(rg)) || rg[2] <= rg[1]) return(rep(NA_real_, length(v)))
    n <- (v - rg[1]) / (rg[2] - rg[1]) * 100
    if (invert) n <- 100 - n
    n
  }

  idx_build <- function(a, wr, wm, wmo, rg) {
    req(wr + wm + wmo > 0)
    det_ccaa_ano(idx_ind_renta, a) %>% rename(Renta = Valor) %>%
      inner_join(det_ccaa_ano(det_ind_medicos, a) %>% rename(Medicos = Valor), by = "Comunidad") %>%
      inner_join(mort_ccaa_ano(a), by = "Comunidad") %>%
      mutate(Indice = (wr * idx_norm(Renta, rg$renta) +
                       wm * idx_norm(Medicos, rg$med) +
                       wmo * idx_norm(Mortalidad, rg$mort, invert = TRUE)) / (wr + wm + wmo)) %>%
      filter(is.finite(Indice)) %>%
      arrange(desc(Indice))
  }

  datos_idx <- reactive({
    req(input$idx_ano, input$idx_wr, input$idx_wm, input$idx_wmo)
    idx_build(as.character(input$idx_ano), input$idx_wr, input$idx_wm, input$idx_wmo, idx_rangos)
  }) %>% bindCache(input$idx_ano, input$idx_wr, input$idx_wm, input$idx_wmo)

  datos_idx_evol <- reactive({
    req(input$idx_wr, input$idx_wm, input$idx_wmo)
    rg <- idx_rangos
    lapply(det_anos, function(x) {
      idx_build(as.character(x), input$idx_wr, input$idx_wm, input$idx_wmo, rg) %>%
        mutate(Año = as.character(x))
    }) %>% bind_rows()
  }) %>% bindCache(input$idx_wr, input$idx_wm, input$idx_wmo)

  output$idx_kpi_lider <- renderText({
    df <- datos_idx()
    req(nrow(df) > 0)
    paste0(df$Comunidad[1], " (",
           format(round(df$Indice[1], 1), decimal.mark = ",", nsmall = 1), ")")
  })

  output$idx_kpi_media <- renderText({
    df <- datos_idx()
    req(nrow(df) > 0)
    format(round(mean(df$Indice, na.rm = TRUE), 1), decimal.mark = ",", nsmall = 1)
  })

  output$idx_kpi_brecha <- renderText({
    df <- datos_idx()
    req(nrow(df) > 0)
    format(round(max(df$Indice, na.rm = TRUE) - min(df$Indice, na.rm = TRUE), 1),
           decimal.mark = ",", nsmall = 1)
  })

  output$idx_mapa <- renderLeaflet({
    df <- datos_idx()
    req(nrow(df) > 0)
    mapa_datos <- mapa_ccaa %>%
      left_join(df %>% select(Comunidad, Indice), by = "Comunidad")
    val_max <- max(mapa_datos$Indice, na.rm = TRUE)
    val_min <- min(mapa_datos$Indice, na.rm = TRUE)
    dominio <- dominio_seguro(mapa_datos$Indice)
    pal <- colorNumeric(palette = PAL_YLGNBU, domain = dominio, na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>Índice sintético: %s",
                         mapa_datos$Comunidad,
                         format(round(mapa_datos$Indice, 1), big.mark = ".", decimal.mark = ",", nsmall = 1)) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
      addPolygons(fillColor = ~pal(Indice), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = dominio, opacity = 0.8, title = "Índice (0-100)", position = "bottomright")
  })

  output$idx_ranking <- renderPlotly({
    df <- datos_idx()
    req(nrow(df) > 0)
    df <- df %>% arrange(Indice) %>% mutate(Comunidad = factor(Comunidad, levels = Comunidad))
    dom <- dominio_seguro(df$Indice)
    plot_ly(df, x = ~Indice, y = ~Comunidad, type = "bar", orientation = "h",
            marker = list(color = ~Indice, cmin = dom[1], cmax = dom[2],
                          colorscale = escala_plotly(PAL_YLGNBU), showscale = FALSE,
                          line = list(color = "white", width = 1)),
            text = ~format(round(Indice, 1), decimal.mark = ",", nsmall = 1), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Índice: %{x:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Índice sintético (0-100)", range = c(0, 105)),
             yaxis = list(title = "", automargin = TRUE),
             margin = list(l = 140, r = 60, t = 20, b = 50))
  })

  output$idx_evol <- renderPlotly({
    df <- datos_idx_evol()
    req(nrow(df) > 0)
    nccaa <- dplyr::n_distinct(df$Comunidad)
    plot_ly(df, x = ~Año, y = ~Indice, color = ~Comunidad,
            colors = grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(max(nccaa, 1)),
            type = "scatter", mode = "lines+markers",
            hovertemplate = "<b>%{fullData.name}</b><br>Año: %{x}<br>Índice: %{y:.1f}<extra></extra>") %>%
      layout(xaxis = list(title = "Año", dtick = 1),
             yaxis = list(title = "Índice sintético (0-100)"),
             legend = list(orientation = "h", x = 0, y = -0.2),
             margin = list(l = 60, r = 20, t = 10, b = 100))
  })

  output$idx_tabla <- DT::renderDT({
    df <- datos_idx() %>%
      transmute(Comunidad = Comunidad,
                Renta = round(Renta),
                Médicos = round(Medicos, 1),
                `Tasa bruta` = round(Mortalidad, 1),
                Índice = round(Indice, 1)) %>%
      arrange(desc(Índice))
    DT::datatable(df, options = list(pageLength = 19, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound("Renta", 0, dec.mark = ",", mark = ".") %>%
      DT::formatRound(c("Médicos", "Tasa bruta", "Índice"), 1, dec.mark = ",", mark = ".")
  })
}
