server_exceso <- function(input, output, session) {

  # Animación de años (checkbox de la pestaña).
  animar_anos(input, session, "ex_animar", "ex_ano",
              if (is.null(datos_edadprov)) character(0) else datos_edadprov$anos)

  # --- EXCESO DE MORTALIDAD SERVER (baseline = media 2015-2019) ---
  ex_base_anos <- function(anos_disp) {
    intersect(as.character(2015:2019), as.character(anos_disp))
  }

  datos_ex_anual <- reactive({
    req(!is.null(datos_edadprov), input$ex_prov, input$ex_sexo)
    df <- datos_edadprov$anual %>% filter(Sexo == input$ex_sexo)
    if (input$ex_prov != "Todas") df <- df %>% filter(Provincia == input$ex_prov)
    df <- df %>%
      group_by(Año) %>%
      summarise(Obs = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      mutate(Ano_Num = suppressWarnings(as.numeric(Año))) %>%
      filter(is.finite(Ano_Num)) %>%
      arrange(Ano_Num)
    req(nrow(df) > 0)
    base <- ex_base_anos(df$Año)
    req(length(base) >= 3)
    esp <- mean(df$Obs[df$Año %in% base], na.rm = TRUE)
    df %>%
      mutate(Esp = esp,
             Exc = Obs - Esp,
             Pscore = if_else(Esp > 0, Exc / Esp * 100, NA_real_))
  })

  ex_fmt <- function(v, dec = 0) {
    format(round(v, dec), big.mark = ".", decimal.mark = ",", nsmall = dec, scientific = FALSE, trim = TRUE)
  }

  ex_kpi_ano <- function(df, ano) {
    x <- df %>% filter(Ano_Num == ano)
    if (!nrow(x)) return("-")
    signo <- ifelse(x$Exc >= 0, "+", "")
    paste0(signo, ex_fmt(x$Exc), " (", signo, ex_fmt(x$Pscore, 1), " %)")
  }

  output$ex_kpi_2020 <- renderText({
    ex_kpi_ano(datos_ex_anual(), 2020)
  })

  output$ex_kpi_2021 <- renderText({
    ex_kpi_ano(datos_ex_anual(), 2021)
  })

  output$ex_kpi_peor <- renderText({
    df <- datos_ex_anual()
    req(nrow(df) > 0)
    x <- df %>% arrange(desc(Pscore)) %>% slice(1)
    paste0(as.integer(x$Ano_Num), " (", ex_fmt(x$Pscore, 1), " %)")
  })

  output$ex_evol <- renderPlotly({
    df <- datos_ex_anual()
    req(nrow(df) > 0)
    df <- df %>% mutate(
      Techo = Esp + pmax(Exc, 0),
      signo = ifelse(Exc >= 0, "+", ""),
      Etiq = paste0(signo, ex_fmt(Exc), " (", signo, ex_fmt(Pscore, 1), " %)")
    )
    pico <- df %>% arrange(desc(Pscore)) %>% slice(1)
    plot_ly() %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Esp, name = "Esperadas (2015-2019)",
                type = "scatter", mode = "lines",
                line = list(color = "#95a5a6", width = 2, dash = "dash"),
                hovertemplate = "<b>%{x}</b><br>Esperadas: %{y:,.0f}<extra></extra>") %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Techo, name = "Exceso",
                type = "scatter", mode = "none", fill = "tonexty",
                fillcolor = "rgba(231,76,60,0.22)", hoverinfo = "skip") %>%
      add_trace(data = df, x = ~Ano_Num, y = ~Obs, name = "Observadas",
                type = "scatter", mode = "lines+markers", text = ~Etiq,
                line = list(color = "#1f3a5f", width = 3.5, shape = "spline", smoothing = 0.4),
                marker = list(color = "#1f3a5f", size = 7, line = list(color = "white", width = 1.5)),
                hovertemplate = "<b>%{x}</b><br>Observadas: %{y:,.0f}<br>Exceso: %{text}<extra></extra>") %>%
      add_annotations(x = pico$Ano_Num, y = pico$Obs,
                      text = paste0("<b>", as.integer(pico$Ano_Num), "</b><br>", pico$Etiq),
                      showarrow = TRUE, arrowhead = 2, arrowcolor = "#c0392b",
                      ax = 55, ay = -55, bgcolor = "white", bordercolor = "#c0392b",
                      borderwidth = 1.5, font = list(size = 12)) %>%
      layout(xaxis = list(title = "Año", tickmode = "linear", dtick = 2),
             yaxis = list(title = "Defunciones"),
             legend = list(orientation = "h", x = 0.1, y = -0.25),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, t = 30, b = 95))
  })

  datos_ex_mes <- reactive({
    req(!is.null(datos_edadprov), input$ex_prov, input$ex_sexo, input$ex_ano)
    df <- datos_edadprov$mes %>% filter(Sexo == input$ex_sexo)
    if (input$ex_prov != "Todas") df <- df %>% filter(Provincia == input$ex_prov)
    obs <- df %>%
      filter(Año == as.character(input$ex_ano)) %>%
      group_by(Mes) %>%
      summarise(Obs = sum(Def, na.rm = TRUE), .groups = "drop")
    base <- df %>%
      filter(Año %in% ex_base_anos(unique(df$Año))) %>%
      group_by(Año, Mes) %>%
      summarise(Tot = sum(Def, na.rm = TRUE), .groups = "drop") %>%
      group_by(Mes) %>%
      summarise(Esp = mean(Tot, na.rm = TRUE), .groups = "drop")
    lv <- intersect(orden_meses, unique(c(as.character(obs$Mes), as.character(base$Mes))))
    req(length(lv) > 0)
    data.frame(Mes = factor(lv, levels = lv), stringsAsFactors = FALSE) %>%
      left_join(obs, by = "Mes") %>%
      left_join(base, by = "Mes") %>%
      mutate(Obs = if_else(is.finite(Obs), Obs, 0),
             Esp = if_else(is.finite(Esp), Esp, NA_real_),
             P = if_else(is.finite(Esp) & Esp > 0, (Obs - Esp) / Esp * 100, NA_real_))
  })

  output$ex_meses <- renderPlotly({
    df <- datos_ex_mes()
    req(nrow(df) > 0)
    mes_abr <- c(Enero = "Ene", Febrero = "Feb", Marzo = "Mar", Abril = "Abr", Mayo = "May",
                 Junio = "Jun", Julio = "Jul", Agosto = "Ago", Septiembre = "Sep",
                 Octubre = "Oct", Noviembre = "Nov", Diciembre = "Dic")
    df <- df %>% mutate(
      MesLab = factor(unname(mes_abr[as.character(Mes)]), levels = unname(mes_abr)),
      signo = ifelse(is.finite(P) & P >= 0, "+", ""),
      Etiq = ifelse(is.finite(P),
                    paste0(signo, ex_fmt(Obs - Esp), " (", signo, ex_fmt(P, 1), " %)"),
                    "sin baseline")
    )
    pr <- dominio_seguro(df$P)
    df <- df %>% mutate(Pplot = ifelse(is.finite(P), P, pr[1]))
    pico <- df %>% filter(is.finite(P)) %>% arrange(desc(P)) %>% slice(1)
    fig <- plot_ly() %>%
      add_trace(data = df, x = ~MesLab, y = ~Obs, text = ~Mes, customdata = ~Etiq,
                name = paste0("Observadas ", input$ex_ano),
                type = "bar",
                marker = list(color = ~Pplot, cmin = pr[1], cmax = pr[2],
                              colorscale = "Reds", showscale = TRUE,
                              colorbar = list(title = "Exceso %"),
                              line = list(color = "white", width = 1)),
                hovertemplate = "<b>%{text}</b><br>Observadas: %{y:,.0f}<br>Exceso: %{customdata}<extra></extra>") %>%
      add_trace(data = df, x = ~MesLab, y = ~Esp, text = ~Mes,
                name = "Esperadas (media 2015-2019)",
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 3, dash = "dash"),
                marker = list(color = "white", size = 8, line = list(color = "#2c3e50", width = 2)),
                hovertemplate = "<b>%{text}</b><br>Esperadas: %{y:,.0f}<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "Mes", categoryorder = "array", categoryarray = levels(df$MesLab)),
             yaxis = list(title = "Defunciones"),
             legend = list(orientation = "h", y = -0.3),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, t = 20, b = 100))
    if (nrow(pico) > 0) {
      fig <- fig %>% add_annotations(x = as.character(pico$MesLab), y = pico$Obs,
                                     text = paste0("<b>", as.character(pico$Mes), "</b><br>", pico$Etiq),
                                     showarrow = TRUE, arrowhead = 2, arrowcolor = "#c0392b",
                                     ax = 0, ay = -60, bgcolor = "white", bordercolor = "#c0392b",
                                     borderwidth = 1.5, font = list(size = 12))
    }
    fig
  })

  output$ex_tabla <- DT::renderDT({
    df <- datos_ex_anual() %>%
      transmute(Año = as.integer(Ano_Num),
                Observadas = Obs,
                Esperadas = round(Esp),
                Exceso = round(Exc),
                `P-score (%)` = round(Pscore, 1)) %>%
      arrange(desc(Año))
    DT::datatable(df, options = list(pageLength = 16, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatRound(c("Observadas", "Esperadas", "Exceso"), 0, dec.mark = ",", mark = ".") %>%
      DT::formatRound("P-score (%)", 1, dec.mark = ",", mark = ".")
  })
}
