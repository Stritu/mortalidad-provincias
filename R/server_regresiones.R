server_regresiones <- function(input, output, session) {

  
  
  # ---------------------------------------------------------------------------
  # REGRESIÓN ENTRE CAUSAS (tasas provinciales por 100.000 hab.)
  # ---------------------------------------------------------------------------
  construir_datos_modelo_p32 <- function(causa_x, causa_y, anio, sexo, usar_log = FALSE) {
    if (identical(causa_x, causa_y)) return(data.frame())
    df <- causas_provinciales %>%
      filter(Año == as.character(anio), Defunción %in% c(causa_x, causa_y)) %>%
      filter(!Provincia %in% c("Ceuta", "Melilla"))
    # causas_provinciales solo trae Hombres/Mujeres: con "Ambos" se suman.
    if (!identical(sexo, "Ambos")) df <- df %>% filter(Sexo == sexo)
    if (!nrow(df)) return(data.frame())
    # Tasas ponderadas por población (coherente con el resto de la app).
    wide <- df %>%
      group_by(Provincia, Defunción) %>%
      summarise(
        Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      select(Provincia, Defunción, Tasa) %>%
      tidyr::pivot_wider(names_from = Defunción, values_from = Tasa, values_fill = NA_real_)
    if (!all(c(causa_x, causa_y) %in% names(wide))) return(data.frame())
    out <- wide %>%
      transmute(
        Provincia = as.character(Provincia),
        X_val = suppressWarnings(as.numeric(.data[[causa_x]])),
        Y_val = suppressWarnings(as.numeric(.data[[causa_y]]))
      ) %>%
      filter(is.finite(X_val), is.finite(Y_val))
    if (isTRUE(usar_log)) {
      out <- out %>%
        transmute(
          Provincia = Provincia,
          X_val = suppressWarnings(log(X_val)),
          Y_val = suppressWarnings(log(Y_val))
        ) %>%
        filter(is.finite(X_val), is.finite(Y_val))
    }
    out
  }

  datos_32 <- reactive({
    req(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo)
    df <- construir_datos_modelo_p32(
      input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo,
      usar_log = isTRUE(input$p32_log)
    )
    req(nrow(df) > 2)
    df
  })

  modelo_32 <- reactive({
    df <- datos_32()
    req(nrow(df) > 2)
    lm(Y_val ~ X_val, data = df)
  })

  supuestos_32 <- reactive({
    df <- datos_32()
    evaluar_supuestos(modelo_32(), df)
  })

  # Submatriz de vecinas limitada a las provincias con datos.
  listw_provincias <- function(provincias) {
    idx <- match(as.character(provincias), mapa_vecinos_base$Provincia)
    idx <- idx[is.finite(idx)]
    if (length(idx) <= 4) return(NULL)
    coords_sub <- get_vecinos()$coords[idx, , drop = FALSE]
    k_sub <- min(4, nrow(coords_sub) - 1)
    nb_sub <- spdep::knn2nb(spdep::knearneigh(coords_sub, k = k_sub))
    spdep::nb2listw(nb_sub, style = "W", zero.policy = TRUE)
  }
  
  moran_32 <- reactive({
    df <- datos_32()
    req(nrow(df) > 4)
    fit <- lm(Y_val ~ X_val, data = df)
    df$residuo <- residuals(fit)
    
    # Submatriz de vecinas limitada a las provincias con datos.
    lw <- listw_provincias(df$Provincia)
    req(!is.null(lw))
    moran <- spdep::moran.test(df$residuo, lw, zero.policy = TRUE)
    list(moran = moran, n = nrow(df))
  })
  
  output$p32_filtro_info <- renderUI({
    req(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo)
    div(class = "filter-help",
        HTML(paste0("<b>Modelo:</b> X = ", htmltools::htmlEscape(input$p32_x),
                    " · Y = ", htmltools::htmlEscape(input$p32_y),
                    " · ", htmltools::htmlEscape(as.character(input$p32_ano)),
                    " · ", htmltools::htmlEscape(input$p32_sexo),
                    if (isTRUE(input$p32_log)) " · <b>escala logarítmica</b>." else ".")))
  })

  output$p32_r2 <- renderText({
    fit <- modelo_32()
    r2 <- summary(fit)$r.squared
    formatC(r2, format = "f", digits = 2, decimal.mark = ".")
  })

  output$p32_val <- renderText({
    tab <- supuestos_32()
    if (all(grepl("^Cumplido:", tab$Conclusion))) "VÁLIDO" else "NO VÁLIDO"
  })

  output$p32_esp <- renderText({
    obj <- moran_32()
    p <- obj$moran$p.value
    if (is.finite(p) && p > 0.05) "VÁLIDO" else "NO VÁLIDO"
  })

  output$p32_scatter <- renderPlotly({
    df <- datos_32()
    req(nrow(df) > 2)
    suf_log <- if (isTRUE(input$p32_log)) " (log)" else ""
    fit <- lm(Y_val ~ X_val, data = df)
    x_seq <- seq(min(df$X_val, na.rm = TRUE), max(df$X_val, na.rm = TRUE), length.out = 100)
    df_line <- data.frame(X_val = x_seq)
    pred_ic <- suppressWarnings(predict(fit, newdata = df_line, interval = "confidence", level = 0.95))
    df_line$Y_val <- pred_ic[, "fit"]
    df_line$lwr <- pred_ic[, "lwr"]
    df_line$upr <- pred_ic[, "upr"]
    p <- plot_ly() %>%
      add_trace(
        data = df,
        x = ~X_val,
        y = ~Y_val,
        text = ~Provincia,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, color = "#1f77b4", line = list(color = "white", width = 1.5)),
        hovertemplate = "<b>%{text}</b><br>X: %{x:.2f}<br>Y: %{y:.2f}<extra></extra>"
      ) %>%
      add_trace(
        data = df_line,
        x = ~X_val,
        y = ~Y_val,
        type = "scatter",
        mode = "lines",
        line = list(color = "#e74c3c", width = 2.5),
        hoverinfo = "none"
      ) %>%
      add_trace(
        data = df_line,
        x = ~X_val,
        y = ~upr,
        type = "scatter",
        mode = "lines",
        line = list(width = 0),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      add_trace(
        data = df_line,
        x = ~X_val,
        y = ~lwr,
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = "rgba(231,76,60,0.15)",
        line = list(width = 0),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = paste0(input$p32_x, suf_log, " (tasa de defunción por 100k hab.)"), showgrid = TRUE, gridcolor = "#E5E5E5"),
        yaxis = list(title = paste0(input$p32_y, suf_log, " (tasa de defunción por 100k hab.)"), showgrid = TRUE, gridcolor = "#E5E5E5"),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 55, r = 20, t = 10, b = 45)
      )
  })

  output$p32_supuestos <- renderTable({
    supuestos_32()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$p32_moran_tabla <- renderTable({
    obj <- moran_32()
    p <- obj$moran$p.value
    i <- unname(obj$moran$estimate[[1]])
    esperado <- unname(obj$moran$estimate[[2]])
    z <- unname(obj$moran$statistic)
    data.frame(
      Elemento = c("Código", "Resultado", "Conclusión"),
      Valor = c(
        "spdep::moran.test(residuo, submatriz de vecinas style = 'W')",
        paste0("Moran's I = ", round(i, 4), "; Esperanza = ", round(esperado, 4),
               "; z = ", round(z, 4), "; p = ", format.pval(p, digits = 4, eps = 0.001)),
        if (is.finite(p) && p > 0.05) "p > 0,05: sin evidencia de autocorrelación espacial" else "p ≤ 0,05: evidencia de autocorrelación espacial"
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$p32_moran_conclusion <- renderText({
    obj <- moran_32()
    p <- obj$moran$p.value
    if (is.finite(p) && p > 0.05) {
      "VÁLIDO: no hay evidencia de autocorrelación espacial en los residuos."
    } else {
      "NO VÁLIDO para inferencia espacial: hay autocorrelación espacial en los residuos."
    }
  })



  # Matriz de validez (clásica + espacial) para todos los pares de causas,
  # con el año, sexo y escala seleccionados. Misma lectura que la demográfica.
  evaluar_candidatos_p32 <- function(anio, sexo, usar_log = FALSE) {
    causas <- setdiff(lista_defunciones, "Total")
    if (length(causas) < 2) return(data.frame())
    df <- causas_provinciales %>%
      filter(Año == as.character(anio), Defunción %in% causas) %>%
      filter(!Provincia %in% c("Ceuta", "Melilla"))
    if (!identical(sexo, "Ambos")) df <- df %>% filter(Sexo == sexo)
    wide <- df %>%
      group_by(Provincia, Defunción) %>%
      summarise(
        Fallecidos = sum(Fallecidos, na.rm = TRUE),
        Poblacion = sum(Poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Tasa = if_else(Poblacion > 0, Fallecidos / Poblacion * 100000, NA_real_)) %>%
      select(Provincia, Defunción, Tasa) %>%
      tidyr::pivot_wider(names_from = Defunción, values_from = Tasa, values_fill = NA_real_)
    cols <- intersect(causas, names(wide))
    if (length(cols) < 2) return(data.frame())
    pares <- expand.grid(x = cols, y = cols, stringsAsFactors = FALSE) %>% filter(x != y)
    resultados <- lapply(seq_len(nrow(pares)), function(i) {
      vx <- pares$x[i]
      vy <- pares$y[i]
      xv <- suppressWarnings(as.numeric(wide[[vx]]))
      yv <- suppressWarnings(as.numeric(wide[[vy]]))
      if (isTRUE(usar_log)) {
        xv <- suppressWarnings(log(xv))
        yv <- suppressWarnings(log(yv))
      }
      ok <- is.finite(xv) & is.finite(yv)
      ddf <- data.frame(
        Provincia = as.character(wide$Provincia[ok]),
        X_val = xv[ok], Y_val = yv[ok],
        stringsAsFactors = FALSE
      )
      if (nrow(ddf) <= 2) {
        return(data.frame(Variable_predictora = vx, Variable_explicativa = vy, N = nrow(ddf), Valido = FALSE, EspacialValido = NA, stringsAsFactors = FALSE))
      }
      fit <- tryCatch(lm(Y_val ~ X_val, data = ddf), error = function(e) NULL)
      if (is.null(fit)) {
        return(data.frame(Variable_predictora = vx, Variable_explicativa = vy, N = nrow(ddf), Valido = FALSE, EspacialValido = NA, stringsAsFactors = FALSE))
      }
      tab <- tryCatch(evaluar_supuestos(fit, ddf), error = function(e) NULL)
      valido <- !is.null(tab) && nrow(tab) == 4 && all(grepl("^Cumplido:", tab$Conclusion))
      # Moran con submatriz limitada a las provincias con datos.
      espacial <- tryCatch({
        ddf$residuo <- residuals(fit)
        lw <- listw_provincias(ddf$Provincia)
        if (is.null(lw)) NA
        else {
          m <- spdep::moran.test(ddf$residuo, lw, zero.policy = TRUE)
          is.finite(m$p.value) && m$p.value > 0.05
        }
      }, error = function(e) NA)
      data.frame(
        Variable_predictora = vx,
        Variable_explicativa = vy,
        N = nrow(ddf),
        Valido = valido,
        EspacialValido = espacial,
        stringsAsFactors = FALSE
      )
    })
    bind_rows(resultados)
  }

  modelos_candidatos_p32 <- reactive({
    evaluar_candidatos_p32(input$p32_ano, input$p32_sexo, usar_log = isTRUE(input$p32_log))
  }) %>% bindCache(input$p32_ano, input$p32_sexo, input$p32_log)

  output$p32_modelos_info <- renderTable({
    res <- modelos_candidatos_p32()
    causas <- setdiff(lista_defunciones, "Total")
    if (!length(causas)) return(data.frame())

    # Matriz: filas = causa explicativa (Y), columnas = causa predictora (X).
    mat <- matrix("—", nrow = length(causas), ncol = length(causas),
                  dimnames = list(causas, causas))

    if (nrow(res)) {
      # OPT: vectorizado con matching por índices
      m <- match(res$Variable_explicativa, causas)
      n <- match(res$Variable_predictora, causas)
      ok <- !is.na(m) & !is.na(n) & m != n
      if (any(ok)) {
        idx <- cbind(m[ok], n[ok])
        v <- res$Valido[ok]
        e <- res$EspacialValido[ok]
        mat[idx] <- ifelse(v & e, "✓ ●", ifelse(v, "✓", ifelse(e, "●", "")))
      }
    }

    out <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    out <- cbind(`Causa explicativa (Y)` = rownames(out), out)
    rownames(out) <- NULL
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs", width = "100%")
}

