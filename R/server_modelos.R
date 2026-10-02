server_modelos <- function(input, output, session) {

  # --- REGRESION ENTRE CAUSAS (provincial) ---

  
  
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
  }) %>% bindCache(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo, input$p32_log, cache = "app")

  modelo_32 <- reactive({
    df <- datos_32()
    req(nrow(df) > 2)
    lm(Y_val ~ X_val, data = df)
  }) %>% bindCache(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo, input$p32_log, cache = "app")

  supuestos_32 <- reactive({
    df <- datos_32()
    evaluar_supuestos(modelo_32(), df)
  }) %>% bindCache(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo, input$p32_log, cache = "app")

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
  }) %>% bindCache(input$p32_x, input$p32_y, input$p32_ano, input$p32_sexo, input$p32_log, cache = "app")
  
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
  }) %>% bindCache(input$p32_ano, input$p32_sexo, input$p32_log, cache = "app")

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

  # --- MODELO EUROPEO: EFECTOS FIJOS ---
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
  }) %>% bindCache(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe, input$eur_reg_log, cache = "app")
  
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
  }) %>% bindCache(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe, input$eur_reg_log, cache = "app")
  
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
  }) %>% bindCache(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe, input$eur_reg_log, cache = "app")
  
  # Diagnostics
  diag_plots <- reactive({
    fit <- modelo_eur_reg()
    df <- datos_eur_reg()
    req(nrow(df) > 10)
    df$.fitted <- fitted(fit)
    df$.resid <- residuals(fit)
    df$.stdresid <- rstandard(fit)
    list(fit = fit, df = df)
  }) %>% bindCache(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe, input$eur_reg_log, cache = "app")
  
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
  }) %>% bindCache(input$eur_reg_causa, input$eur_reg_sexo, input$eur_reg_fe, input$eur_reg_log, cache = "app")
  
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

  # Veredicto clásico absorbido de la antigua "Comparación estadística europea":
  # test F del efecto país sobre tasas originales (independiente del Log).
  datos_eur_veredicto <- reactive({
    req(input$eur_reg_causa, input$eur_reg_sexo)
    df <- europa_agrupada %>%
      filter(causa_grupo == input$eur_reg_causa, sexo == input$eur_reg_sexo,
             is.finite(tasa_100k), is.finite(anio)) %>%
      transmute(pais = factor(pais), anio = as.numeric(anio), Tasa = tasa_100k) %>%
      filter(is.finite(Tasa))
    req(nrow(df) >= 5, dplyr::n_distinct(df$pais) >= 3)
    fit <- if (dplyr::n_distinct(df$anio) >= 2) lm(Tasa ~ pais + anio, data = df) else lm(Tasa ~ pais, data = df)
    a <- anova(fit)
    p <- if ("pais" %in% rownames(a) && "Pr(>F)" %in% names(a)) a["pais", "Pr(>F)"] else NA_real_
    list(p = unname(p), tiene_anio = dplyr::n_distinct(df$anio) >= 2)
  }) %>% bindCache(input$eur_reg_causa, input$eur_reg_sexo, cache = "app")

  output$eur_reg_veredicto <- renderText({
    v <- datos_eur_veredicto()
    if (!is.finite(v$p)) {
      "No evaluable con estos datos"
    } else if (v$p < 0.05) {
      if (v$tiene_anio) "Sí: diferencias entre países, controlando por año" else "Sí: diferencias entre países"
    } else {
      "No: sin evidencia suficiente de diferencias"
    }
  })

  output$eur_reg_boxplot <- renderPlotly({
    req(input$eur_reg_causa, input$eur_reg_sexo)
    df <- europa_agrupada %>%
      filter(causa_grupo == input$eur_reg_causa, sexo == input$eur_reg_sexo,
             is.finite(tasa_100k)) %>%
      mutate(País = traducir_pais(as.character(pais))) %>%
      mutate(País = reorder(País, tasa_100k, FUN = function(v) median(v, na.rm = TRUE)))
    validate(need(nrow(df) > 5, "No hay suficientes datos."))
    plot_ly(df, y = ~País, x = ~tasa_100k, type = "box", orientation = "h",
            boxpoints = "outliers",
            marker = list(color = "#2c3e50"), line = list(color = "#2c3e50"),
            hovertemplate = "<b>%{y}</b><br>Tasa: %{x:.1f} por 100.000 hab.<extra></extra>") %>%
      layout(xaxis = list(title = "Tasa (por 100.000 hab.)"),
             yaxis = list(title = "", automargin = TRUE),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 185, r = 30, t = 15, b = 55))
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

  # --- MULTIVARIANTE: PCA + K-MEANS ---

  
  # ---------------------------------------------------------------------------
  # ANÁLISIS MULTIVARIANTE: PCA + K-MEANS SOBRE TASAS POR CAUSA (PROVINCIAS)
  # ---------------------------------------------------------------------------
  pal_cluster <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

  # Lecturas directas del precomputo global (sin cadenas de cálculo en sesión).
  datos_pm_sel <- reactive({
    req(input$pm_ano, input$pm_sexo)
    obj <- pm_cache[[pm_key(input$pm_ano, input$pm_sexo)]]
    req(!is.null(obj))
    obj
  })

  datos_pm_wide <- reactive({
    obj <- datos_pm_sel()
    list(mat = obj$mat, n_causas_ini = obj$n_causas_ini)
  })

  pca_pm <- reactive({
    datos_pm_sel()$pca
  })

  km_pm <- reactive({
    obj <- datos_pm_sel()
    k <- min(max(2L, as.integer(input$pm_k)), 6L)
    req(k >= 2)
    km <- obj$kms[[k - 1L]]
    req(!is.null(km))
    km
  })

  output$pm_info <- renderUI({
    w <- datos_pm_wide()
    div(class = "filter-help", HTML(paste0(
      "<b>Datos:</b> ", nrow(w$mat), " provincias × ", ncol(w$mat),
      " causas (", w$n_causas_ini - ncol(w$mat), " sin variación). ",
      "PCA centrado y escalado · k-means sobre PC1-PC2."
    )))
  })

  output$pm_kpi_var <- renderText({
    p <- pca_pm()
    v <- summary(p)$importance
    paste0(format(round(100 * sum(v["Proportion of Variance", 1:min(2, ncol(v))]), 1), decimal.mark = ","), " %")
  })

  output$pm_kpi_k <- renderText({
    as.character(nrow(km_pm()$centers))
  })

  output$pm_kpi_n <- renderText({
    w <- datos_pm_wide()
    paste0(nrow(w$mat), " × ", ncol(w$mat))
  })

  output$pm_scree <- renderPlotly({
    p <- pca_pm()
    v <- summary(p)$importance
    df <- data.frame(
      PC_num = seq_len(ncol(v)),
      PC = paste0("PC", seq_len(ncol(v))),
      Var = 100 * v["Proportion of Variance", ]
    )
    df$Cum <- cumsum(df$Var)
    # Eje X numérico con etiquetas (los ejes categóricos fallaban en algunos navegadores).
    plot_ly() %>%
      add_trace(data = df, x = ~PC_num, y = ~Var, type = "bar",
                marker = list(color = ~Var, colorscale = "Blues", showscale = FALSE,
                              line = list(color = "rgba(0,0,0,0.15)", width = 1)),
                text = ~PC,
                hovertemplate = "<b>%{text}</b><br>Varianza: %{y:.1f} %<extra></extra>") %>%
      add_trace(data = df, x = ~PC_num, y = ~Cum, type = "scatter", mode = "lines+markers",
                yaxis = "y2", line = list(color = "#e74c3c", width = 2.5),
                marker = list(color = "#e74c3c", size = 7),
                text = ~PC,
                hovertemplate = "<b>%{text}</b><br>Acumulado: %{y:.1f} %<extra></extra>") %>%
      layout(xaxis = list(title = "Componente", tickmode = "array",
                          tickvals = df$PC_num, ticktext = df$PC),
             yaxis = list(title = "% de varianza explicada"),
             yaxis2 = list(title = "% acumulado", overlaying = "y", side = "right",
                           range = c(0, 100), showgrid = FALSE),
             margin = list(l = 55, r = 60, t = 20, b = 50))
  })

  output$pm_biplot <- renderPlotly({
    p <- pca_pm()
    km <- km_pm()
    k <- nrow(km$centers)
    cols <- pal_cluster[seq_len(k)]
    sc <- as.data.frame(p$x[, 1:2])
    sc$Provincia <- rownames(p$x)
    sc$Cluster <- factor(km$cluster, levels = seq_len(k),
                         labels = paste0("Cluster ", seq_len(k)))
    rot <- p$rotation[, 1:2, drop = FALSE]
    mult <- min(diff(range(sc$PC1)), diff(range(sc$PC2))) /
      max(abs(rot), na.rm = TRUE) * 0.7
    if (!is.finite(mult) || mult <= 0) mult <- 1
    fl <- as.data.frame(rot * mult)
    fl$Causa <- rownames(rot)
    # Flechas como segmentos NaN-separados (una sola traza de líneas estándar).
    seg <- data.frame(x = as.vector(rbind(0, fl$PC1, NA_real_)),
                      y = as.vector(rbind(0, fl$PC2, NA_real_)))
    plot_ly() %>%
      add_trace(data = sc, x = ~PC1, y = ~PC2, color = ~Cluster, colors = cols,
                text = ~Provincia, type = "scatter", mode = "markers",
                marker = list(size = 9),
                hovertemplate = "<b>%{text}</b><br>%{fullData.name}<br>PC1: %{x:.2f}<br>PC2: %{y:.2f}<extra></extra>") %>%
      add_trace(data = seg, x = ~x, y = ~y, type = "scatter", mode = "lines",
                line = list(color = "#7f8c8d", width = 1.5, dash = "dot"),
                hoverinfo = "none", showlegend = FALSE) %>%
      add_text(data = fl, x = ~PC1, y = ~PC2, text = ~Causa,
               textfont = list(size = 10, color = "#2c3e50"),
               hoverinfo = "none", showlegend = FALSE) %>%
      layout(xaxis = list(title = "PC1", zeroline = TRUE, showgrid = TRUE, gridcolor = "#E5E5E5"),
             yaxis = list(title = "PC2", zeroline = TRUE, showgrid = TRUE, gridcolor = "#E5E5E5"),
             legend = list(orientation = "h", x = 0.2, y = -0.18),
             margin = list(l = 55, r = 20, t = 20, b = 90))
  })

  output$pm_mapa <- renderLeaflet({
    p <- pca_pm()
    km <- km_pm()
    k <- nrow(km$centers)
    cols <- pal_cluster[seq_len(k)]
    df <- data.frame(Provincia = rownames(p$x),
                     Cluster = factor(km$cluster, levels = seq_len(k),
                                      labels = paste0("Cluster ", seq_len(k))),
                     stringsAsFactors = FALSE)
    mapa_datos <- mapa_provincias %>% left_join(df, by = c("NAME_2" = "Provincia"))
    pal <- leaflet::colorFactor(palette = cols, domain = levels(df$Cluster), na.color = "#E0E0E0")
    etiquetas <- sprintf("<strong>%s</strong><br/>%s",
                         mapa_datos$NAME_2,
                         ifelse(is.na(mapa_datos$Cluster), "Sin datos", as.character(mapa_datos$Cluster))) %>%
      lapply(htmltools::HTML)
    leaflet(mapa_datos) %>%
      tiles_osm() %>%
      addPolygons(fillColor = ~pal(Cluster), weight = 1, color = "white", fillOpacity = 0.8, label = etiquetas) %>%
      addLegend(pal = pal, values = ~Cluster, opacity = 0.8, title = "Cluster", position = "bottomright") %>%
      control_ano(input$pm_ano)
  })

  # Codo + silueta sobre los k-means precomputados (k = 2..6, espacio PC1-PC2).
  sil_pm <- function(scores, grupos) {
    if (!requireNamespace("cluster", quietly = TRUE)) return(NA_real_)
    suppressWarnings(tryCatch(
      mean(cluster::silhouette(grupos, stats::dist(scores))[, "sil_width"]),
      error = function(e) NA_real_
    ))
  }

  datos_pm_codo <- reactive({
    obj <- datos_pm_sel()
    scores <- obj$pca$x[, 1:min(2, ncol(obj$pca$x)), drop = FALSE]
    ks <- which(!vapply(obj$kms, is.null, logical(1))) + 1L
    data.frame(
      k = ks,
      WSS = vapply(obj$kms[ks - 1L], function(km) km$tot.withinss, numeric(1)),
      Silueta = vapply(obj$kms[ks - 1L], function(km) sil_pm(scores, km$cluster), numeric(1))
    )
  })

  output$pm_codo <- renderPlotly({
    codo <- datos_pm_codo()
    req(nrow(codo) > 0)
    mejor <- if (all(is.na(codo$Silueta))) NA_integer_ else codo$k[which.max(codo$Silueta)]
    p <- plot_ly(codo, x = ~k, y = ~WSS, type = "scatter", mode = "lines+markers",
                 text = ~paste0("k=", k, "<br>Silueta: ", round(Silueta, 3)),
                 hoverinfo = "text",
                 marker = list(color = "#1a2f47", line = list(color = "white", width = 1)),
                 line = list(color = "#1a2f47")) %>%
      layout(xaxis = list(title = "k", dtick = 1), yaxis = list(title = "Dispersión interna (WSS)"),
             hoverlabel = list(bgcolor = "white"),
             margin = list(l = 70, r = 20, b = 60, t = 20))
    if (is.finite(mejor)) {
      p <- p %>% layout(shapes = list(list(type = "line", x0 = mejor, x1 = mejor,
                                           y0 = 0, y1 = 1, yref = "paper",
                                           line = list(color = "#0E9F8A", dash = "dash"))),
                        annotations = list(list(x = mejor, y = 1, yref = "paper",
                                                text = "mejor silueta", showarrow = FALSE,
                                                font = list(color = "#0E9F8A"))))
    }
    p
  })

  output$pm_perfiles <- renderPlotly({
    obj <- datos_pm_sel()
    km <- km_pm()
    mat <- obj$mat
    req(nrow(mat) > 0)
    grupos <- factor(km$cluster, levels = seq_len(nrow(km$centers)),
                     labels = paste0("Cluster ", seq_len(nrow(km$centers))))
    medias <- aggregate(mat, by = list(Cluster = grupos), FUN = function(v) mean(v, na.rm = TRUE))
    nac <- colMeans(mat, na.rm = TRUE)
    logr <- log2(sweep(as.matrix(medias[, -1, drop = FALSE]), 2, nac, `/`))
    logr[!is.finite(logr)] <- 0
    rownames(logr) <- as.character(medias$Cluster)
    m <- suppressWarnings(max(abs(logr)))
    if (!is.finite(m) || m == 0) m <- 1
    plot_ly(x = rownames(logr), y = colnames(logr), z = t(logr), type = "heatmap",
            colorscale = "RdBu", reversescale = TRUE, zmin = -m, zmax = m,
            hovertemplate = "<b>%{x} · %{y}</b><br>log2: %{z:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "", tickfont = list(size = 10)),
             margin = list(l = 220, r = 20, b = 100, t = 20))
  })

  output$pm_tabla <- DT::renderDT({
    p <- pca_pm()
    km <- km_pm()
    k <- nrow(km$centers)
    tab <- data.frame(
      Provincia = rownames(p$x),
      Comunidad = unname(prov_a_comunidad(rownames(p$x))),
      Cluster = paste0("Cluster ", km$cluster),
      stringsAsFactors = FALSE
    ) %>% arrange(Cluster, Provincia)
    DT::datatable(tab, options = list(pageLength = 17, autoWidth = TRUE, scrollX = TRUE),
                  rownames = FALSE)
  })

  output$pm_cor <- renderPlotly({
    m <- datos_pm_sel()$cor
    # Ejes numéricos con etiquetas (los categóricos fallaban en algunos navegadores).
    labs <- stringr::str_wrap(colnames(m), width = 14)
    n <- nrow(m)
    ord <- rev(seq_len(n))
    m <- m[ord, , drop = FALSE]
    labs_y <- labs[ord]
    # array(): paste0 aplana matrices; se restaura la dimensión para que
    # cada celda lleve su etiqueta (orden column-major, igual que z).
    hov <- array(
      paste0(outer(labs_y, labs, FUN = function(a, b) paste0(a, " × ", b)),
             "<br>r = ", format(round(m, 2), nsmall = 2, decimal.mark = ",")),
      dim = dim(m)
    )
    plot_ly(x = seq_len(ncol(m)), y = rev(seq_len(n)), z = m, type = "heatmap",
            colorscale = "RdBu", zmin = -1, zmax = 1, showscale = TRUE,
            colorbar = list(title = "r"),
            text = hov,
            hovertemplate = "%{text}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickmode = "array", tickvals = seq_len(ncol(m)),
                          ticktext = labs, tickfont = list(size = 9)),
             yaxis = list(title = "", tickmode = "array", tickvals = rev(seq_len(n)),
                          ticktext = labs[ord], tickfont = list(size = 9)),
             margin = list(l = 150, r = 40, t = 20, b = 100))
  })

  output$pm_perfil <- renderTable({
    p <- pca_pm()
    km <- km_pm()
    w <- datos_pm_wide()
    df <- as.data.frame(w$mat)
    df$Cluster <- factor(km$cluster, levels = seq_len(nrow(km$centers)),
                         labels = paste0("Cluster ", seq_len(nrow(km$centers))))
    df %>%
      group_by(Cluster) %>%
      summarise(N = dplyr::n(), across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      arrange(Cluster) %>%
      mutate(across(where(is.numeric), ~ round(.x, 1)))
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs")
}

