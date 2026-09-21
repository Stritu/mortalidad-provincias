server_multivariante <- function(input, output, session) {

  
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
      addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
               attribution = "&copy; OpenStreetMap contributors") %>%
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

  # Forzar renderizado aunque la pestaña no esté activa
  for (nm in c("pm_info", "pm_kpi_var", "pm_kpi_k", "pm_kpi_n",
               "pm_scree", "pm_biplot", "pm_mapa", "pm_codo", "pm_perfiles", "pm_tabla",
               "pm_cor", "pm_perfil")) {
    outputOptions(output, nm, suspendWhenHidden = FALSE)
}
}
