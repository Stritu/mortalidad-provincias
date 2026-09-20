
  # PESTAÑA 7: ANÁLISIS MULTIVARIANTE (PCA + CLUSTERS, NO REGRESIÓN)
ui_multivariante <- nav_panel(
    title = "Análisis multivariante",
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuración",
        width = 290,
        selectInput("pm_ano", "Año:", choices = sort(unique(as.character(causas_provinciales$Año))),
                    selected = if ("2022" %in% as.character(causas_provinciales$Año)) "2022" else sort(unique(as.character(causas_provinciales$Año)))[1]),
        radioButtons("pm_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
        sliderInput("pm_k", "Nº de clusters (k-means):", min = 2, max = 6, value = 3, step = 1),
        div(class = "filter-help", HTML(
          "<b>Nota:</b> PCA sobre tasas por 100.000 (centradas y escaladas; sin la causa Total ni causas sin variación entre provincias). K-means sobre PC1-PC2 con semilla fija."
        )),
        uiOutput("pm_info")
      ),
      div(class = "regression-dashboards",
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Varianza PC1 + PC2", value = textOutput("pm_kpi_var"),
                      showcase = bsicons::bs_icon("graph-up"), theme = "primary"),
            value_box(title = "Clusters", value = textOutput("pm_kpi_k"),
                      showcase = bsicons::bs_icon("check-circle"), theme = "success"),
            value_box(title = "Provincias × causas", value = textOutput("pm_kpi_n"),
                      showcase = bsicons::bs_icon("grid"), theme = "info")
          )
      ),
      div(
        class = "mb-4",
        bslib::card(
          card_header("Interpretación"),
          card_body(HTML(paste0(
            "<p><b>Biplot:</b> cada punto es una provincia situada según sus dos primeras componentes (las direcciones que más diferencian los perfiles de mortalidad). Provincias cercanas tienen causas de muerte parecidas.</p>",
            "<p><b>Flechas de las causas:</b> cada flecha es una causa de defunción. Apunta hacia donde aumentan sus tasas: las provincias en esa dirección mueren más por esa causa. Su <b>longitud</b> indica cuánto pesa esa causa en la diferencia entre provincias (flechas largas = causas que más discriminan) y el <b>ángulo</b> entre dos flechas su relación: ángulo pequeño = suben juntas, ángulo llano (~180º) = cuando una sube la otra baja, ángulo recto = independientes.</p>",
            "<p><b>Scree:</b> la barra muestra lo que explica cada componente y la línea el acumulado. Si con dos componentes se supera ~2/3 de la varianza, el plano resume bien el conjunto.</p>",
            "<p><b>K-means y mapa:</b> agrupa provincias por perfil de mortalidad (no por geografía): que un cluster salga geográficamente compacto en el mapa es un hallazgo, no un requisito.</p>",
            "<p class='text-muted mb-0'><small>Nivel ecológico (provincias, no personas): las asociaciones no implican causalidad individual. Tasas centradas y escaladas antes del PCA.</small></p>"
          )))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Biplot PC1-PC2 (provincias por cluster)"),
            div(class = "card-body p-2", plotlyOutput("pm_biplot", height = "850px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Mapa de clusters"),
            div(class = "card-body p-0", leafletOutput("pm_mapa", height = "750px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Varianza explicada (scree)"),
            div(class = "card-body p-2", plotlyOutput("pm_scree", height = "600px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Correlaciones entre causas (Pearson)"),
            div(class = "card-body p-2", plotlyOutput("pm_cor", height = "800px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Perfil medio por cluster (tasas por 100k hab.)"),
            div(class = "card-body p-2", div(class = "table-responsive", tableOutput("pm_perfil")))
        ),
)
    )
  )
