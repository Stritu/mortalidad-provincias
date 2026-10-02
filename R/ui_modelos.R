# PESTAÑA: MODELOS ESTADISTICOS (regresion + panel europeo + multivariante)
ui_modelos <- nav_panel(
    title = "Modelos estadísticos",
    div(class = "tab-hero",
        h2("Modelos estadísticos"),
        p("Regresión entre causas, panel europeo de efectos fijos y multivariante.")),
    navset_tab(
      id = "nav_modelos",
      nav_panel(
        title = "Regresión entre causas",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("p32_x", "Causa predictora (X):", choices = setdiff(lista_defunciones, "Total"),
                        selected = { ch <- setdiff(lista_defunciones, "Total"); if (length(ch)) ch[1] else NULL }),
            selectInput("p32_y", "Causa explicativa (Y):", choices = setdiff(lista_defunciones, "Total"),
                        selected = { ch <- setdiff(lista_defunciones, "Total"); if (length(ch) > 1) ch[2] else if (length(ch)) ch[1] else NULL }),
            selectInput("p32_ano", "Año:", choices = sort(unique(as.character(causas_provinciales$Año))),
                        selected = if ("2020" %in% as.character(causas_provinciales$Año)) "2020" else sort(unique(as.character(causas_provinciales$Año)))[1]),
            checkboxInput("p32_animar", "Animar años", value = FALSE),
            radioButtons("p32_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
            checkboxInput("p32_log", "Aplicar logaritmo a X e Y", value = FALSE),
            div(class = "filter-help", HTML(
              "<b>Nota:</b> tasas de defunción provinciales por causa (defunciones / población × 100.000). Con <b>Ambos</b> se suman ambos sexos. Ceuta y Melilla se excluyen del análisis. Se aplican los mismos 4 supuestos clásicos y el test de Moran que en la pestaña anterior."
            )),
            uiOutput("p32_filtro_info")
          ),
          div(class = "regression-dashboards",
              layout_columns(
                col_widths = c(4, 4, 4),
                value_box(
                  title = "R²",
                  value = textOutput("p32_r2"),
                  showcase = bsicons::bs_icon("graph-up"),
                  theme = "primary"
                ),
                value_box(
                  title = "4 supuestos",
                  value = textOutput("p32_val"),
                  showcase = bsicons::bs_icon("check-circle"),
                  theme = "success"
                ),
                value_box(
                  title = "Validez espacial",
                  value = textOutput("p32_esp"),
                  showcase = bsicons::bs_icon("geo-alt"),
                  theme = "info"
                )
              )
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Cruza las <b>tasas por causa</b> entre sí para el año y sexo elegidos: si dos causas suben y bajan juntas por provincias, comparten patrón territorial (no causalidad). Se aplican los mismos 4 supuestos y el test de Moran que en el análisis anterior.</p>"))
          ),
          bslib::card(
            card_header("Validez clásica y espacial de los modelos entre causas"),
            card_body(
              div(class = "table-responsive", tableOutput("p32_modelos_info")),
              div(class = "filter-help", HTML(paste0(
                "<div><b>Lectura:</b></div>",
                "<div>cada fila representa la causa explicativa (Y) y cada columna la causa predictora (X), para el año, sexo y escala seleccionados.</div>",
                "<div><b>✓</b> = cumple los 4 supuestos clásicos.</div>",
                "<div><b>●</b> = cumple la validez espacial (Moran, p &gt; 0,05), aunque no cumpla los 4 supuestos.</div>",
                "<div><b>✓ ●</b> = cumple ambos: supuestos clásicos y validez espacial.</div>",
                "<div>La diagonal no se puede utilizar porque X e Y serían la misma causa.</div>",
                "<div><b>Comparaciones múltiples:</b> con 156 contrastes al nivel 5 %, se esperan ~8 falsos positivos por azar; valora cada ✓ ● junto a su R².</div>")))
            )
          ),
          bslib::card(card_header("Dispersión, recta e intervalo de confianza"), card_body(plotlyOutput("p32_scatter", height = "390px"))),
          bslib::card(card_header("Resultados y comprobación de los 4 supuestos"), card_body(tableOutput("p32_supuestos"))),
          bslib::card(
            card_header("Autocorrelación espacial de los residuos (Moran)"),
            card_body(
              tableOutput("p32_moran_tabla"),
              div(class = "metric-note", "p > 0,05 indica que no hay evidencia de autocorrelación espacial significativa de los residuos."),
              textOutput("p32_moran_conclusion")
            )
          )
        )
      ),
      nav_panel(
        title = "Modelo europeo: efectos fijos",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("eur_reg_causa", "Causa:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) {
                          causas_europa_raw[ifelse(any(causas_europa_raw != "Total"), which(causas_europa_raw != "Total")[1], 1)]
                        } else NULL),
            selectInput("eur_reg_sexo", "Sexo:", choices = sexos_europa_es,
                        selected = if ("Ambos" %in% sexos_europa_raw) "Ambos" else sexos_europa_raw[1]),
            checkboxGroupInput("eur_reg_fe", "Efectos fijos:",
                               choices = c("País" = "pais", "Año" = "anio", "Causa" = "causa", "Sexo" = "sexo"),
                               selected = c("pais", "anio")),
            checkboxInput("eur_reg_log", "Log(tasa)", value = FALSE),
            div(class = "filter-help", HTML(
              "<b>Modelo:</b> tasa_100k ~ efectos fijos seleccionados. <b>Datos:</b> Eurostat 2018-2022, tasas estandarizadas por 100k hab. <b>N obs:</b> ~9k. <b>Cluster:</b> errores robustos por país."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "R² ajustado", value = textOutput("eur_reg_r2"),
                      showcase = bsicons::bs_icon("graph-up"), theme = "primary"),
            value_box(title = "N observaciones", value = textOutput("eur_reg_n"),
                      showcase = bsicons::bs_icon("database"), theme = "success"),
            value_box(title = "Países", value = textOutput("eur_reg_paises"),
                      showcase = bsicons::bs_icon("globe"), theme = "info")
          ),
          bslib::card(
            card_header("Resumen del modelo"),
            card_body(verbatimTextOutput("eur_reg_summary"))
          ),
          bslib::card(
            card_header("Coeficientes (efectos fijos)"),
            card_body(DT::DTOutput("eur_reg_coef"))
          ),
          bslib::card(
            card_header("Diagnósticos"),
            card_body(
              div(class = "row",
                div(class = "col-md-6",
                  card_header("Residuos vs ajustados"),
                  card_body(plotlyOutput("eur_reg_diag1", height = "350px"))
                ),
                div(class = "col-md-6",
                  card_header("QQ-plot residuos"),
                  card_body(plotlyOutput("eur_reg_diag2", height = "350px"))
                )
              )
            )
          ),
          bslib::card(
            card_header("Tendencias por país (predichos vs observados)"),
            card_body(plotlyOutput("eur_reg_trends", height = "500px"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("¿Difieren los países? (test F sobre tasas originales)"),
                        card_body(h4(textOutput("eur_reg_veredicto")),
                                  HTML("<p class='text-muted mb-0'><small>H0: sin diferencias entre países controlando por año. Veredicto del modelo clásico absorbido de la antigua subpestaña.</small></p>"))),
            bslib::card(card_header("Distribución de tasas por país"),
                        card_body(plotlyOutput("eur_reg_boxplot", height = "500px")))
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Panel europeo con efectos fijos bidireccionales (país + año por defecto). Controla heterogeneidad no observada por país y tendencias temporales comunes. <b>Errores robustos clusterizados por país</b> (Arellano, 1987). <b>Log(tasa)</b> opcional para estabilizar varianza. No causalidad: correlaciones condicionadas a los efectos fijos.</p>"))
          )
        )
      ),
      nav_panel(
        title = "Multivariante (PCA + k-means)",
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuración",
        width = 290,
        selectInput("pm_ano", "Año:", choices = sort(unique(as.character(causas_provinciales$Año))),
                    selected = if ("2022" %in% as.character(causas_provinciales$Año)) "2022" else sort(unique(as.character(causas_provinciales$Año)))[1]),
        checkboxInput("pm_animar", "Animar años", value = FALSE),
        radioButtons("pm_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
        sliderInput("pm_k", "Nº de clusters (k-means):", min = 2, max = 6, value = 3, step = 1),
        div(class = "filter-help", HTML(
          "<b>Nota:</b> PCA sobre tasas por 100.000 (centradas y escaladas; sin la causa Total ni causas sin variación entre provincias). K-means sobre PC1-PC2 con semilla fija. El codo y la silueta ayudan a elegir k."
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
            "<p><b>Codo y silueta:</b> el codo muestra la dispersión interna según k (el punto donde deja de bajar mucho sugiere el k); la línea verde marca el k con mejor silueta media.</p>",
            "<p><b>Mapa de calor log2:</b> perfil de cada cluster frente a la media nacional. Rojo = tasa del cluster por encima, azul = por debajo.</p>",
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
            div(class = "card-header", "Codo y silueta (elección de k)"),
            div(class = "card-body p-2", plotlyOutput("pm_codo", height = "450px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Perfil de cada cluster frente a la media nacional (log2)"),
            div(class = "card-body p-2", plotlyOutput("pm_perfiles", height = "520px"))
        ),
        div(class = "card mb-4",
            div(class = "card-header", "Provincia → cluster"),
            div(class = "card-body p-2", DT::DTOutput("pm_tabla"))
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
    )
  )
