  
  # PESTAÑA 4: EUROPA
ui_europa <- nav_panel(
    title = "Europa",
    navset_tab(
      nav_panel(
        title = "Panel europeo",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros europeos",
            width = 280,
            selectInput("eur_anio", "Año:", choices = as.character(sort(unique(europa_agrupada$anio), decreasing = TRUE)),
                        selected = as.character(max(europa_agrupada$anio, na.rm = TRUE))),
            selectInput("eur_causa", "Causa / grupo de defunción:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) {
                          causas_europa_raw[ifelse(any(causas_europa_raw != "Total"), which(causas_europa_raw != "Total")[1], 1)]
                        } else NULL),
            radioButtons("eur_sexo", "Sexo:",
                         choices = c("Ambos" = "Ambos", "Hombres" = "Hombres", "Mujeres" = "Mujeres"),
                         selected = if ("Ambos" %in% sexos_europa_raw) "Ambos" else sexos_europa_raw[1]),
            uiOutput("eur_filtro_info")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Total de defunciones", value = textOutput("eur_kpi_total"), showcase = bsicons::bs_icon("heartbreak"), theme = "danger"),
            value_box(title = "País con mayor tasa de mortalidad", value = textOutput("eur_kpi_max"), showcase = bsicons::bs_icon("flag"), theme = "primary"),
            value_box(title = "Sexo seleccionado", value = textOutput("eur_kpi_sexo"), showcase = bsicons::bs_icon("people"), theme = "info")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Compara la <b>mortalidad entre países europeos</b> con tasas por 100.000 habitantes (datos Eurostat): el mapa y la evolución muestran dónde y cómo cambia cada causa, y los rankings separan las 4 causas con mayor y menor tasa en <b>hombres y mujeres</b>.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header("Mapa europeo — tasa de defunción por 100.000 habitantes"),
              card_body(padding = 0, leafletOutput("eur_mapa", height = "430px"))
            ),
            bslib::card(
              card_header("Evolución temporal — 5 países más afectados, 5 menos afectados y media europea"),
              card_body(plotlyOutput("eur_plot_barras", height = "500px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header("Causas con mayor tasa por sexo"),
              card_body(plotlyOutput("eur_3_causas_top", height = "330px"))
            ),
            bslib::card(
              card_header("Causas con menor tasa por sexo"),
              card_body(plotlyOutput("eur_3_causas_bottom", height = "330px"))
            )
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(card_header("Detalle de los datos europeos — tasas de mortalidad estandarizadas"), card_body(DT::DTOutput("eur_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Comparador de países europeos",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("eur_comp_pais_a", "País:", choices = paises_europa_es,
                        selected = if ("Spain" %in% paises_europa_raw) "Spain" else paises_europa_raw[1]),
            selectInput("eur_comp_pais_b", "País:", choices = paises_europa_es,
                        selected = if ("France" %in% paises_europa_raw) "France" else if (length(paises_europa_raw) > 1) paises_europa_raw[2] else paises_europa_raw[1]),
            selectInput("eur_comp_causa", "Causa de Defunción:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) causas_europa_raw[1] else NULL),
            selectInput("eur_comp_anio", "Año de referencia:",
                        choices = as.character(sort(unique(europa_agrupada$anio), decreasing = TRUE)),
                        selected = if ("2022" %in% as.character(europa_agrupada$anio)) "2022" else as.character(max(europa_agrupada$anio, na.rm = TRUE))),
            radioButtons("eur_comp_sexo", "Sexo:",
                         choices = c("Ambos" = "Ambos", "Hombres" = "Hombres", "Mujeres" = "Mujeres"),
                         selected = if ("Ambos" %in% sexos_europa_raw) "Ambos" else sexos_europa_raw[1]),
            uiOutput("eur_comp_filtro_info")
          ),
          layout_columns(
            col_widths = c(6, 6),
            value_box(
              title = textOutput("eur_comp_poblacion_titulo_a"),
              value = textOutput("eur_comp_poblacion_a"),
              showcase = bsicons::bs_icon("people"),
              theme = "primary"
            ),
            value_box(
              title = textOutput("eur_comp_poblacion_titulo_b"),
              value = textOutput("eur_comp_poblacion_b"),
              showcase = bsicons::bs_icon("people"),
              theme = "info"
            )
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Enfrenta <b>dos países</b> con la <b>media europea</b>: la evolución muestra si avanzan mejor o peor que el conjunto, y los radares resumen su perfil por grupos de causas en el año elegido (arriba) y en el resto de años (abajo).</p>"))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Evolución Temporal Comparada con la Media Europea (2018-2022)"),
              card_body(plotlyOutput("eur_comp_evol_nacional", height = "350px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_title_a")),
              card_body(plotlyOutput("eur_comp_radar_a", height = "360px"))
            ),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_title_b")),
              card_body(plotlyOutput("eur_comp_radar_b", height = "360px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_bottom_title_a")),
              card_body(plotlyOutput("eur_comp_radar_a_bot", height = "360px"))
            ),
            bslib::card(
              card_header(uiOutput("eur_comp_radar_bottom_title_b")),
              card_body(plotlyOutput("eur_comp_radar_b_bot", height = "360px"))
            )
          )
        )
      ),
      nav_panel(
        title = "Comparación estadística europea",
        layout_sidebar(
          sidebar = sidebar(
            title = "Modelo europeo",
            width = 280,
            selectInput("eur_mod_causa", "Causa / grupo de defunción:", choices = causas_europa_es,
                        selected = if (length(causas_europa_raw)) {
                          idx <- if (any(causas_europa_raw != "Total")) which(causas_europa_raw != "Total")[1] else 1
                          causas_europa_raw[idx]
                        } else NULL),
            radioButtons("eur_mod_sexo", "Sexo:",
                         choices = c("Ambos" = "Ambos", "Hombres" = "Hombres", "Mujeres" = "Mujeres"),
                         selected = "Ambos"),
            selectInput("eur_mod_anio", "Años:",
                        choices = c("Todos los años", as.character(sort(unique(europa_agrupada$anio)))),
                        selected = "Todos los años"),
            div(class = "filter-help", HTML(
              "<b>Método:</b> se ajusta un modelo lineal de la tasa de defunciones por país, incluyendo el año para controlar las diferencias temporales. El efecto de <i>país</i> permite comprobar si existen diferencias estadísticamente significativas entre países."
            )),
            uiOutput("eur_mod_info")
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("R² del modelo europeo"), card_body(textOutput("eur_mod_r2"))),
            bslib::card(card_header("¿Existen diferencias entre países?"), card_body(textOutput("eur_mod_validez")))
          ),
          bslib::card(
            card_header("Interpretación del modelo"),
            card_body(HTML(
              paste0(
                "<p>Este análisis compara las <b>tasas de defunción entre países europeos</b> para la causa y sexo seleccionados. Cuando se incluyen varios años, el año se incorpora al modelo para separar, en la medida de lo posible, el efecto temporal del efecto asociado al país.</p>",
                "<p><b>Hipótesis del efecto país:</b> H<sub>0</sub>: las diferencias medias ajustadas por año entre países son nulas; H<sub>1</sub>: al menos un país presenta una diferencia respecto al resto.</p>",
                "<p>Con <b>p &lt; 0,05</b> se considera que existe evidencia estadísticamente significativa de diferencias entre países.</p>"
              )
            ))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Distribución de tasas por país"), card_body(plotlyOutput("eur_mod_boxplot", height = "640px"))),
            bslib::card(card_header("Resultados del modelo"), card_body(tableOutput("eur_mod_tabla")))
          ),
          bslib::card(card_header("Resumen estadístico del modelo"), card_body(verbatimTextOutput("eur_mod_summary")))
        )
      )
    )
  )
