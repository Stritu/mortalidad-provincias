
  # PESTAÑA 5BIS: DETERMINANTES SOCIOECONÓMICOS (RENTA Y MÉDICOS)
ui_determinantes <- nav_panel(
    title = "Determinantes",
    navset_tab(
      nav_panel(
        title = "Renta y médicos",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("det_ind", "Indicador:", choices = det_indicadores,
                        selected = if ("Renta neta media por persona" %in% det_indicadores) "Renta neta media por persona" else det_indicadores[1]),
            selectInput("det_ano", "Año:", choices = det_anos,
                        selected = if ("2022" %in% det_anos) "2022" else det_anos[1]),
            div(class = "filter-help", HTML(
              "<b>Nota:</b> la renta es provincial (se agrega a CCAA ponderando por población); los médicos vienen por CCAA (colegiados no jubilados por 100.000 hab.). Navarra y País Vasco sin renta 2018-2020."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Media nacional", value = textOutput("det_kpi_media"),
                      showcase = bsicons::bs_icon("bar-chart-line"), theme = "primary"),
            value_box(title = "Mayor valor", value = textOutput("det_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Menor valor", value = textOutput("det_kpi_min"),
                      showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>La <b>renta</b> (media por persona y hogar) y los <b>médicos colegiados no jubilados</b> describen el contexto socioeconómico y sanitario por comunidad. La renta provincial se agrega a CCAA <b>ponderando por población</b>; los médicos vienen directos por CCAA. Sin dato donde la fuente no publica (Navarra 2018-2020).</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa por comunidad autónoma"),
                        card_body(padding = 0, leafletOutput("det_mapa", height = "430px"))),
            bslib::card(card_header("Evolución por comunidad (2018-2022)"),
                        card_body(plotlyOutput("det_evol", height = "430px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Ranking por comunidad"),
                        card_body(plotlyOutput("det_ranking", height = "430px"))),
            bslib::card(card_header("Tabla por comunidad"),
                        card_body(DT::DTOutput("det_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Índice sintético",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            selectInput("idx_ano", "Año:", choices = det_anos,
                        selected = if ("2022" %in% det_anos) "2022" else det_anos[1]),
            sliderInput("idx_wr", "Peso renta:", min = 0, max = 100, value = 34),
            sliderInput("idx_wm", "Peso médicos:", min = 0, max = 100, value = 33),
            sliderInput("idx_wmo", "Peso mortalidad:", min = 0, max = 100, value = 33),
            div(class = "filter-help", HTML(
              "<b>Nota:</b> índice 0-100 (min-max 2018-2022; mortalidad invertida: menos mortalidad, más puntos). Sin valor si falta algún componente (Navarra 2018-2020). La mortalidad es tasa bruta: penaliza a las CCAA más envejecidas."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Comunidad líder", value = textOutput("idx_kpi_lider"),
                      showcase = bsicons::bs_icon("trophy"), theme = "success"),
            value_box(title = "Índice medio nacional", value = textOutput("idx_kpi_media"),
                      showcase = bsicons::bs_icon("bar-chart-line"), theme = "primary"),
            value_box(title = "Brecha máx-mín", value = textOutput("idx_kpi_brecha"),
                      showcase = bsicons::bs_icon("arrows-expand"), theme = "warning")
          ),
          bslib::card(
            card_header("Metodología"),
            card_body(HTML("<p>Cada componente se normaliza a 0-100 con min-max <b>global 2018-2022</b> (la mortalidad, invertida). El índice es la <b>media ponderada</b> con los pesos del panel. Es descriptivo, no causal: conviene leerlo junto a las tasas estandarizadas por edad.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa del índice por comunidad"),
                        card_body(padding = 0, leafletOutput("idx_mapa", height = "430px"))),
            bslib::card(card_header("Ranking por comunidad"),
                        card_body(plotlyOutput("idx_ranking", height = "430px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Evolución del índice (2018-2022)"),
                        card_body(plotlyOutput("idx_evol", height = "430px"))),
            bslib::card(card_header("Tabla por comunidad"),
                        card_body(DT::DTOutput("idx_tabla")))
          )
        )
      )
    )
  )
