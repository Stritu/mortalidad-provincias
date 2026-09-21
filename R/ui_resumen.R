  
  # PESTAÑA 1: RESUMEN GENERAL
ui_resumen <- nav_panel(
    title = "Resumen general",
    bslib::card(
      card_body(
        HTML("<h3 class='mt-1'>Mortalidad en España y Europa</h3><p class='text-muted mb-2'>Defunciones 2018–2022 por causa, provincia, sexo y edad · Exceso 2009–2024 · Comparación europea · Fuentes: INE y Eurostat.</p>"),
        div(class = "d-flex flex-wrap gap-2",
            actionButton("por_ir_causas", "Causas", icon = icon("lungs"), class = "btn-outline-primary"),
            actionButton("por_ir_europa", "Europa", icon = icon("flag"), class = "btn-outline-primary"),
            actionButton("por_ir_exceso", "Exceso", icon = icon("chart-line"), class = "btn-outline-primary"),
            actionButton("por_ir_determinantes", "Determinantes", icon = icon("coins"), class = "btn-outline-primary"),
            actionButton("por_ir_multi", "Multivariante", icon = icon("diagram-project"), class = "btn-outline-primary"))
      )
    ),
    # Div Bootstrap plano (sin bslib::card): crece con el contenido y muestra
    # todo el texto sin scroll interno. Las gráficas quedan debajo.
    div(
      class = "card resumen-intro",
      div(class = "card-header", "Introducción a la aplicación"),
        div(
          class = "card-body",
          div(class = "intro-app",
            h4("Índice: qué hay en cada pestaña"),
            p("Pulsa cualquier apartado para ir directamente a él. Las tasas son defunciones / población × 100.000 (2018–2022, salvo Exceso que cubre 2009–2024)."),
            h5("Causas de defunción"),
            p("Punto de partida: 13 capítulos de causa por provincia, sexo y año."),
            tags$ul(
              tags$li(actionLink("ir_c_prov", "Análisis provincial"), ": mapa, evolución de extremos y rankings por causa."),
              tags$li(actionLink("ir_c_est", "Estacionalidad"), ": patrón mensual por causa y sexo."),
              tags$li(actionLink("ir_c_evo", "Evolución temporal"), ": tasa por CCAA 2018–2022 y mapa de calor de variaciones."),
              tags$li(actionLink("ir_c_comp", "Comparador de Provincias"), ": dos provincias frente a la media nacional, con radares."),
              tags$li(actionLink("ir_c_apvp", "Mortalidad prematura (APVP)"), ": años potenciales de vida perdidos por CCAA (el INE no publica provincias)."),
              tags$li(actionLink("ir_c_std", "Tasas estandarizadas por edad"), ": ESP-2013; bruta frente a ajustada."),
              tags$li(actionLink("ir_c_edad", "Edad y mes"), ": pirámide por edad simple, mediana, mes pico y serie desde 2009."),
              tags$li(actionLink("ir_c_evit", "Mortalidad evitable"), ": cestas prevenible / tratable / mixto (aproximación sin límite <75)."),
              tags$li(actionLink("ir_c_des", "Desigualdad territorial"), ": Gini ponderado, P90/P10 y mapa de brechas."),
              tags$li(actionLink("ir_c_al", "Alertas de atípicos"), ": cambios bruscos y niveles extremos, con serie vinculada."),
              tags$li(actionLink("ir_c_cc", "Comparador de CCAA"), ": radar, brechas y evolución frente a la media nacional."),
              tags$li(actionLink("ir_c_inf", "Informes"), ": Excel por provincia o CCAA (Resumen, Por causa, Evolución).")
            ),
            h5("Métricas demográficas"),
            p("Población y longevidad por provincia y sexo."),
            tags$ul(
              tags$li(actionLink("ir_m_ana", "Análisis demográfico"), ": indicadores por provincia."),
              tags$li(actionLink("ir_m_comp", "Comparador demográfico"), ": dos provincias cara a cara."),
              tags$li(actionLink("ir_m_pir", "Pirámide de población"), ": estructura del Padrón."),
              tags$li(actionLink("ir_m_env", "Envejecimiento y dependencia"), ": % 65+, índice y tasa."),
              tags$li(actionLink("ir_m_bre", "Brecha de género"), ": diferencias H–M por indicador."),
              tags$li(actionLink("ir_m_evo", "Evolución demográfica"), ": indicadores por CCAA 2018–2022."),
              tags$li(actionLink("ir_m_ev", "Esperanza de vida"), ": por provincia, sexo y edad.")
            ),
            h5("Europa"),
            p("35 países (Eurostat), tasas por 100.000."),
            tags$ul(
              tags$li(actionLink("ir_e_mapa", "Mapa europeo"), ": foto por país, causa y año."),
              tags$li(actionLink("ir_e_comp", "Comparador de países europeos"), ": dos países frente a la media europea."),
              tags$li(actionLink("ir_e_ef", "Modelo europeo: efectos fijos"), ": tasa según país, año, causa y sexo, con errores cluster por país; incluye veredicto de diferencias entre países y boxplot.")
            ),
            h5("Determinantes"),
            p("Contexto socioeconómico y sanitario por CCAA (2018–2022)."),
            tags$ul(
              tags$li(actionLink("ir_d_renta", "Renta y médicos"), ": renta por persona y hogar, colegiados por 100k."),
              tags$li(actionLink("ir_d_idx", "Índice sintético"), ": renta + médicos + mortalidad en un 0–100 ponderable.")
            ),
            h5("Regresiones lineales"),
            p("Relaciones entre indicadores provinciales, con supuestos, Moran y matriz de validez."),
            tags$ul(
              tags$li(actionLink("ir_r_causas", "Regresión entre causas"), ": pares de tasas por 100k con diagnóstico completo.")
            ),
            h5("Exceso de mortalidad"),
            p("Observadas frente a esperadas según la media 2015–2019 (P-score), por año y por mes. ", actionLink("ir_ex", "Ir a la pestaña.")),
            h5("Análisis multivariante"),
            p("PCA del perfil de mortalidad + k-means con codo, silueta, heatmap y tabla. ", actionLink("ir_mult", "Ir a la pestaña."))
          )
        )
    ),
    div(
      class = "resumen-dash",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(title = "Tasa de mortalidad nacional 2022 (todas las causas)", value = textOutput("res_kpi_tasa"),
                  showcase = bsicons::bs_icon("activity"), theme = "primary"),
        value_box(title = "Causa con más defunciones (2022)", value = textOutput("res_kpi_causa"),
                  showcase = bsicons::bs_icon("exclamation-circle"), theme = "danger"),
        value_box(title = "Provincia con mayor tasa de mortalidad (2022)", value = textOutput("res_kpi_provincia"),
                  showcase = bsicons::bs_icon("geo-alt"), theme = "warning"),
        value_box(title = "Tasa de mortalidad de España en Europa (2022)", value = textOutput("res_kpi_europa"),
                  showcase = bsicons::bs_icon("flag"), theme = "info")
      ),
      div(class = "filter-help", HTML(
        "<b>Contexto:</b> indicadores de 2022; todas las causas y ambos sexos. Tasas de defunción por 100.000 habitantes. Los gráficos agregan todo el periodo disponible, salvo Europa que muestra 2022."
      )),
      bslib::card(
        card_header("Titulares (2022, calculados con los datos)"),
        card_body(htmlOutput("por_titulares"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(card_header("Top 8 causas por APVP"),
                    card_body(plotlyOutput("res_apvp", height = "380px"))),
        bslib::card(card_header("Top 10 países europeos por tasa de mortalidad"),
                    card_body(plotlyOutput("res_europa", height = "380px")))
      ),
      bslib::card(
        card_header("Pirámide de defunciones por edad y sexo"),
        card_body(
          plotlyOutput("ped_edad_sexo", height = "480px"),
          HTML("<p class='text-muted mb-0'><small>Defunciones agregadas de todo el periodo 2018-2022.</small></p>")
        )
      )
    )
  )
