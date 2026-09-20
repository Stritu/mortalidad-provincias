  
  # PESTAÑA 3: MÉTRICAS DEMOGRÁFICAS
ui_metricas <- nav_panel(
    title = "Métricas demográficas",
    navset_tab(
      nav_panel(
        title = "Análisis demográfico",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros demográficos",
            width = 280,
            selectInput("p3_ano", "Año:", choices = c("2018", "2019", "2020", "2021", "2022"), selected = "2020"),
            selectInput("p3_funcion", "Métrica Demográfica:", choices = funciones_deseadas, selected = funciones_deseadas[1]),
            selectInput("p3_edad", "Tramo de Edad (Mapa y Tabla):", choices = c("Todos los tramos", lista_edades), selected = "Todos los tramos"),
            uiOutput("p3_filtro_info"),
            radioButtons("p3_sexo", "Sexo (Mapa y Tabla):", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Media entre provincias", value = textOutput("p3_kpi_media"), showcase = bsicons::bs_icon("bar-chart-line"), theme = "primary"),
            value_box(title = "Provincia con mayor valor", value = textOutput("p3_kpi_max"), showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Provincia con menor valor", value = textOutput("p3_kpi_min"), showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          
          uiOutput("p3_cuadro_informativo_dinamico"),
          
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              class = "equal-height-card",
              card_header("Mapa de la Métrica Seleccionada"), 
              card_body(padding = 0, leafletOutput("p3_mapa", height = "420px"))
            ),
            bslib::card(
              class = "equal-height-card",
              card_header("Indicador por Tramo de Edad y Sexo"),
              card_body(plotlyOutput("p3_piramide", height = "420px"))
            )
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header(
                div(
                  class = "d-flex justify-content-between align-items-center",
                   span("Resumen estadístico y posición relativa (ordenado de mayor a menor valor)"),
                  div(
                    actionButton("p3_prev_page", "<", class = "btn-sm btn-outline-secondary me-1"),
                    uiOutput("p3_page_info", inline = TRUE),
                    actionButton("p3_next_page", ">", class = "btn-sm btn-outline-secondary ms-1")
                  )
                )
              ),
              card_body(tableOutput("p3_tabla_ranking"))
            )
          )
        )
      ),
      nav_panel(
        title = "Comparador demográfico",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("p31_prov_a", "Provincia:", choices = lista_provincias,
                        selected = if ("Vizcaya" %in% lista_provincias) "Vizcaya" else lista_provincias[1]),
            selectInput("p31_prov_b", "Provincia:", choices = lista_provincias,
                        selected = if ("Asturias" %in% lista_provincias) "Asturias" else lista_provincias[min(2, length(lista_provincias))]),
            selectInput("p31_funcion", "Métrica Demográfica:", choices = funciones_deseadas, selected = funciones_deseadas[1]),
            selectInput("p31_ano_radar", "Año de población:",
                        choices = sort(unique(copia_p$Año), decreasing = TRUE),
                        selected = if ("2022" %in% copia_p$Año) "2022" else sort(unique(copia_p$Año), decreasing = TRUE)[1]),
            uiOutput("p31_filtro_info"),
            radioButtons("p31_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(6, 6),
            value_box(
              title = textOutput("p31_poblacion_titulo_a"),
              value = textOutput("p31_poblacion_a"),
              showcase = bsicons::bs_icon("people"),
              theme = "primary"
            ),
            value_box(
              title = textOutput("p31_poblacion_titulo_b"),
              value = textOutput("p31_poblacion_b"),
              showcase = bsicons::bs_icon("people"),
              theme = "info"
            )
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Enfrenta <b>dos provincias</b> con la <b>media entre provincias</b>: la evolución muestra si mejoran o empeoran respecto al conjunto, y la pirámide compara su estructura por tramos de edad en el sexo elegido.</p>"))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Evolución Temporal Comparada con la Media entre Provincias (2018-2022)"),
              card_body(plotlyOutput("p31_evol_demografica", height = "350px"))
            )
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Pirámide poblacional comparativa"),
              card_body(plotlyOutput("p31_piramide_comparativa", height = "480px"))
            )
          )
        )
      ),
      nav_panel(
        title = "Pirámide de población",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            if (is.null(padron_edad)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Añade <b>poblacion_edad_provincia.csv</b> para ver la pirámide de población real."
            )) else tagList(
              selectInput("demp_prov", "Provincia:", choices = c("Todas", sort(unique(padron_edad$Provincia))),
                          selected = "Todas"),
              selectInput("demp_ano", "Año:", choices = padron_anos,
                          selected = if ("2022" %in% padron_anos) "2022" else padron_anos[1])
            )
          ),
          if (is.null(padron_edad)) bslib::card(
            card_header("Sin datos"),
            card_body(HTML("<p>Añade <b>poblacion_edad_provincia.csv</b> a la carpeta y reinicia.</p>"))
          ) else tagList(
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>Pirámide con la <b>población censal real</b> (Padrón) por grupos quinquenales de edad. A diferencia de las pirámides de funciones o defunciones, aquí el tamaño de cada barra refleja cuánta gente vive en ese tramo.</p>"))
            ),
            bslib::card(
              card_header("Pirámide de población por edad y sexo"),
              card_body(plotlyOutput("demp_piramide", height = "560px"))
            )
          )
        )
      ),
      nav_panel(
        title = "Envejecimiento y dependencia",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            if (is.null(padron_edad)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Añade <b>poblacion_edad_provincia.csv</b>."
            )) else tagList(
              selectInput("deme_ano", "Año:", choices = padron_anos,
                          selected = if ("2022" %in% padron_anos) "2022" else padron_anos[1])
            )
          ),
          if (is.null(padron_edad)) bslib::card(
            card_header("Sin datos"),
            card_body(HTML("<p>Añade <b>poblacion_edad_provincia.csv</b> a la carpeta y reinicia.</p>"))
          ) else tagList(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(title = "% de 65 años o más (España)", value = textOutput("deme_kpi_65"),
                        showcase = bsicons::bs_icon("people"), theme = "primary"),
              value_box(title = "Índice de envejecimiento (España)", value = textOutput("deme_kpi_env"),
                        showcase = bsicons::bs_icon("bar-chart-line"), theme = "info"),
              value_box(title = "Tasa de dependencia (España)", value = textOutput("deme_kpi_dep"),
                        showcase = bsicons::bs_icon("people"), theme = "success")
            ),
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>El <b>% de 65+</b> mide el peso de la población mayor; el <b>índice de envejecimiento</b> compara mayores con jóvenes (65+ por cada 100 menores de 16); y la <b>tasa de dependencia</b> relaciona la población fuera de edad laboral (0-15 y 65+) con la de 16-64. Son los indicadores que explican gran parte de las diferencias de mortalidad bruta entre provincias.</p>"))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Mapa del % de 65 años o más"),
                          card_body(padding = 0, leafletOutput("deme_mapa", height = "430px"))),
              bslib::card(card_header("Ranking por % de 65+"),
                          card_body(plotlyOutput("deme_ranking", height = "430px")))
            ),
            bslib::card(card_header("Indicadores por provincia"),
                        card_body(DT::DTOutput("deme_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Brecha de género",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("demb_ano", "Año:", choices = sort(unique(as.character(copia_func$Año))),
                        selected = if ("2022" %in% as.character(copia_func$Año)) "2022" else sort(unique(as.character(copia_func$Año)))[1]),
            selectInput("demb_func", "Indicador:", choices = funciones_deseadas,
                        selected = if ("Esperanza de vida" %in% funciones_deseadas) "Esperanza de vida" else funciones_deseadas[1]),
            div(class = "filter-help", HTML(
              "Diferencia <b>Hombres − Mujeres</b> del indicador a la <b>edad 0</b> (al nacer). Valores negativos en esperanza de vida significan que las mujeres viven más."
            ))
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>La <b>brecha Hombres − Mujeres</b> resume la desigualdad de género del indicador al nacer, por provincia. En esperanza de vida lo habitual es una brecha negativa (ellas viven más años); el ranking ordena dónde esa diferencia es mayor o menor.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa de la brecha (Hombres − Mujeres)"),
                        card_body(padding = 0, leafletOutput("demb_mapa", height = "430px"))),
            bslib::card(card_header("Ranking de brecha"),
                        card_body(plotlyOutput("demb_ranking", height = "430px")))
          ),
          bslib::card(card_header("Tabla por provincia"),
                      card_body(DT::DTOutput("demb_tabla")))
        )
      ),
      nav_panel(
        title = "Evolución temporal",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("demv_func", "Indicador:", choices = funciones_deseadas,
                        selected = if ("Esperanza de vida" %in% funciones_deseadas) "Esperanza de vida" else funciones_deseadas[1]),
            radioButtons("demv_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
            div(class = "filter-help", HTML(
              "Media entre provincias por comunidad y año. Como en el resto de la pestaña, los valores promedian todos los tramos de edad."
            ))
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Evolución del indicador por <b>comunidad autónoma</b> entre 2018 y 2022 frente a la media entre provincias. Permite ver si las diferencias territoriales se amplían, se reducen o se mantienen en el tiempo.</p>"))
          ),
          bslib::card(card_header("Evolución por comunidad autónoma (2018-2022)"),
                      card_body(plotlyOutput("demv_evol", height = "430px"))),
          bslib::card(card_header("Tabla de valores"),
                      card_body(DT::DTOutput("demv_tabla")))
        )
      ),
      nav_panel(
        title = "Esperanza de vida",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("ev_ano", "Año:", choices = c("2018", "2019", "2020", "2021", "2022"),
                        selected = "2022"),
            selectInput("ev_edad", "Edad:", choices = ev_edades,
                        selected = if ("0" %in% ev_edades) "0" else ev_edades[1]),
            radioButtons("ev_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
            div(class = "filter-help", HTML(
              "Esperanza de vida <b>restante a la edad elegida</b> (años que viviría en promedio quien ya ha cumplido esa edad). A la edad 0 es la esperanza de vida al nacer."
            ))
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Media nacional", value = textOutput("ev_kpi_media"),
                      showcase = bsicons::bs_icon("heart-pulse"), theme = "success"),
            value_box(title = "Mayor esperanza", value = textOutput("ev_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "primary"),
            value_box(title = "Menor esperanza", value = textOutput("ev_kpi_min"),
                      showcase = bsicons::bs_icon("arrow-down-circle"), theme = "warning")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>La <b>esperanza de vida</b> resume la mortalidad de cada territorio en un solo número: cuántos años viviría en promedio una persona si las tasas por edad se mantuvieran. Sube cuando cae la mortalidad y cayó en 2020-2021 por la pandemia. Las mujeres viven más años en todas las provincias; la brecha se aprecia eligiendo sexo.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa por provincia (años)"),
                        card_body(padding = 0, leafletOutput("ev_mapa", height = "430px"))),
            bslib::card(card_header("Ranking por provincia"),
                        card_body(plotlyOutput("ev_ranking", height = "430px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Evolución por comunidad autónoma (2018-2022)"),
                        card_body(plotlyOutput("ev_evol", height = "430px"))),
            bslib::card(card_header("Tabla por provincia"),
                        card_body(DT::DTOutput("ev_tabla")))
          )
        )
      )
    )
  )
