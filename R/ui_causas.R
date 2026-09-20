
  # PESTAÑA 2: CAUSAS DE DEFUNCIÓN
ui_causas <- nav_panel(
    title = "Causas de defunción",
    navset_tab(
      nav_panel(
        title = "Análisis provincial",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros",
            width = 280,
            selectInput("p2_ano", "Año:", choices = sort(unique(copia_causas$Año)), selected = "2020"),
            selectInput("p2_defuncion", "Causa de Defunción:", choices = lista_defunciones, selected = lista_defunciones[1]),
            uiOutput("p2_filtro_info"),
            radioButtons("p2_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Tasa nacional de la causa", value = textOutput("p2_kpi_total"), showcase = bsicons::bs_icon("activity"), theme = "primary"),
            value_box(title = "Provincia más afectada", value = textOutput("p2_kpi_max"), showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Provincia menos afectada", value = textOutput("p2_kpi_min"), showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Las <b>tasas por 100.000 habitantes</b> permiten comparar provincias con distinta población. El mapa muestra la foto del año elegido; la evolución, a las 5 provincias con mayor y menor tasa junto a la media nacional; y los rankings, las 3 causas con mayor y menor tasa por sexo.</p>"))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Mapa de la tasa de la causa seleccionada (por 100k hab.)"), card_body(padding = 0, leafletOutput("p2_mapa", height = "350px"))),
            bslib::card(card_header("Evolución Temporal: Provincias Extremas y Media Nacional"), card_body(plotlyOutput("p2_evolucion_top_bot", height = "350px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header("Causas con mayor tasa por sexo"), card_body(plotlyOutput("p2_causas_mas_sexo", height = "300px"))),
            bslib::card(card_header("Causas con menor tasa por sexo"),
                                              card_body(plotlyOutput("p2_causas_menos_sexo", height = "300px")))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Tabla resumen provincial"),
              card_body(DT::DTOutput("p2_tabla_resumen"))
            )
          )
        )
      ),
      nav_panel(
        title = "Estacionalidad",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros mensuales",
            width = 290,
            selectInput("pmes_causa", "Causa:", choices = meses_causas,
                        selected = if(length(meses_causas)) meses_causas[1] else character(0)),
            selectInput("pmes_ano", "Año:", choices = c("Todos", meses_anios),
                        selected = if(length(meses_anios)) "Todos" else character(0)),
            radioButtons("pmes_sexo", "Sexo:", choices = meses_sexos, selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Defunciones del periodo", value = textOutput("pmes_kpi_total"),
                      showcase = bsicons::bs_icon("calendar-heart"), theme = "primary"),
            value_box(title = "Mes con mayor mortalidad", value = textOutput("pmes_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Mes con menor mortalidad", value = textOutput("pmes_kpi_min"),
                      showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success")
          ),
          bslib::card(
            card_header("Lectura"),
            card_body(HTML("<p>La estacionalidad permite detectar meses o periodos en los que una causa presenta un número de defunciones sistemáticamente mayor o menor. Con <b>Todos</b> se comparan los años disponibles en paralelo.</p>"))
          ),
          bslib::card(
            card_header("Evolución mensual"),
            card_body(plotlyOutput("pmes_evolucion", height = "430px"))
          )
        )
      ),
      nav_panel(
        title = "Evolución temporal",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros temporales",
            width = 290,
            selectInput("pt_causa", "Causa de defunción:", choices = lista_defunciones,
                        selected = if(length(lista_defunciones)) lista_defunciones[1] else NULL),
            radioButtons("pt_sexo", "Sexo:",
                         choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Defunciones 2018–2022", value = textOutput("pt_kpi_defunciones"),
                      showcase = bsicons::bs_icon("heart-pulse"), theme = "secondary"),
            value_box(title = "Mayor cambio 2018–2022", value = textOutput("pt_kpi_cambio"),
                      showcase = bsicons::bs_icon("graph-up-arrow"), theme = "info"),
            value_box(title = "Año con más defunciones", value = textOutput("pt_kpi_ano_max"),
                      showcase = bsicons::bs_icon("calendar-event"), theme = "primary")
          ),
          bslib::card(
            card_header("Evolución de la tasa de la causa por comunidad"),
            card_body(
              HTML("<p>La tasa de la causa se calcula como <b>defunciones / población × 100.000</b>. La gráfica permite seguir la evolución de cada comunidad entre 2018 y 2022 sin perder el detalle de los valores.</p>"),
              plotlyOutput("pt_evolucion_ccaa", height = "430px")
            )
          ),
          bslib::card(
            card_header("Variación interanual de la tasa de la causa"),
            card_body(
              HTML("<p>En lugar de superponer una línea por comunidad, esta visualización utiliza un <b>mapa de intensidad</b>: cada fila representa una comunidad y cada columna un año. El valor indica cuánto ha cambiado la tasa de la causa respecto al año anterior.</p>"),
              plotlyOutput("pt_variacion_interanual", height = "520px"),
              HTML("<p class='text-muted mb-2'><small>2019 compara con 2018; 2020 con 2019; 2021 con 2020; y 2022 con 2021. Los valores están expresados en puntos por 100.000 habitantes.</small></p>"),
              DT::DTOutput("pt_tabla_ccaa")
            )
          )
        )
      ),
      nav_panel(
        title = "Comparador de Provincias",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 280,
            selectInput("p21_prov_a", "Provincia:", choices = lista_provincias,
                        selected = if ("Vizcaya" %in% lista_provincias) "Vizcaya" else lista_provincias[1]),
            selectInput("p21_prov_b", "Provincia:", choices = lista_provincias,
                        selected = if ("Asturias" %in% lista_provincias) "Asturias" else lista_provincias[min(2, length(lista_provincias))]),
            selectInput("p21_defuncion", "Causa de Defunción:", choices = lista_defunciones, selected = lista_defunciones[1]),
            selectInput("p21_ano", "Año (para el radar):", choices = c("Todos los años", "2018", "2019", "2020", "2021", "2022"), selected = "2020"),
            uiOutput("p21_filtro_info")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Compara <b>dos provincias</b> frente a la <b>media nacional</b>: la evolución muestra si avanzan mejor o peor que el conjunto, y los radares resumen el perfil por grupos de causas en el año elegido (arriba) y en el resto de años (abajo).</p>"))
          ),
          layout_columns(
            col_widths = c(12),
            bslib::card(
              card_header("Evolución Temporal Comparada con la Media Nacional (2018-2022)"),
              card_body(plotlyOutput("p21_evol_nacional", height = "350px"))
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header(uiOutput("p21_radar_title_a")), card_body(plotlyOutput("p21_radar_a", height = "320px"))),
            bslib::card(card_header(uiOutput("p21_radar_title_b")), card_body(plotlyOutput("p21_radar_b", height = "320px")))
          ),
          layout_columns(
            col_widths = c(6, 6),
            bslib::card(card_header(uiOutput("p21_radar_bottom_title_a")), card_body(plotlyOutput("p21_radar_a_bot", height = "320px"))),
            bslib::card(card_header(uiOutput("p21_radar_bottom_title_b")), card_body(plotlyOutput("p21_radar_b_bot", height = "320px")))
          )
        )
      ),
      nav_panel(
        title = "Mortalidad prematura (APVP)",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filtros APVP",
            width = 290,
            selectInput("papvp_causa", "Causa:", choices = apvp_causas,
                        selected = if(length(apvp_causas)) apvp_causas[1] else character(0)),
            selectInput("papvp_comunidad", "Comunidad:", choices = c("Todas", apvp_comunidades),
                        selected = "Todas"),
            selectInput("papvp_indicador", "Indicador:", choices = apvp_indicadores,
                        selected = if(length(apvp_indicadores)) {
                          if("Nº de APVP" %in% apvp_indicadores) "Nº de APVP" else apvp_indicadores[1]
                        } else character(0)),
            radioButtons("papvp_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos")
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "APVP / indicador", value = textOutput("papvp_kpi_total"),
                      showcase = bsicons::bs_icon("hourglass-split"), theme = "primary"),
            value_box(title = "Mayor valor", value = textOutput("papvp_kpi_max"),
                      showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
            value_box(title = "Media", value = textOutput("papvp_kpi_media"),
                      showcase = bsicons::bs_icon("bar-chart"), theme = "success")
          ),
          bslib::card(
            card_header("Interpretación"),
            card_body(HTML("<p>Los <b>años potenciales de vida perdidos (APVP)</b> cuantifican el impacto de las defunciones ocurridas a edades relativamente tempranas. Además del número de APVP, el dataset permite consultar su tasa estandarizada y el número medio de APVP.</p>"))
          ),
          bslib::card(
            card_header("Distribución por comunidad autónoma"),
            card_body(plotlyOutput("papvp_ranking", height = "430px"))
          )
        )
      ),
      nav_panel(
        title = "Tasas estandarizadas por edad",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            if (is.null(datos_edad_std)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Esta pestaña se activa al añadir a la carpeta los ficheros <b>defunciones_edad_provincia.csv</b> y <b>poblacion_edad_provincia.csv</b> (formato detallado en el panel principal)."
            )) else tagList(
              selectInput("std_ano", "Año:", choices = datos_edad_std$anos,
                          selected = datos_edad_std$anos[1]),
              radioButtons("std_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
              div(class = "filter-help", HTML(
                "Tasa de <b>mortalidad total</b>: el fichero no trae desglose por causa."
              ))
            )
          ),
          if (is.null(datos_edad_std)) bslib::card(
            card_header("Dataset necesario: defunciones y población por edad y provincia"),
            card_body(HTML(paste0(
              "<p>Las <b>tasas estandarizadas por edad</b> eliminan el efecto de la estructura de edad al comparar provincias, usando como referencia la <b>Población Estándar Europea 2013</b>.</p>",
              "<p>Para activar esta pestaña, añade a la carpeta de la aplicación estos dos ficheros (separador <b>;</b>, latin1 o UTF-8, con fila de cabecera; se admiten filas de metadatos previas):</p>",
              "<ul><li><b>defunciones_edad_provincia.csv</b>: columnas <b>Provincia; Sexo; Año; Edad; Defunciones</b> y opcionalmente <b>Causa</b>.</li>",
              "<li><b>poblacion_edad_provincia.csv</b>: columnas <b>Provincia; Sexo; Año; Edad; Poblacion</b>.</li></ul>",
              "<p>La edad puede venir en tramos quinquenales (<i>De 5 a 9</i>), años simples (<i>80</i>) o intervalos (<i>80-84</i>). Las filas de totales por sexo se ignoran: <b>Ambos</b> se calcula sumando.</p>",
              "<p>Fuente sugerida: <b>INE</b> (defunciones por causa, sexo, edad y provincia; Padrón/Cifras de población por edad, provincia y sexo).</p>"
            )))
          ) else tagList(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(title = "Mayor tasa estandarizada", value = textOutput("std_kpi_max"),
                        showcase = bsicons::bs_icon("arrow-up-circle"), theme = "danger"),
              value_box(title = "Menor tasa estandarizada", value = textOutput("std_kpi_min"),
                        showcase = bsicons::bs_icon("arrow-down-circle"), theme = "success"),
              value_box(title = "Mayor cambio de ranking", value = textOutput("std_kpi_rank"),
                        showcase = bsicons::bs_icon("arrow-left-right"), theme = "primary")
            ),
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>La <b>tasa bruta</b> mezcla riesgo y estructura de edad; la <b>estandarizada</b> repondera cada tramo con la Población Estándar Europea 2013 y permite comparar provincias en igualdad de condiciones. La diagonal del gráfico marca la igualdad: las provincias alejadas cambian mucho de puesto al eliminar el efecto de la edad.</p>"))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Mapa de la tasa estandarizada (ESP 2013, por 100k hab.)"),
                          card_body(padding = 0, leafletOutput("std_mapa", height = "430px"))),
              bslib::card(card_header("Bruta frente a estandarizada"),
                          card_body(plotlyOutput("std_scatter", height = "430px")))
            ),
            bslib::card(card_header("Tabla por provincia (bruta, estandarizada y puestos)"),
                        card_body(DT::DTOutput("std_tabla")))
          )
        )
      ),
      nav_panel(
        title = "Edad y mes",
        layout_sidebar(
          sidebar = sidebar(
            title = "Configuración",
            width = 290,
            if (is.null(datos_edadprov)) div(class = "filter-help", HTML(
              "<b>Dataset pendiente.</b> Esta pestaña se activa al añadir <b>defunciones_edad_provincia.csv</b> (ver formato en Tasas estandarizadas)."
            )) else tagList(
              selectInput("ep_prov", "Provincia:", choices = c("Todas", datos_edadprov$provincias),
                          selected = "Todas"),
              selectInput("ep_ano", "Año:", choices = datos_edadprov$anos,
                          selected = if ("2022" %in% datos_edadprov$anos) "2022" else datos_edadprov$anos[1])
            )
          ),
          if (is.null(datos_edadprov)) bslib::card(
            card_header("Sin datos"),
            card_body(HTML("<p>Añade <b>defunciones_edad_provincia.csv</b> a la carpeta y reinicia para ver pirámides por edad simple, mediana de fallecimiento y estacionalidad por provincia (2009-2024).</p>"))
          ) else tagList(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(title = "Edad mediana de fallecimiento", value = textOutput("ep_kpi_mediana"),
                        showcase = bsicons::bs_icon("person-standing"), theme = "primary"),
              value_box(title = "% defunciones con 65 años o más", value = textOutput("ep_kpi_65"),
                        showcase = bsicons::bs_icon("people"), theme = "info"),
              value_box(title = "Mes pico", value = textOutput("ep_kpi_mes"),
                        showcase = bsicons::bs_icon("calendar-event"), theme = "success")
            ),
            bslib::card(
              card_header("Interpretación"),
              card_body(HTML("<p>La <b>pirámide</b> usa edades simples agregadas en quinquenios; la <b>mediana</b> resume en qué edad se concentra la mitad de las defunciones. La <b>estacionalidad mensual</b> suele mostrar picos invernales. La serie anual cubre desde 2009 e incluye el exceso de 2020-2021.</p>"))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Pirámide por edad simple (ambos sexos)"),
                          card_body(plotlyOutput("ep_piramide", height = "520px"))),
              bslib::card(card_header("Defunciones por mes"),
                          card_body(plotlyOutput("ep_meses", height = "520px")))
            ),
            layout_columns(
              col_widths = c(6, 6),
              bslib::card(card_header("Serie anual de defunciones"),
                          card_body(plotlyOutput("ep_evol", height = "350px"))),
              bslib::card(card_header("Totales anuales"),
                          card_body(DT::DTOutput("ep_tabla")))
            )
          )
        )
      )
    )
  )
