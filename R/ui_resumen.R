  
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
        HTML(paste0(
          "<div class='intro-app'>",
          "<h4>¿Qué encontrarás en esta aplicación?</h4>",
          "<p>Esta aplicación ofrece una visión conjunta de la mortalidad en España y permite estudiar las defunciones desde diferentes perspectivas. ",
          "Los datos permiten analizar <b>qué causas de muerte son más frecuentes, dónde se concentran, cómo afectan según el sexo y la edad y cómo cambian a lo largo del tiempo y del año</b>. ",
          "También se incorpora una comparación con otros países europeos y un análisis estadístico de las relaciones entre variables territoriales.</p>",
          "<h5>Causas de defunción</h5>",
          "<p>Esta sección es el punto de partida para analizar las causas de muerte en España. El <b>análisis provincial</b> permite observar la distribución territorial mediante mapas, tasas y evolución temporal. ",
          "El <b>comparador de provincias</b> permite enfrentar dos provincias y estudiar sus diferencias en las causas seleccionadas. ",
          "La sección de <b>evolución temporal</b> resume cómo cambian las tasas de las comunidades autónomas entre 2018 y 2022 y permite estudiar sus variaciones interanuales. ",
          "Las <b>tasas estandarizadas por edad</b> permiten comparar provincias eliminando el efecto de la estructura de edad: reponderan las tasas de cada tramo con la <b>Población Estándar Europea 2013</b> (población ficticia de referencia de Eurostat), de modo que las diferencias restantes reflejan riesgo y no envejecimiento. Son cifras hipotéticas para comparar, no defunciones ocurridas. ",
          "La sección de <b>edad y mes</b> muestra pirámides por edad simple, la <b>mediana de fallecimiento</b> (edad que concentra la mitad de las defunciones), el <b>mes pico</b> y la serie anual por provincia desde 2009.</p>",
          "<h5>Estacionalidad</h5>",
          "<p>Analiza la distribución de las defunciones a lo largo de los meses. Permite comprobar si determinadas causas presentan comportamientos estacionales y comparar estos patrones entre <b>hombres y mujeres</b>.</p>",
          "<h5>Análisis demográfico</h5>",
          "<p>Estudia la mortalidad teniendo en cuenta la <b>edad y el sexo</b> de las personas fallecidas y la estructura de la población. Los gráficos permiten observar cómo se distribuyen las defunciones entre los distintos grupos de edad y comparar la situación de diferentes provincias. ",
          "Se añaden la <b>pirámide de población</b> real del Padrón, el <b>envejecimiento y la dependencia</b> por provincia, la <b>brecha de género</b> de los indicadores, su <b>evolución temporal</b> por comunidades y la <b>esperanza de vida</b> por provincia, sexo y edad.</p>",
          "<h5>Europa</h5>",
          "<p>Amplía el estudio al contexto europeo. Se pueden comparar países, seguir la evolución de sus tasas, analizar las diferencias por sexo y estudiar qué causas presentan los niveles de mortalidad más altos o más bajos. ",
          "Además, la <b>comparación estadística europea</b> permite evaluar si existen diferencias entre países teniendo en cuenta también el año.</p>",
          "<h5>Mortalidad por edad y sexo y mortalidad prematura</h5>",
          "<p>La distribución por <b>edad y sexo</b> se resume en esta misma pestaña. La <b>mortalidad prematura</b> se estudia dentro de <b>Causas de defunción</b> mediante los <b>años potenciales de vida perdidos (APVP)</b>, que cuantifican el impacto de las defunciones ocurridas antes de edades avanzadas.</p>",
          "<h5>Regresiones lineales</h5>",
          "<p>Esta sección busca estudiar si existe relación entre distintos indicadores de mortalidad a nivel provincial. ",
          "Los modelos comprueban sus principales supuestos estadísticos —linealidad, homoscedasticidad, incorrelación de los errores y normalidad de los residuos— y se complementan con el <b>test de Moran</b> para detectar posible dependencia espacial entre provincias. ",
          "La <b>matriz de validez</b> resume de un vistazo qué pares cumplen los supuestos (✓), la validez espacial (●) o ambos (✓ ●). ",
          "La <b>escala logarítmica</b> opcional suele estabilizar la varianza y rescatar supuestos; y cuando hay dependencia espacial, el <b>modelo de rezago espacial</b> (parámetro &rho;) es la alternativa adecuada, pues incorpora la influencia de las provincias vecinas. ",
          "La subpestaña de <b>regresión entre causas</b> aplica el mismo marco a las tasas de causas por 100.000 habitantes.</p>",
          "<h5>Determinantes e índice sintético</h5>",
          "<p>La renta por persona y hogar y los médicos colegiados por comunidad describen el contexto socioeconómico y sanitario por territorios, y el <b>índice sintético</b> los combina con la mortalidad en un solo indicador 0-100 por comunidad.</p>",
          "<h5>Exceso de mortalidad</h5>",
          "<p>Compara las defunciones observadas con las <b>esperadas</b> según la media 2015-2019 (P-score), por año y por mes, para cuantificar el impacto de la pandemia y de otros picos de mortalidad.</p>",
          "<h5>Análisis multivariante</h5>",
          "<p>Más allá de la regresión: el <b>PCA</b> resume el perfil de mortalidad de cada provincia en dos dimensiones y <b>k-means</b> agrupa las provincias parecidas, con mapa de clusters y matriz de correlaciones entre causas.</p>",
          "<h5>Cómo interpretar la aplicación</h5>",
          "<p>Las distintas pestañas se complementan entre sí: las causas ayudan a entender <b>qué ocurre</b>; los análisis provinciales, demográficos y europeos muestran <b>dónde y en quién</b>; la evolución temporal y la estacionalidad permiten estudiar <b>cuándo cambia</b>; y las regresiones permiten explorar <b>qué relaciones estadísticas pueden existir</b> entre los indicadores.</p>",
          "</div>"
        ))
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
