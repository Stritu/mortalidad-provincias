
  # PESTAÑA 6BIS: EXCESO DE MORTALIDAD (OBSERVADO VS 2015-2019)
ui_exceso <- nav_panel(
    title = "Exceso de mortalidad",
    layout_sidebar(
      sidebar = sidebar(
        title = "Configuración",
        width = 290,
        if (is.null(datos_edadprov)) div(class = "filter-help", HTML(
          "<b>Dataset pendiente.</b> Añade <b>defunciones_edad_provincia.csv</b>."
        )) else tagList(
          selectInput("ex_prov", "Provincia:", choices = c("Todas", datos_edadprov$provincias),
                      selected = "Todas"),
          radioButtons("ex_sexo", "Sexo:", choices = c("Ambos", "Hombres", "Mujeres"), selected = "Ambos"),
          selectInput("ex_ano", "Año (mensual):", choices = datos_edadprov$anos,
                      selected = if ("2020" %in% datos_edadprov$anos) "2020" else datos_edadprov$anos[1]),
          checkboxInput("ex_animar", "Animar años", value = FALSE)
        )
      ),
      if (is.null(datos_edadprov)) bslib::card(
        card_header("Sin datos"),
        card_body(HTML("<p>Añade <b>defunciones_edad_provincia.csv</b> a la carpeta y reinicia.</p>"))
      ) else div(class = "exceso-dash",
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(title = "Exceso 2020", value = textOutput("ex_kpi_2020"),
                    showcase = bsicons::bs_icon("graph-up"), theme = "danger"),
          value_box(title = "Exceso 2021", value = textOutput("ex_kpi_2021"),
                    showcase = bsicons::bs_icon("bar-chart-line"), theme = "warning"),
          value_box(title = "Peor año", value = textOutput("ex_kpi_peor"),
                    showcase = bsicons::bs_icon("calendar-event"), theme = "primary")
        ),
        bslib::card(
          card_header("Interpretación"),
          card_body(HTML("<p>El <b>exceso</b> compara las defunciones observadas con las <b>esperadas</b> (media 2015-2019 del mismo territorio y sexo). El <b>P-score</b> lo expresa en porcentaje. No distingue causas: incluye COVID directo, indirecto y cambios de registro.</p>"))
        ),
        bslib::card(card_header("Serie anual: observadas frente a esperadas (media 2015-2019)"),
                    card_body(plotlyOutput("ex_evol", height = "620px"))),
        bslib::card(card_header("Defunciones por mes (año seleccionado frente a baseline)"),
                    card_body(plotlyOutput("ex_meses", height = "560px"))),
        bslib::card(card_header("Tabla anual"),
                    card_body(DT::DTOutput("ex_tabla")))
      )
    )
  )
