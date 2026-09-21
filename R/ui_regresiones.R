  
  
# PESTAÑA 5: REGRESIONES LINEALES
ui_regresiones <- nav_panel(
    title = "Regresiones lineales",
    navset_tab(
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
      )
    )
  )

