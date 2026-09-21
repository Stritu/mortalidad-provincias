# ============================================================================
# App de mortalidad (Espana + Europa): punto de entrada.
# Codigo modularizado por pestana:
#   R/global.R   librerias, utilidades, datos y caches
#   R/ui_*.R     una pestana de la interfaz (objetos ui_*)
#   R/server_*.R logica de cada pestana (funciones server_*())
# ============================================================================
orden_tabs <- c("resumen", "causas", "metricas", "europa", "determinantes", "regresiones", "exceso", "multivariante")
source("R/global.R", encoding = "UTF-8")
source("R/ui_resumen.R", encoding = "UTF-8")
source("R/ui_causas.R", encoding = "UTF-8")
source("R/ui_metricas.R", encoding = "UTF-8")
source("R/ui_europa.R", encoding = "UTF-8")
source("R/ui_determinantes.R", encoding = "UTF-8")
source("R/ui_regresiones.R", encoding = "UTF-8")
source("R/ui_exceso.R", encoding = "UTF-8")
source("R/ui_multivariante.R", encoding = "UTF-8")
source("R/server_resumen.R", encoding = "UTF-8")
source("R/server_causas.R", encoding = "UTF-8")
source("R/server_metricas.R", encoding = "UTF-8")
source("R/server_europa.R", encoding = "UTF-8")
source("R/server_determinantes.R", encoding = "UTF-8")
source("R/server_regresiones.R", encoding = "UTF-8")
source("R/server_exceso.R", encoding = "UTF-8")
source("R/server_multivariante.R", encoding = "UTF-8")


# ==============================================================================
# 3. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- page_navbar(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50",
    # OPT: pila de fuentes del sistema en lugar de font_google("Roboto").
    # font_google() descarga la fuente de Google Fonts al construir la UI
    # (petición de red en cada arranque); la pila local es indistinguible
    # en la práctica y funciona sin conexión.
    base_font = font_collection(
      "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"
    )
  ),
  title = "Análisis de defunciones",
  
  header = tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "theme-pro.css"),
    tags$style(HTML("
      body { background: #f5f7fa; }
      .card { margin-bottom: 16px !important; border-radius: 12px; }
      .sidebar { border-radius: 12px; }
      .filter-help {
        font-size: 0.86rem;
        line-height: 1.35;
        color: #5f6b76;
        background: #f8fafc;
        border-left: 3px solid #2c3e50;
        padding: 7px 10px;
        margin: -2px 0 10px 0;
        border-radius: 0 6px 6px 0;
      }
      .metric-note { font-size: 0.9rem; color: #5f6b76; margin: 4px 0 12px 0; }
      .value-box { border-radius: 12px; }
      .regression-dashboards .value-box { min-height: 92px; margin-bottom: 4px; }
      .regression-dashboards .value-box .value-box-value { font-size: 1.35rem; line-height: 1.1; }
      .regression-dashboards .value-box .value-box-title { font-size: 0.88rem; }

      /* Responsive layout for phones and small tablets */
      .card-body { min-width: 0; overflow-x: hidden; }
      .card-header { overflow-wrap: anywhere; }
      .selectize-control, .form-group, .shiny-input-container { max-width: 100%; }
      .plotly.html-widget { width: 100% !important; max-width: 100%; }
      .leaflet-container { width: 100% !important; max-width: 100%; }
      .dataTables_wrapper { width: 100%; overflow-x: auto; }
      /* Tablas clásicas (renderTable): scroll horizontal sin romper DT */
      .card-body table.table:not(.dataTable) { display: block; overflow-x: auto; max-width: 100%; -webkit-overflow-scrolling: touch; }
      .card-body table.table:not(.dataTable) th, .card-body table.table:not(.dataTable) td { white-space: nowrap; }
      /* Salida de texto de R sin desbordes */
      pre.shiny-text-output { white-space: pre-wrap; word-break: break-word; }
      /* Value boxes: el texto largo se ajusta en cualquier pestaña */
      .value-box .value-box-title { white-space: normal; overflow-wrap: anywhere; line-height: 1.25; }
      .value-box .value-box-value { white-space: normal; overflow-wrap: anywhere; line-height: 1.2; }
      .value-box .value-box-showcase { flex-shrink: 0; }
      /* Subpestañas: que nunca rompan el ancho */
      .nav-tabs { flex-wrap: wrap; }

      @media (max-width: 767.98px) {
        body { font-size: 0.95rem; }
        .navbar-brand { font-size: 1rem; white-space: normal; line-height: 1.15; }
        /* Navbar clásico sin hamburguesa: pestañas en una fila deslizable */
        .navbar-nav { flex-wrap: nowrap !important; overflow-x: auto; max-width: 100%; -webkit-overflow-scrolling: touch; }
        .navbar-nav > li { flex-shrink: 0; }
        .nav-tabs .nav-link { padding: 0.5rem 0.7rem; font-size: 0.9rem; }
        .leaflet.html-widget { height: 300px !important; }
        .bslib-sidebar-layout { --_sidebar-width: min(88vw, 320px); }
        .bslib-sidebar-layout > .sidebar { width: 100% !important; max-width: 100% !important; }
        .bslib-grid { grid-template-columns: minmax(0, 1fr) !important; }
        .card { margin-bottom: 12px !important; }
        .card-header { font-size: 0.98rem; padding: 0.65rem 0.8rem; }
        .card-body { padding: 0.75rem !important; }
        .value-box { min-height: 105px; }
        .plotly-graph-div { min-width: 0 !important; }
        .plotly .main-svg { max-width: 100%; }
      }

      @media (min-width: 768px) and (max-width: 1100px) {
        .bslib-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }

      /* Resumen general: texto completo sin scroll interno y cajas sin recortes */
      .resumen-intro { height: auto !important; min-height: 0 !important; }
      .resumen-intro .card-body { overflow: visible !important; max-height: none !important; }
      .resumen-dash .value-box { min-height: 130px; }
      .resumen-dash .value-box .value-box-title { font-size: 0.85rem; white-space: normal; overflow-wrap: anywhere; line-height: 1.25; }
      .resumen-dash .value-box .value-box-value { font-size: 1.2rem; line-height: 1.25; white-space: normal; overflow-wrap: anywhere; }
      .resumen-dash .value-box .value-box-showcase { flex-shrink: 0; }

      /* Exceso de mortalidad: KPIs compactos sin recortes */
      .exceso-dash .value-box { min-height: 118px; }
      .exceso-dash .value-box .value-box-title { font-size: 0.85rem; white-space: normal; overflow-wrap: anywhere; line-height: 1.25; }
      .exceso-dash .value-box .value-box-value { font-size: 1.45rem; line-height: 1.2; white-space: normal; overflow-wrap: anywhere; }
      .exceso-dash .value-box .value-box-showcase { flex-shrink: 0; }

      /* Desplegables: el input de búsqueda de selectize ocupa una línea propia
         bajo valores seleccionados largos (se ve como un hueco en blanco).
         Cerrado se saca del flujo; abierto sigue permitiendo filtrar. */
      .selectize-control.single .selectize-input > input { margin: 0 !important; }
      .selectize-control.single:not(.dropdown-active) .selectize-input > input {
        position: absolute !important;
        opacity: 0 !important;
        pointer-events: none !important;
        width: 0 !important;
        padding: 0 !important;
      }
    "))
  ),
  ui_resumen,
  ui_causas,
  ui_metricas,
  ui_europa,
  ui_determinantes,
  ui_regresiones,
  ui_exceso,
  ui_multivariante
)

# ==============================================================================
# 4. SERVIDOR (SERVER)
# ==============================================================================
  

server <- function(input, output, session) {
  server_resumen(input, output, session)
  server_causas(input, output, session)
  server_metricas(input, output, session)
  server_europa(input, output, session)
  server_determinantes(input, output, session)
  server_regresiones(input, output, session)
  server_exceso(input, output, session)
  server_multivariante(input, output, session)

  # Animación temporal: cada checkbox avanza su selector de año
  animar_anos(input, session, "det_animar", "det_ano", det_anos)
  animar_anos(input, session, "idx_animar", "idx_ano", det_anos)
  animar_anos(input, session, "eur_animar", "eur_anio", sort(unique(as.character(europa_agrupada$anio))))
  animar_anos(input, session, "eur_comp_animar", "eur_comp_anio", sort(unique(as.character(europa_agrupada$anio))))
  animar_anos(input, session, "ex_animar", "ex_ano", datos_edadprov$anos)
  animar_anos(input, session, "p2_animar", "p2_ano", sort(unique(copia_causas$Año)))
  animar_anos(input, session, "pmes_animar", "pmes_ano", meses_anios)
  animar_anos(input, session, "std_animar", "std_ano", datos_edad_std$anos)
  animar_anos(input, session, "ep_animar", "ep_ano", datos_edadprov$anos)
  animar_anos(input, session, "p3_animar", "p3_ano", c("2018", "2019", "2020", "2021", "2022"))
  animar_anos(input, session, "p31_animar", "p31_ano_radar", sort(unique(copia_p$Año)))
  animar_anos(input, session, "demp_animar", "demp_ano", padron_anos)
  animar_anos(input, session, "deme_animar", "deme_ano", padron_anos)
  animar_anos(input, session, "demb_animar", "demb_ano", sort(unique(as.character(copia_func$Año))))
  animar_anos(input, session, "pm_animar", "pm_ano", sort(unique(as.character(causas_provinciales$Año))))
  animar_anos(input, session, "p32_animar", "p32_ano", sort(unique(as.character(causas_provinciales$Año))))
}


# ==============================================================================
# 5. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
# Validaciones mínimas para fallar con un mensaje claro si faltan datos.
if (!nrow(copia_causas)) warning("No se han cargado datos de causas de defunción de España.")
if (!nrow(copia_func)) warning("No se han cargado datos de funciones demográficas.")
if (!nrow(europa_agrupada)) warning("No se han cargado datos europeos o no se han podido agrupar las causas.")

shinyApp(ui = ui, server = server)
