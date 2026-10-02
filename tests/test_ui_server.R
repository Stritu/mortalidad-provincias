# Cruce UI <-> server: outputs declarados vs definidos, inputs usados vs creados.
# Caza outputs huérfanos (pantalla rota) e inputs inexistentes (filtros muertos).
ui_txt <- paste(unlist(lapply(list.files("R", pattern = "^ui_.*[.]R$", full.names = TRUE),
                              readLines, encoding = "UTF-8", warn = FALSE)), collapse = "\n")
server_txt <- paste(unlist(lapply(list.files("R", pattern = "^server_.*[.]R$", full.names = TRUE),
                                  readLines, encoding = "UTF-8", warn = FALSE)), collapse = "\n")

extrae_ids <- function(txto, patron) {
  m <- regmatches(txto, gregexpr(patron, txto, perl = TRUE))[[1]]
  unique(vapply(m, function(x) sub('".*$', "", sub('^.*\\("', "", x)), character(1)))
}

# Inputs implícitos del framework (DT, plotly): existen sin declararse en UI.
es_implicito <- function(id) {
  grepl("_(rows_selected|rows_all|rows_current|click|hover|brush|relayout|restyle|afterplot|search|page|order|selected|cell_clicked)$",
        id, perl = TRUE)
}

ids_ui_out <- extrae_ids(ui_txt, '(plotlyOutput|leafletOutput|DTOutput|tableOutput|verbatimTextOutput|textOutput|htmlOutput|uiOutput)\\("[A-Za-z0-9_.]+"')
ids_dl <- extrae_ids(ui_txt, 'downloadButton\\("[A-Za-z0-9_.]+"')
m_out <- regmatches(server_txt, gregexpr('output\\$[A-Za-z0-9_.]+\\s*<-', server_txt, perl = TRUE))[[1]]
ids_server_out <- unique(sub('\\s*<-.*$', "", sub('^output\\$', "", m_out)))
m_in <- regmatches(server_txt, gregexpr('input\\$[A-Za-z0-9_.]+', server_txt, perl = TRUE))[[1]]
ids_in_used <- unique(sub('^input\\$', "", m_in))

check("outputs UI definidos en server",
      { x <- setdiff(ids_ui_out, ids_server_out); length(x) == 0 || { cat("       !", paste(x, collapse = ", "), "\n"); FALSE } })
check("outputs server consumidos en UI",
      { x <- setdiff(ids_server_out, c(ids_ui_out, ids_dl)); length(x) == 0 || { cat("       !", paste(x, collapse = ", "), "\n"); FALSE } })
check("inputs usados existen en UI",
      {
        x <- ids_in_used[!vapply(ids_in_used, function(id) {
          es_implicito(id) || grepl(paste0('"', id, '"'), ui_txt, fixed = TRUE)
        }, logical(1))]
        length(x) == 0 || { cat("       !", paste(x, collapse = ", "), "\n"); FALSE }
      })
