# Tests del proyecto (sin dependencias extra).
# Uso desde la raíz:  Rscript tests/run.R
# Sale con estado 1 si alguna comprobación falla (útil en CI).
if (basename(getwd()) == "tests") setwd("..")

fails <- 0
# cond es promesa: se evalúa dentro del tryCatch y los errores no abortan.
check <- function(nombre, cond) {
  res <- tryCatch(list(ok = isTRUE(cond), msg = ""),
                  error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  if (!res$ok) fails <<- fails + 1
  cat(if (res$ok) "OK   " else "FALLO", nombre, "\n")
  if (!res$ok && nzchar(res$msg)) cat("       !", res$msg, "\n")
}

cerca <- function(x, valor, tol = 0.1) {
  length(x) == 1 && is.finite(x) && abs(x - valor) <= tol
}

t0 <- proc.time()
cat("== parse todos los R ==\n")
r_files <- c(list.files("R", pattern = "[.]R$", full.names = TRUE), "app.R")
for (f in r_files) {
  ok <- isTRUE(tryCatch({ invisible(parse(f, encoding = "UTF-8")); TRUE },
                        error = function(e) FALSE))
  if (!ok) fails <- fails + 1
  cat(if (ok) "OK   " else "FALLO", "parse", f, "\n")
}
invisible(capture.output(source("R/global.R")))
cat("arranque global:", round((proc.time() - t0)[["elapsed"]], 1), "s\n")

for (f in c("test_datos.R", "test_kpis.R", "test_modelos.R", "test_ui_server.R")) {
  cat("== ", f, " ==\n", sep = "")
  source(file.path("tests", f), encoding = "UTF-8")
}

cat(if (fails == 0) "TODOS LOS TESTS PASAN" else paste("FALLOS:", fails), "\n")
quit(status = if (fails == 0) 0 else 1)
