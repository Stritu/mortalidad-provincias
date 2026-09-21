# Tests del proyecto (sin dependencias extra).
# Uso desde la raíz:  Rscript tests/run.R
# Sale con estado 1 si alguna comprobación falla (útil en CI).
if (basename(getwd()) == "tests") setwd("..")

fails <- 0
check <- function(nombre, cond) {
  ok <- isTRUE(cond)
  if (!ok) fails <<- fails + 1
  cat(if (ok) "OK   " else "FALLO", nombre, "\n")
}

cerca <- function(x, valor, tol = 0.1) is.finite(x) && abs(x - valor) <= tol

t0 <- proc.time()
invisible(capture.output(source("R/global.R")))
cat("arranque global:", round((proc.time() - t0)[["elapsed"]], 1), "s\n")

for (f in c("test_datos.R", "test_kpis.R", "test_modelos.R")) {
  cat("== ", f, " ==\n", sep = "")
  source(file.path("tests", f), encoding = "UTF-8")
}

cat(if (fails == 0) "TODOS LOS TESTS PASAN" else paste("FALLOS:", fails), "\n")
quit(status = if (fails == 0) 0 else 1)
