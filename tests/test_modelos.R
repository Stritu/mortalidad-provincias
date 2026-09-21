# Integridad de cachés de modelos + humo del panel europeo.
obj <- pm_cache[["2022||Ambos"]]
wss <- vapply(obj$kms, function(km) km$tot.withinss, numeric(1))
check("pm_cache: k=2..6 con WSS decreciente",
      length(wss) == 5 && all(diff(wss) < 0))
check("pm_cache: 52 provincias x 13 causas",
      nrow(obj$mat) == 52 && ncol(obj$mat) == 13)

eu <- europa_agrupada[europa_agrupada$causa_grupo == "Tumores" &
  europa_agrupada$sexo == "Ambos" &
  is.finite(europa_agrupada$tasa_100k) & europa_agrupada$tasa_100k > 0, ]
fit <- lm(tasa_100k ~ pais + anio, data = eu)
r2 <- summary(fit)$adj.r.squared
check("eur_reg Tumores: n=167, 35 países, R²>0,98",
      nrow(eu) == 167 && length(unique(eu$pais)) == 35 && r2 > 0.98)
