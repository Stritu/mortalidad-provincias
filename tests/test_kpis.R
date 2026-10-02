# KPIs de referencia 2022 (tolerancias ajustadas a redondeo).
c22 <- causas_provinciales[causas_provinciales$Año_Num == 2022, ]
tot <- sum(c22$Fallecidos)
pob <- sum(aggregate(Poblacion ~ Provincia + Sexo,
                     data = unique(c22[, c("Provincia", "Sexo", "Poblacion")]),
                     FUN = sum)$Poblacion)

check("2022: 459.515 fallecidos", tot == 459515)
check("2022: tasa 967,9", cerca(tot / pob * 1e5, 967.9, 0.05))

top_c <- aggregate(Fallecidos ~ Defunción, data = c22, FUN = sum)
top_c <- top_c[order(-top_c$Fallecidos), ]
check("2022: primera causa circulatorio con 121.341",
      top_c$Defunción[1] == "Enfermedades del sistema circulatorio" &&
        top_c$Fallecidos[1] == 121341)

t_prov <- aggregate(cbind(Fallecidos, Poblacion) ~ Provincia, data = c22,
                    FUN = function(v) sum(v))
# OJO: Poblacion viene repetida por causa; se corrige dividiendo por nº de causas.
n_c <- length(unique(c22$Defunción))
t_prov$Tasa <- with(t_prov, Fallecidos / (Poblacion / n_c) * 1e5)
t_prov <- t_prov[order(-t_prov$Tasa), ]
check("2022: Lugo mayor tasa (1.652,5)",
      t_prov$Provincia[1] == "Lugo" && cerca(t_prov$Tasa[1], 1652.5, 0.1))

ev <- funciones_provinciales[funciones_provinciales$Funciones == "Esperanza de vida" &
  funciones_provinciales$Año == "2022" &
  funciones_provinciales$Sexo == "Ambos" &
  as.character(funciones_provinciales$Edad) == "0", ]
ev <- ev[order(-ev$Valor), ]
check("2022: Madrid mayor EV al nacer (84,8)",
      ev$Provincia[1] == "Madrid" && cerca(ev$Valor[1], 84.8, 0.05))

g <- gini_pond(t_prov$Tasa, t_prov$Poblacion / n_c)
check("2022: Gini entre provincias 0,102", cerca(g, 0.102, 0.002))

# % sensible con la función REAL de la app (no lista duplicada: si el mapeo
# cambia y el test sigue verde, el test no sirve).
sens <- sum(c22$Fallecidos[asignar_cesta(c22$Defunción) != "Resto"]) / tot * 100
check("2022: % sensible evitable 85,6", cerca(sens, 85.6, 0.1))

# Helpers del mapa único: mismos números que el cálculo directo.
h1 <- tasa_causa_prov("2022", "Ambos", "Todas")
check("helper tasa_causa_prov: 52 prov y 459.515", nrow(h1) == 52 &&
  sum(h1$Fallecidos) == 459515)
h2 <- brecha_prov("2022", "Ambos", "Tumores")
check("helper brecha_prov: 52 ratios finitos",
      nrow(h2) == 52 && all(is.finite(h2$Ratio)))
h3 <- tasa_sensible_prov("2022", "Ambos")
check("helper tasa_sensible_prov: 52 y media 85,6",
      nrow(h3) == 52 && cerca(sum(h3$Fall_Sens) / sum(h3$Fall_Tot) * 100, 85.6, 0.1))
h4 <- tasa_std_prov("2022", "Ambos")
check("helper tasa_std_prov: 52 con bruta y std finitas",
      nrow(h4) == 52 && all(is.finite(h4$Tasa_std)) && all(is.finite(h4$Tasa_bruta)))
