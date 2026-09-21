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

sens <- sum(c22$Fallecidos[!c22$Defunción %in% c(
  "Trastornos mentales y del comportamiento",
  "Enfermedades del sistema nervioso y de los órganos de los sentidos",
  "Enfermedades del sistema osteomuscular y del tejido conjuntivo",
  "Malformaciones congénitas, deformidades y anomalías cromosómicas",
  "Síntomas, signos y hallazgos anormales clínicos y de laboratorio, no clasificados en otra parte")]) / tot * 100
check("2022: % sensible evitable 85,6", cerca(sens, 85.6, 0.1))
