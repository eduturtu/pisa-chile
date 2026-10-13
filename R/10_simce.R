# =============================================================================
# 10_simce.R
# Validación con SIMCE II medio (censal, por establecimiento), 2016-2025.
# Fuente: bases públicas por RBD de la Agencia de Calidad de la Educación,
# descargadas de informacionestadistica.agenciaeducacion.cl el 8-10-2026.
# Archivos en data/raw/simce/rbd/simce2m<año>_rbd.csv (y sus glosas).
#
# Diseño:
# - Grupos fijados con datos de 2018 (antes de la ventana 2018-2022):
#   dependencia y grupo socioeconómico (GSE) del establecimiento. Fijar el
#   grupo en un año previo evita que el cambio de clasificación se confunda
#   con el cambio de puntaje. Agrupar por GSE, y no por puntaje, evita la
#   regresión a la media mecánica.
# - Panel balanceado por transición: solo colegios con puntaje en ambos años.
# - Cada colegio pondera por el promedio de alumnos evaluados en los dos años.
# - Errores estándar robustos a nivel de colegio (variación entre colegios,
#   no muestral: SIMCE es censal).
# - 2016-2018 sirve como tendencia previa (placebo).
#
# Códigos (glosas): cod_depe2 1 municipal, 2 particular subvencionado,
# 3 particular pagado, 4 SLEP (se agrupa con municipal como "publico").
# cod_grupo 1 bajo, 2 medio bajo, 3 medio, 4 medio alto, 5 alto.
# En 2016 y 2017 ambos vienen como texto.
# =============================================================================

SIMCE_DIR   <- file.path(PATHS$raw, "simce", "rbd")
SIMCE_YEARS <- c(2016, 2017, 2018, 2022, 2023, 2024, 2025)
BASE_YEAR   <- 2018
SIMCE_TRANS <- list(c(2016, 2018), c(2018, 2022), c(2022, 2025), c(2018, 2025))

dep_txt <- c("Municipal" = 1L, "Particular subvencionado" = 2L, "Particular pagado" = 3L)
gse_txt <- c("Bajo" = 1L, "Medio bajo" = 2L, "Medio" = 3L, "Medio alto" = 4L, "Alto" = 5L)

read_simce <- function(y) {
  f <- file.path(SIMCE_DIR, sprintf("simce2m%d_rbd.csv", y))
  d <- fread(f, encoding = "UTF-8", showProgress = FALSE)
  dep <- d$cod_depe2; gse <- d$cod_grupo
  if (is.character(dep)) dep <- dep_txt[trimws(dep)]
  if (is.character(gse)) gse <- gse_txt[trimws(gse)]
  out <- data.table(year = y, rbd = d$rbd, dep = as.integer(dep), gse = as.integer(gse),
                    n_mate = as.numeric(d$nalu_mate2m_rbd), mate = as.numeric(d$prom_mate2m_rbd),
                    n_lect = as.numeric(d$nalu_lect2m_rbd), lect = as.numeric(d$prom_lect2m_rbd))
  out[dep == 4L, dep := 1L]
  out[, dep := c("publico", "subvencionado", "particular_pagado")[dep]]
  out[mate <= 0, mate := NA][lect <= 0, lect := NA]
  out
}

S <- rbindlist(lapply(SIMCE_YEARS, read_simce))
base <- S[year == BASE_YEAR, .(rbd, dep0 = dep, gse0 = gse)]
S <- merge(S, base, by = "rbd", all.x = TRUE)

# Grupo principal: particular pagado aparte; el resto por GSE de 2018.
S[, grupo := fcase(dep0 == "particular_pagado", "particular_pagado",
                   gse0 %in% 1:2, "gse_bajo_medio_bajo",
                   gse0 == 3L,    "gse_medio",
                   gse0 %in% 4:5, "gse_medio_alto_alto",
                   default = NA_character_)]

# --- Niveles: promedio ponderado por alumnos, por año y grupo -------------------
lev <- rbindlist(lapply(c("mate", "lect"), function(s) {
  n <- paste0("n_", s)
  S[!is.na(get(s)) & get(n) > 0,
    .(subject = s, mean = weighted.mean(get(s), get(n)), students = sum(get(n)), schools = .N),
    by = .(year, group = fifelse(is.na(grupo), "sin_grupo_2018", grupo))]
}))
nat <- rbindlist(lapply(c("mate", "lect"), function(s) {
  n <- paste0("n_", s)
  S[!is.na(get(s)) & get(n) > 0,
    .(subject = s, group = "nacional", mean = weighted.mean(get(s), get(n)),
      students = sum(get(n)), schools = .N), by = year]
}))
lev <- rbind(lev, nat)
setorder(lev, subject, group, year)
save_table(lev, "simce_levels.csv")

# --- Cambios en panel balanceado ------------------------------------------------
panel_change <- function(s, a, b, by = "grupo") {
  n <- paste0("n_", s)
  x <- S[year %in% c(a, b) & !is.na(get(s)) & get(n) > 0 & !is.na(get(by)),
         .(rbd, year, g = get(by), y = get(s), n = get(n))]
  w <- dcast(x, rbd + g ~ year, value.var = c("y", "n"))
  setnames(w, c("rbd", "g", "y1", "y2", "n1", "n2"))
  w <- w[!is.na(y1) & !is.na(y2)]
  w[, `:=`(dy = y2 - y1, wt = (n1 + n2) / 2)]
  w[, {
    m <- weighted.mean(dy, wt)
    .(schools = .N, change = m, se = sqrt(sum(wt^2 * (dy - m)^2)) / sum(wt))
  }, by = .(group = g)][, `:=`(subject = s, from = a, to = b)]
}

chg <- rbindlist(lapply(c("mate", "lect"), function(s)
  rbindlist(lapply(SIMCE_TRANS, function(tr) panel_change(s, tr[1], tr[2])))))

# Brecha: grupo alto (y pagado) menos grupo bajo, por transición.
gap <- chg[, {
  lo <- .SD[group == "gse_bajo_medio_bajo"]
  rbindlist(lapply(c("gse_medio", "gse_medio_alto_alto", "particular_pagado"), function(h) {
    hi <- .SD[group == h]
    data.table(group = paste0(h, "_menos_bajo"), schools = hi$schools + lo$schools,
               change = hi$change - lo$change, se = sqrt(hi$se^2 + lo$se^2))
  }))
}, by = .(subject, from, to)]
chg <- rbind(chg, gap, use.names = TRUE)
chg[, z := change / se]
setcolorder(chg, c("subject", "from", "to", "group"))
save_table(chg, "simce_changes.csv")

# Detalle por dependencia x GSE (2018), solo descriptivo.
det <- rbindlist(lapply(SIMCE_TRANS, function(tr) {
  S[, cell := fifelse(is.na(dep0) | is.na(gse0), NA_character_, paste(dep0, gse0, sep = "_gse"))]
  panel_change("mate", tr[1], tr[2], by = "cell")
}))
save_table(det, "simce_changes_dep_gse.csv")

r1 <- function(x) round(x, 1)
message("\nSIMCE II medio, matemáticas: cambio en panel balanceado (e.e.)")
print(dcast(chg[subject == "mate", .(group, tr = paste(from, to, sep = "-"),
                                    v = paste0(r1(change), " (", r1(se), ")"))],
            group ~ tr, value.var = "v"))
