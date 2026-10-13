# =============================================================================
# 18_pisa_pantallas.R
# Pantallas, redes sociales, videojuegos y distracción en PISA, Chile.
#
# Comparabilidad (según 17_pisa_pantallas_explorar.R):
#   - 2015 y 2018: ENTUSE (índice de uso recreativo de TIC fuera del colegio).
#   - 2022 y 2025: ST326Q05JA (horas diarias de uso recreativo antes y después
#     del colegio), IC177Q01JA (videojuegos, día de semana), IC177Q02JA
#     (redes sociales, día de semana).
#   - Distracción en clases: ST273Q06JA en 2022 (clases de matemática) y
#     ST097Q06DA en 2025 (clases de ciencias). No son la misma pregunta.
# No hay ninguna medida comparable entre 2018 y 2022: con PISA no se puede
# medir si el uso de pantallas cambió más en la parte alta justo en la
# ventana de la compresión. Lo que sí se puede ver:
#   A. Exposición por cuartil de ESCS en cada ciclo.
#   B. Asociación dentro de cada cuartil entre uso alto y puntaje en
#      matemática (correlacional).
#   C. Cuánto de la brecha Q4-Q1 de 2022 y 2025 sería atribuible a
#      diferencias de exposición, usando la asociación de B (contable).
# Errores estándar: BRR + valores plausibles (lib_survey.R).
# =============================================================================

suppressPackageStartupMessages(library(haven))

ITEMS <- list(
  "2015" = c("ENTUSE"),
  "2018" = c("ENTUSE"),
  "2022" = c("ST326Q05JA", "IC177Q01JA", "IC177Q02JA", "ST273Q06JA"),
  "2025" = c("ST326Q05JA", "IC177Q01JA", "IC177Q02JA", "ST097Q06DA"))

read_items <- function(cy) {
  f <- file.path(PATHS$raw, FILES$stu[[cy]])
  h <- names(read_sav(f, n_max = 0))
  want <- c("CNT", "CNTSTUID", "W_FSTUWT", rep_cols(), pv_cols("math"), "ESCS", ITEMS[[cy]])
  hit <- h[toupper(h) %in% want]
  d <- read_sav(f, col_select = all_of(hit), user_na = FALSE)
  d <- as.data.table(zap_labels(d)); setnames(d, toupper(names(d)))
  d[, CNT := as.character(CNT)]
  d[CNT == SPEC$country]
}

# Variables derivadas (missing ya son NA por user_na = FALSE)
derive <- function(d, cy) {
  if ("ENTUSE" %in% names(d)) d[, ocio_idx := ENTUSE]
  if ("ST326Q05JA" %in% names(d)) {
    d[, ocio_horas := c(0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)[ST326Q05JA]]
    d[, ocio_alto := as.numeric(ST326Q05JA >= 5)]          # más de 3 horas al día
  }
  if ("IC177Q01JA" %in% names(d)) d[, juegos_alto := as.numeric(IC177Q01JA >= 4)]  # más de 3 h/día
  if ("IC177Q02JA" %in% names(d)) d[, redes_alto  := as.numeric(IC177Q02JA >= 4)]
  dv <- intersect(c("ST273Q06JA", "ST097Q06DA"), names(d))
  if (length(dv)) d[, distraccion := as.numeric(get(dv) %in% 1:2)][is.na(get(dv)), distraccion := NA]
  d
}

expo_vars <- function(d) intersect(c("ocio_idx", "ocio_horas", "ocio_alto", "juegos_alto",
                                     "redes_alto", "distraccion"), names(d))

# A. Media de cada exposición por cuartil de ESCS (cuartiles recalculados por réplica)
stat_expo <- function(vars) function(d, w) {
  g <- assign_groups(d$ESCS, w, d$u, 4L)
  out <- sapply(vars, function(v) {
    x <- d[[v]]
    q <- sapply(1:4, function(k) { i <- which(g == k & !is.na(x)); sum(w[i] * x[i]) / sum(w[i]) })
    c(q, q[4] - q[1])
  })
  m <- matrix(out, ncol = 1)
  rownames(m) <- as.vector(outer(c("Q1", "Q2", "Q3", "Q4", "Q4-Q1"), vars, function(a, b) paste(b, a, sep = "|")))
  m
}

# B. Diferencia de puntaje (uso alto - resto) dentro de cada cuartil, y C.
stat_assoc <- function(v) {
  pvc <- pv_cols("math")
  function(d, w) {
    g <- assign_groups(d$ESCS, w, d$u, 4L)
    x <- d[[v]]
    res <- sapply(pvc, function(p) {
      y <- d[[p]]
      dif <- sapply(1:4, function(k) {
        i1 <- which(g == k & x == 1); i0 <- which(g == k & x == 0)
        sum(w[i1] * y[i1]) / sum(w[i1]) - sum(w[i0] * y[i0]) / sum(w[i0])
      })
      sh <- sapply(1:4, function(k) { i <- which(g == k & !is.na(x)); sum(w[i] * x[i]) / sum(w[i]) })
      pooled <- mean(dif)
      c(dif, (sh[4] - sh[1]) * pooled)   # C: parte contable de la brecha Q4-Q1
    })
    rownames(res) <- c(paste0("dif_Q", 1:4), "brecha_atribuible")
    res
  }
}

expo_out <- list(); assoc_out <- list(); cov_out <- list()
for (cy in names(ITEMS)) {
  d <- read_items(cy)
  if (!nrow(d)) next
  d <- derive(add_tiebreak(d), cy)
  vars <- expo_vars(d)
  cov_out[[cy]] <- data.table(cycle = cy, var = vars,
                              pct_con_dato = sapply(vars, function(v) mean(!is.na(d[[v]]))))
  vars <- vars[sapply(vars, function(v) mean(!is.na(d[[v]])) > 0.5)]
  if (!length(vars)) next
  e <- brr_pv(d, stat_expo(vars))
  e[, c("var", "stat") := tstrsplit(stat, "|", fixed = TRUE)][, cycle := cy]
  expo_out[[cy]] <- e
  for (v in intersect(vars, c("ocio_alto", "juegos_alto", "redes_alto", "distraccion"))) {
    a <- brr_pv(d[!is.na(get(v))], stat_assoc(v))
    a[, `:=`(var = v, cycle = cy)]
    assoc_out[[paste(cy, v)]] <- a
  }
  message(cy, " listo")
}
save_table(rbindlist(cov_out), "pant_cobertura_items.csv")
EXP <- rbindlist(expo_out); save_table(EXP, "pant_exposicion_cuartil.csv")
ASO <- rbindlist(assoc_out); save_table(ASO, "pant_asociacion.csv")

r2 <- function(x) round(x, 2)
message("\nCobertura de los ítems en Chile:"); print(rbindlist(cov_out))
message("\nA. Exposición por cuartil de ESCS:")
print(dcast(EXP[, .(cycle, var, stat, v = sprintf("%.2f (%.2f)", est, se))], cycle + var ~ stat, value.var = "v"))
message("\nB y C. Diferencia en matemática (uso alto - resto) y brecha Q4-Q1 atribuible:")
print(dcast(ASO[, .(cycle, var, stat, v = sprintf("%.1f (%.1f)", est, se))], cycle + var ~ stat, value.var = "v"))
