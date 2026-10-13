# =============================================================================
# 08_strata.R
# Diagnóstico de los estratos explícitos de muestreo de Chile (STRATUM).
# SCHLTYPE viene del cuestionario del director y en 2025 da una proporción de
# particular pagado (17,7%) incompatible con la matrícula administrativa
# (cerca de 9%). Los estratos de muestreo se construyen con el registro
# oficial de establecimientos, así que pueden servir como dependencia
# administrativa. Este script lista los estratos de cada ciclo con su
# etiqueta, su peso y su puntaje, y los cruza con SCHLTYPE.
# Salida: output/tables/chl_strata.csv y chl_strata_vs_schltype.csv
# =============================================================================

suppressPackageStartupMessages(library(haven))

strata_labels <- function(cycle) {
  f <- file.path(PATHS$raw, FILES$stu[[cycle]])
  h <- read_sav(f, n_max = 0)
  v <- names(h)[toupper(names(h)) == "STRATUM"]
  lab <- attr(h[[v]], "labels")
  if (is.null(lab)) return(data.table(STRATUM = character(0), label = character(0)))
  data.table(STRATUM = as.character(unname(lab)), label = names(lab))
}

out <- list(); cross <- list()
for (cy in CYCLES) {
  d <- load_cycle(cy)[CNT == SPEC$country]
  w <- d$W_FSTUWT
  pv <- as.matrix(d[, paste0("PV", 1:N_PV, "MATH"), with = FALSE])
  d[, math := rowMeans(pv)]
  s <- d[, .(n_students = .N, n_schools = uniqueN(CNTSCHID),
             share = sum(W_FSTUWT), math_mean = weighted.mean(math, W_FSTUWT),
             escs_mean = weighted.mean(ESCS, W_FSTUWT, na.rm = TRUE)),
         by = STRATUM]
  s[, share := share / sum(share)]
  s <- merge(s, strata_labels(cy), by = "STRATUM", all.x = TRUE)
  s[, cycle := cy]
  out[[cy]] <- s

  f_sch <- file.path(PATHS$interim, paste0("sch_", cy, ".rds"))
  if (file.exists(f_sch)) {
    sc <- readRDS(f_sch)[CNT == SPEC$country, .(CNTSCHID, SCHLTYPE)]
    x  <- merge(unique(d[, .(CNTSCHID, STRATUM)]), sc, by = "CNTSCHID", all.x = TRUE)
    x  <- x[, .N, by = .(STRATUM, SCHLTYPE)]
    x[, cycle := cy]
    cross[[cy]] <- x
  }
}
strata <- rbindlist(out, fill = TRUE)
setcolorder(strata, c("cycle", "STRATUM", "label"))
save_table(strata, "chl_strata.csv")
if (length(cross)) save_table(rbindlist(cross), "chl_strata_vs_schltype.csv")

print(strata[, .(cycle, STRATUM, label, n_schools, share = round(share, 3),
                 math = round(math_mean, 1))])
