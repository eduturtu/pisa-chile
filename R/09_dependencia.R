# =============================================================================
# 09_dependencia.R
# Dependencia administrativa desde los estratos explícitos de muestreo
# (STRATUM), en reemplazo de SCHLTYPE (autorreporte del director, que en 2025
# clasifica 17,7% del peso como particular pagado).
#
# Categorías: publico (municipal / SLEP), subvencionado, particular_pagado y
# solo_basica. Desde 2018 los colegios que solo imparten básica forman un
# estrato aparte sin dependencia; en 2015 estaban separados por dependencia y
# se reasignan a solo_basica para que las categorías sean iguales en los
# cuatro ciclos.
#
# Produce:
#   chl_dep_levels.csv     por ciclo y dependencia: peso, puntaje medio, y
#                          composición y puntaje de Q1 y Q4 (cuartiles
#                          nacionales de ESCS)
#   chl_dep_changes.csv    cambios entre ciclos con error estándar
#   chl_dep_decomp.csv     descomposición del cambio de Q4, Q1 y de la brecha
#                          en un efecto dentro de dependencia y un efecto
#                          composición:
#                            d mean_G = sum_d pbar_Gd * d m_Gd   (dentro)
#                                     + sum_d mbar_Gd * d p_Gd   (composición)
#
# Errores estándar: los ciclos son muestras independientes. Para cualquier
# función f(theta_1, theta_2) se perturba un ciclo a la vez con sus 80 pesos
# replicados (Fay) y se suman ambas varianzas; la varianza de imputación se
# obtiene repitiendo el cálculo con cada valor plausible (Rubin).
# =============================================================================

suppressPackageStartupMessages(library(haven))

DEPS <- c("publico", "subvencionado", "particular_pagado", "solo_basica")

dep_from_label <- function(lab) {
  L <- toupper(lab)
  fifelse(grepl("PRIMARY ONLY|PRIMARY,PRIMARY,NO 10TH", L), "solo_basica",
  fifelse(grepl("MUNICIPAL|PUBLIC", L), "publico",
  fifelse(grepl("SUBSIDIZED", L), "subvencionado",
  fifelse(grepl("PRIVATE", L), "particular_pagado", NA_character_))))
}

strata_map <- function(cycle) {
  h <- read_sav(file.path(PATHS$raw, FILES$stu[[cycle]]), n_max = 0)
  v <- names(h)[toupper(names(h)) == "STRATUM"]
  lab <- attr(h[[v]], "labels")
  m <- data.table(STRATUM = as.character(unname(lab)), label = names(lab))
  m <- m[grepl(paste0("^", SPEC$country), STRATUM)]
  m[, dep := dep_from_label(label)]
  m
}

# Estadísticos de un ciclo para un peso dado: matriz stats x PV.
dep_stat <- function(domain) {
  pvc <- pv_cols(domain)
  function(d, w) {
    pv <- as.matrix(d[, ..pvc])
    g  <- assign_groups(d[[SPEC$ses_index]], w, d[["u"]], SPEC$n_groups)
    ng <- SPEC$n_groups
    rows <- list()
    for (k in DEPS) {
      idx <- which(d$dep == k)
      rows[[paste0("share|", k)]] <- rep(sum(w[idx]) / sum(w), ncol(pv))
      rows[[paste0("mean|", k)]]  <- wmeans(pv, w, idx)
      for (q in c(1L, ng)) {
        top <- which(g == q); iq <- intersect(idx, top)
        rows[[paste0("Q", q, "share|", k)]] <- rep(sum(w[iq]) / sum(w[top]), ncol(pv))
        rows[[paste0("Q", q, "mean|", k)]]  <- wmeans(pv, w, iq)
      }
    }
    rows[["Q1|all"]] <- wmeans(pv, w, which(g == 1L))
    rows[[paste0("Q", ng, "|all")]] <- wmeans(pv, w, which(g == ng))
    out <- do.call(rbind, rows)
    out[is.nan(out)] <- NA
    out
  }
}

# Guarda la estimación completa y las 80 réplicas: lista(full, reps[[r]]).
brr_store <- function(d, f) {
  list(full = f(d, d[["W_FSTUWT"]]),
       reps = lapply(rep_cols(), function(rc) f(d, d[[rc]])))
}

# Varianza de g(th1, th2) con th1, th2 de ciclos independientes.
# g recibe dos matrices stats x PV y devuelve un vector con nombre por PV
# (matriz resultados x PV).
combine_cycles <- function(s1, s2, g) {
  base <- g(s1$full, s2$full)
  M <- ncol(base)
  v <- matrix(0, nrow(base), M)
  for (r in seq_len(N_REP)) {
    v <- v + (g(s1$reps[[r]], s2$full) - base)^2
    v <- v + (g(s1$full, s2$reps[[r]]) - base)^2
  }
  v <- v / (N_REP * (1 - FAY_K)^2)
  est <- rowMeans(base, na.rm = TRUE)
  U   <- rowMeans(v, na.rm = TRUE)
  B   <- apply(base, 1, var, na.rm = TRUE)
  data.table(stat = rownames(base), est = est, se = sqrt(U + (1 + 1 / M) * B))
}

# Cambio simple de cada estadístico.
g_change <- function(a, b) b - a

# Descomposición dentro / composición para Q1, Q4 y la brecha.
g_decomp <- function(a, b) {
  ng <- SPEC$n_groups
  part <- function(q) {
    p1 <- a[paste0("Q", q, "share|", DEPS), , drop = FALSE]
    p2 <- b[paste0("Q", q, "share|", DEPS), , drop = FALSE]
    m1 <- a[paste0("Q", q, "mean|", DEPS), , drop = FALSE]
    m2 <- b[paste0("Q", q, "mean|", DEPS), , drop = FALSE]
    # Celdas vacías (sin estudiantes de ese cuartil en esa dependencia)
    # aportan cero a la descomposición.
    m1[is.na(m1)] <- 0; m2[is.na(m2)] <- 0
    pbar <- (p1 + p2) / 2; mbar <- (m1 + m2) / 2
    within_d <- pbar * (m2 - m1)
    comp_d   <- mbar * (p2 - p1)
    rownames(within_d) <- paste0("Q", q, "_within|", DEPS)
    rbind(within_d,
          matrix(colSums(within_d), 1, dimnames = list(paste0("Q", q, "_within|total"), NULL)),
          matrix(colSums(comp_d), 1, dimnames = list(paste0("Q", q, "_composition|total"), NULL)),
          matrix(b[paste0("Q", q, "|all"), ] - a[paste0("Q", q, "|all"), ], 1,
                 dimnames = list(paste0("Q", q, "_change|total"), NULL)))
  }
  top <- part(ng); bot <- part(1L)
  gap <- rbind(
    matrix(top[paste0("Q", ng, "_within|total"), ] - bot["Q1_within|total", ], 1,
           dimnames = list("gap_within|total", NULL)),
    matrix(top[paste0("Q", ng, "_composition|total"), ] - bot["Q1_composition|total", ], 1,
           dimnames = list("gap_composition|total", NULL)),
    matrix(top[paste0("Q", ng, "_change|total"), ] - bot["Q1_change|total", ], 1,
           dimnames = list("gap_change|total", NULL)))
  rbind(top, bot, gap)
}

# --- Estimación por ciclo -----------------------------------------------------
store <- list(); lev <- list()
for (cy in CYCLES) {
  map <- strata_map(cy)
  d <- load_cycle(cy)[CNT == SPEC$country]
  d <- merge(d, map[, .(STRATUM, dep)], by = "STRATUM", all.x = TRUE)
  if (anyNA(d$dep)) stop(cy, ": hay estratos sin dependencia asignada: ",
                         paste(unique(d[is.na(dep), STRATUM]), collapse = ", "))
  d <- add_tiebreak(d)
  for (dom in names(DOMAINS)) {
    s <- brr_store(d, dep_stat(dom))
    store[[paste(cy, dom)]] <- s
    # Niveles con su error estándar (mismo motor que lib_survey)
    r <- brr_pv(d, dep_stat(dom))
    r[, `:=`(cycle = cy, domain = dom)]
    lev[[paste(cy, dom)]] <- r
  }
  message(cy, " listo")
}
levels_dt <- rbindlist(lev)
levels_dt[, c("stat", "dep") := tstrsplit(stat, "|", fixed = TRUE)]
setcolorder(levels_dt, c("cycle", "domain", "dep", "stat"))
save_table(levels_dt, "chl_dep_levels.csv")

# --- Cambios y descomposición por transición ---------------------------------
chg <- list(); dec <- list()
for (tr in SPEC$transitions) for (dom in names(DOMAINS)) {
  s1 <- store[[paste(tr[1], dom)]]; s2 <- store[[paste(tr[2], dom)]]
  a <- combine_cycles(s1, s2, g_change)
  a[, `:=`(domain = dom, from = tr[1], to = tr[2])]
  chg[[paste(tr[1], tr[2], dom)]] <- a
  b <- combine_cycles(s1, s2, g_decomp)
  b[, `:=`(domain = dom, from = tr[1], to = tr[2])]
  dec[[paste(tr[1], tr[2], dom)]] <- b
}
chg_dt <- rbindlist(chg)
chg_dt[, c("stat", "dep") := tstrsplit(stat, "|", fixed = TRUE)]
setcolorder(chg_dt, c("domain", "from", "to", "dep", "stat"))
chg_dt[, z := est / se]
save_table(chg_dt, "chl_dep_changes.csv")

dec_dt <- rbindlist(dec)
dec_dt[, c("term", "dep") := tstrsplit(stat, "|", fixed = TRUE)]
dec_dt[, stat := NULL]
setcolorder(dec_dt, c("domain", "from", "to", "term", "dep"))
dec_dt[, z := est / se]
save_table(dec_dt, "chl_dep_decomp.csv")

# --- Resumen en consola -------------------------------------------------------
r1 <- function(x) round(x, 1)
message("\nMatemáticas, puntaje medio por dependencia:")
print(dcast(levels_dt[domain == "math" & stat == "mean",
                      .(dep, cycle, v = paste0(r1(est), " (", r1(se), ")"))],
            dep ~ cycle, value.var = "v"))
message("\nMatemáticas, descomposición de la brecha Q4-Q1 (2018-2022):")
print(dec_dt[domain == "math" & from == "2018" & to == "2022",
             .(term, dep, est = r1(est), se = r1(se), z = r1(z))])
