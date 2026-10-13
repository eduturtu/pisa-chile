# =============================================================================
# 04_school_type.R
# Chile por tipo de establecimiento (SCHLTYPE del cuestionario de escuelas):
#   1 = privado independiente (particular pagado)
#   2 = privado dependiente del Estado (particular subvencionado)
#   3 = público
# Distingue dos preguntas que no son equivalentes:
#   (a) ¿cambió el puntaje según el tipo de colegio?
#   (b) ¿cambió la composición del cuartil alto de ESCS por tipo de colegio?
# Revisar el libro de códigos de cada ciclo antes de interpretar: la
# codificación de SCHLTYPE debe confirmarse para 2025.
# =============================================================================

TYPES <- c("1" = "particular_pagado", "2" = "subvencionado", "3" = "publico")

school_stat <- function(domain) {
  pvc <- pv_cols(domain)
  function(d, w) {
    pv <- as.matrix(d[, ..pvc])
    g  <- assign_groups(d[[SPEC$ses_index]], w, d[["u"]], SPEC$n_groups)
    top <- which(g == SPEC$n_groups)
    rows <- list()
    for (k in names(TYPES)) {
      idx  <- which(d$SCHLTYPE == as.integer(k))
      idxq <- intersect(idx, top)
      lab  <- TYPES[[k]]
      rows[[paste0("share_", lab)]]    <- rep(sum(w[idx]) / sum(w), ncol(pv))
      rows[[paste0("mean_", lab)]]     <- wmeans(pv, w, idx)
      rows[[paste0("q4share_", lab)]]  <- rep(sum(w[idxq]) / sum(w[top]), ncol(pv))
      rows[[paste0("q4mean_", lab)]]   <- wmeans(pv, w, idxq)
    }
    do.call(rbind, rows)
  }
}

out <- list()
for (cy in CYCLES) {
  f_sch <- file.path(PATHS$interim, paste0("sch_", cy, ".rds"))
  if (!file.exists(f_sch)) { message(cy, ": sin archivo de escuelas, se omite"); next }
  s <- readRDS(f_sch)[CNT == SPEC$country, .(CNTSCHID, SCHLTYPE)]
  d <- load_cycle(cy)[CNT == SPEC$country]
  d <- merge(d, s, by = "CNTSCHID", all.x = TRUE)
  d <- add_tiebreak(d)
  message(cy, ": ", round(100 * mean(is.na(d$SCHLTYPE)), 1),
          "% de estudiantes sin SCHLTYPE")
  for (dom in names(DOMAINS)) {
    res <- brr_pv(d, school_stat(dom))
    res[, `:=`(cycle = cy, domain = dom)]
    out[[paste(cy, dom)]] <- res
  }
}
if (length(out)) {
  st <- rbindlist(out)
  setcolorder(st, c("cycle", "domain", "stat"))
  save_table(st, "chl_school_type.csv")
  message("\nChile, matemáticas, media por tipo de colegio:")
  print(dcast(st[domain == "math" & grepl("^mean_", stat)],
              stat ~ cycle, value.var = "est")[
    , lapply(.SD, function(x) if (is.numeric(x)) round(x, 1) else x)])
}
