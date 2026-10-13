# =============================================================================
# 03_missing_escs.R
# Diagnóstico de estudiantes sin ESCS en Chile. Los cuartiles se calculan
# solo con quienes tienen índice; si la falta de datos crece en el tiempo y no
# es aleatoria, la serie de brechas puede estar sesgada.
#
# Reporta por ciclo: proporción ponderada sin ESCS, puntaje medio con y sin
# ESCS, y su diferencia.
# =============================================================================

missing_stat <- function(domain, ses = SPEC$ses_index) {
  pvc <- pv_cols(domain)
  function(d, w) {
    pv   <- as.matrix(d[, ..pvc])
    miss <- is.na(d[[ses]])
    share <- sum(w[miss]) / sum(w)
    m_miss <- wmeans(pv, w, which(miss))
    m_ok   <- wmeans(pv, w, which(!miss))
    rbind(share_missing = rep(share, ncol(pv)),
          mean_missing  = m_miss,
          mean_valid    = m_ok,
          diff_missing_minus_valid = m_miss - m_ok)
  }
}

out <- list()
for (cy in CYCLES) {
  d <- load_cycle(cy)[CNT == SPEC$country]
  for (dom in names(DOMAINS)) {
    res <- brr_pv(d, missing_stat(dom))
    res[, `:=`(cycle = cy, domain = dom,
               n_missing = sum(is.na(d[[SPEC$ses_index]])), n = nrow(d))]
    out[[paste(cy, dom)]] <- res
  }
}
miss <- rbindlist(out)
setcolorder(miss, c("cycle", "domain", "stat"))
save_table(miss, "chl_missing_escs.csv")

message("\nChile, matemáticas: estudiantes sin ESCS")
print(dcast(miss[domain == "math"], cycle ~ stat, value.var = "est")[
  , lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
