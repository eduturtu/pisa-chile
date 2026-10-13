# =============================================================================
# 02_chile_quartiles.R
# Puntaje medio de Chile por cuartil de ESCS (dentro de cada ciclo), brecha
# Q4-Q1 y media total, para los tres dominios y los cuatro ciclos.
# Luego, cambios entre ciclos para las transiciones de SPEC$transitions.
#
# Nota: los cuartiles son posiciones relativas dentro de país y ciclo, por lo
# que cualquier reescalamiento monótono del ESCS no los altera. Lo que sí
# importa es si cambiaron los componentes o ítems del índice (fase 1b).
# =============================================================================

levels_list <- list()
for (cy in CYCLES) {
  d <- load_cycle(cy)[CNT == SPEC$country]
  d <- add_tiebreak(d)
  for (dom in names(DOMAINS)) {
    message(cy, " ", dom)
    f   <- make_group_stat(dom, SPEC$ses_index, SPEC$n_groups, SPEC$requantile_rep)
    res <- brr_pv(d, f)
    res[, `:=`(cycle = cy, domain = dom, n_students = nrow(d),
               n_valid_ses = sum(!is.na(d[[SPEC$ses_index]])))]
    levels_list[[paste(cy, dom)]] <- res
  }
}
chl_levels <- rbindlist(levels_list)
setcolorder(chl_levels, c("cycle", "domain", "stat"))
save_table(chl_levels, "chl_quartiles_levels.csv")

# --- Cambios entre ciclos ----------------------------------------------------
# Para medias de grupo se agrega el error de enlace (si está en config.R).
# Para la brecha Q4-Q1 no: el desplazamiento común de escala se cancela.
trans_list <- list()
for (tr in SPEC$transitions) {
  a <- chl_levels[cycle == tr[1]]
  b <- chl_levels[cycle == tr[2]]
  m <- merge(a, b, by = c("domain", "stat"), suffixes = c("_1", "_2"))
  m[, link := ifelse(grepl("-", stat), 0,
                     vapply(domain, link_error, numeric(1), tr[1], tr[2]))]
  m[, `:=`(from = tr[1], to = tr[2],
           change = est_2 - est_1,
           se_change = sqrt(se_1^2 + se_2^2 + fifelse(is.na(link), 0, link^2)),
           link_included = !is.na(link) & link > 0)]
  trans_list[[paste(tr, collapse = "-")]] <-
    m[, .(domain, stat, from, to, change, se_change, link_included,
          z = change / se_change, mde = mde(se_change))]
}
chl_changes <- rbindlist(trans_list)
save_table(chl_changes, "chl_quartiles_changes.csv")

message("\nBrecha Q4-Q1 en matemáticas:")
print(chl_levels[domain == "math" & stat == "Q4-Q1",
                 .(cycle, est = round(est, 1), se = round(se, 1))])
if (any(!chl_changes$link_included & !grepl("-", chl_changes$stat)))
  message("Aviso: faltan errores de enlace en config.R; los cambios de nivel ",
          "subestiman su error estándar. Las brechas no se ven afectadas.")
