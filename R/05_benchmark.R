# =============================================================================
# 05_benchmark.R
# Benchmark internacional y triple diferencia descriptiva:
#   delta = (dQ4 - dQ1)_Chile - (dQ4 - dQ1)_benchmark
# El benchmark es el promedio simple de un panel balanceado de sistemas OCDE
# (regla en config.R). Los países son muestras independientes, así que el
# error estándar del promedio es sqrt(sum se_c^2) / N.
# delta describe una diferencia relativa de tendencias, no un efecto causal.
# =============================================================================

# --- Panel balanceado ---------------------------------------------------------
ok_by_cycle <- lapply(CYCLES, function(cy) {
  d <- load_cycle(cy)
  d[, .(ok = sum(!is.na(ESCS)) > 0 & sum(!is.na(PV1MATH)) > 0,
        oecd = any(OECD == 1, na.rm = TRUE)), by = CNT][ok == TRUE]
})
names(ok_by_cycle) <- CYCLES
panel <- Reduce(intersect, lapply(ok_by_cycle, `[[`, "CNT"))
panel <- intersect(panel, ok_by_cycle[["2025"]][oecd == TRUE, CNT])
bench_cnt <- setdiff(panel, c(BENCH$exclude, BENCH$manual_exclude))
message("Benchmark: ", length(bench_cnt), " sistemas: ",
        paste(sort(bench_cnt), collapse = " "))
save_table(data.table(CNT = sort(bench_cnt)), "benchmark_countries.csv")

# --- Brechas por país y ciclo -------------------------------------------------
keep_stats <- c("Q1", paste0("Q", SPEC$n_groups),
                paste0("Q", SPEC$n_groups, "-Q1"), "all")
rows <- list()
for (cy in CYCLES) {
  d_all <- load_cycle(cy)[CNT %in% c(bench_cnt, SPEC$country)]
  for (cn in unique(d_all$CNT)) {
    d <- add_tiebreak(d_all[CNT == cn])  # mismo desempate que en 02 y 04
    for (dom in names(DOMAINS)) {
      f   <- make_group_stat(dom, SPEC$ses_index, SPEC$n_groups, SPEC$requantile_rep)
      res <- brr_pv(d, f)[stat %in% keep_stats]
      res[, `:=`(CNT = cn, cycle = cy, domain = dom)]
      rows[[paste(cy, cn, dom)]] <- res
    }
  }
  message(cy, " listo")
}
by_country <- rbindlist(rows)
setcolorder(by_country, c("CNT", "cycle", "domain", "stat"))
save_table(by_country, "country_quartiles.csv")

# --- Promedio del benchmark ---------------------------------------------------
bench <- by_country[CNT %in% bench_cnt,
                    .(est = mean(est), se = sqrt(sum(se^2)) / .N, n_sys = .N),
                    by = .(cycle, domain, stat)]
save_table(bench, "benchmark_levels.csv")

# --- Triple diferencia por transición ----------------------------------------
gap_stat <- paste0("Q", SPEC$n_groups, "-Q1")
chl <- by_country[CNT == SPEC$country]
td <- rbindlist(lapply(SPEC$transitions, function(tr) {
  get_change <- function(tab) {
    a <- tab[cycle == tr[1] & stat == gap_stat]
    b <- tab[cycle == tr[2] & stat == gap_stat]
    m <- merge(a[, .(domain, e1 = est, s1 = se)],
               b[, .(domain, e2 = est, s2 = se)], by = "domain")
    m[, .(domain, change = e2 - e1, se = sqrt(s1^2 + s2^2))]
  }
  c_chl <- get_change(chl)
  c_bch <- get_change(bench)
  m <- merge(c_chl, c_bch, by = "domain", suffixes = c("_chl", "_bench"))
  # Posición de Chile entre los cambios de brecha de cada país del panel
  per_cnt <- by_country[stat == gap_stat & cycle %in% tr,
                        .(chg = est[cycle == tr[2]] - est[cycle == tr[1]]),
                        by = .(CNT, domain)]
  rank_tab <- per_cnt[, .(chl_rank = rank(chg)[CNT == SPEC$country],
                          n_rank = .N), by = domain]
  m <- merge(m, rank_tab, by = "domain")
  m[, `:=`(from = tr[1], to = tr[2],
           delta = change_chl - change_bench,
           se_delta = sqrt(se_chl^2 + se_bench^2))]
  m[, `:=`(z = delta / se_delta, mde_delta = mde(se_delta),
           ci_low = delta - 1.96 * se_delta, ci_high = delta + 1.96 * se_delta)]
  m
}))
setcolorder(td, c("domain", "from", "to"))
save_table(td, "triple_difference.csv")

message("\nTriple diferencia (brecha Q4-Q1), matemáticas:")
print(td[domain == SPEC$primary_domain,
         .(from, to, chl = round(change_chl, 1), bench = round(change_bench, 1),
           delta = round(delta, 1), se = round(se_delta, 1),
           mde = round(mde_delta, 1), rank = paste0(chl_rank, "/", n_rank))])
