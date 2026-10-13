# =============================================================================
# tests/test_engine.R
# Verifica el motor de estimación con datos simulados, sin archivos OCDE.
#   Rscript tests/test_engine.R
# 1. Los cuartiles contienen 25% del peso cada uno.
# 2. El error estándar de la media coincide con un cálculo a mano (BRR + Rubin).
# 3. La brecha Q4-Q1 recupera el valor teórico de la simulación.
# =============================================================================

source("config.R")
source("R/lib_survey.R")
set.seed(123)

n <- 20000
sch  <- sample(1:150, n, TRUE)
escs <- rnorm(n)
true <- 450 + 35 * escs + rnorm(150)[sch] * 15 + rnorm(n, 0, 60)
d <- data.table(CNT = "SIM", CNTSTUID = as.numeric(1:n), ESCS = escs,
                W_FSTUWT = runif(n, 5, 15))
for (r in 1:N_REP)
  set(d, j = paste0("W_FSTURWT", r),
      value = d$W_FSTUWT * ifelse((sch + r) %% 2 == 0, 1 + FAY_K, 1 - FAY_K))
for (p in 1:N_PV) set(d, j = paste0("PV", p, "MATH"), value = true + rnorm(n, 0, 20))
d <- add_tiebreak(d)

# 1
g  <- assign_groups(d$ESCS, d$W_FSTUWT, d$u)
sh <- tapply(d$W_FSTUWT, g, sum) / sum(d$W_FSTUWT)
stopifnot(all(abs(sh - 0.25) < 0.001))

# 2
pv <- paste0("PV", 1:N_PV, "MATH")
th <- sapply(pv, function(p) weighted.mean(d[[p]], d$W_FSTUWT))
vs <- sapply(pv, function(p) sum(sapply(1:N_REP, function(r)
  (weighted.mean(d[[p]], d[[paste0("W_FSTURWT", r)]]) - th[[p]])^2)) /
  (N_REP * (1 - FAY_K)^2))
manual_se <- sqrt(mean(vs) + (1 + 1 / N_PV) * var(th))
res <- brr_pv(d, make_group_stat("math"))
stopifnot(abs(res[stat == "all", se] - manual_se) < 1e-8)

# 3  Media de N(0,1) en el cuartil superior: dnorm(qnorm(.75)) / .25
expected_gap <- 35 * 2 * dnorm(qnorm(0.75)) / 0.25
got <- res[stat == "Q4-Q1", est]
stopifnot(abs(got - expected_gap) < 3 * res[stat == "Q4-Q1", se])

cat(sprintf("OK: cuartiles 25%%, SE = %.4f (a mano %.4f), brecha %.1f (teórica %.1f)\n",
            res[stat == "all", se], manual_se, got, expected_gap))
