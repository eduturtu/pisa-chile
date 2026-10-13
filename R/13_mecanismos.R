# =============================================================================
# 13_mecanismos.R
# ¿Qué explica el gradiente socioeconómico de la caída SIMCE II medio
# 2018-2022 dentro de cada comuna?
#
# Mecanismos con datos por colegio (RBD):
#  1. Presencialidad. Mineduc, "Estado de apertura de establecimientos"
#     (datosabiertos.mineduc.cl, descargado 8-10-2026). Estado diario
#     autorreportado en días muestreados de cada mes (oct-dic 2020 y
#     mar-dic 2021): Abierto, Cerrado, En cuarentena, Sin información.
#     Exposición = proporción de días con información en que el colegio
#     estuvo abierto. "Abierto" no implica asistencia de todo el curso.
#  2. Socioemocional. Agencia, IDPS II medio por RBD (2016, 2017, 2018,
#     2022): autoestima académica y motivación (am), clima de convivencia
#     (cc). Medidos en los mismos alumnos que rinden SIMCE, así que son
#     resultados simultáneos, no mediadores con identificación propia.
#
# Análisis:
#  A. Exposición por grupo.
#  B. Descomposición de Gelbach (2016) del gradiente con efectos fijos de
#     comuna: cuánto cambia el coeficiente de cada grupo al agregar la
#     exposición. Error estándar por bootstrap de comunas.
#  C. Placebo: la exposición de 2021 no debería predecir el cambio
#     2016-2018 (antes de la pandemia).
#  D. IDPS: cambio 2018-2022 por grupo y placebo 2016-2018.
# =============================================================================

if (!exists("S")) source("R/10_simce.R", local = TRUE)
if (!exists("cl_se")) source("R/12_gradiente_comuna.R", local = TRUE)

APERTURA_DIR <- file.path(PATHS$raw, "apertura")
IDPS_DIR     <- file.path(PATHS$raw, "idps")
N_BOOT       <- 500
set.seed(SPEC$tie_seed)

# --- 1. Presencialidad por RBD -------------------------------------------------
read_apertura <- function(f) {
  d <- fread(f, encoding = "Latin-1", showProgress = FALSE)
  dc <- grep("^DIA", names(d), value = TRUE)
  # 2021 incluye JUNJI e INTEGRA (ORIGEN 2 y 3); 2020 trae solo Mineduc.
  if ("ORIGEN" %in% names(d)) d <- d[ORIGEN == 1]
  m <- melt(d[, c("AGNO", "MES_ESCOLAR", "ID_ESTAB", dc), with = FALSE],
            id.vars = c("AGNO", "MES_ESCOLAR", "ID_ESTAB"), value.name = "estado")
  m[, estado := fcase(estado == "Abierto", "abierto", estado == "Cerrado", "cerrado",
                      grepl("cuarentena", estado, ignore.case = TRUE), "cuarentena",
                      default = NA_character_)]
  m[!is.na(estado)]
}
A <- rbindlist(lapply(list.files(APERTURA_DIR, "^Apertura_EE_.*\\.csv$", full.names = TRUE),
                      read_apertura))
expo <- dcast(A[, .N, by = .(rbd = ID_ESTAB, AGNO, estado)],
              rbd ~ paste0(estado, "_", AGNO), value.var = "N", fill = 0)
for (y in c(2020, 2021)) {
  ab <- paste0("abierto_", y); ce <- paste0("cerrado_", y); cu <- paste0("cuarentena_", y)
  if (!cu %in% names(expo)) expo[, (cu) := 0]
  tot <- expo[[ab]] + expo[[ce]] + expo[[cu]]
  expo[, paste0("open_", y) := get(ab) / tot]
  expo[, paste0("quar_", y) := get(cu) / tot]
}
expo <- expo[, .(rbd, open_2020, open_2021, quar_2021)]

# --- 2. IDPS por RBD -----------------------------------------------------------
read_idps <- function(y) {
  f <- list.files(IDPS_DIR, sprintf("^idps2m%d_rbd\\.(txt|csv)$", y), full.names = TRUE)
  d <- fread(f, encoding = "Latin-1", showProgress = FALSE)
  setnames(d, tolower(names(d)))
  if ("ind" %in% names(d)) {               # 2022: formato largo
    d <- dcast(d, rbd ~ tolower(ind), value.var = "prom")
    d <- d[, .(rbd, am, cc)]
  } else {
    setnames(d, sub("_rbd$", "", names(d)))
    d <- d[, .(rbd, am = as.numeric(ind_am), cc = as.numeric(ind_cc))]
  }
  d[, year := y]
}
I <- rbindlist(lapply(c(2016, 2017, 2018, 2022), read_idps))

# --- Panel de cambios por colegio ---------------------------------------------
wide_change <- function(var_dt, v, a, b, wvar = NULL) {
  x <- dcast(var_dt[year %in% c(a, b)], rbd ~ year, value.var = v)
  setnames(x, c("rbd", "y1", "y2"))
  x[!is.na(y1) & !is.na(y2), .(rbd, d = y2 - y1)]
}
mk <- function(a, b) {
  x <- dcast(S[year %in% c(a, b) & !is.na(mate) & n_mate > 0 & !is.na(grupo)],
             rbd + grupo ~ year, value.var = c("mate", "n_mate"))
  setnames(x, c("rbd", "grupo", "y1", "y2", "n1", "n2"))
  x <- x[!is.na(y1) & !is.na(y2)]
  x[, `:=`(dy = y2 - y1, w = (n1 + n2) / 2)]
  x <- merge(x, geo, by = "rbd")
  x[, grupo := relevel(factor(grupo), ref = "gse_medio_alto_alto")]
  x
}
D  <- merge(mk(2018, 2022), expo, by = "rbd", all.x = TRUE)
D0 <- merge(mk(2016, 2018), expo, by = "rbd", all.x = TRUE)
message("Colegios con exposición: ", D[!is.na(open_2021), .N], " de ", nrow(D))

# --- A. Exposición por grupo ----------------------------------------------------
expo_grp <- D[!is.na(open_2021), .(schools = .N,
                                   open_2020 = weighted.mean(open_2020, w, na.rm = TRUE),
                                   open_2021 = weighted.mean(open_2021, w),
                                   quar_2021 = weighted.mean(quar_2021, w)), by = grupo]
save_table(expo_grp, "mec_exposicion_grupo.csv")

# --- B. Descomposición de Gelbach -----------------------------------------------
# Efectos fijos de comuna por eliminación de medias ponderadas (Frisch-Waugh),
# para que el bootstrap sea rápido.
wls_fe <- function(y, X, w, g) {
  dm <- function(v) v - ave(v * w, g, FUN = sum) / ave(w, g, FUN = sum)
  yd <- dm(y); Xd <- apply(X, 2, dm)
  keep <- colSums(abs(Xd)) > 1e-10
  b <- rep(NA_real_, ncol(X)); names(b) <- colnames(X)
  b[keep] <- lm.wfit(Xd[, keep, drop = FALSE], yd, w)$coefficients
  b
}
gelbach_fast <- function(y, G, M, w, g) {
  b0 <- wls_fe(y, G, w, g); b1 <- wls_fe(y, cbind(G, M), w, g)
  k <- colnames(G)
  c(setNames(b0[k], paste0("base:", k)), setNames(b1[k], paste0("con_mediador:", k)),
    setNames(b0[k] - b1[k], paste0("explicado:", k)), setNames(b1[colnames(M)], paste0("coef:", colnames(M))))
}
boot_gelbach <- function(dd, mediators, B = N_BOOT) {
  G <- model.matrix(~ grupo, dd)[, -1, drop = FALSE]
  M <- as.matrix(dd[, ..mediators])
  est <- gelbach_fast(dd$dy, G, M, dd$w, dd$com)
  rows <- split(seq_len(nrow(dd)), dd$com)
  reps <- t(replicate(B, {
    s <- sample(length(rows), replace = TRUE)
    idx <- unlist(rows[s], use.names = FALSE)
    gid <- rep(seq_along(s), lengths(rows[s]))
    gelbach_fast(dd$dy[idx], G[idx, , drop = FALSE], M[idx, , drop = FALSE], dd$w[idx], gid)
  }))
  data.table(term = names(est), est = est, se = apply(reps, 2, sd, na.rm = TRUE))
}
DD <- D[!is.na(open_2021) & !is.na(open_2020)]
gb <- rbind(
  boot_gelbach(DD, "open_2021")[, spec := "apertura_2021"],
  boot_gelbach(DD, c("open_2020", "open_2021"))[, spec := "apertura_2020_2021"])
gb[, z := est / se]
save_table(gb, "mec_gelbach_apertura.csv")

# --- C. Placebo: apertura 2021 frente al cambio 2016-2018 -----------------------
pl <- lm(dy ~ grupo + open_2021 + factor(com), D0[!is.na(open_2021)], weights = w)
pl_dt <- data.table(term = "open_2021 (cambio 2016-2018)", est = coef(pl)["open_2021"],
                    se = cl_se(pl, D0[!is.na(open_2021)]$com)["open_2021"])
m21 <- lm(dy ~ grupo + open_2021 + factor(com), DD, weights = w)
pl_dt <- rbind(pl_dt, data.table(term = "open_2021 (cambio 2018-2022)",
                                 est = coef(m21)["open_2021"], se = cl_se(m21, DD$com)["open_2021"]))
save_table(pl_dt, "mec_placebo_apertura.csv")

# --- D. IDPS -------------------------------------------------------------------
idps_change <- function(v, a, b) {
  x <- dcast(I[year %in% c(a, b)], rbd ~ year, value.var = v); setnames(x, c("rbd", "y1", "y2"))
  x <- merge(x[!is.na(y1) & !is.na(y2)], unique(S[year == BASE_YEAR, .(rbd, grupo, w = n_mate)]), by = "rbd")
  x <- merge(x[!is.na(grupo) & w > 0], geo, by = "rbd")
  x[, grupo := relevel(factor(grupo), ref = "gse_medio_alto_alto")]
  fit <- lm(I(y2 - y1) ~ grupo + factor(com), x, weights = w)
  k <- grep("^grupo", names(coef(fit)), value = TRUE)
  lev <- x[, .(change = weighted.mean(y2 - y1, w)), by = grupo]
  list(rel = data.table(indicator = v, from = a, to = b, term = k, est = coef(fit)[k], se = cl_se(fit, x$com)[k]),
       lev = lev[, `:=`(indicator = v, from = a, to = b)])
}
idps_res <- lapply(list(c("am", 2016, 2018), c("am", 2018, 2022), c("cc", 2016, 2018), c("cc", 2018, 2022)),
                   function(s) idps_change(s[1], as.integer(s[2]), as.integer(s[3])))
save_table(rbindlist(lapply(idps_res, `[[`, "rel")), "mec_idps_gradiente.csv")
save_table(rbindlist(lapply(idps_res, `[[`, "lev")), "mec_idps_cambio.csv")

# --- E. Brecha neta de la tendencia previa ------------------------------------
# Exceso_s = (y22 - y18) - k * 2 * (y18 - y16): extrapola k veces la tendencia
# 2016-2018 (dos años) a la ventana 2018-2022 (cuatro años). k = 1 es tendencia
# lineal completa; k = 0.5, la mitad. Mismo espíritu que HonestDiD con
# restricción de suavidad, en versión simple.
T3 <- dcast(S[year %in% c(2016, 2018, 2022) & !is.na(mate) & n_mate > 0 & !is.na(grupo)],
            rbd + grupo ~ year, value.var = c("mate", "n_mate"))
setnames(T3, c("rbd", "grupo", "m16", "m18", "m22", "n16", "n18", "n22"))
T3 <- T3[complete.cases(T3)][, w := (n16 + n18 + n22) / 3]
trend <- rbindlist(lapply(c(0, 0.5, 1), function(k) {
  T3[, ex := (m22 - m18) - k * 2 * (m18 - m16)]
  r <- T3[, { m <- weighted.mean(ex, w); .(n = .N, est = m, se = sqrt(sum(w^2 * (ex - m)^2)) / sum(w)) }, by = grupo]
  lo <- r[grupo == "gse_bajo_medio_bajo"]; hi <- r[grupo == "gse_medio_alto_alto"]
  rbind(r, data.table(grupo = "brecha_alto_menos_bajo", n = hi$n + lo$n,
                      est = hi$est - lo$est, se = sqrt(hi$se^2 + lo$se^2)))[, k_tendencia := k]
}))
save_table(trend, "mec_brecha_neta_tendencia.csv")

# --- Resumen ---------------------------------------------------------------------
r1 <- function(x) round(x, 3)
message("\nA. Proporción de días abiertos por grupo:"); print(expo_grp[, lapply(.SD, function(x) if (is.numeric(x)) r1(x) else x)])
message("\nB. Gelbach (e.e. bootstrap de comunas):"); print(gb[, .(spec, term, est = r1(est), se = r1(se))])
message("\nC. Placebo:"); print(pl_dt[, .(term, est = r1(est), se = r1(se))])
message("\nE. Brecha neta de tendencia previa:"); print(trend[grupo == "brecha_alto_menos_bajo", .(k_tendencia, est = r1(est), se = r1(se))])
message("\nD. IDPS, cambio por grupo:"); print(rbindlist(lapply(idps_res, `[[`, "lev"))[, change := r1(change)][])
