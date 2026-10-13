# =============================================================================
# lib_survey.R
# Motor de estimación para PISA: pesos replicados BRR (Fay) y valores
# plausibles combinados con las reglas de Rubin. Sin dependencias externas
# salvo data.table, para que cada paso sea auditable.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

rep_cols <- function() paste0("W_FSTURWT", seq_len(N_REP))
pv_cols  <- function(domain) paste0("PV", seq_len(N_PV), DOMAINS[[domain]])

# -----------------------------------------------------------------------------
# Asigna grupos de igual peso (cuartiles por defecto) dentro de un país-ciclo.
# - Ordena por el índice socioeconómico y desempata con un número aleatorio
#   fijo por estudiante (u), para que los empates no dependan del orden de
#   las filas.
# - Cada grupo acumula 1/n del peso total; un estudiante se asigna según el
#   punto medio de su peso acumulado.
# - Estudiantes sin índice quedan como NA.
# -----------------------------------------------------------------------------
assign_groups <- function(x, w, u, n_groups = 4L) {
  g  <- rep(NA_integer_, length(x))
  ok <- which(!is.na(x) & !is.na(w) & w > 0)
  if (length(ok) == 0L) return(g)
  o   <- ok[order(x[ok], u[ok])]
  cw  <- cumsum(w[o])
  mid <- (cw - w[o] / 2) / cw[length(cw)]
  g[o] <- pmin(n_groups, as.integer(floor(mid * n_groups)) + 1L)
  g
}

# Media ponderada de cada columna de una matriz, para un subconjunto de filas.
wmeans <- function(m, w, idx) {
  if (length(idx) == 0L) return(rep(NA_real_, ncol(m)))
  colSums(m[idx, , drop = FALSE] * w[idx]) / sum(w[idx])
}

# -----------------------------------------------------------------------------
# Estimador genérico.
# stat_fun(d, w) debe devolver una matriz: filas = estadísticos (con nombre),
# columnas = valores plausibles (o una sola columna si no usa puntajes).
# Se evalúa una vez con el peso final y una vez por cada peso replicado.
#
# Varianza muestral por VP: sum_r (theta_r - theta)^2 / (R (1 - k)^2)
# Varianza total (Rubin):   U + (1 + 1/M) B
# -----------------------------------------------------------------------------
brr_pv <- function(d, stat_fun) {
  theta <- stat_fun(d, d[["W_FSTUWT"]])
  if (is.null(dim(theta))) theta <- matrix(theta, ncol = 1,
                                           dimnames = list(names(theta), NULL))
  M  <- ncol(theta)
  ss <- matrix(0, nrow(theta), M)
  for (rc in rep_cols()) {
    th_r <- stat_fun(d, d[[rc]])
    if (is.null(dim(th_r))) th_r <- matrix(th_r, ncol = 1)
    ss <- ss + (th_r - theta)^2
  }
  v_samp <- ss / (N_REP * (1 - FAY_K)^2)
  est <- rowMeans(theta)
  U   <- rowMeans(v_samp)
  B   <- if (M > 1) apply(theta, 1, var) else rep(0, nrow(theta))
  data.table(stat = rownames(theta), est = est,
             se = sqrt(U + (1 + 1 / M) * B),
             se_sampling = sqrt(U), se_imputation = sqrt((1 + 1 / M) * B))
}

# -----------------------------------------------------------------------------
# Estadístico: media por grupo socioeconómico, brecha alto-bajo y media total.
# Si requantile = TRUE los grupos se recalculan con cada peso replicado, de
# modo que la incertidumbre sobre los puntos de corte entra en el error
# estándar (mismo criterio que Cobreros et al. 2026).
# -----------------------------------------------------------------------------
make_group_stat <- function(domain, ses = "ESCS", n_groups = 4L,
                            requantile = TRUE) {
  pvc <- pv_cols(domain)
  g_fixed <- NULL
  function(d, w) {
    pv <- as.matrix(d[, ..pvc])
    if (requantile || is.null(g_fixed)) {
      g <- assign_groups(d[[ses]], w, d[["u"]], n_groups)
      if (!requantile) g_fixed <<- g
    } else {
      g <- g_fixed
    }
    out <- t(vapply(seq_len(n_groups),
                    function(k) wmeans(pv, w, which(g == k)),
                    numeric(ncol(pv))))
    if (ncol(pv) == 1L) out <- t(out)
    rownames(out) <- paste0("Q", seq_len(n_groups))
    gap <- out[n_groups, , drop = FALSE] - out[1, , drop = FALSE]
    rownames(gap) <- paste0("Q", n_groups, "-Q1")
    all <- matrix(wmeans(pv, w, seq_len(nrow(pv))), nrow = 1,
                  dimnames = list("all", NULL))
    rbind(out, gap, all)
  }
}

# Agrega un desempate aleatorio reproducible por estudiante.
add_tiebreak <- function(d, seed = SPEC$tie_seed) {
  setorderv(d, c("CNT", "CNTSTUID"))
  set.seed(seed)
  d[, u := runif(.N)]
  d
}

# Diferencia entre dos estimaciones de muestras independientes (dos ciclos).
diff_independent <- function(est1, se1, est2, se2, link = 0) {
  list(diff = est2 - est1, se = sqrt(se1^2 + se2^2 + link^2))
}

# Efecto mínimo detectable, bilateral al 5% con potencia de 80%.
mde <- function(se, alpha = 0.05, power = 0.80) {
  (qnorm(1 - alpha / 2) + qnorm(power)) * se
}

link_error <- function(domain, c1, c2) {
  le <- LINK_ERROR[[domain]][[paste0(c1, "-", c2)]]
  if (is.null(le)) NA_real_ else le
}

load_cycle <- function(cycle) {
  f <- file.path(PATHS$interim, paste0("stu_", cycle, ".rds"))
  if (!file.exists(f)) stop("Falta ", f, ". Correr primero R/01_extract.R")
  readRDS(f)
}

save_table <- function(x, name) {
  dir.create(PATHS$tables, recursive = TRUE, showWarnings = FALSE)
  fwrite(x, file.path(PATHS$tables, name))
  invisible(x)
}

# Busca un archivo de datos por patrón en cualquier subcarpeta de data/raw
# (ignora mayúsculas y los comprimidos). Así los scripts no dependen de cómo
# se ordenen las carpetas.
raw_file <- function(pattern) {
  f <- list.files(PATHS$raw, pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  f <- f[!grepl("raw_orig|\\.(rar|zip|7z|pdf|xlsx?|sav)$", f, ignore.case = TRUE)]
  if (!length(f)) stop("No encuentro un archivo que calce con '", pattern, "' dentro de ", PATHS$raw)
  sort(f)[1]
}
