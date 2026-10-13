# =============================================================================
# 12_gradiente_comuna.R
# ¿El gradiente socioeconómico de la caída SIMCE 2018-2022 se explica por
# factores comunales (cuarentenas Paso a Paso, conectividad local, mercado
# laboral, etc.)?
# Si el gradiente sobrevive a efectos fijos de comuna, ningún factor que varíe
# solo entre comunas puede explicarlo; quedan los factores de cada colegio
# (días de apertura, composición, docentes).
#
# Modelo (colegios en ambos años, ponderado por alumnos evaluados):
#   dy_s = a + b_g * grupo_s [+ rural_s + RM_s | + FE comuna] + e_s
# Grupo de referencia: GSE medio alto y alto no pagado.
# Errores estándar agrupados por comuna (estimador sandwich CR1 manual).
# =============================================================================

if (!exists("S")) source("R/10_simce.R", local = TRUE)

read_extra <- function(y) {
  f <- file.path(SIMCE_DIR, sprintf("simce2m%d_rbd.csv", y))
  d <- fread(f, encoding = "UTF-8", showProgress = FALSE,
             select = c("rbd", "cod_rural_rbd", "cod_reg_rbd", "cod_com_rbd"))
  setnames(d, c("rbd", "rural", "reg", "com"))
  d[, rural := if (is.character(rural)) trimws(rural) == "Rural" else rural == 2L]
  d
}
geo <- read_extra(BASE_YEAR)

W <- dcast(S[year %in% c(2018, 2022) & !is.na(mate) & n_mate > 0 & !is.na(grupo)],
           rbd + grupo ~ year, value.var = c("mate", "n_mate"))
setnames(W, c("rbd", "grupo", "y1", "y2", "n1", "n2"))
W <- merge(W[!is.na(y1) & !is.na(y2)], geo, by = "rbd")
W[, `:=`(dy = y2 - y1, w = (n1 + n2) / 2, rm = reg == 13L,
         grupo = relevel(factor(grupo), ref = "gse_medio_alto_alto"))]

cl_se <- function(fit, cl) {
  X <- model.matrix(fit); u <- residuals(fit) * sqrt(weights(fit))
  Xw <- X * sqrt(weights(fit)); bread <- solve(crossprod(Xw))
  meat <- crossprod(rowsum(Xw * u, cl))
  G <- length(unique(cl)); n <- nrow(X); k <- ncol(X)
  V <- bread %*% meat %*% bread * (G / (G - 1)) * ((n - 1) / (n - k))
  sqrt(diag(V))
}

specs <- list(
  base      = dy ~ grupo,
  rural_rm  = dy ~ grupo + rural + rm,
  fe_comuna = dy ~ grupo + factor(com)
)
res <- rbindlist(lapply(names(specs), function(nm) {
  fit <- lm(specs[[nm]], data = W, weights = w)
  se  <- cl_se(fit, W$com)
  k   <- grep("^grupo", names(coef(fit)), value = TRUE)
  data.table(spec = nm, term = k, est = coef(fit)[k], se = se[k])
}))
res[, z := est / se]
save_table(res, "simce_gradiente_comuna.csv")

zona <- W[rural == FALSE, .(change = weighted.mean(dy, w), schools = .N),
          by = .(grupo, zona = fifelse(rm, "RM", "regiones"))]
save_table(zona, "simce_cambio_zona.csv")

message("\nCambio SIMCE matemáticas 2018-2022 relativo a GSE medio alto y alto (e.e. por comuna):")
print(dcast(res[, .(spec, term, v = sprintf("%.1f (%.1f)", est, se))], term ~ spec, value.var = "v"))
