# =============================================================================
# 14_cobertura.R
# ¿Puede la caída de cobertura de SIMCE (alumnos evaluados / matriculados)
# explicar la compresión del gradiente 2018-2022?
#
# Matrícula: Mineduc, "Resumen de matrícula por curso" (datosabiertos,
# descargado 8-10-2026). II medio jóvenes = COD_ENSE2 5 (HC) o 7 (TP),
# COD_GRADO 2, cursos simples. Se suma N_ALU por RBD.
# Cobertura del colegio = alumnos con puntaje en Matemática / matrícula II medio.
#
# Cotas tipo Lee (2009), versión con normalidad dentro del colegio:
# si en 2022 se evaluó una fracción p = cob22 / cob18 de la población que se
# habría evaluado con la cobertura de 2018, y los no evaluados son los de
# peor rendimiento, la media observada sobrestima la media comparable en
#   sesgo = sigma * dnorm(qnorm(1 - p)) / p.
# sigma = desviación estándar dentro del colegio (supuesto: 40 y 50 puntos).
# Escenarios:
#   (a) peor caso en todos los colegios con p < 1;
#   (b) peor caso solo en GSE bajo y medio bajo (el más desfavorable para la
#       conclusión de compresión: los colegios altos no se corrigen).
# =============================================================================

if (!exists("S")) source("R/10_simce.R", local = TRUE)
if (!exists("cl_se")) source("R/12_gradiente_comuna.R", local = TRUE)

MAT_FILE <- file.path(PATHS$raw, "matricula", "matricula_2m.csv")
if (!file.exists(MAT_FILE)) {
  # Construye el archivo desde las bases originales (data/raw/matricula/<año>/)
  f <- list.files(file.path(PATHS$raw, "matricula"), "Resumen.*Matr.*\\.csv$|^20221014",
                  recursive = TRUE, full.names = TRUE)
  M <- rbindlist(lapply(f, function(x) {
    d <- fread(x, showProgress = FALSE)
    setnames(d, toupper(sub("^﻿", "", names(d))))
    yy <- as.integer(d$AGNO[1])
    d[COD_ENSE2 %in% c(5, 7) & COD_GRADO == 2 & COD_TIP_CUR == 0,
      .(mat2m = sum(N_ALU)), by = .(rbd = RBD)][, year := yy]
  }))
  fwrite(M, MAT_FILE)
}
M <- fread(MAT_FILE)

C <- merge(S[year %in% c(2016, 2018, 2022)], M, by = c("rbd", "year"))
C[, cob := pmin(n_mate / mat2m, 1)]

cob_grp <- C[!is.na(grupo) & n_mate > 0,
             .(evaluados = sum(n_mate), matricula = sum(mat2m), cobertura = sum(n_mate) / sum(mat2m)),
             by = .(year, grupo)]
setorder(cob_grp, grupo, year)
save_table(cob_grp, "cob_por_grupo.csv")

W <- dcast(C[!is.na(grupo) & !is.na(mate) & n_mate > 0 & year %in% c(2018, 2022)],
           rbd + grupo ~ year, value.var = c("mate", "n_mate", "cob"))
setnames(W, c("rbd", "grupo", "y1", "y2", "n1", "n2", "c1", "c2"))
W <- merge(W[complete.cases(W)], geo, by = "rbd")
W[, `:=`(w = (n1 + n2) / 2, p = pmin(c2 / c1, 1), dcob = c2 - c1,
         grupo = relevel(factor(grupo), ref = "gse_medio_alto_alto"))]

lee_bias <- function(p, sigma) fifelse(p < 1, sigma * dnorm(qnorm(1 - p)) / p, 0)

fit_fe <- function(dd, y) {
  dd <- copy(dd)[, dy := y]
  fit <- lm(dy ~ grupo + factor(com), dd, weights = w)
  k <- grep("bajo_medio_bajo", names(coef(fit)), value = TRUE)
  c(est = unname(coef(fit)[k]), se = unname(cl_se(fit, dd$com)[k]))
}

res <- list()
res[["sin_ajuste"]] <- fit_fe(W, W$y2 - W$y1)
fit_c <- lm(I(y2 - y1) ~ grupo + dcob + factor(com), W, weights = w)
k <- grep("bajo_medio_bajo", names(coef(fit_c)), value = TRUE)
res[["control_cambio_cobertura"]] <- c(est = unname(coef(fit_c)[k]), se = unname(cl_se(fit_c, W$com)[k]))
for (sg in c(40, 50)) {
  b <- lee_bias(W$p, sg)
  res[[paste0("peor_caso_todos_sigma", sg)]] <- fit_fe(W, (W$y2 - b) - W$y1)
  b_lo <- fifelse(W$grupo == "gse_bajo_medio_bajo", b, 0)
  res[[paste0("peor_caso_solo_bajo_sigma", sg)]] <- fit_fe(W, (W$y2 - b_lo) - W$y1)
}
out <- data.table(spec = names(res), est = sapply(res, `[`, "est"), se = sapply(res, `[`, "se"))
save_table(out, "cob_gradiente_ajustado.csv")

message("\nCobertura SIMCE II medio por grupo:")
print(dcast(cob_grp[, .(grupo, year, v = round(cobertura, 3))], grupo ~ year, value.var = "v"))
message("\nVentaja de GSE bajo frente a GSE medio alto y alto (FE comuna), 2018-2022:")
print(out[, .(spec, est = round(est, 1), se = round(se, 1))])
