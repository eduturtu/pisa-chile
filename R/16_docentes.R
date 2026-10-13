# =============================================================================
# 16_docentes.R
# ¿Explica la rotación docente la caída de los colegios efectivos y el
# gradiente socioeconómico en SIMCE II medio 2018-2022?
#
# Datos: Mineduc, "Cargos docentes" (Directorio Docentes 2016-2022, corte a
# junio de cada año; descargado 8-10-2026). Se conservan docentes de aula
# (ID_IFP = 1) que hacen clases en media jóvenes (COD_ENS 310-910). MRUN es
# el identificador anonimizado del docente, estable entre años.
#
# Medidas por colegio:
#   ret_19_21: fracción de los docentes de media de 2019 que siguen en el
#              mismo colegio en 2021 (dos años, plena pandemia)
#   ret_16_18: lo mismo entre 2016 y 2018 (dos años, antes de la pandemia)
#   d_ret:     ret_19_21 - ret_16_18 (rotación anormal del colegio)
#   edad y experiencia promedio en 2019
#
# Pruebas (FE comuna, e.e. por comuna, ponderado por alumnos):
#   A. Retención por grupo y por categoría 2017.
#   B. Cambio SIMCE 2018-2022 ~ grupo + categoría 2017 [+ medidas docentes]:
#      ¿cuánto cambian los coeficientes de grupo y de categoría Alto?
#   C. Placebos: la retención 2019-2021 no debería predecir el cambio
#      2016-2018; la retención 2016-2018 sí puede (es contemporánea).
# =============================================================================

if (!exists("S")) source("R/10_simce.R", local = TRUE)
if (!exists("cl_se")) source("R/12_gradiente_comuna.R", local = TRUE)

DOC_FILE <- file.path(PATHS$raw, "docentes", "docentes_media.csv")
CDM_FILE <- file.path(PATHS$raw, "categorias", "cdm_media.csv")
Dd <- fread(DOC_FILE)
K  <- fread(CDM_FILE)[year == 2017 & categoria %in% c("ALTO", "MEDIO", "MEDIO-BAJO", "INSUFICIENTE"),
                      .(rbd, cat = categoria)]

retention <- function(a, b) {
  base <- Dd[year == a, .(rbd, mrun)]
  stay <- Dd[year == b, .(rbd, mrun, stay = 1L)]
  x <- merge(base, stay, by = c("rbd", "mrun"), all.x = TRUE)
  x[, .(ret = mean(!is.na(stay)), n_doc = .N), by = rbd]
}
r1 <- retention(2019, 2021)[, .(rbd, ret_19_21 = ret, n_doc19 = n_doc)]
r0 <- retention(2016, 2018)[, .(rbd, ret_16_18 = ret)]
r2 <- retention(2019, 2022)[, .(rbd, ret_19_22 = ret)]
perf <- Dd[year == 2019, .(edad19 = mean(edad, na.rm = TRUE), exp19 = mean(exp, na.rm = TRUE)), by = rbd]
T <- Reduce(function(a, b) merge(a, b, by = "rbd", all = TRUE), list(r1, r0, r2, perf))
T[, d_ret := ret_19_21 - ret_16_18]

mk_panel <- function(a, b) {
  x <- dcast(S[year %in% c(a, b) & !is.na(mate) & n_mate > 0 & !is.na(grupo)],
             rbd + grupo ~ year, value.var = c("mate", "n_mate"))
  setnames(x, c("rbd", "grupo", "y1", "y2", "n1", "n2"))
  x <- merge(merge(merge(x[!is.na(y1) & !is.na(y2)], geo, by = "rbd"), K, by = "rbd"), T, by = "rbd")
  x[, `:=`(dy = y2 - y1, w = (n1 + n2) / 2,
           cat = relevel(factor(cat), ref = "MEDIO"),
           grupo = relevel(factor(grupo), ref = "gse_medio_alto_alto"))]
  x[n_doc19 >= 5 & !is.na(d_ret)]
}
P22 <- mk_panel(2018, 2022)
P18 <- mk_panel(2016, 2018)

# --- A. Retención por grupo y categoría --------------------------------------
sumr <- function(dd, byvar, dimname) {
  out <- dd[, .(n = .N, ret_16_18 = weighted.mean(ret_16_18, w), ret_19_21 = weighted.mean(ret_19_21, w),
                d_ret = weighted.mean(d_ret, w), edad19 = weighted.mean(edad19, w, na.rm = TRUE)),
            by = c(byvar)]
  setnames(out, byvar, "nivel"); out[, `:=`(nivel = as.character(nivel), dim = dimname)]
}
desc <- rbind(sumr(P22, "grupo", "grupo"), sumr(P22, "cat", "categoria_2017"))
save_table(desc, "doc_retencion_descriptiva.csv")

# --- B. ¿Explica la rotación el gradiente y la caída de los Alto? -------------
fit_terms <- function(f, dd, label) {
  m <- lm(f, dd, weights = w)
  k <- grep("^grupo|^cat|ret|edad|exp", names(coef(m)), value = TRUE)
  data.table(spec = label, term = k, est = coef(m)[k], se = cl_se(m, dd$com)[k])
}
B <- rbind(
  fit_terms(dy ~ grupo + cat + factor(com), P22, "base"),
  fit_terms(dy ~ grupo + cat + d_ret + factor(com), P22, "+ rotacion_anormal"),
  fit_terms(dy ~ grupo + cat + ret_19_21 + ret_16_18 + factor(com), P22, "+ retenciones"),
  fit_terms(dy ~ grupo + cat + d_ret + edad19 + exp19 + factor(com), P22, "+ rotacion + edad + experiencia"))
B[, z := est / se]
save_table(B, "doc_gradiente.csv")

# --- C. Placebos ----------------------------------------------------------------
C <- rbind(
  fit_terms(dy ~ grupo + cat + ret_19_21 + factor(com), P18, "cambio 2016-2018 ~ ret 2019-2021 (futura)"),
  fit_terms(dy ~ grupo + cat + ret_16_18 + factor(com), P18, "cambio 2016-2018 ~ ret 2016-2018 (contemporanea)"),
  fit_terms(dy ~ grupo + cat + d_ret + factor(com), P18, "cambio 2016-2018 ~ rotacion anormal 2019-2021"))
C <- C[grepl("ret", term)]
save_table(C, "doc_placebo.csv")

r1f <- function(x) round(x, 3)
message("\nA. Retención docente por grupo y categoría 2017:"); print(desc[, lapply(.SD, function(v) if (is.numeric(v)) r1f(v) else v)])
message("\nB. Cambio SIMCE 2018-2022 (FE comuna):")
print(dcast(B[, .(term, spec, v = sprintf("%.2f (%.2f)", est, se))], term ~ spec, value.var = "v"))
message("\nC. Placebos:"); print(C[, .(spec, term, est = round(est, 2), se = round(se, 2))])
