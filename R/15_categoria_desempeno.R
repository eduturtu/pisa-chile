# =============================================================================
# 15_categoria_desempeno.R
# Efectividad previa (Categoría de Desempeño de educación media, Agencia de
# Calidad) y gradiente SIMCE. Prueba la hipótesis de Kuzmanic et al.: los
# colegios más efectivos antes de la pandemia perdieron más.
#
# Datos: CDM2017, CDM2018 y CDM2019 (portal de la Agencia, descargados
# 8-10-2026). Solo traen la categoría (Alto, Medio, Medio-bajo,
# Insuficiente), no el índice continuo: no permiten un RD.
#
# Regresión a la media: la categoría 2019 usa resultados hasta 2018, así que
# su relación con el cambio 2018-2022 incluye reversión mecánica del ruido de
# 2018. La categoría 2017 usa datos hasta 2016: no comparte ruido con 2018 ni
# 2022. Diseño:
#   - categoría 2017, cambio 2016-2018: placebo (antes de la pandemia)
#   - categoría 2017, cambio 2018-2022: efecto fuera de muestra
#   - categoría 2019, cambio 2018-2022: referencia, con reversión mecánica
# Referencia: categoría Medio y GSE medio alto y alto. FE de comuna.
# =============================================================================

if (!exists("S")) source("R/10_simce.R", local = TRUE)
if (!exists("cl_se")) source("R/12_gradiente_comuna.R", local = TRUE)

CDM_FILE <- file.path(PATHS$raw, "categorias", "cdm_media.csv")
K <- fread(CDM_FILE)[categoria %in% c("ALTO", "MEDIO", "MEDIO-BAJO", "INSUFICIENTE")]

run_cat <- function(cy, a, b) {
  x <- dcast(S[year %in% c(a, b) & !is.na(mate) & n_mate > 0 & !is.na(grupo)],
             rbd + grupo ~ year, value.var = c("mate", "n_mate"))
  setnames(x, c("rbd", "grupo", "y1", "y2", "n1", "n2"))
  x <- merge(merge(x[!is.na(y1) & !is.na(y2)], K[year == cy, .(rbd, cat = categoria)], by = "rbd"),
             geo, by = "rbd")
  x[, `:=`(dy = y2 - y1, w = (n1 + n2) / 2,
           cat = relevel(factor(cat), ref = "MEDIO"),
           grupo = relevel(factor(grupo), ref = "gse_medio_alto_alto"))]
  f0 <- lm(dy ~ grupo + factor(com), x, weights = w)
  f1 <- lm(dy ~ cat + grupo + factor(com), x, weights = w)
  k0 <- grep("^grupo", names(coef(f0)), value = TRUE)
  k1 <- grep("^cat|^grupo", names(coef(f1)), value = TRUE)
  rbind(data.table(modelo = "sin_categoria", term = k0, est = coef(f0)[k0], se = cl_se(f0, x$com)[k0]),
        data.table(modelo = "con_categoria", term = k1, est = coef(f1)[k1], se = cl_se(f1, x$com)[k1]))[
    , `:=`(categoria_de = cy, cambio = paste(a, b, sep = "-"), n = nrow(x))]
}

res <- rbind(run_cat(2017, 2016, 2018), run_cat(2017, 2018, 2022), run_cat(2019, 2018, 2022))
setcolorder(res, c("categoria_de", "cambio", "modelo", "term"))
save_table(res, "cat_desempeno_gradiente.csv")

message("\nCategoría de desempeño y cambio SIMCE matemáticas (FE comuna, e.e. por comuna):")
print(dcast(res[, .(term = paste(modelo, term), col = paste0("cat", categoria_de, " ", cambio),
                    v = sprintf("%.1f (%.1f)", est, se))], term ~ col, value.var = "v"))
