# =============================================================================
# 11_alta_exigencia.R
# Hipótesis: la compresión de la brecha en SIMCE 2018-2022 se concentra en
# los liceos selectivos (emblemáticos), afectados por el fin de la selección
# con el Sistema de Admisión Escolar (SAE).
#
# Grupo de tratamiento: los 21 establecimientos de la Región Metropolitana
# que en junio de 2019 obtuvieron la categoría de "alta exigencia" para seguir
# seleccionando en forma decreciente (85%, 70%, 50%, 30%) durante la
# transición al SAE (Emol, 27-6-2019). El RBD se asignó por nombre en la base
# SIMCE 2018; el pareo de "Colegio San Juan Diego" (24423, Recoleta) y
# "Liceo Politécnico Andes" (24482, Renca) es el más probable pero no está
# confirmado con una fuente oficial.
#
# Calendario relevante: el SAE se aplicó en la RM en el proceso 2019 (ingreso
# 2020). Los alumnos de II medio 2022 que entraron en 7° básico (2019)
# fueron seleccionados con el sistema anterior; los que entraron en I medio
# (2021) ya postularon con SAE. Por eso este análisis no identifica un efecto
# causal del SAE.
#
# Salida: output/tables/simce_alta_exigencia.csv y simce_alta_exigencia_rbd.csv
# =============================================================================

AE_RBD <- c(8927, 8928, 8926, 8925, 8930, 25855, 10658, 8487, 9100, 24482,
            11965, 8503, 10726, 24423, 9406, 9078, 10479, 8645, 24498, 8485, 25770)

if (!exists("S")) source("R/10_simce.R", local = TRUE)

S[, ae := rbd %in% AE_RBD]
S[, grupo_ae := fifelse(is.na(grupo), NA_character_,
                        paste0(grupo, fifelse(ae, "_alta_exigencia", "_resto")))]

res <- rbindlist(lapply(SIMCE_TRANS, function(tr) rbind(
  panel_change("mate", tr[1], tr[2], by = "grupo_ae"),
  panel_change("lect", tr[1], tr[2], by = "grupo_ae"))))

# Compresión sin los liceos de alta exigencia
S[, grupo_sin_ae := fifelse(ae, NA_character_, grupo)]
sin <- rbindlist(lapply(SIMCE_TRANS, function(tr) rbind(
  panel_change("mate", tr[1], tr[2], by = "grupo_sin_ae"),
  panel_change("lect", tr[1], tr[2], by = "grupo_sin_ae"))))
sin[, group := paste0(group, "_sin_alta_exigencia")]

out <- rbind(res, sin)
setcolorder(out, c("subject", "from", "to", "group"))
save_table(out, "simce_alta_exigencia.csv")

ind <- dcast(S[ae == TRUE], rbd + dep0 + gse0 ~ year, value.var = "mate")
save_table(ind, "simce_alta_exigencia_rbd.csv")

message("\nSIMCE matemáticas 2018-2022, alta exigencia frente al resto:")
print(out[subject == "mate" & from == 2018 & to == 2022,
          .(group, schools, change = round(change, 1), se = round(se, 1))])
