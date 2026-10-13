# =============================================================================
# 06_validate.R
# Compara la serie propia de Chile (matemáticas, ESCS publicado) con las
# cifras de Cobreros et al. (2026). Si no coinciden dentro de la tolerancia,
# no se interpreta nada más: primero hay que encontrar la diferencia.
# Fuentes posibles de desvío: regla de corte de cuartiles, desempate de
# valores repetidos, tratamiento de missing o versión del archivo OCDE.
# =============================================================================

lv <- fread(file.path(PATHS$tables, "chl_quartiles_levels.csv"),
            colClasses = list(character = "cycle"))
lv <- lv[domain == "math"]

check <- rbind(
  data.table(item = paste("brecha", names(VALIDATION$chl_math_gap_published)),
             ref  = VALIDATION$chl_math_gap_published,
             own  = lv[stat == "Q4-Q1"][match(names(VALIDATION$chl_math_gap_published), cycle), est]),
  data.table(item = paste("2022", names(VALIDATION$chl_math_q_2022)),
             ref  = VALIDATION$chl_math_q_2022,
             own  = lv[cycle == "2022"][match(names(VALIDATION$chl_math_q_2022), stat), est]),
  data.table(item = paste("2025", names(VALIDATION$chl_math_q_2025)),
             ref  = VALIDATION$chl_math_q_2025,
             own  = lv[cycle == "2025"][match(names(VALIDATION$chl_math_q_2025), stat), est])
)
check[, `:=`(diff = round(own - ref, 2),
             pass = abs(own - ref) <= VALIDATION$tolerance)]
save_table(check, "validation_cobreros.csv")
print(check)
if (all(check$pass)) {
  message("\nValidación aprobada: la serie reproduce a Cobreros et al. (2026).")
} else {
  message("\nValidación NO aprobada en ", sum(!check$pass), " de ", nrow(check),
          " cifras. Revisar antes de seguir.")
}
