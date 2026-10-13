# =============================================================================
# 17_pisa_pantallas_explorar.R
# Paso previo a la prueba de pantallas y distracción con PISA: lista, para
# cada ciclo, las variables del archivo de estudiantes cuya etiqueta habla de
# dispositivos digitales, uso recreativo, redes sociales, videojuegos o
# distracción. Las preguntas TIC cambian entre ciclos, así que antes de
# estimar hay que ver qué se puede comparar.
#
# Lee solo los encabezados de los .sav (no carga datos): tarda segundos.
# Salida: output/tables/pisa_ict_variables.csv
# =============================================================================

suppressPackageStartupMessages(library(haven))

PATRON <- paste(c("distract", "leisure", "digital", "device", "video game", "gaming",
                  "social (media|network)", "chat", "brows", "internet", "smartphone",
                  "phone", "screen", "ICT", "online", "entertain", "for fun"),
                collapse = "|")

lab_of <- function(x) {
  l <- attr(x, "label"); if (is.null(l)) "" else as.character(l)
}
val_of <- function(x) {
  v <- attr(x, "labels"); if (is.null(v)) return("")
  paste(paste0(unname(v), "=", names(v)), collapse = "; ")
}

out <- list()
for (cy in CYCLES) {
  f <- file.path(PATHS$raw, FILES$stu[[cy]])
  h <- read_sav(f, n_max = 0)
  labs <- vapply(h, lab_of, character(1))
  hit  <- which(grepl(PATRON, labs, ignore.case = TRUE) |
                grepl("^(IC|ST32[0-9]|ST27[0-9]|ENTUSE|HOMESCH|USESCH|ICT)", names(h)))
  out[[cy]] <- data.table(cycle = cy, var = names(h)[hit], label = labs[hit],
                          values = vapply(h[hit], val_of, character(1)))
  message(cy, ": ", length(hit), " variables")
}
res <- rbindlist(out)
save_table(res, "pisa_ict_variables.csv")
message("Listo: output/tables/pisa_ict_variables.csv")
