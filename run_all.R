# =============================================================================
# run_all.R
# Corre el análisis completo desde la raíz del repositorio:
#   Rscript run_all.R          (todo)
#   Rscript run_all.R 2        (desde el paso 2, si los extractos ya existen)
# 17_pisa_pantallas_explorar.R y 18_pisa_pantallas.R se corren por separado.
# =============================================================================

source("config.R")
source("R/lib_survey.R")

steps <- c("R/01_extract.R", "R/02_chile_quartiles.R", "R/03_missing_escs.R",
           "R/04_school_type.R", "R/05_benchmark.R", "R/06_validate.R",
           "R/07_figures.R", "R/08_strata.R", "R/09_dependencia.R",
           "R/10_simce.R", "R/11_alta_exigencia.R", "R/12_gradiente_comuna.R",
           "R/13_mecanismos.R", "R/14_cobertura.R", "R/15_categoria_desempeno.R",
           "R/16_docentes.R", "R/figuras_readme.R")

args  <- commandArgs(trailingOnly = TRUE)
start <- if (length(args)) as.integer(args[1]) else 1L

for (i in seq(start, length(steps))) {
  message("\n==== ", steps[i], " ====")
  t0 <- Sys.time()
  source(steps[i], local = new.env())
  message("(", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)")
}
