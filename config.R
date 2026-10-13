# =============================================================================
# config.R
# Parámetros del proyecto. Todo lo que pueda cambiar entre corridas vive aquí.
# =============================================================================

# --- Rutas -------------------------------------------------------------------
# Se asume que el proyecto se corre desde la raíz del repositorio.
PATHS <- list(
  raw     = "data/raw",       # archivos .sav originales de la OCDE (no se versionan)
  interim = "data/interim",   # extractos .rds generados por 01_extract.R
  tables  = "output/tables",
  figures = "output/figures"
)

# --- Archivos originales por ciclo -------------------------------------------
# Nombres tal como los publica la OCDE. Descargar desde
# https://www.oecd.org/en/about/programmes/pisa/pisa-data.html
# y dejarlos en data/raw/. Los de escuela solo se usan en 04_school_type.R.
# VERIFICAR el nombre del archivo de escuela 2025 al descargarlo.
FILES <- list(
  stu = c(
    "2015" = "CY6_MS_CMB_STU_QQQ.sav",
    "2018" = "CY07_MSU_STU_QQQ.sav",
    "2022" = "CY08MSP_STU_QQQ.SAV",
    "2025" = "CY09_MS_STU_PUF.sav"
  ),
  sch = c(
    "2015" = "CY6_MS_CMB_SCH_QQQ.sav",
    "2018" = "CY07_MSU_SCH_QQQ.sav",
    "2022" = "CY08MSP_SCH_QQQ.SAV",
    "2025" = "CY09_MS_SCH_PUF.sav"
  )
)

CYCLES <- c("2015", "2018", "2022", "2025")

# --- Diseño muestral PISA ----------------------------------------------------
N_REP   <- 80     # pesos replicados BRR (W_FSTURWT1-80)
FAY_K   <- 0.5    # factor de Fay
N_PV    <- 10     # valores plausibles por dominio
DOMAINS <- c(math = "MATH", read = "READ", scie = "SCIE")

# --- Especificación principal (fijada antes de mirar resultados) -------------
SPEC <- list(
  country        = "CHL",
  ses_index      = "ESCS",       # índice publicado; el armonizado es la fase 1b
  n_groups       = 4,            # cuartiles dentro de país y ciclo
  tie_seed       = 20261008,     # semilla para desempatar ESCS repetidos
  requantile_rep = TRUE,         # recalcular cuartiles con cada peso replicado
  primary_domain = "math",
  transitions    = list(c("2015", "2018"), c("2018", "2022"),
                        c("2022", "2025"), c("2015", "2025"))
)

# --- Benchmark internacional --------------------------------------------------
# Regla: sistemas con OECD == 1 en 2025, presentes con ESCS y puntajes en los
# cuatro ciclos, excluyendo Chile. Cada sistema pesa lo mismo.
BENCH <- list(
  exclude = c("CHL"),
  # Sistemas que se excluyen por decisión previa (vacío por defecto).
  # Documentar aquí cualquier exclusión y su motivo ANTES de correr 05.
  manual_exclude = character(0)
)

# --- Errores de enlace --------------------------------------------------------
# Solo afectan comparaciones de NIVELES entre ciclos (no brechas dentro de un
# mismo país y ciclo). Completar con los valores de los informes técnicos.
# Formato: LINK_ERROR[["math"]][["2015-2025"]] <- valor
LINK_ERROR <- list(math = list(), read = list(), scie = list())

# --- Valores de validación ----------------------------------------------------
# Chile, matemáticas, ESCS publicado, según Cobreros et al. (2026),
# repositorio Innovamat-Technology/pisa_2025. La primera tarea es reproducirlos.
VALIDATION <- list(
  chl_math_gap_published = c("2015" = 95.3, "2018" = 89.2,
                             "2022" = 69.1, "2025" = 69.7),
  chl_math_q_2022 = c(Q1 = 383.7, Q2 = 403.4, Q3 = 415.2, Q4 = 452.9),
  chl_math_q_2025 = c(Q1 = 377.6, Q2 = 392.0, Q3 = 408.3, Q4 = 447.3),
  tolerance = 0.5
)
