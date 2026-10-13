# =============================================================================
# 01_extract.R
# Lee los archivos .sav de la OCDE y guarda un extracto liviano por ciclo.
# - Se conservan todos los países (el benchmark los necesita).
# - Los códigos de missing definidos por usuario en SPSS (valid skip, N/A,
#   invalid, no response) quedan como NA.
# - Los nombres de variables se pasan a mayúsculas para homogeneizar ciclos.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(haven)
})

STU_VARS <- function() c(
  "CNT", "CNTSCHID", "CNTSTUID", "OECD", "STRATUM",
  "W_FSTUWT", rep_cols(),
  unlist(lapply(names(DOMAINS), pv_cols)),
  "ESCS", "HOMEPOS", "HISEI", "PAREDINT", "IMMIG", "GRADE", "ST004D01T"
)
SCH_VARS <- c("CNT", "CNTSCHID", "SCHLTYPE")

# Lee solo las columnas pedidas, sin importar mayúsculas o minúsculas.
read_cols <- function(path, wanted) {
  header <- names(read_sav(path, n_max = 0))
  hit    <- header[toupper(header) %in% wanted]
  miss   <- setdiff(wanted, toupper(header))
  if (length(miss)) message("  no están en ", basename(path), ": ",
                            paste(miss, collapse = ", "))
  d <- read_sav(path, col_select = all_of(hit), user_na = FALSE)
  d <- as.data.table(zap_labels(d))
  setnames(d, toupper(names(d)))
  for (v in intersect(c("CNT", "STRATUM"), names(d))) d[, (v) := as.character(get(v))]
  d
}

extract_cycle <- function(cycle) {
  f_stu <- file.path(PATHS$raw, FILES$stu[[cycle]])
  out   <- file.path(PATHS$interim, paste0("stu_", cycle, ".rds"))
  if (file.exists(out)) { message(cycle, ": ya existe, se omite"); return(invisible()) }
  if (!file.exists(f_stu)) stop("Falta ", f_stu)
  message(cycle, ": leyendo estudiantes")
  d <- read_cols(f_stu, STU_VARS())
  d[, CYCLE := cycle]
  saveRDS(d, out, compress = FALSE)

  f_sch <- file.path(PATHS$raw, FILES$sch[[cycle]])
  if (file.exists(f_sch)) {
    message(cycle, ": leyendo escuelas")
    s <- read_cols(f_sch, SCH_VARS)
    saveRDS(s, file.path(PATHS$interim, paste0("sch_", cycle, ".rds")))
  } else {
    message(cycle, ": sin archivo de escuelas (04_school_type.R lo necesita)")
  }
  invisible()
}

dir.create(PATHS$interim, recursive = TRUE, showWarnings = FALSE)
for (cy in CYCLES) extract_cycle(cy)
