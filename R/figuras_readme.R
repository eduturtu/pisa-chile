# =============================================================================
# figuras_readme.R
# Figuras PNG para el README, a partir de las tablas de output/tables.
# No requiere microdatos: basta con haber corrido 02, 05 y 10.
#   fig_pisa_cuartiles.png   Chile, matemáticas por cuartil de ESCS, 2015-2025
#   fig_pisa_brecha.png      brecha Q4-Q1 de Chile frente al benchmark OCDE
#   fig_simce_grupos.png     SIMCE II medio, matemáticas por grupo, 2016-2025
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
dir.create(PATHS$figures, recursive = TRUE, showWarnings = FALSE)

tema <- theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title.position = "plot")
guardar <- function(p, nombre)
  ggsave(file.path(PATHS$figures, nombre), p, width = 7, height = 4.5, dpi = 200, bg = "white")

dom <- SPEC$primary_domain
lv  <- fread(file.path(PATHS$tables, "chl_quartiles_levels.csv"))

# --- PISA por cuartil ----------------------------------------------------------
q <- lv[domain == dom & grepl("^Q[0-9]$", stat)]
p1 <- ggplot(q, aes(cycle, est, colour = stat, group = stat)) +
  annotate("rect", xmin = 2018, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0.4) +
  scale_x_continuous(breaks = as.integer(CYCLES)) +
  scale_colour_manual(values = c(Q1 = "#9ecae1", Q2 = "#6baed6", Q3 = "#3182bd", Q4 = "#08519c")) +
  labs(x = NULL, y = "Puntaje medio", colour = "Cuartil de ESCS",
       title = "PISA matemáticas, Chile, por cuartil socioeconómico",
       caption = "Cuartiles dentro de país y ciclo. IC 95% con BRR (Fay) y 10 valores plausibles.") +
  tema
guardar(p1, "fig_pisa_cuartiles.png")

# --- Brecha Chile frente al benchmark ------------------------------------------
gap_stat <- paste0("Q", SPEC$n_groups, "-Q1")
b <- fread(file.path(PATHS$tables, "benchmark_levels.csv"))[
  domain == dom & stat == gap_stat, .(cycle, est, se, serie = "Benchmark OCDE (35 sistemas)")]
g <- rbind(lv[domain == dom & stat == gap_stat, .(cycle, est, se, serie = "Chile")], b)
p2 <- ggplot(g, aes(cycle, est, colour = serie)) +
  annotate("rect", xmin = 2018, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0.4) +
  scale_x_continuous(breaks = as.integer(CYCLES)) +
  scale_colour_manual(values = c("Chile" = "#c0392b", "Benchmark OCDE (35 sistemas)" = "#7f8c8d")) +
  labs(x = NULL, y = "Brecha Q4 - Q1 (puntos)", colour = NULL,
       title = "Brecha socioeconómica en PISA matemáticas",
       caption = "Benchmark: promedio simple de sistemas OCDE con datos en los cuatro ciclos. IC 95%.") +
  tema
guardar(p2, "fig_pisa_brecha.png")

# --- SIMCE II medio por grupo ---------------------------------------------------
etiquetas <- c(gse_bajo_medio_bajo = "GSE bajo y medio bajo",
               gse_medio           = "GSE medio",
               gse_medio_alto_alto = "GSE medio alto y alto (no pagados)",
               particular_pagado   = "Particular pagado")
s <- fread(file.path(PATHS$tables, "simce_levels.csv"))[subject == "mate" & group %in% names(etiquetas)]
s[, grupo := factor(etiquetas[group], levels = etiquetas)]
p3 <- ggplot(s, aes(year, mean, colour = grupo, group = interaction(grupo, year > 2018))) +
  annotate("rect", xmin = 2018, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_line() + geom_point() +
  geom_line(data = s[year %in% c(2018, 2022)], aes(group = grupo), linetype = "dotted") +
  scale_x_continuous(breaks = sort(unique(s$year))) +
  scale_colour_manual(values = c("#9ecae1", "#4292c6", "#08519c", "#7f8c8d")) +
  labs(x = NULL, y = "Puntaje medio", colour = NULL,
       title = "SIMCE II medio matemáticas por grupo del establecimiento",
       caption = paste("Grupos fijados con dependencia y GSE de 2018. Promedio ponderado por alumnos.",
                       "Sin SIMCE II medio en 2019 a 2021.", sep = "\n")) +
  tema + guides(colour = guide_legend(nrow = 2))
guardar(p3, "fig_simce_grupos.png")

message("Figuras del README en ", PATHS$figures)
