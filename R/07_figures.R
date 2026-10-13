# =============================================================================
# 07_figures.R
# Figura 1: puntaje de Chile por cuartil de ESCS, 2015-2025.
# Figura 2: brecha Q4-Q1 de Chile frente al benchmark, con IC 95%.
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))
dir.create(PATHS$figures, recursive = TRUE, showWarnings = FALSE)

lv <- fread(file.path(PATHS$tables, "chl_quartiles_levels.csv"))
dom <- SPEC$primary_domain

q <- lv[domain == dom & grepl("^Q[0-9]$", stat)]
p1 <- ggplot(q, aes(cycle, est, colour = stat, group = stat)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0.4) +
  scale_x_continuous(breaks = as.integer(CYCLES)) +
  labs(x = NULL, y = "Puntaje medio", colour = "Cuartil ESCS",
       title = paste("Chile:", dom, "por cuartil socioeconómico"),
       caption = "Cuartiles dentro de país y ciclo. IC 95% con BRR y valores plausibles.") +
  theme_minimal(base_size = 11)
ggsave(file.path(PATHS$figures, paste0("chl_quartiles_", dom, ".pdf")), p1,
       width = 7, height = 4.5)

f_b <- file.path(PATHS$tables, "benchmark_levels.csv")
if (file.exists(f_b)) {
  gap_stat <- paste0("Q", SPEC$n_groups, "-Q1")
  b <- fread(f_b)[domain == dom & stat == gap_stat, .(cycle, est, se, serie = "Benchmark OCDE")]
  c <- lv[domain == dom & stat == gap_stat, .(cycle, est, se, serie = "Chile")]
  g <- rbind(c, b)
  p2 <- ggplot(g, aes(cycle, est, colour = serie)) +
    geom_line() + geom_point() +
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0.4) +
    scale_x_continuous(breaks = as.integer(CYCLES)) +
    labs(x = NULL, y = "Brecha Q4 - Q1 (puntos)", colour = NULL,
         title = paste("Brecha socioeconómica en", dom)) +
    theme_minimal(base_size = 11)
  ggsave(file.path(PATHS$figures, paste0("gap_chl_vs_bench_", dom, ".pdf")), p2,
         width = 7, height = 4.5)
}
message("Figuras en ", PATHS$figures)
