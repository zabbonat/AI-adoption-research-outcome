# Figure del manoscritto v4. Nessun titolo o sottotitolo dentro l'immagine: i pannelli
# portano solo la lettera identificativa, gli assi e la legenda. La descrizione sta nella
# caption LaTeX.
rm(list = ls()); options(scipen = 999)
suppressMessages({ library(tidyverse); library(ggplot2); library(gridExtra); library(grid) })
BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
FIG  <- file.path("output", "figures")
ev <- readRDS(file.path(FIX, "event_v4_results.rds")); tab <- ev$tab

tema <- theme_classic(base_size = 9) +
  theme(plot.title = element_text(face = "bold", size = 10, hjust = 0, margin = margin(b = 2)),
        legend.position = "bottom", legend.title = element_blank(),
        legend.margin = margin(t = -2), legend.key.height = unit(8, "pt"),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        axis.title = element_text(size = 8.5))
esw <- function(tg, y) (tab %>% filter(tag == tg, outcome == y))$es[[1]] %>%
  mutate(lo = att - 1.96 * se, hi = att + 1.96 * se)

# ---------- Figura 2: output lordo e netto ----------
d2 <- bind_rows(esw("C", "log_works") %>% mutate(serie = "All papers"),
                esw("C", "log_net") %>% mutate(serie = "Net of transformer-citing papers"))
p2a <- ggplot(d2, aes(e, att, colour = serie, shape = serie)) +
  annotate("rect", xmin = -0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.07, fill = "black") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.16, position = position_dodge(0.45), linewidth = 0.4) +
  geom_point(position = position_dodge(0.45), size = 1.9) +
  scale_colour_manual(values = c("black", "grey58")) + scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(breaks = -6:3) +
  labs(title = "a", x = "Years since adoption", y = "Effect on log annual output") + tema
d2b <- esw("C", "log_works") %>% dplyr::select(e, g = att) %>%
  inner_join(esw("C", "log_net") %>% dplyr::select(e, n = att), by = "e") %>% mutate(diff = g - n)
p2b <- ggplot(d2b, aes(e, diff)) +
  annotate("rect", xmin = -0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.07, fill = "black") +
  geom_hline(yintercept = 0, linewidth = 0.3) + geom_col(fill = "grey35", width = 0.62) +
  scale_x_continuous(breaks = -6:3) +
  labs(title = "b", x = "Years since adoption", y = "Contribution of transformer-citing papers") +
  tema + theme(legend.position = "none")
pdf(file.path(FIG, "fig2_event_output.pdf"), width = 7.0, height = 3.0)
grid.arrange(p2a, p2b, ncol = 2, widths = c(1.3, 1))
dev.off()

# ---------- Figura 3: citazioni e coda ----------
pan <- function(y, lettera, ylab) {
  d <- esw("C", y)
  ggplot(d, aes(e, att)) +
    annotate("rect", xmin = -2.5, xmax = -0.5, ymin = -Inf, ymax = Inf, alpha = 0.11, fill = "grey40") +
    annotate("rect", xmin = -0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.07, fill = "black") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.16, linewidth = 0.4) +
    geom_point(size = 1.9) + scale_x_continuous(breaks = seq(-6, 2, 2)) +
    labs(title = lettera, x = "Years since adoption", y = ylab) +
    tema + theme(legend.position = "none")
}
pdf(file.path(FIG, "fig3_event_citations.pdf"), width = 7.0, height = 2.7)
grid.arrange(pan("citation_three_years", "a", "Effect on three-year citations per paper"),
             pan("top10_fw", "b", "Effect on top-decile papers"),
             pan("top1_fw", "c", "Effect on top-percentile papers"), ncol = 3)
dev.off()

# ---------- Figura 4: i tre bacini ----------
lab4 <- c(citation_three_years = "Three-year citations per paper", top10_fw = "Top-decile papers",
          top1_fw = "Top-percentile papers", log_works = "Log annual output",
          log_net = "Log output, net of transformer papers")
pool_lab <- c(A = "A: all non-adopters", B = "B: never active in AI", C = "C: active in AI")
pan4 <- function(y, lettera, mostra_y) {
  d <- tab %>% filter(tag %in% c("A", "B", "C"), outcome == y) %>%
    mutate(pool = factor(pool_lab[tag], levels = rev(pool_lab)),
           lo = att_post - 1.96 * se_post, hi = att_post + 1.96 * se_post)
  p <- ggplot(d, aes(att_post, pool)) + geom_vline(xintercept = 0, linewidth = 0.3) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16, linewidth = 0.4) +
    geom_point(size = 1.9) +
    labs(title = lettera, x = lab4[[y]], y = NULL) +
    tema + theme(legend.position = "none", panel.grid.major.y = element_blank(),
                 plot.margin = margin(4, 8, 4, 4))
  if (mostra_y) p + theme(axis.text.y = element_text(size = 7.8))
  else p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}
pdf(file.path(FIG, "fig4_pools_event.pdf"), width = 7.2, height = 3.6)
grid.arrange(pan4("citation_three_years", "a", TRUE), pan4("top10_fw", "b", FALSE),
             pan4("top1_fw", "c", FALSE), pan4("log_works", "d", TRUE),
             pan4("log_net", "e", FALSE),
             layout_matrix = rbind(c(1, 2, 3), c(4, 5, NA)), widths = c(1.55, 1, 1))
dev.off()

# ---------- Figura S: Rambachan-Roth ----------
labr <- c(log_works = "Log annual output", log_net = "Log output, net of transformer papers",
          top10_fw = "Top-decile papers", top10_fw_net = "Top-decile papers, net")
panr <- function(y, lettera) {
  r <- tab %>% filter(tag == "C", outcome == y)
  g <- r$grid[[1]] %>% mutate(est = r$att_post)
  ggplot(g, aes(Mbar)) + geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey78", alpha = 0.65) +
    geom_line(aes(y = est), linewidth = 0.45) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3) +
    scale_x_continuous(breaks = seq(0, 2, 0.5)) +
    labs(title = lettera, x = expression(bar(M)), y = labr[[y]]) +
    tema + theme(legend.position = "none")
}
pdf(file.path(FIG, "figS_rr.pdf"), width = 7.2, height = 2.2)
grid.arrange(panr("log_works", "a"), panr("log_net", "b"),
             panr("top10_fw", "c"), panr("top10_fw_net", "d"), ncol = 4)
dev.off()
cat("figure scritte in", FIG, "\n")
