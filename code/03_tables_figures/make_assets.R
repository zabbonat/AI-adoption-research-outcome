# Figures (PDF, vector) and LaTeX tables for the submission.
# Everything is regenerated from the saved estimation objects, so the numbers in
# the manuscript cannot drift from the numbers in the logs.
rm(list = ls()); options(scipen = 999); set.seed(42)
suppressMessages({ library(tidyverse); library(fixest); library(patchwork) })

BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
SUB  <- "output"
FIGD <- file.path(SUB, "figures"); dir.create(FIGD, showWarnings = FALSE)
TABD <- file.path(SUB, "tables");  dir.create(TABD, showWarnings = FALSE)

th <- theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
        panel.border = element_rect(linewidth = 0.4, colour = "grey40"),
        strip.background = element_rect(fill = "grey95", colour = "grey40", linewidth = 0.4),
        strip.text = element_text(face = "bold", size = 8.5),
        plot.tag = element_text(face = "bold", size = 11),
        legend.key.size = unit(0.9, "lines"))
sv <- function(p, nome, w, h) {
  ggsave(file.path(FIGD, nome), p, width = w, height = h, device = cairo_pdf)
  cat("figure:", nome, "\n")
}

fin_r  <- readRDS(file.path(FIX, "paper_final_results.rds"))
net_r  <- list(prof = fin_r$prof, res = fin_r$main %>% filter(outcome == "log_net"))
md     <- fin_r$md
pools  <- fin_r$pools %>% rename(bacino = spec)

# ---------------- panel, needed for a few figures ----------------
rt <- read_csv(file.path(BASE, "RTransformerSISTEMATO.csv.gz"), show_col_types = FALSE,
               col_select = c(id, publication_year, author_id, field, trans_year))
ap <- rt %>% filter(!is.na(author_id), !is.na(publication_year)) %>%
  mutate(aid_list = str_extract_all(author_id, "A\\d+")) %>%
  dplyr::select(id, publication_year, field, trans_year, aid_list) %>% unnest(aid_list) %>%
  mutate(aid = as.numeric(sub("^A", "", aid_list)))
g_aut <- ap %>% group_by(aid) %>% summarise(g = min(publication_year), .groups = "drop")

df <- read_csv(file.path(BASE, "panel_didi_correctJULY.csv.gz"), show_col_types = FALSE) %>%
  left_join(read_csv(file.path(BASE, "works_fix_author_year.csv.gz"), show_col_types = FALSE),
            by = c("author_id", "publication_year")) %>%
  mutate(citation_three_years = replace_na(citation_three_years, 0),
         works_annual = replace_na(works_annual, 0), works_net = replace_na(works_net, 0),
         log_works = log(works_annual + 1), log_net = log(works_net + 1),
         aid = as.numeric(gsub("https://openalex.org/A", "", author_id, fixed = TRUE)),
         field = as.character(field))
campo <- df %>% filter(!is.na(field)) %>% count(aid, field) %>% group_by(aid) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% dplyr::select(aid, campo = field)
df <- df %>% left_join(campo, by = "aid") %>% left_join(g_aut %>% rename(g_adopt = g), by = "aid")
dd <- df %>% inner_join(md %>% dplyr::select(aid, trattato, w), by = "aid") %>%
  filter(!is.na(campo), campo != "Computer Science")

# ================= FIGURE 1 : diffusion =================
d1 <- ap %>% inner_join(campo, by = "aid") %>% inner_join(g_aut, by = "aid") %>%
  distinct(aid, g, campo) %>% filter(g >= 2018, g <= 2024) %>%
  count(campo, g) %>% group_by(campo) %>% arrange(g) %>% mutate(cum = cumsum(n)) %>% ungroup()
top6 <- d1 %>% group_by(campo) %>% summarise(t = sum(n)) %>% slice_max(t, n = 6) %>% pull(campo)
lab <- c("Computer Science" = "Computer science", "Engineering" = "Engineering",
         "Medicine" = "Medicine",
         "Biochemistry, Genetics and Molecular Biology" = "Biochemistry",
         "Social Sciences" = "Social sciences",
         "Agricultural and Biological Sciences" = "Agricultural sciences")
p1 <- ggplot(d1 %>% filter(campo %in% top6) %>%
               mutate(campo = recode(campo, !!!lab)), aes(g, cum, colour = campo, shape = campo)) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.5) +
  scale_y_log10(labels = scales::comma) + scale_x_continuous(breaks = 2018:2024) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = NULL, y = "Cumulative first-time adopters", colour = NULL, shape = NULL) +
  th + theme(legend.position = "right")
sv(p1, "fig1_diffusion.pdf", 6.6, 3.0)

# ================= FIGURE 2 : gross versus net (the main figure) =================
prof <- net_r$prof %>%
  mutate(outcome = recode(outcome, log_works = "All publications",
                          log_net = "Excluding transformer-citing papers"))
se_l <- net_r$prof %>% dplyr::select(outcome, anno, se) %>%
  mutate(outcome = recode(outcome, log_works = "All publications",
                          log_net = "Excluding transformer-citing papers"))
p2a <- ggplot(prof, aes(anno, att, colour = outcome, shape = outcome, fill = outcome)) +
  annotate("rect", xmin = 2017.5, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "grey95", colour = NA) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = 2016.5, linetype = 2, colour = "grey55", linewidth = 0.3) +
  geom_ribbon(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.6) +
  scale_colour_manual(values = c("All publications" = "#1b3a5c",
                                 "Excluding transformer-citing papers" = "#c0392b")) +
  scale_fill_manual(values = c("All publications" = "#1b3a5c",
                               "Excluding transformer-citing papers" = "#c0392b")) +
  scale_x_continuous(breaks = 2012:2024) +
  labs(x = NULL, y = "Effect on log annual output", colour = NULL, shape = NULL, fill = NULL,
       tag = "a") +
  th + theme(legend.position = "top", legend.direction = "horizontal",
             legend.margin = margin(b = -4), legend.text = element_text(size = 8),
             axis.title.y = element_text(size = 8.5, margin = margin(r = 4)),
             plot.margin = margin(4, 8, 2, 6))

gap <- net_r$prof %>% dplyr::select(outcome, anno, att) %>%
  pivot_wider(names_from = outcome, values_from = att) %>%
  mutate(diff = log_works - log_net)
p2b <- ggplot(gap, aes(anno, diff)) +
  annotate("rect", xmin = 2017.5, xmax = 2024.5, ymin = -Inf, ymax = Inf,
           fill = "grey95", colour = NA) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_col(fill = "#7f8c8d", width = 0.65) +
  scale_x_continuous(breaks = 2012:2024) +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  labs(x = NULL, y = "Contribution of
transformer-citing papers", tag = "b") +
  th + theme(axis.title.y = element_text(size = 8.5, margin = margin(r = 4)),
             plot.margin = margin(2, 8, 4, 6))
p2 <- p2a / p2b + plot_layout(heights = c(2.1, 1))
sv(p2, "fig2_gross_vs_net.pdf", 7.2, 5.2)

# ================= FIGURE 3 : three control pools =================
pl <- pools %>%
  mutate(pool = recode(bacino,
                       "A" = "(A) All non-adopters", "B" = "(B) Never active in AI",
                       "C" = "(C) AI-active non-adopters"),
         outcome = recode(outcome,
                          citation_three_years = "Three-year citations",
                          log_works = "Log annual output",
                          big_hits1 = "Papers in top 1%")) %>%
  mutate(outcome = factor(outcome, levels = c("Three-year citations", "Log annual output",
                                              "Papers in top 1%")),
         pool = factor(pool, levels = rev(c("(A) All non-adopters", "(B) Never active in AI",
                                            "(C) AI-active non-adopters"))))
p3 <- ggplot(pl, aes(att, pool, colour = pre_sig == 0)) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = att - 1.96 * se, xmax = att + 1.96 * se), height = 0.16,
                 linewidth = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  scale_colour_manual(values = c("TRUE" = "#1b5e20", "FALSE" = "grey55"),
                      labels = c("TRUE" = "No pre-trend rejected", "FALSE" = "Pre-trend rejected"),
                      name = NULL) +
  labs(x = "Average post-adoption effect", y = NULL) +
  th + theme(legend.position = "bottom")
sv(p3, "fig3_control_pools.pdf", 6.6, 2.7)

# ================= FIGURE 4 : mean versus tail =================
base_pre <- dd %>% filter(publication_year <= 2017) %>%
  summarise(cit = mean(citation_three_years, na.rm = TRUE),
            b10 = mean(big_hits10, na.rm = TRUE), b1 = mean(big_hits1, na.rm = TRUE))
t4 <- fin_r$main %>% filter(!outcome %in% c("log_works", "log_net")) %>%
  mutate(base = c(fin_r$base$cit, fin_r$base$b10, fin_r$base$b1),
         lab = recode(outcome, citation_three_years = "Mean three-year\ncitations",
                      big_hits1 = "Papers in\ntop 1%", big_hits10 = "Papers in\ntop 10%"),
         pct = 100 * att / base, lo = 100 * (att - 1.96 * se) / base,
         hi = 100 * (att + 1.96 * se) / base) %>%
  mutate(lab = factor(lab, levels = c("Mean three-year\ncitations", "Papers in\ntop 10%",
                                      "Papers in\ntop 1%")))
p4 <- ggplot(t4, aes(lab, pct)) +
  geom_col(fill = "#1b3a5c", width = 0.5) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1, linewidth = 0.4) +
  geom_text(aes(label = sprintf("+%.0f%%", pct)), vjust = -1.4, size = 3) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = NULL, y = "Effect relative to pre-adoption mean (%)") + th
sv(p4, "fig4_tail.pdf", 4.4, 3.0)

# ================= FIGURE 5 : persistence =================
sec <- ap %>% inner_join(g_aut, by = "aid") %>% filter(publication_year > g) %>%
  group_by(aid) %>% summarise(anno2 = min(publication_year), .groups = "drop")
surv <- g_aut %>% left_join(sec, by = "aid") %>% inner_join(campo, by = "aid") %>%
  mutate(side = ifelse(campo == "Computer Science", "Computer science",
                       "Outside computer science")) %>%
  filter(g >= 2019, g <= 2023) %>%
  mutate(fine = pmin(coalesce(anno2, 2024), 2024),
         ev = as.integer(!is.na(anno2) & anno2 <= 2024),
         dur = pmax(1L, as.integer(fine - g))) %>%
  uncount(dur, .id = "t", .remove = FALSE) %>% mutate(y = as.integer(ev == 1L & t == dur)) %>%
  group_by(side, t) %>% summarise(h = mean(y), .groups = "drop") %>%
  group_by(side) %>% mutate(S = cumprod(1 - h)) %>% ungroup() %>% filter(t <= 5)
p5 <- ggplot(surv, aes(t, S, colour = side, shape = side)) +
  geom_line(linewidth = 0.6) + geom_point(size = 1.8) +
  scale_y_continuous(limits = c(0.4, 1), labels = scales::percent) +
  scale_colour_manual(values = c("Computer science" = "grey55",
                                 "Outside computer science" = "#1b3a5c")) +
  labs(x = "Years since first adoption", y = "Share without a second\ntransformer-citing paper",
       colour = NULL, shape = NULL) +
  th + theme(legend.position = "bottom")
sv(p5, "fig5_persistence.pdf", 4.4, 3.2)

# ================= TABLES =================
w <- function(x, f) { writeLines(x, file.path(TABD, f)); cat("table:", f, "\n") }
fmt <- function(v, d = 3) formatC(v, format = "f", digits = d, big.mark = ",")
st <- function(e, s) { t <- abs(e / s); if (t > 2.576) "$^{***}$" else if (t > 1.96) "$^{**}$"
                       else if (t > 1.645) "$^{*}$" else "" }

# Table 1: main results, final specification
f <- fin_r$main %>%
  mutate(lab = recode(outcome,
                      citation_three_years = "Three-year citations",
                      log_works = "Log annual output (all papers)",
                      log_net   = "\\;\\;\\emph{net of transformer-citing papers}",
                      big_hits1 = "Papers in top 1\\%",
                      big_hits10 = "Papers in top 10\\%")) %>%
  slice(match(c("citation_three_years", "big_hits10", "big_hits1", "log_works", "log_net"),
              outcome))
w(c("\\begin{tabular}{lccccc}", "\\toprule",
    "Outcome & Estimate & SE & 95\\% CI & $\\bar{M}$ & Pre-trends \\\\",
    "\\midrule",
    paste0(f$lab, " & ", fmt(f$att, 4), sapply(seq_len(nrow(f)), function(i) st(f$att[i], f$se[i])),
           " & ", fmt(f$se, 4), " & [", fmt(f$att - 1.96 * f$se, 3), ", ",
           fmt(f$att + 1.96 * f$se, 3), "] & ",
           ifelse(is.na(f$bd), "$>$2", fmt(f$bd, 2)), " & ", f$pre_sig, "/", f$npre, " \\\\"),
    "\\bottomrule", "\\end{tabular}"), "tab1_main.tex")

# Table 2: three control pools
p2t <- pools %>% mutate(
  pool = recode(bacino, "A" = "(A) All non-adopters", "B" = "(B) Never active in AI",
                "C" = "(C) AI-active non-adopters"),
  out = recode(outcome, citation_three_years = "Three-year citations",
               log_works = "Log annual output", big_hits1 = "Papers in top 1\\%"))
tab2 <- p2t %>% dplyr::select(pool, out, att, se, bd, pre_sig) %>%
  mutate(cell = paste0(fmt(att, 3), " (", fmt(se, 3), ")")) %>%
  dplyr::select(pool, out, cell) %>% pivot_wider(names_from = out, values_from = cell)
mb <- p2t %>% dplyr::select(pool, out, bd) %>% pivot_wider(names_from = out, values_from = bd)
ps <- p2t %>% group_by(pool) %>% summarise(ps = paste0(sum(pre_sig), "/10"), .groups = "drop")
w(c("\\begin{tabular}{lcccc}", "\\toprule",
    "Control pool & Three-year citations & Log annual output & Papers in top 1\\% & Pre-trends \\\\",
    "\\midrule",
    paste0(tab2$pool, " & ", tab2$`Three-year citations`, " & ", tab2$`Log annual output`,
           " & ", tab2$`Papers in top 1\\%`, " & ", ps$ps, " \\\\"),
    "\\bottomrule", "\\end{tabular}"), "tab2_pools.tex")

# Table 3: covariate balance in the final specification
v <- c("works_pre", "cit_pre", "h_pre", "eta_pre", "slope_pre", "ai_anni", "ai_quota")
vl <- c("Annual publications, 2012--2017", "Three-year citations, 2012--2017",
        "$H$-index in 2017", "Academic age in 2017", "Pre-adoption output slope",
        "Years with AI activity before 2018", "Share of years with AI activity")
bal <- map_dfr(seq_along(v), function(i) {
  x <- md[[v[i]]]
  tibble(var = vl[i], tr = mean(x[md$trattato == 1]), ct = mean(x[md$trattato == 0]),
         d = (mean(x[md$trattato == 1]) - mean(x[md$trattato == 0])) / sd(x))
})
w(c("\\begin{tabular}{lccc}", "\\toprule",
    "Pre-adoption characteristic & Adopters & Matched controls & Std.\\ diff. \\\\",
    "\\midrule",
    paste0(bal$var, " & ", fmt(bal$tr, 2), " & ", fmt(bal$ct, 2), " & ",
           fmt(bal$d, 3), " \\\\"),
    "\\midrule",
    paste0("Authors & ", format(sum(md$trattato), big.mark = ","), " & ",
           format(sum(md$trattato == 0), big.mark = ","), " & \\\\"),
    "\\bottomrule", "\\end{tabular}"), "tab3_balance.tex")

# Table 4 is produced separately and kept as tab4_robustness.tex

# Table S1: persistence
pers <- g_aut %>% left_join(sec, by = "aid") %>% inner_join(campo, by = "aid") %>%
  mutate(side = ifelse(campo == "Computer Science", "Computer science", "Outside computer science"))
np <- ap %>% group_by(aid) %>% summarise(np = n_distinct(id), .groups = "drop")
pt <- pers %>% inner_join(np, by = "aid") %>% group_by(side) %>%
  summarise(n = n(), one = mean(np == 1), two = mean(np >= 2), three = mean(np >= 3),
            .groups = "drop")
s5 <- surv %>% filter(t == 5) %>% dplyr::select(side, S)
pt <- pt %>% left_join(s5, by = "side")
w(c("\\begin{tabular}{lccccc}", "\\toprule",
    "& Adopters & One paper only & $\\geq$2 papers & $\\geq$3 papers & No second paper after 5 yrs \\\\",
    "\\midrule",
    paste0(pt$side, " & ", format(pt$n, big.mark = ","), " & ",
           fmt(100 * pt$one, 1), "\\% & ", fmt(100 * pt$two, 1), "\\% & ",
           fmt(100 * pt$three, 1), "\\% & ", fmt(100 * pt$S, 1), "\\% \\\\"),
    "\\bottomrule", "\\end{tabular}"), "tabS1_persistence.tex")

cat("\nDONE\n")
