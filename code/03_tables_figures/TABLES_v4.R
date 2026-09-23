# Tutte le tabelle e le figure del manoscritto v4, da event_v4_results.rds e calendar_v2_results.rds.
rm(list = ls()); options(scipen = 999)
suppressMessages({ library(tidyverse); library(ggplot2) })
BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
TAB  <- file.path("output", "tables")
FIG  <- file.path("output", "figures")
ev <- readRDS(file.path(FIX, "event_v4_results.rds")); tab <- ev$tab; pre_m <- ev$pre_m
CALF <- file.path(FIX, "calendar_v4_results.rds")
cal <- if (file.exists(CALF)) readRDS(CALF) else NULL

lab <- c(citation_three_years = "Three-year citations per paper", top10_fw = "Papers in the top decile",
         top1_fw = "Papers in the top percentile", top10_fw_net = "\\;\\;net of transformer-citing papers",
         top1_fw_net = "\\;\\;net of transformer-citing papers", log_works = "Log annual output",
         log_net = "\\;\\;net of transformer-citing papers")
premean <- c(citation_three_years = pre_m$citation_three_years, top10_fw = pre_m$top10_fw,
             top1_fw = pre_m$top1_fw, top10_fw_net = pre_m$top10_fw_net, top1_fw_net = pre_m$top1_fw_net,
             log_works = NA, log_net = NA)
f3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))
fbd <- function(x) ifelse(is.na(x), "$>$2", sprintf("%.2f", x))
fp <- function(p) ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))
st <- function(e, s) { t <- abs(e / s); ifelse(t > 2.576, "$^{***}$", ifelse(t > 1.96, "$^{**}$", ifelse(t > 1.645, "$^{*}$", ""))) }
ordine <- c("citation_three_years", "top10_fw", "top10_fw_net", "top1_fw", "top1_fw_net", "log_works", "log_net")
riga <- function(r, media = TRUE) {
  pm <- premean[[r$outcome]]
  if (media) sprintf("%s & %s & %s%s & %s & %s%s & %s & %s & %d/%d & %s \\\\", lab[[r$outcome]],
                     ifelse(is.na(pm), "---", sprintf("%.2f", pm)),
                     f3(r$att_simple), st(r$att_simple, r$se_simple), f3(r$se_simple),
                     f3(r$att_post), st(r$att_post, r$se_post), f3(r$se_post), fbd(r$bd), r$pre_sig, r$npre, fp(r$wald_p))
  else sprintf("%s & %s%s & %s & %s%s & %s & %s & %d/%d & %s \\\\", lab[[r$outcome]],
               f3(r$att_simple), st(r$att_simple, r$se_simple), f3(r$se_simple),
               f3(r$att_post), st(r$att_post, r$se_post), f3(r$se_post), fbd(r$bd), r$pre_sig, r$npre, fp(r$wald_p))
}

# ---- Tabella 1 ----
m <- tab %>% filter(tag == "C", outcome %in% ordine) %>% mutate(o = match(outcome, ordine)) %>% arrange(o)
writeLines(c("\\begin{tabular}{lcccccccc}", "\\toprule",
             " & Pre-adoption & \\multicolumn{2}{c}{Average effect} & \\multicolumn{2}{c}{Post-adoption} & & Placebos & Joint \\\\",
             "Outcome & mean & ATT & SE & average & SE & $\\bar{M}$ & rejected & test $p$ \\\\", "\\midrule",
             sapply(seq_len(nrow(m)), function(i) riga(m[i, ])), "\\midrule",
             sprintf("\\multicolumn{9}{l}{\\emph{Estimation sample: %s matched pairs, %s researchers}} \\\\",
                     format(m$n_tr[1], big.mark = ","), format(m$n[1], big.mark = ",")),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tab1_main.tex"))

# ---- Tabella 2: bacini ----
CORE <- c("citation_three_years", "top10_fw", "top1_fw", "log_works", "log_net")
cel <- function(tg, y) { r <- tab %>% filter(tag == tg, outcome == y); sprintf("%s (%s)", f3(r$att_post), f3(r$se_post)) }
cbd <- function(tg, y) { r <- tab %>% filter(tag == tg, outcome == y); sprintf("%s / %s", fbd(r$bd), fp(r$wald_p)) }
np <- function(tg) { r <- tab %>% filter(tag == tg); format(r$n_tr[1], big.mark = ",") }
writeLines(c("\\begin{tabular}{lccc}", "\\toprule", " & (A) All & (B) Never active & (C) Active in AI \\\\",
             "Outcome & non-adopters & in AI & before 2018 \\\\", "\\midrule",
             "\\multicolumn{4}{l}{\\emph{Post-adoption average effect (SE)}} \\\\",
             sapply(CORE, function(y) sprintf("%s & %s & %s & %s \\\\", lab[[y]], cel("A", y), cel("B", y), cel("C", y))),
             "\\addlinespace", "\\multicolumn{4}{l}{\\emph{Breakdown value $\\bar{M}$ / joint placebo test $p$}} \\\\",
             sapply(CORE, function(y) sprintf("%s & %s & %s & %s \\\\", lab[[y]], cbd("A", y), cbd("B", y), cbd("C", y))),
             "\\midrule", sprintf("Matched pairs & %s & %s & %s \\\\", np("A"), np("B"), np("C")),
             sprintf("Largest standardised difference & %.3f & %.3f & %.3f \\\\",
                     max(abs(ev$bal$A$d)), max(abs(ev$bal$B$d)), max(abs(ev$bal$C$d))),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tab2_pools.tex"))

# ---- Tabella bilanciamento ----
etich <- c(works_pre = "Annual publications, 2012--2017", cit_pre = "Three-year citations, 2012--2017",
           h_pre = "$H$-index in 2017", eta_pre = "Academic age in 2017", slope_pre = "Pre-adoption output slope",
           ai_anni = "Years with AI activity before 2018", ai_quota = "Share of years with AI activity")
b <- ev$bal$C
writeLines(c("\\begin{tabular}{lccc}", "\\toprule",
             "Pre-adoption characteristic & Adopters & Matched controls & Std.\\ diff. \\\\", "\\midrule",
             sapply(seq_len(nrow(b)), function(i) sprintf("%s & %.2f & %.2f & %.3f \\\\",
                                                          etich[[b$var[i]]], b$tr[i], b$co[i], b$d[i])),
             "\\midrule", sprintf("Researchers & %s & %s & \\\\",
                                  format(tab$n_tr[1], big.mark = ","), format(tab$n_tr[1], big.mark = ",")),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tab3_balance.tex"))

# ---- Tabella event-time ----
esw <- function(tg, y) (tab %>% filter(tag == tg, outcome == y))$es[[1]]
es_all <- lapply(CORE, function(y) esw("C", y) %>% dplyr::select(e, att, se) %>% mutate(outcome = y))
ee <- sort(unique(unlist(lapply(es_all, function(x) x$e))))
cc <- function(x, e0) { r <- x %>% filter(e == e0); if (nrow(r) == 0) "---" else sprintf("%s (%s)", f3(r$att), f3(r$se)) }
writeLines(c("\\begin{tabular}{lccccc}", "\\toprule",
             "Years since & Three-year & Top & Top & Log annual & Output net of \\\\",
             "adoption & citations & decile & percentile & output & transformer papers \\\\", "\\midrule",
             sapply(ee, function(e0) sprintf("%s & %s \\\\", ifelse(e0 < 0, paste0("$", e0, "$"), as.character(e0)),
                                             paste(sapply(es_all, cc, e0 = e0), collapse = " & "))),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tabS_event.tex"))

# ---- Tabella coorti ----
gw <- function(y) (tab %>% filter(tag == "C", outcome == y))$g[[1]]
gs <- sort(unique(unlist(lapply(CORE, function(y) gw(y)$g))))
writeLines(c("\\begin{tabular}{lccccc}", "\\toprule",
             "Adoption & Three-year & Top & Top & Log annual & Output net of \\\\",
             "cohort & citations & decile & percentile & output & transformer papers \\\\", "\\midrule",
             sapply(gs, function(g0) sprintf("%d & %s \\\\", g0, paste(sapply(CORE, function(y) {
               r <- gw(y) %>% filter(g == g0); if (nrow(r) == 0) "---" else sprintf("%s (%s)", f3(r$att), f3(r$se)) }), collapse = " & "))),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tabS_cohorts.tex"))

# ---- Tabella controlli (anticipazione, biochem, lead, campo) ----
blocco <- function(tg, titolo, ys = CORE) {
  m <- tab %>% filter(tag == tg, outcome %in% ys) %>% mutate(o = match(outcome, ys)) %>% arrange(o)
  c(sprintf("\\multicolumn{8}{l}{\\emph{%s}} \\\\", titolo),
    sapply(seq_len(nrow(m)), function(i) riga(m[i, ], media = FALSE)), "\\addlinespace")
}
nn <- function(tg) { r <- tab %>% filter(tag == tg); sprintf("%s pairs", format(r$n_tr[1], big.mark = ",")) }
writeLines(c("\\begin{tabular}{lccccccc}", "\\toprule",
             " & \\multicolumn{2}{c}{Average effect} & \\multicolumn{2}{c}{Post-adoption} & & Placebos & Joint \\\\",
             "Outcome & ATT & SE & average & SE & $\\bar{M}$ & rejected & test $p$ \\\\", "\\midrule",
             blocco("C_antic0", "Panel A. No anticipation window: effects relative to the year before adoption"),
             blocco("C_noBiochem", sprintf("Panel B. Excluding authors whose modal field is biochemistry (%s)", nn("C_noBiochem"))),
             blocco("lead", sprintf("Panel C. Adopters who are first, last or corresponding authors of their first transformer-citing paper (%s, largest standardised difference %.3f)",
                                    nn("lead"), max(abs(ev$bal$lead$d)))),
             blocco("campo", sprintf("Panel D. Matching within the modal field (%s, %.3f)", nn("campo"), max(abs(ev$bal$campo$d)))),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tabS_checks.tex"))

# ---- Tabella calendar-time ----
if (!is.null(cal)) {
rc <- function(ref0, y) { r <- cal %>% filter(ref == ref0, outcome == y)
  sprintf("%s & %s & %s & %d/%d & %s", f3(r$est), f3(r$se), fbd(r$bd), r$pre_sig, r$npre, fp(r$wald_p)) }
writeLines(c("\\begin{tabular}{lccccc|ccccc}", "\\toprule",
             " & \\multicolumn{5}{c|}{Reference year 2016, pre-period 2014--2015} & \\multicolumn{5}{c}{Reference year 2018, pre-period 2014--2017} \\\\",
             "Outcome & Estimate & SE & $\\bar{M}$ & Pre & $p$ & Estimate & SE & $\\bar{M}$ & Pre & $p$ \\\\", "\\midrule",
             sapply(ordine, function(y) sprintf("%s & %s & %s \\\\", lab[[y]], rc(2016, y), rc(2018, y))),
             "\\bottomrule", "\\end{tabular}"), file.path(TAB, "tabS_calendar.tex"))
}

# ================= figure =================
tema <- theme_classic(base_size = 10) +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold", hjust = 0, size = 9),
        legend.position = "bottom", legend.title = element_blank(),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3))
es_of <- function(tg, y) esw(tg, y) %>% mutate(lo = att - 1.96 * se, hi = att + 1.96 * se, outcome = y)

d2 <- bind_rows(es_of("C", "log_works") %>% mutate(serie = "All papers"),
                es_of("C", "log_net") %>% mutate(serie = "Net of transformer-citing papers"))
p2a <- ggplot(d2, aes(e, att, colour = serie, shape = serie)) +
  annotate("rect", xmin = -0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "black") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, position = position_dodge(0.4), linewidth = 0.4) +
  geom_point(position = position_dodge(0.4), size = 2) +
  scale_colour_manual(values = c("black", "grey55")) + scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(breaks = -6:3) +
  labs(x = "Years since adoption", y = "Effect on log annual output", subtitle = "a") + tema
d2b <- es_of("C", "log_works") %>% dplyr::select(e, g = att) %>%
  inner_join(es_of("C", "log_net") %>% dplyr::select(e, n = att), by = "e") %>% mutate(diff = g - n)
p2b <- ggplot(d2b, aes(e, diff)) +
  annotate("rect", xmin = -0.5, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "black") +
  geom_hline(yintercept = 0, linewidth = 0.3) + geom_col(fill = "grey30", width = 0.6) +
  scale_x_continuous(breaks = -6:3) +
  labs(x = "Years since adoption", y = "Contribution of transformer-citing papers", subtitle = "b") + tema
pdf(file.path(FIG, "fig2_event_output.pdf"), width = 7.2, height = 3.2)
gridExtra::grid.arrange(p2a, p2b, ncol = 2, widths = c(1.25, 1))
dev.off()

labf <- c(citation_three_years = "Three-year citations per paper", top10_fw = "Papers in the top decile",
          top1_fw = "Papers in the top percentile")
d3 <- bind_rows(lapply(names(labf), function(y) es_of("C", y))) %>%
  mutate(outcome = factor(labf[outcome], levels = labf))
p3 <- ggplot(d3, aes(e, att)) +
  annotate("rect", xmin = -2.5, xmax = -0.5, ymin = -Inf, ymax = Inf, alpha = 0.10, fill = "grey40") +
  annotate("rect", xmin = -0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.06, fill = "black") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, linewidth = 0.4) + geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_y") + scale_x_continuous(breaks = -6:2) +
  labs(x = "Years since adoption (reference: three years before)", y = "Effect") + tema
ggsave(file.path(FIG, "fig3_event_citations.pdf"), p3, width = 7.2, height = 2.9)

lab4 <- c(citation_three_years = "Three-year citations", top10_fw = "Top decile", top1_fw = "Top percentile",
          log_works = "Log output", log_net = "Log output, net")
d4 <- tab %>% filter(tag %in% c("A", "B", "C"), outcome %in% names(lab4)) %>%
  mutate(pool = recode(tag, A = "A: all non-adopters", B = "B: never active in AI", C = "C: active in AI"),
         outcome = factor(lab4[outcome], levels = lab4),
         lo = att_post - 1.96 * se_post, hi = att_post + 1.96 * se_post)
p4 <- ggplot(d4, aes(att_post, pool)) + geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 0.4) + geom_point(size = 2) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  labs(x = "Average post-adoption effect (event time)", y = NULL) + tema +
  theme(axis.text.y = element_text(size = 8))
ggsave(file.path(FIG, "fig4_pools_event.pdf"), p4, width = 7.2, height = 2.3)

gr <- tab %>% filter(tag == "C", outcome %in% c("log_works", "log_net", "top10_fw", "top10_fw_net")) %>%
  dplyr::select(outcome, att_post, grid) %>% unnest(grid) %>%
  mutate(outcome = factor(recode(outcome, log_works = "Log annual output",
                                 log_net = "Log output, net of transformer papers",
                                 top10_fw = "Papers in the top decile",
                                 top10_fw_net = "Top decile, net of transformer papers"),
                          levels = c("Log annual output", "Log output, net of transformer papers",
                                     "Papers in the top decile", "Top decile, net of transformer papers")))
p5 <- ggplot(gr, aes(Mbar)) + geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey75", alpha = 0.6) +
  geom_line(aes(y = att_post), linewidth = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 1) + scale_x_continuous(breaks = seq(0, 2, 0.5)) +
  labs(x = expression(paste("Relative magnitude of the post-adoption violation, ", bar(M))),
       y = "Post-adoption average effect") + tema +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 8))
ggsave(file.path(FIG, "figS_rr.pdf"), p5, width = 7.2, height = 2.4)

cat("tabelle e figure scritte\n")
print(as.data.frame(tab %>% dplyr::select(tag, outcome, n, n_tr, att_post, se_post, bd, pre_sig, npre, wald_p)), digits = 3)
