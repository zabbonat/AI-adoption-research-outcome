# Specifica calendar-time (quella del manoscritto precedente) sugli stessi esiti della
# versione event-time, con riferimento 2016 (pre 2014-2015) e riferimento 2018 (pre 2014-2017),
# per i supplementari. Richiede tail_fixed_author_year.csv.
rm(list = ls()); options(scipen = 999); set.seed(42)
suppressMessages({ library(tidyverse); library(fixest); library(HonestDiD) })

BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
con <- file(file.path("logs", "calendar_v4_log.txt"), "wt"); sink(con, split = TRUE)
cat("=== CALENDAR-TIME, RIFERIMENTO 2016 E 2018, ESITI DELLA VERSIONE EVENT-TIME ===\n\n")

md <- readRDS(file.path(FIX, "event_v4_results.rds"))$md_C
rt <- read_csv(file.path(BASE, "RTransformerSISTEMATO.csv.gz"), show_col_types = FALSE,
               col_select = c(id, publication_year, author_id))
g_ad <- rt %>% filter(!is.na(author_id), !is.na(publication_year)) %>%
  mutate(aid_list = str_extract_all(author_id, "A\\d+")) %>%
  dplyr::select(id, publication_year, aid_list) %>% unnest(aid_list) %>%
  mutate(aid = as.numeric(sub("^A", "", aid_list))) %>%
  group_by(aid) %>% summarise(g_adopt = min(publication_year), .groups = "drop")
df <- read_csv(file.path(BASE, "panel_didi_correctJULY.csv.gz"), show_col_types = FALSE,
               col_select = c(author_id, publication_year, treated, citation_three_years, field)) %>%
  left_join(read_csv(file.path(BASE, "works_fix_author_year.csv.gz"), show_col_types = FALSE),
            by = c("author_id", "publication_year")) %>%
  mutate(citation_three_years = replace_na(citation_three_years, 0),
         works_annual = replace_na(works_annual, 0), works_net = replace_na(works_net, 0),
         log_works = log(works_annual + 1), log_net = log(works_net + 1),
         aid = as.numeric(gsub("https://openalex.org/A", "", author_id, fixed = TRUE)),
         field = as.character(field)) %>%
  left_join(g_ad, by = "aid") %>%
  left_join(read_csv(file.path(BASE, "tail_fixed_author_year.csv.gz"), show_col_types = FALSE),
            by = c("aid", "publication_year"))
campo <- df %>% filter(!is.na(field)) %>% count(aid, field) %>% group_by(aid) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% dplyr::select(aid, campo = field)
df <- df %>% left_join(campo, by = "aid")
dd <- df %>% inner_join(md %>% dplyr::select(aid, trattato, w, subclass), by = "aid")
a18 <- dd %>% filter(trattato == 1, g_adopt == 2018) %>% distinct(subclass) %>% pull(subclass)
d18 <- dd %>% filter(!(subclass %in% a18))
cat(sprintf("campione: %d autori (trattati %d); senza coorte 2018: %d (trattati %d)\n",
            n_distinct(dd$aid), n_distinct(dd$aid[dd$trattato == 1]), n_distinct(d18$aid), n_distinct(d18$aid[d18$trattato == 1])))

fit_es <- function(d, y, ymax, ref, ymin = 2014) {
  m <- feols(as.formula(paste0(y, " ~ i(publication_year, trattato, ref = ", ref, ") | aid + publication_year")),
             data = d %>% filter(publication_year >= ymin, publication_year <= ymax, !is.na(.data[[y]])),
             weights = ~ w, cluster = ~ aid)
  k <- grep("publication_year::", names(coef(m)))
  b <- coef(m)[k]; sg <- vcov(m)[k, k]
  yr <- as.numeric(gsub(".*::(\\d+).*", "\\1", names(b))); o <- order(yr)
  list(b = b[o], sg = sg[o, o], yr = yr[o])
}
honest <- function(e, ref) {
  b <- e$b; sg <- e$sg; yr <- e$yr
  npre <- sum(yr < ref); npost <- sum(yr > ref)
  l <- as.matrix(rep(1 / npost, npost)); ip <- (npre + 1):(npre + npost)
  est <- sum(l * b[ip]); se_l <- sqrt(as.numeric(t(l) %*% sg[ip, ip] %*% l))
  r <- createSensitivityResults_relativeMagnitudes(
    betahat = b, sigma = sg, numPrePeriods = npre, numPostPeriods = npost, l_vec = l,
    Mbarvec = seq(0, 2, by = 0.1), gridPoints = 400, grid.lb = est - 25 * se_l, grid.ub = est + 25 * se_l, seed = 42) %>%
    mutate(covers0 = lb <= 0 & ub >= 0)
  bd <- r %>% filter(covers0) %>% slice_min(Mbar, n = 1) %>% pull(Mbar)
  bp <- b[1:npre]; Vp <- sg[1:npre, 1:npre, drop = FALSE]
  wp <- 1 - pchisq(as.numeric(t(bp) %*% solve(Vp) %*% bp), npre)
  z <- bp / sqrt(diag(Vp))
  list(est = est, se = se_l, bd = ifelse(length(bd) == 0, NA, bd), pre_sig = sum(abs(z) > 1.96), npre = npre, wald_p = wp)
}
outc <- c(citation_three_years = 2022, top10_fw = 2022, top1_fw = 2022, top10_fw_net = 2022, top1_fw_net = 2022,
          log_works = 2024, log_net = 2024)
out <- list()
for (ref in c(2016, 2018)) {
  cat(sprintf("\n=================== RIFERIMENTO %d ===================\n", ref))
  d <- if (ref == 2016) dd else d18
  for (y in names(outc)) {
    e <- fit_es(d, y, outc[[y]], ref); h <- honest(e, ref)
    cat(sprintf("  %-16s %+.4f (SE %.4f)  Mbar %s  pre_sig %d/%d  Wald p %.3f\n", y, h$est, h$se,
                ifelse(is.na(h$bd), ">2", sprintf("%.2f", h$bd)), h$pre_sig, h$npre, h$wald_p))
    out[[length(out) + 1]] <- tibble(ref = ref, outcome = y, est = h$est, se = h$se, bd = h$bd, pre_sig = h$pre_sig,
                                     npre = h$npre, wald_p = h$wald_p,
                                     prof = list(tibble(anno = e$yr, beta = as.numeric(e$b), se = sqrt(diag(e$sg)))))
  }
}
tab <- bind_rows(out)
saveRDS(tab, file.path(FIX, "calendar_v4_results.rds"))
cat("\nprofili per anno, riferimento 2016:\n")
for (y in c("citation_three_years", "top1_fw", "log_works", "log_net")) {
  p <- tab %>% filter(ref == 2016, outcome == y) %>% pull(prof) %>% .[[1]]
  cat(y, ": "); cat(sprintf("%d %+.3f(%.3f) ", p$anno, p$beta, p$se)); cat("\n")
}
cat("\nFATTO.\n"); sink(); close(con)
