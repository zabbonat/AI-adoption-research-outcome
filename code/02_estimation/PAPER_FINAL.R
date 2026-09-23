# Stime definitive del paper.
#
# Specifica: adottanti fuori dal Computer Science, coorti fino al 2022, appaiati
# a non-adottanti attivi in AI prima del 2018 (bacino C) su caratteristiche
# pre-adozione compresa l intensita di esperienza AI, caliper 0.05.
# Finestra pre-trattamento 2014-2016, anno di riferimento 2016.
# La versione con pre dal 2012 resta nei supplementari.
rm(list = ls()); options(scipen = 999); set.seed(42)
suppressMessages({ library(tidyverse); library(fixest); library(HonestDiD); library(MatchIt) })

BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
con <- file(file.path("logs", "paper_final_log.txt"), "wt"); sink(con, split = TRUE)
cat("=== STIME DEFINITIVE, PRE DAL 2014 ===\n\n")

rt <- read_csv(file.path(BASE, "RTransformerSISTEMATO.csv.gz"), show_col_types = FALSE,
               col_select = c(id, publication_year, author_id))
traj <- rt %>% filter(!is.na(author_id), !is.na(publication_year)) %>%
  mutate(aid_list = str_extract_all(author_id, "A\\d+")) %>%
  dplyr::select(id, publication_year, aid_list) %>% unnest(aid_list) %>%
  mutate(aid = as.numeric(sub("^A", "", aid_list))) %>%
  group_by(aid) %>% summarise(g_adopt = min(publication_year), .groups = "drop")

df <- read_csv(file.path(BASE, "panel_didi_correctJULY.csv.gz"), show_col_types = FALSE) %>%
  left_join(read_csv(file.path(BASE, "works_fix_author_year.csv.gz"), show_col_types = FALSE),
            by = c("author_id", "publication_year")) %>%
  mutate(citation_three_years = replace_na(citation_three_years, 0),
         works_annual = replace_na(works_annual, 0), works_net = replace_na(works_net, 0),
         n_trans = replace_na(n_trans, 0),
         log_works = log(works_annual + 1), log_net = log(works_net + 1),
         aid = as.numeric(gsub("https://openalex.org/A", "", author_id, fixed = TRUE)),
         field = as.character(field)) %>%
  left_join(traj, by = "aid")
campo <- df %>% filter(!is.na(field)) %>% count(aid, field) %>% group_by(aid) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% dplyr::select(aid, campo = field)
df <- df %>% left_join(campo, by = "aid")
ai_int <- df %>% filter(publication_year <= 2017) %>% group_by(aid) %>%
  summarise(ai_anni = sum(ai == 1, na.rm = TRUE), ai_quota = mean(ai == 1, na.rm = TRUE),
            ai_pre = as.integer(any(ai == 1, na.rm = TRUE)), .groups = "drop")
df <- df %>% left_join(ai_int, by = "aid")

aut <- df %>% distinct(aid, treated, g_adopt, ai_pre)
trattati <- aut %>% filter(treated == 1, !is.na(g_adopt), g_adopt <= 2022) %>% pull(aid)
pool_C   <- aut %>% filter(treated == 0, is.na(g_adopt), ai_pre == 1) %>% pull(aid)
pool_A   <- aut %>% filter(treated == 0, is.na(g_adopt)) %>% pull(aid)
pool_B   <- aut %>% filter(treated == 0, is.na(g_adopt), replace_na(ai_pre, 0L) == 0) %>% pull(aid)

pre_all <- df %>% filter(publication_year >= 2012, publication_year <= 2017) %>%
  group_by(aid) %>%
  summarise(works_pre = mean(works_annual), cit_pre = mean(citation_three_years),
            h_pre = max(`summary_stats.h_index`, na.rm = TRUE),
            eta_pre = max(academicAge, na.rm = TRUE),
            slope_pre = if (n() >= 3) coef(lm(log_works ~ publication_year))[2] else NA_real_,
            n_anni = n(), .groups = "drop") %>%
  filter(n_anni >= 4, is.finite(h_pre), is.finite(eta_pre), !is.na(slope_pre)) %>%
  left_join(ai_int %>% dplyr::select(aid, ai_anni, ai_quota), by = "aid") %>%
  mutate(across(c(ai_anni, ai_quota), ~ replace_na(.x, 0)),
         l_works = log(works_pre + 1), l_cit = log(cit_pre + 1), l_h = log(h_pre + 1))

appaia <- function(ctrl, con_ai = TRUE) {
  pr <- pre_all %>% filter(aid %in% c(trattati, ctrl)) %>%
    mutate(trattato = as.integer(aid %in% trattati))
  f <- if (con_ai)
    trattato ~ l_works + I(l_works^2) + l_cit + l_h + I(l_h^2) + eta_pre + slope_pre + ai_anni + ai_quota
  else
    trattato ~ l_works + I(l_works^2) + l_cit + l_h + I(l_h^2) + eta_pre + slope_pre
  m <- matchit(f, data = pr, method = "nearest", distance = "glm",
               caliper = 0.05, std.caliper = TRUE, replace = FALSE)
  match.data(m) %>% mutate(w = weights)
}

PRE_MIN <- 2014
fit_es <- function(d, y, ymax, ymin = PRE_MIN) {
  m <- feols(as.formula(paste0(y, " ~ i(publication_year, trattato, ref = 2016) | aid + publication_year")),
             data = d %>% filter(publication_year >= ymin, publication_year <= ymax,
                                 !is.na(.data[[y]])),
             weights = ~ w, cluster = ~ aid)
  k <- grep("publication_year::", names(coef(m)))
  b <- coef(m)[k]; sg <- vcov(m)[k, k]
  yr <- as.numeric(gsub(".*::(\\d+).*", "\\1", names(b))); o <- order(yr)
  list(b = b[o], sg = sg[o, o], yr = yr[o])
}
honest <- function(e) {
  sel <- which(e$yr <= 2015 | e$yr >= 2017)
  b <- e$b[sel]; sg <- e$sg[sel, sel]; yr <- e$yr[sel]
  npre <- sum(yr <= 2015); npost <- sum(yr >= 2017)
  l <- as.matrix(rep(1 / npost, npost)); ip <- (npre + 1):(npre + npost)
  est <- sum(l * b[ip]); se_l <- sqrt(as.numeric(t(l) %*% sg[ip, ip] %*% l))
  r <- createSensitivityResults_relativeMagnitudes(
    betahat = b, sigma = sg, numPrePeriods = npre, numPostPeriods = npost, l_vec = l,
    Mbarvec = seq(0, 2, by = 0.1), gridPoints = 400,
    grid.lb = est - 25 * se_l, grid.ub = est + 25 * se_l, seed = 42) %>%
    mutate(covers0 = lb <= 0 & ub >= 0)
  bd <- r %>% filter(covers0) %>% slice_min(Mbar, n = 1) %>% pull(Mbar)
  wp <- tryCatch({
    z <- b[1:npre] / sqrt(diag(sg))[1:npre]; 1 - pchisq(sum(z^2), npre)
  }, error = function(x) NA_real_)
  list(est = est, se = se_l, bd = ifelse(length(bd) == 0, NA, bd),
       pre_sig = sum(abs(b[1:npre] / sqrt(diag(sg))[1:npre]) > 1.96), npre = npre, wald_p = wp)
}
esiti <- function(d, tag) {
  map_dfr(c("citation_three_years", "big_hits10", "big_hits1", "log_works", "log_net"),
          function(y) {
            ymax <- if (y %in% c("log_works", "log_net")) 2024 else 2022
            h <- honest(fit_es(d, y, ymax))
            cat(sprintf("  %-22s %+.4f (SE %.4f)  Mbar %s  pre_sig %d/%d  Wald p %.3f\n",
                        y, h$est, h$se, ifelse(is.na(h$bd), ">2", sprintf("%.2f", h$bd)),
                        h$pre_sig, h$npre, h$wald_p))
            tibble(spec = tag, outcome = y, att = h$est, se = h$se, bd = h$bd,
                   pre_sig = h$pre_sig, npre = h$npre, wald_p = h$wald_p)
          })
}

# ---- specifica principale ----
cat("=================== SPECIFICA PRINCIPALE ===================\n")
md <- appaia(pool_C, con_ai = TRUE)
v <- c("works_pre", "cit_pre", "h_pre", "eta_pre", "slope_pre", "ai_anni", "ai_quota")
b <- sapply(v, function(x) (mean(md[[x]][md$trattato == 1]) - mean(md[[x]][md$trattato == 0])) / sd(md[[x]]))
cat(sprintf("appaiati %d (trattati %d) | max abs d_std = %.3f\n", nrow(md), sum(md$trattato), max(abs(b))))
print(round(b, 3))
dd <- df %>% inner_join(md %>% dplyr::select(aid, trattato, w), by = "aid") %>%
  filter(!is.na(campo), campo != "Computer Science")
cat(sprintf("campione di stima: %d autori (trattati %d)\n\n",
            n_distinct(dd$aid), n_distinct(dd$aid[dd$trattato == 1])))
main <- esiti(dd, "principale")

cat("\nmedie pre-adozione nel campione di stima:\n")
bs <- dd %>% filter(publication_year <= 2016) %>%
  summarise(cit = mean(citation_three_years), b10 = mean(big_hits10), b1 = mean(big_hits1),
            works = mean(works_annual))
print(as.data.frame(bs), digits = 4)

# ---- i tre bacini, stessa finestra ----
cat("\n=================== I TRE BACINI ===================\n")
pools <- list("A" = pool_A, "B" = pool_B, "C" = pool_C)
pl <- map_dfr(names(pools), function(nm) {
  m2 <- appaia(pools[[nm]], con_ai = (nm == "C"))
  d2 <- df %>% inner_join(m2 %>% dplyr::select(aid, trattato, w), by = "aid") %>%
    filter(!is.na(campo), campo != "Computer Science")
  bb <- sapply(v[1:5], function(x) (mean(m2[[x]][m2$trattato == 1]) - mean(m2[[x]][m2$trattato == 0])) / sd(m2[[x]]))
  cat(sprintf("\nbacino %s | appaiati %d | max abs d_std = %.3f\n", nm, nrow(m2), max(abs(bb))))
  esiti(d2, nm) %>% mutate(bal = max(abs(bb)))
})

# ---- profili per la figura ----
cat("\n=================== PROFILI ===================\n")
prof <- map_dfr(c("log_works", "log_net"), function(y) {
  e <- fit_es(dd, y, 2024)
  tibble(outcome = y, anno = e$yr, att = as.numeric(e$b), se = sqrt(diag(e$sg)))
})
prof_cit <- { e <- fit_es(dd, "citation_three_years", 2022)
  tibble(outcome = "citation_three_years", anno = e$yr, att = as.numeric(e$b),
         se = sqrt(diag(e$sg))) }
print(as.data.frame(prof %>% dplyr::select(-se) %>%
  pivot_wider(names_from = outcome, values_from = att) %>%
  mutate(differenza = log_works - log_net)), digits = 4)

# ---- pre dal 2012, per i supplementari ----
cat("\n=================== PRE DAL 2012, PER I SUPPLEMENTARI ===================\n")
alt <- map_dfr(c("citation_three_years", "big_hits1", "log_works", "log_net"), function(y) {
  ymax <- if (y %in% c("log_works", "log_net")) 2024 else 2022
  h <- honest(fit_es(dd, y, ymax, ymin = 2012))
  cat(sprintf("  %-22s %+.4f (SE %.4f)  Mbar %s  pre_sig %d/%d\n", y, h$est, h$se,
              ifelse(is.na(h$bd), ">2", sprintf("%.2f", h$bd)), h$pre_sig, h$npre))
  tibble(spec = "pre dal 2012", outcome = y, att = h$est, se = h$se, bd = h$bd,
         pre_sig = h$pre_sig, npre = h$npre)
})

saveRDS(list(md = md, main = main, pools = pl, prof = prof, prof_cit = prof_cit,
             alt = alt, base = bs), file.path(FIX, "paper_final_results.rds"))
cat("\n\nFATTO.\n"); sink(); close(con)
