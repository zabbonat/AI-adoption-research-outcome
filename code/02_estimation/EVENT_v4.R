# Stime definitive v4: il campione e' appaiato 1:1 DENTRO il sottoinsieme non-CS, cosi' le
# coppie restano intatte e il bilanciamento e' quello del campione di stima. Sostituisce
# EVENT_v2.R (che appaiava su tutto e poi filtrava, rompendo le coppie) e EVENT_v3.R.
#
# Contiene: specifica principale, tre bacini, senza anticipazione, senza Biochemistry,
# primi/ultimi/corrispondenti autori, appaiamento entro campo, griglie Rambachan-Roth,
# bilanciamento e medie pre-adozione.
rm(list = ls()); options(scipen = 999); set.seed(42)
suppressMessages({ library(tidyverse); library(fixest); library(HonestDiD); library(did); library(MatchIt) })

BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
con <- file(file.path("logs", "event_v4_log.txt"), "wt"); sink(con, split = TRUE)
cat("=== V4: APPAIAMENTO DENTRO IL CAMPIONE NON-CS, COPPIE INTATTE ===\n\n")

# ---------------- dati ----------------
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
df <- df %>% left_join(ai_int, by = "aid") %>%
  left_join(read_csv(file.path(BASE, "tail_fixed_author_year.csv.gz"), show_col_types = FALSE),
            by = c("aid", "publication_year"))

# insiemi, gia' ristretti a campo modale non-CS
aut <- df %>% distinct(aid, treated, g_adopt, ai_pre, campo) %>%
  filter(!is.na(campo), campo != "Computer Science")
trattati <- aut %>% filter(treated == 1, !is.na(g_adopt), g_adopt <= 2022) %>% pull(aid)
pool_C <- aut %>% filter(treated == 0, is.na(g_adopt), ai_pre == 1) %>% pull(aid)
pool_A <- aut %>% filter(treated == 0, is.na(g_adopt)) %>% pull(aid)
pool_B <- aut %>% filter(treated == 0, is.na(g_adopt), replace_na(ai_pre, 0L) == 0) %>% pull(aid)
cat(sprintf("fuori dal computer science: trattati g<=2022 %d | bacino A %d | bacino B %d | bacino C %d\n",
            length(trattati), length(pool_A), length(pool_B), length(pool_C)))

pre_all <- df %>% filter(publication_year >= 2012, publication_year <= 2017) %>%
  group_by(aid) %>%
  summarise(works_pre = mean(works_annual), cit_pre = mean(citation_three_years),
            h_pre = max(`summary_stats.h_index`, na.rm = TRUE), eta_pre = max(academicAge, na.rm = TRUE),
            slope_pre = if (n() >= 3) coef(lm(log_works ~ publication_year))[2] else NA_real_,
            n_anni = n(), .groups = "drop") %>%
  filter(n_anni >= 4, is.finite(h_pre), is.finite(eta_pre), !is.na(slope_pre)) %>%
  left_join(ai_int %>% dplyr::select(aid, ai_anni, ai_quota), by = "aid") %>%
  left_join(campo, by = "aid") %>%
  mutate(across(c(ai_anni, ai_quota), ~ replace_na(.x, 0)),
         l_works = log(works_pre + 1), l_cit = log(cit_pre + 1), l_h = log(h_pre + 1))
cat(sprintf("con requisiti pre-trattamento: trattati %d | C %d\n",
            sum(pre_all$aid %in% trattati), sum(pre_all$aid %in% pool_C)))

VV <- c("works_pre", "cit_pre", "h_pre", "eta_pre", "slope_pre", "ai_anni", "ai_quota")
appaia <- function(tr, ctrl, con_ai = TRUE, exact_campo = FALSE) {
  pr <- pre_all %>% filter(aid %in% c(tr, ctrl), !is.na(campo)) %>%
    mutate(trattato = as.integer(aid %in% tr))
  f <- if (con_ai)
    trattato ~ l_works + I(l_works^2) + l_cit + l_h + I(l_h^2) + eta_pre + slope_pre + ai_anni + ai_quota
  else trattato ~ l_works + I(l_works^2) + l_cit + l_h + I(l_h^2) + eta_pre + slope_pre
  m <- if (exact_campo)
    matchit(f, data = pr, method = "nearest", distance = "glm", caliper = 0.05,
            std.caliper = TRUE, replace = FALSE, exact = ~ campo)
  else matchit(f, data = pr, method = "nearest", distance = "glm", caliper = 0.05,
               std.caliper = TRUE, replace = FALSE)
  match.data(m) %>% mutate(w = weights)
}
bal_tab <- function(m2, vv = VV) {
  tibble(var = vv,
         tr = sapply(vv, function(x) mean(m2[[x]][m2$trattato == 1])),
         co = sapply(vv, function(x) mean(m2[[x]][m2$trattato == 0])),
         d = sapply(vv, function(x) (mean(m2[[x]][m2$trattato == 1]) - mean(m2[[x]][m2$trattato == 0])) / sd(m2[[x]])))
}
descrivi <- function(m2, nome) {
  b <- bal_tab(m2)
  cat(sprintf("\n[%s] appaiati %d = %d coppie 1:1 | max |d_std| %.3f\n", nome, nrow(m2),
              sum(m2$trattato), max(abs(b$d))))
  print(as.data.frame(b %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
  invisible(b)
}
prep <- function(md) {
  df %>% inner_join(md %>% dplyr::select(aid, trattato, w), by = "aid") %>%
    mutate(gname = ifelse(trattato == 1, g_adopt, 0))
}

# ---------------- stimatore ----------------
cs_fit <- function(d, y, ymax, antic, min_e, max_e, ymin = 2012) {
  d1 <- d %>% filter(publication_year >= ymin, publication_year <= ymax, !is.na(.data[[y]])) %>%
    dplyr::select(aid, publication_year, gname, w, all_of(y)) %>% as.data.frame()
  a <- att_gt(yname = y, tname = "publication_year", idname = "aid", gname = "gname", data = d1,
              control_group = "nevertreated", weightsname = "w", base_period = "universal",
              anticipation = antic, est_method = "reg", bstrap = FALSE, cband = FALSE,
              allow_unbalanced_panel = TRUE, print_details = FALSE)
  es <- aggte(a, type = "dynamic", min_e = min_e, max_e = max_e, na.rm = TRUE, cband = FALSE)
  s  <- aggte(a, type = "simple", na.rm = TRUE)
  g  <- aggte(a, type = "group", na.rm = TRUE)
  gg <- d1 %>% filter(gname > 0) %>% distinct(aid, gname)
  n_e <- sapply(es$egt, function(e) sum(gg$gname + e <= ymax & gg$gname + e >= ymin))
  list(es = es, s = s, g = g, n = n_distinct(d1$aid), n_tr = nrow(gg), n_e = n_e)
}
honest_es <- function(es, antic, n_e, grid = FALSE) {
  inf <- es$inf.function$dynamic.inf.func.e
  n <- nrow(inf); V <- t(inf) %*% inf / n / n
  egt <- es$egt; ref <- -1 - antic; keep <- egt != ref
  V <- V[keep, keep, drop = FALSE]; beta <- es$att.egt[keep]; e2 <- egt[keep]; ne <- n_e[keep]
  npre <- sum(e2 < ref); post <- which(e2 > ref); npost <- length(post)
  wv <- ifelse(e2[post] >= 0, ne[post], 0); l <- as.matrix(wv / sum(wv))
  est <- sum(l * beta[post]); se <- sqrt(as.numeric(t(l) %*% V[post, post] %*% l))
  r <- createSensitivityResults_relativeMagnitudes(
    betahat = beta, sigma = V, numPrePeriods = npre, numPostPeriods = npost, l_vec = l,
    Mbarvec = seq(0, 2, by = 0.1), gridPoints = 400,
    grid.lb = est - 25 * se, grid.ub = est + 25 * se, seed = 42) %>%
    mutate(covers0 = lb <= 0 & ub >= 0)
  bd <- r %>% filter(covers0) %>% slice_min(Mbar, n = 1) %>% pull(Mbar)
  bp <- beta[1:npre]; Vp <- V[1:npre, 1:npre, drop = FALSE]
  wp <- 1 - pchisq(as.numeric(t(bp) %*% solve(Vp) %*% bp), npre); z <- bp / sqrt(diag(Vp))
  list(est = est, se = se, bd = ifelse(length(bd) == 0, NA, bd), npre = npre,
       pre_sig = sum(abs(z) > 1.96), wald_p = wp, grid = if (grid) r %>% dplyr::select(Mbar, lb, ub) else NULL)
}
# esito -> c(anno massimo, anticipazione, max_e)
OUT <- list(citation_three_years = c(2022, 2, 2), top10_fw = c(2022, 2, 2), top1_fw = c(2022, 2, 2),
            top10_fw_net = c(2022, 2, 2), top1_fw_net = c(2022, 2, 2),
            log_works = c(2024, 2, 3), log_net = c(2024, 2, 3))
CORE <- c("citation_three_years", "top10_fw", "top1_fw", "log_works", "log_net")
run <- function(d, y, tag, antic = NULL, grid = FALSE, mostra = TRUE) {
  o <- OUT[[y]]; a <- if (is.null(antic)) o[2] else antic
  r <- cs_fit(d, y, o[1], a, -6, o[3]); h <- honest_es(r$es, a, r$n_e, grid = grid)
  cat(sprintf("[%s] %-21s n %d (tr %d) | ATT %+.4f (%.4f) | post %+.4f (%.4f) | Mbar %s | placebo %d/%d | Wald %.3f\n",
              tag, y, r$n, r$n_tr, r$s$overall.att, r$s$overall.se, h$est, h$se,
              ifelse(is.na(h$bd), ">2", sprintf("%.2f", h$bd)), h$pre_sig, h$npre, h$wald_p))
  if (mostra) print(data.frame(e = r$es$egt, n_e = r$n_e, att = round(r$es$att.egt, 4),
                               se = round(r$es$se.egt, 4), t = round(r$es$att.egt / r$es$se.egt, 2)))
  tibble(tag = tag, outcome = y, antic = a, n = r$n, n_tr = r$n_tr,
         att_simple = r$s$overall.att, se_simple = r$s$overall.se, att_post = h$est, se_post = h$se,
         bd = h$bd, pre_sig = h$pre_sig, npre = h$npre, wald_p = h$wald_p,
         es = list(tibble(e = r$es$egt, n_e = r$n_e, att = r$es$att.egt, se = r$es$se.egt)),
         g = list(tibble(g = r$g$egt, att = r$g$att.egt, se = r$g$se.egt)),
         grid = list(h$grid))
}

res <- list()
cat("\n\n=================== (1) SPECIFICA PRINCIPALE, bacino C dentro il non-CS ===================\n")
md_C <- appaia(trattati, pool_C, con_ai = TRUE)
b_C <- descrivi(md_C, "C")
dd <- prep(md_C)
cat("\ncoorti di adozione:\n"); print(as.data.frame(dd %>% filter(trattato == 1) %>% distinct(aid, g_adopt) %>% count(g_adopt)))
cat("\nmedie pre-adozione dei trattati (e <= -3):\n")
pre_m <- dd %>% filter(trattato == 1, publication_year <= g_adopt - 3) %>%
  summarise(across(c(citation_three_years, c3_medio, top10_fw, top1_fw, top10_fw_net, top1_fw_net,
                     works_annual, works_net, n_works_fw), ~ mean(.x, na.rm = TRUE)))
print(as.data.frame(pre_m), digits = 4)
post <- dd %>% filter(trattato == 1, publication_year >= g_adopt)
quota <- c(top10_tr_su_top10 = sum(post$top10_tr, na.rm = TRUE) / sum(post$top10_fw, na.rm = TRUE),
           top1_tr_su_top1 = sum(post$top1_tr, na.rm = TRUE) / sum(post$top1_fw, na.rm = TRUE),
           p_top10_trans = sum(post$top10_tr, na.rm = TRUE) / sum(post$n_trans_fw, na.rm = TRUE),
           p_top10_altri = (sum(post$top10_fw, na.rm = TRUE) - sum(post$top10_tr, na.rm = TRUE)) /
             (sum(post$n_works_fw, na.rm = TRUE) - sum(post$n_trans_fw, na.rm = TRUE)))
cat("\nquote post-adozione:\n"); print(round(quota, 4))
cat("\n")
for (y in names(OUT)) res[[length(res) + 1]] <- run(dd, y, "C", grid = y %in% c("log_works", "log_net", "top10_fw", "top10_fw_net"))

cat("\n\n=================== (2) SENZA FINESTRA DI ANTICIPAZIONE ===================\n")
for (y in CORE) res[[length(res) + 1]] <- run(dd, y, "C_antic0", antic = 0, mostra = FALSE)

cat("\n\n=================== (3) SENZA BIOCHEMISTRY ===================\n")
db <- dd %>% filter(campo != "Biochemistry, Genetics and Molecular Biology")
for (y in CORE) res[[length(res) + 1]] <- run(db, y, "C_noBiochem", mostra = FALSE)

cat("\n\n=================== (4) PRIMI, ULTIMI O CORRISPONDENTI AUTORI ===================\n")
pos <- read_csv(file.path(BASE, "posizioni_primo_paper.csv"), show_col_types = FALSE)
lead <- pos %>% filter(posizione %in% c("first", "last") | corrispondente == TRUE) %>% pull(aid)
cat(sprintf("posizione nota per %d adottanti | primi %d, ultimi %d, corrispondenti %d, almeno una %d\n",
            nrow(pos), sum(pos$posizione == "first", na.rm = TRUE), sum(pos$posizione == "last", na.rm = TRUE),
            sum(pos$corrispondente == TRUE, na.rm = TRUE), length(lead)))
md_L <- appaia(intersect(trattati, lead), pool_C, con_ai = TRUE)
b_L <- descrivi(md_L, "lead")
d_L <- prep(md_L)
for (y in CORE) res[[length(res) + 1]] <- run(d_L, y, "lead", mostra = FALSE)

cat("\n\n=================== (5) APPAIAMENTO ENTRO CAMPO MODALE ===================\n")
md_F <- appaia(trattati, pool_C, con_ai = TRUE, exact_campo = TRUE)
b_F <- descrivi(md_F, "campo")
d_F <- prep(md_F)
for (y in CORE) res[[length(res) + 1]] <- run(d_F, y, "campo", mostra = FALSE)

cat("\n\n=================== (6) I TRE BACINI ===================\n")
md_A <- appaia(trattati, pool_A, con_ai = FALSE); b_A <- descrivi(md_A, "A")
md_B <- appaia(trattati, pool_B, con_ai = FALSE); b_B <- descrivi(md_B, "B")
for (nm in c("A", "B")) {
  d2 <- prep(if (nm == "A") md_A else md_B)
  cat("\n")
  for (y in CORE) res[[length(res) + 1]] <- run(d2, y, nm, mostra = FALSE)
}

tab <- bind_rows(res)
saveRDS(list(tab = tab, pre_m = pre_m, quota = quota,
             bal = list(C = b_C, lead = b_L, campo = b_F, A = b_A, B = b_B),
             md_C = md_C %>% dplyr::select(aid, trattato, w, subclass),
             coorti = dd %>% filter(trattato == 1) %>% distinct(aid, g_adopt) %>% count(g_adopt),
             n_pos = nrow(pos), n_lead = length(lead)),
        file.path(FIX, "event_v4_results.rds"))
cat("\n\n=================== SINTESI ===================\n")
print(as.data.frame(tab %>% dplyr::select(tag, outcome, antic, n, n_tr, att_simple, se_simple,
                                          att_post, se_post, bd, pre_sig, npre, wald_p)), digits = 4)
cat("\nFATTO.\n"); sink(); close(con)
