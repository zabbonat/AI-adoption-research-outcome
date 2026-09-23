# Bilanciamento delle tre opzioni di campione:
# (B) appaiamento su tutta la popolazione, poi restrizione non-CS per autore (quello usato finora)
# (C) appaiamento su tutta la popolazione, poi solo le coppie in cui entrambi sono non-CS
# (A) appaiamento dentro il non-CS (1,984 coppie), gia' in event_v4
rm(list = ls()); options(scipen = 999)
suppressMessages({ library(tidyverse) })
BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"
con <- file(file.path("logs", "bilanciamento_opzioni_log.txt"), "wt"); sink(con, split = TRUE)

md <- readRDS(file.path(FIX, "paper_final_results.rds"))$md
ids <- read_csv(file.path(BASE, "ids_poolC_matched.csv"), show_col_types = FALSE)
md <- md %>% left_join(ids %>% dplyr::select(aid, non_cs, campo), by = "aid")
VV <- c("works_pre", "cit_pre", "h_pre", "eta_pre", "slope_pre", "ai_anni", "ai_quota")
bal <- function(d, nome) {
  b <- tibble(var = VV,
              tr = sapply(VV, function(x) mean(d[[x]][d$trattato == 1])),
              co = sapply(VV, function(x) mean(d[[x]][d$trattato == 0])),
              d = sapply(VV, function(x) (mean(d[[x]][d$trattato == 1]) - mean(d[[x]][d$trattato == 0])) / sd(d[[x]])))
  cat(sprintf("\n[%s] trattati %d | controlli %d | max |d_std| %.3f\n", nome,
              sum(d$trattato), sum(d$trattato == 0), max(abs(b$d))))
  print(as.data.frame(b %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
  invisible(b)
}
bal(md, "matched completo, 16,821 coppie")
B <- md %>% filter(non_cs == 1)
bal(B, "B: restrizione non-CS per autore (usato finora)")
sc_ok <- md %>% group_by(subclass) %>% summarise(ok = all(non_cs == 1), .groups = "drop") %>% filter(ok) %>% pull(subclass)
C <- md %>% filter(subclass %in% sc_ok)
bal(C, "C: solo coppie con entrambi non-CS")
cat(sprintf("\ncoppie intatte in C: %d\n", length(sc_ok)))
cat("\ncomposizione per campo, trattati contro controlli in B (quota):\n")
q <- B %>% count(trattato, campo) %>% group_by(trattato) %>% mutate(q = n / sum(n)) %>%
  dplyr::select(-n) %>% pivot_wider(names_from = trattato, values_from = q, names_prefix = "g") %>%
  mutate(diff = g1 - g0) %>% arrange(desc(abs(diff)))
print(as.data.frame(q %>% head(8) %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
cat("\nstessa cosa in C:\n")
q2 <- C %>% count(trattato, campo) %>% group_by(trattato) %>% mutate(q = n / sum(n)) %>%
  dplyr::select(-n) %>% pivot_wider(names_from = trattato, values_from = q, names_prefix = "g") %>%
  mutate(diff = g1 - g0) %>% arrange(desc(abs(diff)))
print(as.data.frame(q2 %>% head(8) %>% mutate(across(where(is.numeric), ~ round(.x, 3)))))
saveRDS(list(sc_ok = sc_ok), file.path(FIX, "coppie_intatte.rds"))
sink(); close(con)
