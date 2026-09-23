# Estrae gli autori del campione appaiato del bacino C con campo modale, coorte e pesi,
# per il download dei lavori da OpenAlex (coda in finestra fissa).
rm(list = ls()); options(scipen = 999)
suppressMessages({ library(tidyverse) })

BASE <- "data"   # scripts are run from the repository root
FIX  <- "results"

md <- readRDS(file.path(FIX, "paper_final_results.rds"))$md

rt <- read_csv(file.path(BASE, "RTransformerSISTEMATO.csv.gz"), show_col_types = FALSE,
               col_select = c(id, publication_year, author_id))
g_ad <- rt %>% filter(!is.na(author_id), !is.na(publication_year)) %>%
  mutate(aid_list = str_extract_all(author_id, "A\\d+")) %>%
  dplyr::select(id, publication_year, aid_list) %>% unnest(aid_list) %>%
  mutate(aid = as.numeric(sub("^A", "", aid_list))) %>%
  group_by(aid) %>% summarise(g_adopt = min(publication_year), .groups = "drop")

df <- read_csv(file.path(BASE, "panel_didi_correctJULY.csv.gz"), show_col_types = FALSE,
               col_select = c(author_id, publication_year, field)) %>%
  mutate(aid = as.numeric(gsub("https://openalex.org/A", "", author_id, fixed = TRUE)),
         field = as.character(field))
campo <- df %>% filter(!is.na(field)) %>% count(aid, field) %>% group_by(aid) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>% dplyr::select(aid, campo = field)

out <- md %>% dplyr::select(aid, trattato, w, subclass) %>%
  left_join(campo, by = "aid") %>% left_join(g_ad, by = "aid") %>%
  mutate(non_cs = as.integer(!is.na(campo) & campo != "Computer Science"))
write_csv(out, file.path(BASE, "ids_poolC_matched.csv"))
cat(sprintf("appaiati %d | non-CS %d (trattati %d)\n", nrow(out), sum(out$non_cs),
            sum(out$non_cs == 1 & out$trattato == 1)))
print(out %>% filter(non_cs == 1, trattato == 1) %>% count(g_adopt))
