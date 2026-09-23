# Runs estimation and output scripts in order. Run from the repository root.
for (d in c("results", "logs", "output/tables", "output/figures")) dir.create(d, recursive = TRUE, showWarnings = FALSE)

scripts <- c("code/02_estimation/PAPER_FINAL.R",
             "code/02_estimation/EVENT_v4.R",
             "code/02_estimation/CALENDAR_v4.R",
             "code/02_estimation/bilanciamento_opzioni.R",
             "code/03_tables_figures/make_assets.R",
             "code/03_tables_figures/TABLES_v4.R",
             "code/03_tables_figures/FIGURE_v4.R")

for (s in scripts) {
  cat("running", s, "\n")
  source(s, local = new.env())
  sink.number() |> seq_len() |> rev() |> lapply(\(i) sink())
}
