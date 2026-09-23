# Artificial intelligence adoption and research outcomes: evidence from transformer models

Data and code accompanying the paper "Artificial intelligence adoption and research outcomes: evidence from transformer models", with scripts to reproduce the analyses, tables and figures.

Authors: Diletta Abbonato, Stefano Bianchini, Patrick Llerena (BETA UMR 7522, University of Strasbourg; CPS, University of Turin).
Contact: s.bianchini@unistra.fr

## Repository structure

```
data/                     analysis datasets (derived from OpenAlex)
code/01_build_data/       Python scripts that build the derived files from raw OpenAlex downloads
code/02_estimation/       R scripts for matching and estimation
code/03_tables_figures/   R and Python scripts that write the tables and figures
results/                  saved estimation objects (.rds) used by the table and figure scripts
logs/                     logs of the runs that produced the numbers in the paper
output/tables/            LaTeX tables as they appear in the paper and Supplementary Information
output/figures/           figures as they appear in the paper and Supplementary Information
run_all.R                 runs the estimation and output scripts in order
```

All scripts are meant to be run from the repository root. Comments inside the scripts are partly in Italian.

## Data

All data come from OpenAlex (https://openalex.org), downloaded in May 2025.

| File | Content |
|---|---|
| `RTransformerSISTEMATO.csv.gz` | Papers citing a transformer architecture, with authors, field, year and citation indicators |
| `panel_didi_correctJULY.csv.gz` | Author-year panel of adopters and non-adopters |
| `works_fix_author_year.csv.gz` | Annual publication counts by author-year, total and net of transformer-citing papers |
| `tail_fixed_author_year.csv.gz` | Fixed-window (three-year) citation counts and field-year top-decile / top-percentile papers by author-year |
| `soglie_campo_anno.csv` | Field-year citation thresholds for the top 10% and top 1% |
| `posizioni_primo_paper.csv` | Author position of adopters on their first transformer-citing paper |
| `ids_poolC_matched.csv` | Matched sample (pool C) with cohort, modal field and weights |
| `lessico_campo_anno.csv` | Lexical index of transformer-related terms in abstracts, by field and year |

Because of GitHub file-size limits, the three largest files (`panel_didi_correctJULY.csv.gz`, `works_fix_author_year.csv.gz`, `tail_fixed_author_year.csv.gz`, about 104 MB in total) are not stored in this repository. They are available at [ZENODO DOI] and should be placed in `data/` before running the estimation scripts. The tables and figures can be regenerated without them from the objects in `results/` (see below).

The raw per-author OpenAlex downloads used by `code/01_build_data/` are not included because of their size (several GB). The scripts expect them under `raw/`; they can be rebuilt from the OpenAlex API.

## Reproducing the results

Requirements: R (>= 4.3) with `tidyverse`, `fixest`, `did`, `HonestDiD`, `MatchIt`, `patchwork`, `gridExtra`; Python (>= 3.10) with `pandas`, `numpy`, `pyarrow`, `matplotlib`, `requests`, `tqdm`.

From the repository root:

```
Rscript run_all.R
python code/03_tables_figures/pipeline_v4.py
```

`run_all.R` runs, in order:

1. `code/02_estimation/PAPER_FINAL.R`: baseline matching and estimates, saves `results/paper_final_results.rds`
2. `code/02_estimation/EVENT_v4.R`: main event-time specification (Callaway and Sant'Anna), control pools A, B and C, robustness checks, Rambachan-Roth sensitivity, balance; saves `results/event_v4_results.rds`
3. `code/02_estimation/CALENDAR_v4.R`: calendar-time specification for the Supplementary Information; saves `results/calendar_v4_results.rds`
4. `code/02_estimation/bilanciamento_opzioni.R`: balance under alternative sample definitions (Table S on matching)
5. `code/03_tables_figures/make_assets.R`: Figure 1 (diffusion), persistence figure and table
6. `code/03_tables_figures/TABLES_v4.R`: main and supplementary tables
7. `code/03_tables_figures/FIGURE_v4.R`: event-study figures (final versions)

The order of steps 5 to 7 matters: later scripts overwrite some files written by earlier ones with their final version.

A few supplementary items were typeset directly from the logs rather than written by a script: `tabS_matching.tex` (from `logs/bilanciamento_opzioni_log.txt` and `logs/event_v4_log.txt`), `tabS_thresholds_v3.tex` (from `data/soglie_campo_anno.csv`), `tabS5_flags.tex` (from `logs/flags_log.txt`), and the lexical-index figures `fig6_lexicon.pdf` and `fig7_lexicon_fields.pdf` (from `data/lessico_campo_anno.csv`).

The estimation objects in `results/` are included, so the main and supplementary tables and the event-study figures can be regenerated with steps 6 and 7 only, without re-running the estimation and without the Zenodo files. Step 5 also needs the panel.

The scripts in `code/01_build_data/` rebuild the derived data files from the raw downloads:

- `coda_da_disco_v1.py`: fixed-window citations, field-year thresholds (`tail_fixed_author_year.csv.gz`, `soglie_campo_anno.csv`)
- `posizioni_autori_v1.py`: author position on the first transformer paper (`posizioni_primo_paper.csv`); needs `ids_poolC_matched.csv`, written by `code/02_estimation/ids_campione_v1.R`
- `scarica_abstract_campi.py`: stratified abstract download from the OpenAlex API and lexical index (`lessico_campo_anno.csv`)
- `flags_check.py`: calibration of the OpenAlex top-percentile flags (Supplementary Information)


