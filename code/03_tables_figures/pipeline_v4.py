# Figura della pipeline dei dati, con i numeri della versione corrente e senza titolo interno.
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch

OUT = "output/figures/data_pipeline.png"

fig, ax = plt.subplots(figsize=(7.2, 6.4))
ax.set_xlim(-0.6, 10.6); ax.set_ylim(0, 11.4); ax.axis("off")

def box(x, y, w, h, title, lines, fill="#f2f2f2", edge="#444444"):
    ax.add_patch(FancyBboxPatch((x - w / 2, y - h / 2), w, h, boxstyle="round,pad=0.06",
                                linewidth=0.8, edgecolor=edge, facecolor=fill))
    ax.text(x, y + h / 2 - 0.30, title, ha="center", va="center", fontsize=9, fontweight="bold")
    for i, ln in enumerate(lines):
        ax.text(x, y + h / 2 - 0.62 - 0.28 * i, ln, ha="center", va="center", fontsize=7.3, color="#333333")

def arrow(x1, y1, x2, y2):
    ax.add_patch(FancyArrowPatch((x1, y1), (x2, y2), arrowstyle="-|>", mutation_scale=9,
                                 linewidth=0.8, color="#444444", shrinkA=0, shrinkB=0))

box(2.6, 10.6, 4.0, 1.1, "Transformer catalogue", ["392 architectures, 2017–2024", "230 cited at least once"], fill="#ffffff")
box(7.4, 10.6, 4.0, 1.1, "OpenAlex", ["Publication and citation records", "retrieved May 2025"], fill="#ffffff")
arrow(2.6, 10.05, 4.3, 9.35); arrow(7.4, 10.05, 5.7, 9.35)

box(5.0, 8.75, 7.4, 1.2, "Transformer-citing papers",
    ["89,034 records, 45,603 distinct papers, 2018–2024", "papers classified in computer science alone excluded"])
arrow(5.0, 8.15, 5.0, 7.55)

box(5.0, 6.9, 7.4, 1.3, "Author-year panel",
    ["357,106 researchers, 3,489,667 observations, 2012–2024",
     "built from their 15.6 million distinct publications",
     "citation window fixed at three years; field-year thresholds"])
arrow(5.0, 6.25, 5.0, 5.65)

box(5.0, 4.95, 8.0, 1.3, "Population of interest",
    ["authors whose modal field is not computer science",
     "adopters dated by first transformer-citing paper, cohorts to 2022",
     "8,356 adopters and 13,862 pool C controls meet the pre-2018 requirements"])
arrow(5.0, 4.30, 5.0, 3.70)

box(5.0, 3.05, 7.4, 1.1, "Matching within that population",
    ["1:1 propensity score, caliper 0.05 SD, no replacement",
     "1,984 pairs, largest standardised difference 0.093"])
arrow(5.0, 2.50, 5.0, 1.90)

box(5.0, 1.30, 8.0, 1.1, "Event-time difference-in-differences",
    ["Callaway and Sant'Anna estimator, never-adopting controls, two-year anticipation",
     "relative-magnitudes sensitivity analysis on every estimate"])

fig.savefig(OUT, dpi=400, bbox_inches="tight", facecolor="white")
print("scritto", OUT)
