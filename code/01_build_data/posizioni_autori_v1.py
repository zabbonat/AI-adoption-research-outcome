# Posizione dell'adottante sul suo primo paper transformer (first, middle, last, corresponding),
# dai file per autore su D:\Transformer (cartelle POST2018 e ANTE2018 dei trattati).

import ast
import csv
import os
from pathlib import Path

import pandas as pd
from tqdm import tqdm

csv.field_size_limit(2**31 - 1)
BASE = Path("data")  # scripts are run from the repository root
FIX = BASE
ROOT = Path("raw/openalex_authors")  # per-author OpenAlex downloads (May 2025), not included

ids = pd.read_csv(FIX / "ids_poolC_matched.csv")
adopters = ids[(ids["trattato"] == 1)]["aid"].astype(int).tolist()
rt = pd.read_csv(BASE / "RTransformerSISTEMATO.csv.gz", usecols=["id"])
trans_ids = set(rt["id"].astype(str))

rows = []
for aid in tqdm(adopters):
    best = None
    for fo in ["POST2018", "ANTE2018"]:
        p = ROOT / fo / f"A{aid}_publications.csv"
        if not p.exists():
            continue
        with open(p, encoding="utf-8", errors="replace", newline="") as f:
            r = csv.DictReader(f)
            for row in r:
                if row.get("id") not in trans_ids:
                    continue
                try:
                    y = int(float(row["publication_year"]))
                except Exception:
                    continue
                if best is not None and y >= best["anno"]:
                    continue
                try:
                    au = ast.literal_eval(row["authorships"])
                except Exception:
                    au = []
                pos, corr, n = None, None, len(au)
                for a in au:
                    if (a.get("author") or {}).get("id", "").endswith(f"/A{aid}"):
                        pos = a.get("author_position"); corr = a.get("is_corresponding")
                        break
                best = {"aid": aid, "anno": y, "posizione": pos, "corrispondente": corr, "n_autori": n,
                        "wid": row["id"].rsplit("/", 1)[-1]}
    if best is not None:
        rows.append(best)

out = pd.DataFrame(rows)
out.to_csv(FIX / "posizioni_primo_paper.csv", index=False)
with open(Path("logs") / "posizioni_log.txt", "w", encoding="utf-8") as f:
    f.write(f"adottanti appaiati: {len(adopters)} | con primo paper trovato: {len(out)}\n")
    f.write(out["posizione"].value_counts(dropna=False).to_string() + "\n")
    f.write("corrispondente:\n" + out["corrispondente"].value_counts(dropna=False).to_string() + "\n")
    f.write("n_autori:\n" + out["n_autori"].describe().to_string() + "\n")
    f.write("primo o ultimo o corrispondente: %d\n" % ((out["posizione"].isin(["first", "last"])) | (out["corrispondente"] == True)).sum())
print("fatto", len(out))
