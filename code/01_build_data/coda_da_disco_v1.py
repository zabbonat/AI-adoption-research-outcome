# Coda in finestra fissa dai file per autore su D:\Transformer (download OpenAlex di
# maggio 2025, gli stessi da cui viene citation_three_years del panel).
# Per ogni lavoro: citazioni nei tre anni dalla pubblicazione (counts_by_year) e campo del
# topic principale. Soglie del decile e del centile per campo-anno sui lavori distinti
# dell'intero universo del panel; poi conteggi per autore-anno, lordi e al netto dei
# lavori che citano un transformer.

import csv
import os
import re
import sys
import time
from multiprocessing import Pool
from pathlib import Path

import numpy as np
import pandas as pd

csv.field_size_limit(2**31 - 1)
ROOT = "raw/openalex_authors"  # per-author OpenAlex downloads (May 2025), not included
FOLDERS = ["ANTE2018", "POST2018", "ANTE2018_notrans", "POST2018_notrans"]
BASE = Path("data")  # scripts are run from the repository root
FIX = BASE
OUT = Path("raw/coda_disco")
OUT.mkdir(exist_ok=True)

re_cy = re.compile(r"'year': (\d{4}), 'cited_by_count': (\d+)")
re_field = re.compile(r"'field': \{'id': 'https://openalex\.org/fields/(\d+)', 'display_name': '([^']*)'")


def parse_batch(paths):
    out = []
    names = {}
    for path in paths:
        mm = re.match(r"A(\d+)_publications\.csv$", os.path.basename(path))
        if not mm:
            continue
        aid = int(mm.group(1))
        try:
            with open(path, encoding="utf-8", errors="replace", newline="") as f:
                r = csv.reader(f)
                h = next(r, None)
                if not h:
                    continue
                idx = {k: i for i, k in enumerate(h)}
                ii, iy, ic, ip = idx.get("id"), idx.get("publication_year"), idx.get("counts_by_year"), idx.get("primary_topic")
                if ii is None or iy is None:
                    continue
                for row in r:
                    try:
                        y = int(float(row[iy]))
                    except Exception:
                        continue
                    c3 = 0
                    if ic is not None and ic < len(row):
                        for yr, c in re_cy.findall(row[ic]):
                            if y <= int(yr) <= y + 2:
                                c3 += int(c)
                    fid = -1
                    if ip is not None and ip < len(row):
                        m = re_field.search(row[ip])
                        if m:
                            fid = int(m.group(1)); names[fid] = m.group(2)
                    out.append((aid, row[ii].rsplit("/", 1)[-1], y, fid, c3))
        except Exception:
            continue
    return out, names


if __name__ == "__main__":
    t0 = time.time()
    files = []
    for fo in FOLDERS:
        with os.scandir(os.path.join(ROOT, fo)) as it:
            files += [e.path for e in it if e.name.endswith(".csv")]
    print("file:", len(files), f"({time.time()-t0:.0f}s)", flush=True)
    B = 400
    batches = [files[i:i + B] for i in range(0, len(files), B)]
    rows, names = [], {}
    done = 0
    with Pool(12) as pool:
        for out, nm in pool.imap_unordered(parse_batch, batches, chunksize=1):
            rows.extend(out); names.update(nm); done += 1
            if done % 100 == 0:
                print(f"batch {done}/{len(batches)} | righe {len(rows)} | {time.time()-t0:.0f}s", flush=True)
    df = pd.DataFrame(rows, columns=["aid", "wid", "year", "fid", "c3"])
    del rows
    df = df.drop_duplicates(["aid", "wid"])
    df["aid"] = df["aid"].astype("int64"); df["year"] = df["year"].astype("int32")
    df["fid"] = df["fid"].astype("int16"); df["c3"] = df["c3"].astype("int32")
    df.to_parquet(OUT / "works_panel.parquet", index=False)
    pd.Series(names).rename("field").to_csv(OUT / "field_names.csv", index_label="fid")
    print("coppie autore-lavoro:", len(df), "| lavori distinti:", df["wid"].nunique(), "| autori:", df["aid"].nunique(), f"({time.time()-t0:.0f}s)", flush=True)

    # soglie per campo-anno sui lavori distinti
    wd = df.drop_duplicates("wid")
    wd = wd[(wd["fid"] >= 0) & wd["year"].between(2012, 2024)]
    q = (wd.groupby(["fid", "year"])["c3"]
         .agg(n="size", q90=lambda s: np.quantile(s, 0.90, method="higher"),
              q99=lambda s: np.quantile(s, 0.99, method="higher"), media="mean")
         .reset_index())
    wd = wd.merge(q[["fid", "year", "q90", "q99"]], on=["fid", "year"])
    q = q.merge((wd["c3"] >= wd["q90"]).groupby([wd["fid"], wd["year"]]).mean().rename("quota90").reset_index(), on=["fid", "year"])
    q = q.merge((wd["c3"] >= wd["q99"]).groupby([wd["fid"], wd["year"]]).mean().rename("quota99").reset_index(), on=["fid", "year"])
    q["field"] = q["fid"].map(names)
    q.to_csv(FIX / "soglie_campo_anno.csv", index=False)

    # conteggi per autore-anno
    rt = pd.read_csv(BASE / "RTransformerSISTEMATO.csv.gz", usecols=["id"])
    trans_ids = set(rt["id"].astype(str).str.rsplit("/", n=1).str[-1])
    d = df[(df["fid"] >= 0) & df["year"].between(2012, 2024)].merge(q[["fid", "year", "q90", "q99"]], on=["fid", "year"], how="left")
    d["top10"] = (d["c3"] >= d["q90"]).astype("int8"); d["top1"] = (d["c3"] >= d["q99"]).astype("int8")
    d["trans"] = d["wid"].isin(trans_ids).astype("int8")
    d["top10_tr"] = d["top10"] * d["trans"]; d["top1_tr"] = d["top1"] * d["trans"]
    ay = (d.groupby(["aid", "year"])
          .agg(n_works_fw=("wid", "size"), c3_medio=("c3", "mean"), top10_fw=("top10", "sum"), top1_fw=("top1", "sum"),
               top10_tr=("top10_tr", "sum"), top1_tr=("top1_tr", "sum"), n_trans_fw=("trans", "sum"))
          .reset_index().rename(columns={"year": "publication_year"}))
    ay["top10_fw_net"] = ay["top10_fw"] - ay["top10_tr"]
    ay["top1_fw_net"] = ay["top1_fw"] - ay["top1_tr"]
    ay.to_csv(FIX / "tail_fixed_author_year.csv.gz", index=False)

    with open(Path("logs") / "coda_disco_log.txt", "w", encoding="utf-8") as f:
        f.write(f"file letti: {len(files)}\ncoppie autore-lavoro: {len(df)} | lavori distinti: {df['wid'].nunique()} | autori: {df['aid'].nunique()}\n")
        f.write(f"lavori distinti 2012-2024 con campo: {len(wd)} | senza campo: {int((df.drop_duplicates('wid')['fid'] < 0).sum())}\n\n")
        f.write("soglie medie per anno (media sui campi):\n" + q.groupby("year")[["n", "q90", "q99", "quota90", "quota99", "media"]].mean().round(3).to_string() + "\n\n")
        f.write("soglie 2019 per campo:\n" + q[q["year"] == 2019][["field", "n", "q90", "q99", "quota90", "quota99"]].to_string(index=False) + "\n\n")
        f.write(f"quota top10 fra i lavori (coppie): {d['top10'].mean():.4f} | top1: {d['top1'].mean():.4f} | lavori transformer: {int(d['trans'].sum())}\n")
        f.write(f"anni-autore: {len(ay)}\n" + ay[["n_works_fw", "c3_medio", "top10_fw", "top1_fw", "top10_fw_net", "top1_fw_net"]].describe().round(3).to_string() + "\n")
    print("fatto", f"({time.time()-t0:.0f}s)", flush=True)
