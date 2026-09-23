# Abstract stratificati per campo e anno, per la sezione sull uso implicito.
#
# Il pilota su un campione generale mostra che l indice lessicale passa da 1.00
# nel 2021 a 4.96 nel 2024 mentre le parole di controllo restano ferme. Per
# poter dire qualcosa sui campi serve un campione stratificato, altrimenti le
# differenze fra discipline sono rumore di Poisson.

import json
import os
import re
import time
from collections import Counter
from pathlib import Path

import pandas as pd
import requests
from tqdm import tqdm

BASE = Path("data")  # scripts are run from the repository root
FIX = BASE
GREZZI = Path("raw/openalex_abstract")
GREZZI.mkdir(exist_ok=True)

# campi OpenAlex, identificativi numerici
CAMPI = {
    "Medicine": 27, "Engineering": 22, "Computer Science": 17,
    "Biochemistry, Genetics and Molecular Biology": 13, "Social Sciences": 33,
    "Agricultural and Biological Sciences": 11, "Physics and Astronomy": 31,
    "Chemistry": 16, "Materials Science": 25, "Environmental Science": 23,
}
ANNI = list(range(2018, 2025))
N = 1500

MARC = ["delve", "delves", "delving", "intricate", "intricacies", "meticulous",
        "meticulously", "underscore", "underscores", "underscoring", "pivotal",
        "realm", "realms", "showcasing", "showcase", "garnered", "testament",
        "unwavering", "noteworthy", "versatile", "adept", "commendable",
        "multifaceted", "nuanced", "harnessing"]
CTRL = ["however", "therefore", "results", "method", "methods", "analysis",
        "data", "study", "observed", "measured", "significant", "sample",
        "model", "effect", "compared", "increase", "reported", "found"]
PAROLE = set(MARC) | set(CTRL)
tok = re.compile(r"[a-z]+")


def testo(inv):
    if not inv:
        return ""
    return " ".join(w for _, w in sorted((p, w) for w, ps in inv.items() for p in ps))


righe = []
for campo, fid in tqdm(CAMPI.items(), desc="campi"):
    for anno in ANNI:
        cache = GREZZI / f"abs_{fid}_{anno}.json"
        if cache.exists():
            testi = json.loads(cache.read_text(encoding="utf-8"))
        else:
            testi = []
            for pagina in range(1, N // 200 + 2):
                try:
                    r = requests.get(
                        "https://api.openalex.org/works",
                        params={
                            "filter": f"publication_year:{anno},type:article,"
                                      f"has_abstract:true,primary_topic.field.id:fields/{fid}",
                            "sample": N, "seed": 42, "per-page": 200, "page": pagina,
                            "select": "id,abstract_inverted_index",
                            "mailto": "didiabbo@gmail.com",
                        "api_key": os.environ.get("OPENALEX_KEY", ""),
                        },
                        timeout=120,
                    )
                    if r.status_code in (429, 503):
                        time.sleep(8)
                        continue
                    if r.status_code != 200:
                        break
                    res = r.json().get("results", [])
                except Exception:
                    time.sleep(5)
                    continue
                if not res:
                    break
                for w in res:
                    t = testo(w.get("abstract_inverted_index"))
                    if len(t.split()) >= 30:
                        testi.append(t)
                time.sleep(0.4)
            if testi:
                cache.write_text(json.dumps(testi), encoding="utf-8")

        c, tot = Counter(), 0
        for t in testi:
            ws = tok.findall(t.lower())
            tot += len(ws)
            for x in ws:
                if x in PAROLE:
                    c[x] += 1
        if tot == 0:
            continue
        m = sum(c.get(p, 0) for p in MARC)
        k = sum(c.get(p, 0) for p in CTRL)
        righe.append({"campo": campo, "anno": anno, "n_abs": len(testi), "n_parole": tot,
                      "marc_per10k": 10000 * m / tot, "ctrl_per10k": 10000 * k / tot,
                      "rapporto": m / max(k, 1)})

d = pd.DataFrame(righe)
d.to_csv(FIX / "lessico_campo_anno.csv", index=False)
print(d.pivot_table(index="campo", columns="anno", values="marc_per10k").round(2).to_string())
