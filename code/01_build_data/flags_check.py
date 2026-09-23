import pandas as pd

base = "data"
out = "logs/flags_log.txt"

lines = []
rt = pd.read_csv(f"{base}/RTransformerSISTEMATO.csv.gz", usecols=["id", "publication_year", "big_hits1%", "big_hits10%", "fwci", "cited_by_count"])
rt = rt.drop_duplicates("id")
rt["b1"] = rt["big_hits1%"].astype(str).str.lower().isin(["true", "1", "1.0"])
rt["b10"] = rt["big_hits10%"].astype(str).str.lower().isin(["true", "1", "1.0"])
g = rt.groupby("publication_year").agg(n=("id", "size"), top1=("b1", "mean"), top10=("b10", "mean"),
                                       fwci_med=("fwci", "median"), cit_med=("cited_by_count", "median"))
lines.append("=== corpus transformer-citing, per anno ===\n" + g.round(3).to_string())
lines.append("\nfwci mediano dei paper flaggati top1: %.2f | non flaggati: %.2f" % (
    rt.loc[rt.b1, "fwci"].median(), rt.loc[~rt.b1, "fwci"].median()))
lines.append("cited_by mediano dei paper flaggati top1: %.1f | non flaggati: %.1f" % (
    rt.loc[rt.b1, "cited_by_count"].median(), rt.loc[~rt.b1, "cited_by_count"].median()))
lines.append("top1 ma non top10 (incoerenza): %.4f" % (rt.b1 & ~rt.b10).mean())

df = pd.read_csv(f"{base}/panel_didi_correctJULY.csv.gz",
                 usecols=["author_id", "publication_year", "treated", "total_count", "big_hits1", "big_hits10", "citation_three_years"])
df["b1"] = df["big_hits1"].fillna(0); df["b10"] = df["big_hits10"].fillna(0); df["w"] = df["total_count"].fillna(0)
p = df.groupby("publication_year").apply(lambda d: pd.Series({
    "autori": len(d), "works_medi": d.w.mean(),
    "top1_su_works": d.b1.sum() / max(d.w.sum(), 1), "top10_su_works": d.b10.sum() / max(d.w.sum(), 1),
    "quota_ay_con_top1": (d.b1 > 0).mean(), "cit3y_media": d.citation_three_years.mean()}))
lines.append("\n=== panel, tutti gli autori, per anno ===\n" + p.round(3).to_string())
lines.append("\nb1 > b10 (incoerenza, righe): %.5f" % (df.b1 > df.b10).mean())
lines.append("b1 > works (incoerenza, righe): %.5f" % (df.b1 > df.w).mean())
t = df.groupby("treated").apply(lambda d: pd.Series({
    "top1_su_works": d.b1.sum() / d.w.sum(), "top10_su_works": d.b10.sum() / d.w.sum(), "works_medi": d.w.mean()}))
lines.append("\n=== panel, per trattato ===\n" + t.round(3).to_string())
open(out, "w", encoding="utf-8").write("\n".join(lines) + "\nFATTO.\n")
