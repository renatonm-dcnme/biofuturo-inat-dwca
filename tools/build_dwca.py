#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Build a Darwin Core Archive (DwC-A) from the BioFuturo iNaturalist Excel export.

Inputs:
  - Raw.xlsx (Excel) with sheet containing observation-level columns
  - bioblitz_windows_summary.csv (municipality window definitions)

Outputs (in outputs/dwca by default):
  - occurrence.txt (TSV, UTF-8)
  - meta.xml
  - eml.xml
  - biofuturo_dwca.zip

Usage example:
  python tools/build_dwca.py --config dwc_terms.yaml --excel data/raw/Raw.xlsx --out outputs/dwca
"""
from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import pandas as pd
import yaml


DWC_NS = "http://rs.tdwg.org/dwc/terms/"
DWC_TEXT_NS = "http://rs.tdwg.org/dwc/text/"
ROWTYPE_OCCURRENCE = DWC_NS + "Occurrence"

LICENSE_URLS = {
    "cc0": "https://creativecommons.org/publicdomain/zero/1.0/",
    "cc-by": "https://creativecommons.org/licenses/by/4.0/",
    "cc-by-sa": "https://creativecommons.org/licenses/by-sa/4.0/",
    "cc-by-nc": "https://creativecommons.org/licenses/by-nc/4.0/",
    "cc-by-nc-sa": "https://creativecommons.org/licenses/by-nc-sa/4.0/",
    "cc-by-nd": "https://creativecommons.org/licenses/by-nd/4.0/",
    "cc-by-nc-nd": "https://creativecommons.org/licenses/by-nc-nd/4.0/",
}


def parse_iso_datetime(val: Any) -> Optional[str]:
    """Return ISO 8601 string (with Z if UTC) or None."""
    if val is None or (isinstance(val, float) and pd.isna(val)) or (isinstance(val, str) and val.strip() == ""):
        return None
    try:
        # pandas handles many ISO strings; force UTC if Z present.
        ts = pd.to_datetime(val, utc=True, errors="coerce")
        if pd.isna(ts):
            return None
        # Output as ISO 8601 with Z
        return ts.strftime("%Y-%m-%dT%H:%M:%SZ")
    except Exception:
        return None


def split_location(loc: Any) -> Tuple[Optional[float], Optional[float]]:
    """Split 'lat,lon' into floats."""
    if loc is None or (isinstance(loc, float) and pd.isna(loc)):
        return None, None
    s = str(loc).strip()
    m = re.match(r"^\s*(-?\d+(?:\.\d+)?)\s*,\s*(-?\d+(?:\.\d+)?)\s*$", s)
    if not m:
        return None, None
    return float(m.group(1)), float(m.group(2))


def pick_first_nonempty(*vals: Any) -> Optional[str]:
    for v in vals:
        if v is None:
            continue
        if isinstance(v, float) and pd.isna(v):
            continue
        s = str(v).strip()
        if s != "" and s.lower() != "nan":
            return s
    return None


def license_to_url(code: Any) -> Optional[str]:
    if code is None or (isinstance(code, float) and pd.isna(code)):
        return None
    s = str(code).strip().lower()
    if s == "" or s == "nan":
        return None
    return LICENSE_URLS.get(s, s)


def taxon_to_uri(taxon_id: Any) -> Optional[str]:
    if taxon_id is None or (isinstance(taxon_id, float) and pd.isna(taxon_id)):
        return None
    try:
        i = int(float(taxon_id))
        if i <= 0:
            return None
        return f"https://www.inaturalist.org/taxa/{i}"
    except Exception:
        return None


def compute_period(municipio: str, obs_date: datetime, windows_df: pd.DataFrame) -> Optional[str]:
    """Compute Before/During/After given windows summary."""
    row = windows_df.loc[windows_df["municipio_tag"] == municipio]
    if row.empty:
        return None
    r = row.iloc[0]
    # Dates in CSV are YYYY-MM-DD
    b0 = datetime.fromisoformat(str(r["Before_start_date"])).date()
    b1 = datetime.fromisoformat(str(r["Before_end_date"])).date()
    d0 = datetime.fromisoformat(str(r["During_start_date"])).date()
    d1 = datetime.fromisoformat(str(r["During_end_date"])).date()
    a0 = datetime.fromisoformat(str(r["After_start_date"])).date()
    a1 = datetime.fromisoformat(str(r["After_end_date"])).date()

    od = obs_date.date()
    if b0 <= od <= b1:
        return "Before"
    if d0 <= od <= d1:
        return "During"
    if a0 <= od <= a1:
        return "After"
    return None


def compute_event_id(municipio: Optional[str], period: Optional[str], windows_df: pd.DataFrame) -> Optional[str]:
    if not municipio or not period:
        return None
    row = windows_df.loc[windows_df["municipio_tag"] == municipio]
    if row.empty:
        return f"BioFuturo:{municipio}:{period}"
    r = row.iloc[0]
    start = r[f"{period}_start_date"]
    end = r[f"{period}_end_date"]
    return f"BioFuturo:{municipio}:{period}:{start}..{end}"


def dynamic_props(row: pd.Series, computed: Dict[str, Any]) -> str:
    obj = {
        "municipio": computed.get("municipio"),
        "period": computed.get("period"),
        "quality_grade": pick_first_nonempty(row.get("quality_grade")),
        "inat_observation_id": pick_first_nonempty(row.get("id")),
        "inat_uri": pick_first_nonempty(row.get("uri")),
        "project_ids": row.get("project_ids") if "project_ids" in row else None,
        "coordinates_obscured": bool(row.get("coordinates_obscured")) if pick_first_nonempty(row.get("coordinates_obscured")) is not None else None,
        "geoprivacy": pick_first_nonempty(row.get("geoprivacy")),
        "license_code": pick_first_nonempty(row.get("license_code")),
    }
    # drop None keys
    obj = {k: v for k, v in obj.items() if v is not None and str(v) != "nan"}
    return json.dumps(obj, ensure_ascii=False, separators=(",", ":"))


def data_generalizations(row: pd.Series) -> Optional[str]:
    # If coordinates obscured/obscured geoprivacy, state it.
    if pick_first_nonempty(row.get("geoprivacy")) == "obscured" or str(row.get("coordinates_obscured")).lower() == "true":
        return "Coordinates may be generalized/obscured by iNaturalist geoprivacy."
    return None


def information_withheld(row: pd.Series) -> Optional[str]:
    if pick_first_nonempty(row.get("geoprivacy")) == "obscured" or str(row.get("coordinates_obscured")).lower() == "true":
        return "Exact coordinates may be withheld/obscured by iNaturalist."
    return None


def coord_uncertainty_m(row: pd.Series) -> Optional[str]:
    v = row.get("positional_accuracy")
    w = row.get("public_positional_accuracy")
    for x in (v, w):
        if x is None or (isinstance(x, float) and pd.isna(x)):
            continue
        try:
            return str(int(round(float(x))))
        except Exception:
            continue
    return None


def recorded_by(row: pd.Series) -> Optional[str]:
    return pick_first_nonempty(row.get("user_name"), row.get("user_login"))


def rights_holder(row: pd.Series) -> Optional[str]:
    # Default to recordedBy, but keep blank if unknown.
    return recorded_by(row)


def build_meta_xml(fields: List[Dict[str, Any]]) -> str:
    # fields are in output order; occurrenceID must be index 0.
    lines = []
    for i, f in enumerate(fields):
        term = f["term"]
        term_uri = term if term.startswith("http") else (DWC_NS + term)
        lines.append(f'    <field index="{i}" term="{term_uri}" />')
    fields_block = "\n".join(lines)
    tpl = (Path(__file__).parent / "templates" / "meta.xml.tpl").read_text(encoding="utf-8")
    return tpl.replace("{FIELDS_BLOCK}", fields_block)


def build_eml_xml(cfg: Dict[str, Any], bbox: Dict[str, float], date_begin: str, date_end: str, geo_desc: str) -> str:
    tpl = (Path(__file__).parent / "templates" / "eml.xml.tpl").read_text(encoding="utf-8")
    ds = cfg["dataset"]
    now = datetime.now(timezone.utc).date().isoformat()

    # naive split name
    contact_name = ds.get("contact", {}).get("name", "FILL_ME")
    cn_parts = contact_name.split()
    contact_given = cn_parts[0] if cn_parts else "FILL_ME"
    contact_surname = cn_parts[-1] if cn_parts else "FILL_ME"

    # creator placeholders (you will edit later)
    creator_given = "FILL_ME"
    creator_surname = "FILL_ME"
    creator_email = "FILL_ME"

    filled = tpl.format(
        PACKAGE_ID="biofuturo.inat.dwca",
        GITHUB_REPO="FILL_ME/FILL_ME",
        LANG=ds.get("language", "en"),
        TITLE=ds.get("title", "FILL_ME"),
        CREATOR_SURNAME=creator_surname,
        CREATOR_GIVENNAME=creator_given,
        CREATOR_EMAIL=creator_email,
        CONTACT_SURNAME=contact_surname,
        CONTACT_GIVENNAME=contact_given,
        CONTACT_EMAIL=ds.get("contact", {}).get("email", "FILL_ME"),
        PUB_DATE=now,
        ABSTRACT=ds.get("description", ""),
        LICENSE_NAME=ds.get("license", {}).get("name", ""),
        LICENSE_URL=ds.get("license", {}).get("url", ""),
        GEO_DESC=geo_desc,
        WEST=bbox["west"],
        EAST=bbox["east"],
        NORTH=bbox["north"],
        SOUTH=bbox["south"],
        DATE_BEGIN=date_begin,
        DATE_END=date_end,
    )
    return filled


def validate_occurrence(df_out: pd.DataFrame) -> Dict[str, Any]:
    problems: Dict[str, Any] = {"errors": [], "warnings": [], "summary": {}}

    # occurrenceID uniqueness
    if df_out["occurrenceID"].isna().any():
        problems["errors"].append("Some rows have missing occurrenceID.")
    dup = df_out["occurrenceID"][df_out["occurrenceID"].duplicated()].unique().tolist()
    if dup:
        problems["errors"].append(f"Duplicate occurrenceID values found: {dup[:10]} (showing up to 10).")

    # required minimal fields check (IPT occurrence required fields)
    for col in ["occurrenceID", "basisOfRecord", "scientificName", "eventDate"]:
        if col in df_out.columns:
            miss = df_out[col].isna().sum()
            if miss > 0:
                problems["errors"].append(f"Missing required field '{col}' in {miss} rows.")

    # lat/lon range
    for col, lo, hi in [("decimalLatitude", -90, 90), ("decimalLongitude", -180, 180)]:
        if col in df_out.columns:
            bad = df_out[col].apply(lambda x: pd.notna(x) and (float(x) < lo or float(x) > hi)).sum()
            if bad:
                problems["errors"].append(f"Out-of-range values in {col}: {bad} rows.")

    problems["summary"] = {
        "n_rows": int(df_out.shape[0]),
        "n_cols": int(df_out.shape[1]),
        "n_errors": int(len(problems["errors"])),
        "n_warnings": int(len(problems["warnings"])),
    }
    return problems


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--config", default="dwc_terms.yaml", help="Path to dwc_terms.yaml")
    ap.add_argument("--excel", default="data/raw/Raw.xlsx", help="Path to Excel input")
    ap.add_argument("--out", default="outputs/dwca", help="Output directory")
    args = ap.parse_args()

    cfg_path = Path(args.config)
    cfg = yaml.safe_load(cfg_path.read_text(encoding="utf-8"))

    excel_path = Path(args.excel)
    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)

    windows_path = (cfg_path.parent / cfg["inputs"]["windows_csv"]).resolve()
    windows_df = pd.read_csv(windows_path)

    sheet = cfg["inputs"].get("excel_sheet")
    xls = pd.ExcelFile(excel_path)
    if sheet not in xls.sheet_names:
        sheet = xls.sheet_names[0]
    df = pd.read_excel(excel_path, sheet_name=sheet)

    # Build computed helpers
    # Event datetime
    event_iso = []
    event_dt = []
    for _, row in df.iterrows():
        raw = pick_first_nonempty(row.get("time_observed_at"), row.get("observed_on"))
        iso = parse_iso_datetime(raw)
        event_iso.append(iso)
        dt_obj = pd.to_datetime(iso, utc=True) if iso else pd.NaT
        event_dt.append(dt_obj)

    event_dt = pd.Series(event_dt)
    df["_eventDate_iso"] = event_iso

    # Municipality and period
    municipio = df.get("municipio_tag")
    if municipio is None:
        municipio = pd.Series([None] * len(df))
    period = df.get("period_en")
    if period is None:
        period = pd.Series([None] * len(df))

    # Fill missing period using windows
    period_filled = []
    for i in range(len(df)):
        mun = pick_first_nonempty(municipio.iloc[i])
        per = pick_first_nonempty(period.iloc[i])
        if per:
            period_filled.append(per)
            continue
        dt_i = event_dt.iloc[i]
        if pd.isna(dt_i) or not mun:
            period_filled.append(None)
            continue
        p = compute_period(mun, dt_i.to_pydatetime(), windows_df)
        period_filled.append(p)
    df["_period"] = period_filled
    df["_municipio"] = municipio

    # Build output rows
    out_rows: Dict[str, List[Any]] = {f["term"]: [] for f in cfg["fields"]}

    for _, row in df.iterrows():
        computed = {
            "municipio": pick_first_nonempty(row.get("municipio_tag")),
            "period": pick_first_nonempty(row.get("period_en"), row.get("_period")),
        }
        for f in cfg["fields"]:
            term = f["term"]
            val = None
            if "value" in f:
                val = f["value"]
            elif "transform" in f:
                t = f["transform"]
                if t == "iso_datetime":
                    val = pick_first_nonempty(row.get("time_observed_at"), row.get("observed_on"))
                    val = parse_iso_datetime(val)
                elif t == "location_lat":
                    lat, _ = split_location(row.get(f.get("source", "location")))
                    val = lat
                elif t == "location_lon":
                    _, lon = split_location(row.get(f.get("source", "location")))
                    val = lon
                elif t == "license_url":
                    val = license_to_url(row.get(f.get("source")))
                elif t == "taxon_uri":
                    val = taxon_to_uri(row.get(f.get("source")))
                elif t == "coord_uncertainty_m":
                    val = coord_uncertainty_m(row)
                elif t == "event_id":
                    val = compute_event_id(computed.get("municipio"), computed.get("period"), windows_df)
                elif t == "dynamic_properties_json":
                    val = dynamic_props(row, computed)
                elif t == "data_generalizations":
                    val = data_generalizations(row)
                elif t == "information_withheld":
                    val = information_withheld(row)
                elif t == "recorded_by":
                    val = recorded_by(row)
                elif t == "rights_holder":
                    val = rights_holder(row)
                else:
                    raise ValueError(f"Unknown transform: {t}")
            else:
                # copy source with fallbacks
                src = f.get("source")
                val = pick_first_nonempty(row.get(src))
                if val is None and f.get("fallback_source"):
                    val = pick_first_nonempty(row.get(f["fallback_source"]))
                if val is None and f.get("fallback_source2"):
                    val = pick_first_nonempty(row.get(f["fallback_source2"]))

            if (val is None or (isinstance(val, float) and pd.isna(val)) or (isinstance(val, str) and val.strip()=="")) and "value_if_missing" in f:
                val = f["value_if_missing"]
            out_rows[term].append(val)

    df_out = pd.DataFrame(out_rows)

    # Stringify lat/lon and keep numeric-friendly output
    for col in ["decimalLatitude", "decimalLongitude"]:
        if col in df_out.columns:
            df_out[col] = df_out[col].apply(lambda x: "" if x is None or (isinstance(x, float) and pd.isna(x)) else f"{float(x):.7f}")

    # eventDate already ISO strings
    # Write occurrence.txt (TSV with header)
    occ_path = out_dir / "occurrence.txt"
    df_out.to_csv(occ_path, sep="\t", index=False, encoding="utf-8", quoting=csv.QUOTE_MINIMAL)

    # Build meta.xml
    meta_xml = build_meta_xml(cfg["fields"])
    (out_dir / "meta.xml").write_text(meta_xml, encoding="utf-8")

    # Coverage for EML
    # bbox
    lat = pd.to_numeric(df_out["decimalLatitude"], errors="coerce")
    lon = pd.to_numeric(df_out["decimalLongitude"], errors="coerce")
    bbox = {
        "west": float(lon.min()),
        "east": float(lon.max()),
        "north": float(lat.max()),
        "south": float(lat.min()),
    }
    # temporal
    dt_series = pd.to_datetime(df_out["eventDate"], utc=True, errors="coerce")
    date_begin = dt_series.min().date().isoformat()
    date_end = dt_series.max().date().isoformat()

    municipios = sorted(set([m for m in df_out["municipality"].dropna().astype(str).tolist() if m.strip() != ""]))
    geo_desc = "Municipalities in São Paulo state, Brazil: " + ", ".join(municipios)

    eml_xml = build_eml_xml(cfg, bbox, date_begin, date_end, geo_desc)
    (out_dir / "eml.xml").write_text(eml_xml, encoding="utf-8")

    # Validate
    report = validate_occurrence(df_out)
    (out_dir.parent / "reports" / "dwca_validation.json").write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding="utf-8")

    # Package ZIP (DwC-A)
    zip_path = out_dir / "biofuturo_dwca.zip"
    import zipfile
    with zipfile.ZipFile(zip_path, "w", compression=zipfile.ZIP_DEFLATED) as z:
        z.write(occ_path, arcname="occurrence.txt")
        z.write(out_dir / "meta.xml", arcname="meta.xml")
        z.write(out_dir / "eml.xml", arcname="eml.xml")

    print(f"✅ Wrote: {occ_path}")
    print(f"✅ Wrote: {zip_path}")
    print(f"📋 Validation report: {out_dir.parent / 'reports' / 'dwca_validation.json'}")
    if report["errors"]:
        print("⚠️ Errors:")
        for e in report["errors"]:
            print(" -", e)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
