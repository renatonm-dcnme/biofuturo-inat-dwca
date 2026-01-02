#!/usr/bin/env python3
"""
Export a processed CSV used by the R analysis scripts from the raw Excel export.

This does NOT modify the DwC-A. It simply provides a local CSV mirror of the main
table expected by several R scripts (data/processed/02_mestre_filtrada_bioblitz.csv).

Usage:
  python tools/export_processed_csv.py --excel data/raw/Raw.xlsx --out data/processed
"""
from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--excel", required=True, help="Path to the raw Excel export (e.g., data/raw/Raw.xlsx)")
    ap.add_argument("--sheet", default="02_mestre_filtrada_bioblitz", help="Excel sheet name to export")
    ap.add_argument("--out", required=True, help="Output folder (e.g., data/processed)")
    args = ap.parse_args()

    excel_path = Path(args.excel)
    out_dir = Path(args.out)
    out_dir.mkdir(parents=True, exist_ok=True)

    df = pd.read_excel(excel_path, sheet_name=args.sheet)
    out_path = out_dir / f"{args.sheet}.csv"
    df.to_csv(out_path, index=False)

    print(f"✅ Wrote: {out_path}")


if __name__ == "__main__":
    main()
