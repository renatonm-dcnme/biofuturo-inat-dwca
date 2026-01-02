# BioFuturo (iNaturalist) — Reproducible Data Package (DwC-A + code)

This repository skeleton builds a Darwin Core Archive (DwC-A) from the BioFuturo iNaturalist raw Excel export.

## What it produces

- `outputs/dwca/occurrence.txt` (TSV)
- `outputs/dwca/meta.xml`
- `outputs/dwca/eml.xml` (template; you will edit creator/contact/repo fields)
- `outputs/dwca/biofuturo_dwca.zip` (the DwC-A to deposit / publish)

## Folder layout

- `data/raw/` — raw inputs (Excel + window summary)
- `tools/` — Python tools to curate/publish
- `R/` — R pipeline (analysis, figures) (to be added)
- `data/derived/` — tidy derived CSVs (to be added from R)
- `outputs/` — generated artifacts

## Quick start (Windows)

1) Install Python 3.11+ (from python.org) and check it works:
   - Open **PowerShell** and run:
     ```powershell
     python --version
     ```

2) Create a virtual environment (recommended):
   ```powershell
   cd "C:\Users\user\OneDrive\Documentos\UFSCar\Publicações\Artigo Biotropica (BioFuturo)\Datapaper"
   python -m venv .venv
   .\.venv\Scripts\Activate.ps1
   ```

3) Install requirements:
   ```powershell
   pip install -r requirements.txt
   ```

4) Build the DwC-A:
   ```powershell
   python tools\build_dwca.py --config dwc_terms.yaml --excel data\raw\Raw.xlsx --out outputs\dwca
   ```

5) Check outputs:
   - `outputs/dwca/biofuturo_dwca.zip`
   - validation report: `outputs/reports/dwca_validation.json`

## Notes

- We start with **Occurrence Core**. The BioBlitz windows (Before/During/After, municipality) are stored in `dynamicProperties` and `eventID`.
- Individual records keep their original iNaturalist license when available (`license_code` mapped to a CC URL in `license`).
