# BioFuturo na Cidade (iNaturalist) — Data & Code Package

This repository contains the **data package (Darwin Core Archive, DwC-A)** and the **analysis code (R)** used in the study:

> **Patterns of people’s engagement set off by Bioblitzes in urban green areas** (Biotropica — Special Issue, urban ecology)

The goals of this repository are to:
1. Provide a **reproducible pipeline** to build a DwC-A from the project’s raw iNaturalist export;
2. Preserve **per-record licensing and attribution fields** as provided by iNaturalist;
3. Provide the **R scripts** used to compute metrics and generate figures/tables in the manuscript.

---

## What is included

### 1) Darwin Core Archive (DwC-A)
The DwC-A is generated as a standard ZIP file containing:
- `occurrence.txt` (TSV)
- `meta.xml`
- `eml.xml`

In addition to core Darwin Core terms, the package includes project-specific context:
- `eventID` encodes the BioBlitz municipality and window (Before/During/After) derived from `data/raw/bioblitz_windows_summary.csv`.
- `dynamicProperties` stores a compact JSON with additional fields useful for reuse (e.g., municipality tag, window, quality grade).

### 2) Code
- `tools/build_dwca.py`: builds and validates the DwC-A from the raw Excel export.
- `tools/export_processed_csv.py`: exports a local CSV input expected by several R scripts.
- `R/scripts/*.R`: R scripts used for analysis and figure exports (comment-free versions for readability).

---

## Quick start (for co-authors)

### A) Download the repository
- **Option 1:** Use GitHub “Code → Download ZIP”.
- **Option 2:** Clone with Git:
  ```bash
  git clone https://github.com/renatonm-dcnme/biofuturo-inat-dwca.git
  ```

### B) Build the DwC-A (Python)
From the repository root:

```bash
python -m venv .venv
# Windows PowerShell:
.\.venv\Scripts\Activate.ps1
pip install -r requirements.txt
python tools\build_dwca.py --config dwc_terms.yaml --excel data\raw\Raw.xlsx --out outputs\dwca
```

This produces:
- `outputs/dwca/occurrence.txt`
- `outputs/dwca/eml.xml`
- `outputs/dwca/meta.xml`
- `outputs/dwca/biofuturo_dwca.zip`
- `outputs/reports/dwca_validation.json`

> If PowerShell blocks activation scripts, run:
> `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass`

### C) Prepare inputs for R scripts (optional, for full reproducibility)
Several R scripts read a local CSV version of the main table at:
`data/processed/02_mestre_filtrada_bioblitz.csv`.

To generate that file from the Excel export, run:

```bash
python tools\export_processed_csv.py --excel data\raw\Raw.xlsx --out data\processed
```

(Outputs are local; `data/processed/` is expected to be ignored by git.)

### D) Run R analyses / export figures
Scripts are in `R/scripts/`. In general, you can run them in numeric order (06 → 35).  
Most scripts write figures and intermediate CSV summaries into `outputs/`.

See `R/README.md` for a script-by-script overview and which scripts correspond to each figure.

---

## Citation

GitHub reads `CITATION.cff` and shows a “Cite this repository” panel.  
When we connect this repository to Zenodo, each GitHub Release will be archived and will receive:
- a **versioned DOI** (e.g., v1.0.0), and
- a **concept DOI** pointing to the latest version.

Until the DOI is minted, please cite the repository URL and the related manuscript.

---

## Licensing and attribution (important)

This project is based on iNaturalist observations. iNaturalist allows different licenses **per observation**.
Accordingly:
- the DwC-A preserves licensing/attribution fields when provided by iNaturalist (e.g., `license`, `rightsHolder`, `references`);
- the repository-level license applies to the **code and repository content**, but does not override per-record licenses.

If you reuse records, please comply with the license associated with each observation.

---

## AI assistance transparency

Some parts of the data-packaging and scripting workflow were refined with assistance from LLM-based tools (e.g., OpenAI Codex).
All analytical decisions, validation, and final implementation were performed and verified by the authors.

---

## Contact
For questions about the dataset or this repository, please contact:
- **Renato Nallin Montagnolli** (UFSCar) — renatonm@ufscar.br
