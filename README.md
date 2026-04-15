# HERA: Hormonal changes in Emotion Regulation and Alcohol

**R01 Analysis Repository**

This repository contains the R analysis pipeline for the **HERA R01** study, which investigates alcohol use and mood/symptom fluctuations across the menstrual cycle. Data are collected via daily ecological momentary assessment (EMA) and include symptom ratings (DRSP), alcohol use measures, and biologically-confirmed ovulation tracking (LH surge + Oura basal body temperature).

---

## Study Overview

The HERA project examines how alcohol use and related outcomes vary as a function of menstrual cycle phase, and how individual differences in affective sensitivity to the cycle might explain such changes. Cycle timing is standardized using the **PACTS** (Phase-Anchored Cycle Time Scaling) method via the [`menstrualcycleR`](https://github.com/eisenlohrmoullab/menstrualcycleR) package, which uses both menses onset and ovulation (LH surge) as anchors.

**Primary outcomes include:**
- Alcohol use (drinks consumed, any drinking, binge drinking)
- Premenstrual symptoms (DRSP; Daily Record of Severity of Problems — 21 individual items plus composite subscales)
- Emotion regulation difficulties

---

## Repository Structure

```
hera/
├── HERA_00_setup.R              # Package loading, global options, output folder setup
├── HERA_01_data_prep.Rmd        # Data import, cleaning, cycle coding, DRSP scoring
├── HERA_04_descriptives.Rmd     # Descriptive statistics and visualizations
├── HERA_05_gamms.Rmd            # Generalized Additive Mixed Models (GAMMs)
├── HERA_06_smms.Rmd             # Smoothing Mixture Models (SMMs) - trajectory subgroups
├── HERA_07_smms_binary.Rmd      # SMMs for binary outcomes (Bernoulli)
├── HERA_08_single_GAMs.Rmd      # Individual-level GAM reports (per-person & per-outcome PDFs)
├── scripts/                     # Helper/utility R scripts (see below)
├── data/                        # Local copies of cleaned analysis datasets (not tracked by git)
├── zArchive/                    # Archived/deprecated scripts
└── hera.Rproj                   # RStudio project file
```

---

## Analysis Pipeline

Scripts are numbered and intended to be run in order:

| Script | Purpose |
|--------|---------|
| `HERA_00_setup.R` | Loads ~50 packages via `pacman`, sets global options (no scientific notation, 3-digit precision), resolves namespace conflicts, creates date-stamped output folder on Box, and sources the centralized graphics configuration |
| `HERA_01_data_prep.Rmd` | Imports raw daily survey data from Box, cleans variables, parses dates, applies PACTS cycle-time scaling via `menstrualcycleR`, computes DRSP subscales, and saves a clean `.rds` dataset |
| `HERA_04_descriptives.Rmd` | Descriptive statistics, data visualization, and missingness summaries for all continuous and binary outcomes |
| `HERA_05_gamms.Rmd` | Fits cyclic GAMMs (via `mgcv`) for all DRSP and alcohol outcomes using both menses- and ovulation-centered time; includes covariates for day-of-week and lab-session effects; saves model objects, summaries, and prediction plots |
| `HERA_06_smms.Rmd` | Fits Smoothing Mixture Models (SMMs) via EM algorithm to identify latent trajectory subgroups across continuous outcomes; generates BIC comparison tables, trajectory plots, posterior-probability classifications, and post-hoc group labeling |
| `HERA_07_smms_binary.Rmd` | SMMs for binary outcomes (e.g., any drinking, binge drinking, felt drunk) using Bernoulli distribution with logit link; Pearson-residual-based group assignment |
| `HERA_08_single_GAMs.Rmd` | Individual-level cyclic GAM analyses (~2800 lines). Generates per-person and per-outcome landscape PDF reports including: cover/interpretation guide, summary tables, overlay GAM plots (color-coded by significance and range), logistic probability plots, histograms, empirical + model plots, peak/trough timing, and individual trajectory pages with quarter-coverage gating |

---

## Helper Scripts (`scripts/`)

| Script | Purpose |
|--------|---------|
| `graphics_config.R` | Centralized graphics configuration: `ragg` device setup (200 DPI), publication-ready theme (`theme_ukalc`), save helpers (`save_plot`, `save_png`) with PNG + SVG dual export and input validation |
| `preprocess_outcome.R` | Standardized outcome preprocessing: log(x+1) transformation, person-mean centering, 5-day rolling averages |
| `run_smm_cyclic.R` | Core SMM engine: EM algorithm with k-means initialization, cyclic GAMMs, RSS-based group reassignment, posterior probabilities, BIC computation |
| `run_smm_bernoulli.R` | Bernoulli-specific SMM implementation for binary outcomes with logit link and probability-scale predictions |
| `model_plot_modx_gam_cyclic.R` | Fits group-moderated GAMs with SMM group membership as moderator; generates group-specific trajectory plots |
| `label_smm_groups.R` | Interactive workflow for assigning descriptive labels to SMM groups (e.g., "Perimenstrual-Onset", "Luteal-Peak") with CSV save/load for reproducibility |
| `harmonize_traits.R` | Trait-level data harmonization: imports, recodes, and merges participant-level trait datasets across studies |
| `count_rows_ids.R` | Quick diagnostic: prints row counts and unique participant IDs |
| `compare_bic_cyclic.R` | BIC comparison across SMM group solutions |
| `subset_by_phase_cyclic.R` | Subset data by menstrual cycle phase |
| `plot_day_of_week.R` | Day-of-week effect visualization |
| `check_r_syntax.py` | Python-based R syntax checker |

---

## Setup & Requirements

### Software
- **R** (>= 4.2 recommended)
- **RStudio** (recommended)
- **Box Desktop** (for accessing raw and output data stored in Box)

### R Packages

All packages are managed via [`pacman`](https://github.com/trinker/pacman) and installed/loaded automatically when `HERA_00_setup.R` is sourced. Key packages include:

| Category | Packages |
|----------|---------|
| Data wrangling | `tidyverse`, `lubridate`, `janitor`, `data.table`, `zoo`, `readxl`, `haven` |
| Modeling | `mgcv`, `lme4`, `lmerTest`, `nlme`, `rmcorr` |
| Visualization | `ggplot2`, `ragg`, `patchwork`, `ggdist`, `ggrepel`, `sjPlot`, `corrplot`, `plotly`, `naniar` |
| Cycle analysis | [`menstrualcycleR`](https://github.com/eisenlohrmoullab/menstrualcycleR) |
| EDA & diagnostics | `skimr`, `visdat`, `dlookr`, `psych`, `performance` |
| Tidy model output | `broom`, `broom.mixed`, `emmeans`, `marginaleffects` |
| Matching & linkage | `fuzzyjoin`, `stringdist`, `MatchIt`, `RecordLinkage` |

### Box Configuration

Raw data and outputs are stored in **Box**. Before running the pipeline, update the Box path in `HERA_00_setup.R` to match your local Box sync folder:

```r
# Update this path to match your Box Desktop location:
output_folder <- paste0(
  "~/Library/CloudStorage/Box-Box/.../HERA-Primary ms/02_datasets_output/",
  current_date
)
```

To find your Box path: **Box Desktop > Preferences > Account > Open Box Folder**.

---

## Output Structure

All outputs are saved to date-stamped folders on Box (`YYYYMMDD/`), organized by pipeline stage:

```
02_datasets_output/
├── YYYYMMDD/
│   ├── 01_data_preparation/    # Data prep diagnostics and intermediate outputs
│   ├── 02_descriptives/        # Descriptive statistics and visualizations
│   ├── 03_gamms/               # GAMM models (per-outcome subfolders)
│   └── 04_smms/                # SMM models and classification results
└── 06_Single_GAMs/
    └── GAM_Reports_YYYYMMDD/   # Individual GAM PDF reports (per-person & per-outcome)
```

---

## Key Methods

### PACTS Cycle-Time Scaling
Menstrual cycle timing is standardized to a [-1, 1] scale using the PACTS method, with 0 anchored to either menses onset or ovulation (LH surge). This enables cross-participant comparison of cycle-phase effects.

### Cyclic GAMMs (Script 05)
Group-level generalized additive mixed models with cyclic cubic regression splines (`bs = "cc"`) to model smooth, periodic outcome trajectories across the cycle. Models include random intercepts and slopes per participant, plus covariates for day-of-week and lab-session effects.

### Smoothing Mixture Models (Scripts 06-07)
EM-algorithm-based latent class models that identify subgroups with distinct cycle trajectory shapes. K-means initialization, iterative GAM refitting, RSS/likelihood-based reassignment, posterior probability classification, and BIC-based model selection.

### Individual GAMs (Script 08)
Per-participant cyclic GAMs with quarter-coverage gating (all 4 cycle quarters must have >= 1 observation, and at most one quarter may have < 2 observations). Generates multi-page PDF reports with significance testing, person-specific range benchmarking (30% of each person's DRSP scale range, equivalent to the C-PASS-equivalent 30% change criterion), overlay plots, and individual trajectory visualization. Y-axis defaults to 1-4.5 with dynamic expansion to 5 or 6 when data exceed 4.5 (expanded region shaded pink).
