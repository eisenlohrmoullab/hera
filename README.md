# HERA: Hormones, Emotions, and Related Alcohol Use

**R01 Analysis Repository **

This repository contains the R analysis pipeline for the **HERA R01** study, which investigates alcohol use and mood/symptom fluctuations across the menstrual cycle. Data are collected via daily ecological momentary assessment (EMA) and include symptom ratings, alcohol use measures, and biologically-confirmed ovulation tracking (LH surge + Oura basal body temperature).

---

## Study Overview

The HERA project examines how alcohol use and related outcomes vary as a function of menstrual cycle phase, and how individual differences in affective sensitivity to the cycle might explain such changes). Cycle timing is standardized using the **PACTS** (Phase-Anchored Cycle Time Scaling) method via the [`menstrualcycleR`](https://github.com/eisenlohrmoullab/menstrualcycleR) package, which uses both menses onset and ovulation (LH surge) as anchors.

**Primary outcomes include:**
- Alcohol use (drinks consumed, any drinking, binge drinking)
- Premenstrual symptoms (DRSP; Daily Record of Severity of Problems)
---

## Repository Structure

```
hera/
├── HERA_00_setup.R              # Package loading, global options, output folder setup
├── HERA_01_data_prep.Rmd        # Data import, cleaning, cycle coding, DRSP scoring
├── HERA_04_descriptives.Rmd     # Descriptive statistics and visualizations
├��─ HERA_05_gamms.Rmd            # Generalized Additive Mixed Models (GAMMs)
├── HERA_06_smms.Rmd             # Smoothing Mixture Models (SMMs) – trajectory subgroups
├── HERA_07_smms_binary.Rmd      # SMMs for binary outcomes
├── HERA_08_single_GAMs.Rmd      # Individual-level GAMs
├── scripts/                     # Helper/utility R scripts sourced by the main Rmds
├── data/                        # Local copies of cleaned analysis datasets (not tracked)
├── zArchive/                    # Archived/deprecated scripts
└── hera.Rproj                   # RStudio project file
```

---

## Analysis Pipeline

Scripts are numbered and intended to be run in order:

| Script | Purpose |
|--------|---------|
| `HERA_00_setup.R` | Loads ~50 packages via `pacman`, sets global options, creates date-stamped output folder |
| `HERA_01_data_prep.Rmd` | Imports raw daily survey data, cleans variables, applies `pacts_scaling()`, computes DRSP subscales, saves clean `.rds` dataset |
| `HERA_04_descriptives.Rmd` | Descriptive statistics, data visualization, missingness summaries |
| `HERA_05_gamms.Rmd` | Fits cyclic GAMMs (via `mgcv`) for all DRSP outcomes; both menses- and ovulation-centered; saves model objects, summaries, and prediction plots |
| `HERA_06_smms.Rmd` | Fits Smoothing Mixture Models (SMMs) to identify latent trajectory subgroups across all outcomes; generates BIC comparison plots and PDF reports |
| `HERA_07_smms_binary.Rmd` | SMMs for binary outcomes (e.g., any alcohol use) |
| `HERA_08_single_GAMs.Rmd` | Single-subject GAM analyses |

---

## Setup & Requirements

### Software
- **R** (≥ 4.2 recommended)
- **RStudio** (recommended)
- **Box Desktop** (for accessing raw and output data stored in Box)

### R Packages

All packages are managed via [`pacman`](https://github.com/trinker/pacman) and installed/loaded automatically when `HERA_00_setup.R` is sourced. Key packages include:

| Category | Packages |
|----------|---------|
| Data wrangling | `tidyverse`, `lubridate`, `janitor`, `data.table`, `zoo` |
| Modeling | `mgcv`, `lme4`, `lmerTest`, `nlme` |
| Visualization | `ggplot2`, `patchwork`, `ggdist`, `sjPlot`, `corrplot`, `plotly` |
| Cycle analysis | `menstrualcycleR` |
| EDA | `skimr`, `visdat`, `naniar`, `psych` |
| Tidy outputs | `broom`, `broom.mixed`, `emmeans`, `marginaleffects` |

### Box Configuration

Raw data and outputs are stored in **Box**. Before running the pipeline, update the Box path in `HERA_`
