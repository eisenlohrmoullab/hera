# DRSP GAM Template

A reusable R Markdown template for automated, participant-centric analysis of DRSP (Daily Record of Severity of Problems) symptom cyclicity using cyclic Generalized Additive Models (GAMs).

Originally derived from the [HERA project](https://github.com/eisenlohrmoullab/hera)'s `HERA_08_single_GAMs.Rmd`.

## Features

- **Flexible DRSP column detection**: Automatically recognizes DRSP items regardless of naming convention. Any column containing "drsp" (case-insensitive) plus an item number is detected and renamed to the canonical `drsp_N` format. Supported examples:
  - `drsp_1`, `drsp1`, `DRSP1`, `DRSP_1`, `DRSP__1`, `drsp.1`
  - `DRSP1_depblue`, `drsp_1_raw`, `DRSP.01`
  - All automatically mapped to `drsp_1`, `drsp_2`, etc.
- **Automatic composite scoring**: Computes standard DRSP composites (Negative Affect, Distress, Social/Affect Lability, Low Arousal, Breast Sx, Eating Sx) from available items. Composites requiring missing items are skipped gracefully.
- **Per-participant PDF reports**: Landscape PDFs with raw data coverage, interpretation key, composite definitions, score distributions, overlaid GAM curves, significance tables, and individual outcome plots.
- **Overall sample summary PDF**: Key metrics, quarter coverage, per-outcome descriptives, range distributions, and significance rates across the full sample.
- **CSV audit file**: Per-ID × outcome × time-variable statistics for QC/triage.
- **Study-aware output structure**: Output folders and filenames are prefixed with your study name for easy organization (e.g., `HERA_GAM_20260416/`).
- **Self-contained graphics config**: All plotting functions accept their configuration via parameters — no hidden globals. The `build_domain_colors()` factory produces a self-contained color configuration object.
- **Configurable**: All paths, time variables, quality gates, benchmark settings, and report options are set in a single configuration file.

## Requirements

### R Packages

```r
install.packages(c(
  "dplyr", "ggplot2", "ggtext", "mgcv", "patchwork",
  "gridExtra", "grid", "tibble", "tidyr", "scales", "glue"
))
```

### Data Requirements

Your input dataset (`.rds` or `.csv`) must contain:

| Column | Description |
|--------|-------------|
| `id` | Participant identifier |
| DRSP items | Some or all DRSP items 1–21 (any naming convention — see below) |
| A cyclic time variable | Scaled to [−1, 1], e.g., `cyclic_time_impute` |

**Flexible column detection:** DRSP columns don't need to follow any exact naming convention. The template will automatically detect and rename any column whose name contains "drsp" (case-insensitive) and an item number (1–21). For example, all of these are recognized as item 1:

```
drsp_1, drsp1, DRSP1, DRSP_1, DRSP__1, drsp.1, DRSP1_depblue
```

If multiple columns map to the same item, the first match is kept and others are skipped with a warning.

## Quick Start

1. **Copy the template folder** to your project:

   ```
   templates/drsp_gam_template/
   ├── DRSP_GAM_Template.Rmd     # Main analysis template
   ├── drsp_gam_config.R          # User configuration (edit this!)
   ├── drsp_gam_scoring.R         # DRSP detection & composite scoring
   ├── drsp_gam_functions.R       # Core GAM fitting & plotting functions
   └── README.md                  # This file
   ```

2. **Edit `drsp_gam_config.R`**: Set your data file path, output directory, study name, and time variable names. At minimum, change:

   ```r
   data_file_path   <- "path/to/your_data.rds"
   gam_output_base  <- "path/to/output/"
   study_name       <- "HERA"         # Short slug for folder/filenames
   study_label      <- "HERA Study"   # Human-readable for report titles
   time_vars_to_run <- c("cyclic_time_impute")
   ```

3. **Knit `DRSP_GAM_Template.Rmd`** (or run chunk-by-chunk in RStudio). Reports will be generated in a study-specific, dated subfolder.

## Output Structure

```
output/
└── HERA_GAM_20260416/                    # <study_name>_GAM_<date>
    ├── per_id_reports/                    # Per-participant PDFs
    │   ├── HERA_ID_P001.pdf
    │   ├── HERA_ID_P002.pdf
    │   └── ...
    ├── per_outcome_reports/               # Per-outcome exploration PDFs
    │   ├── HERA_outcome_drsp_1.pdf
    │   ├── HERA_outcome_drsp_NA.pdf
    │   └── ...
    ├── HERA_overall_summary.pdf           # Sample-level summary
    └── HERA_gam_summary.csv              # Per-ID × outcome stats
```

## File Descriptions

### `drsp_gam_config.R`

User-facing configuration file. Edit this to customize:

- **Data input**: Path to your `.rds` or `.csv` file
- **Output directory**: Where reports and CSV summaries are saved
- **Study identification**: `study_name` (short slug for filenames/folders) and `study_label` (human-readable for titles)
- **Time variables**: Cyclic time variable name(s) in your data
- **Cycle phasing**: Whether time is centered on menses or ovulation
- **GAM quality gates**: `sparse_quarter_cutoff` (obs per quarter) and `min_obs_for_gam` (total obs per outcome)
- **Benchmark settings**: C-PASS-equivalent change percentage (default 30%)
- **Report layout**: PDF dimensions, plots per page
- **Report toggles**: Which outputs to generate
- **Domain colors**: Colorblind-safe color scheme for affective/physical domains

### `drsp_gam_scoring.R`

DRSP detection and composite scoring module. Provides:

- `detect_and_rename_drsp_columns(df)`: Fuzzy-matches DRSP columns in any naming convention and renames to canonical `drsp_N` format
- `score_drsp_composites(df)`: Auto-detects, renames, and scores composites; returns metadata including rename log
- `build_outcomes_map(items_found, composites_created)`: Builds the outcome label map
- `classify_outcomes(items_found, composite_specs)`: Classifies outcomes into affective/physical domains

### `drsp_gam_functions.R`

Core analysis and plotting functions (all self-contained — no global dependencies):

- `build_drsp_color_map()`: Colorblind-safe color assignments for individual items and composites
- `build_domain_colors()`: Self-contained domain color configuration factory (produces an object with `$affective`, `$physical`, `$label_colors()`, `$fill_scale()`, `$color_scale()`)
- `build_axis_meta()`: Cycle phase axis labels and reference lines
- `quarter_bin_counts()` / `check_quarter_coverage()`: Coverage quality checks
- `fit_outcome()`: Fits cyclic GAM with configurable quality gates (`sparse_cutoff`, `min_obs`)
- `create_annotated_plot()`: Individual outcome plot with annotations
- `plot_overlay_page()`: Multi-outcome overlay plot
- `expand_ymax()`: Dynamic Y-axis expansion

### `DRSP_GAM_Template.Rmd`

Main analysis template. Runs through:

1. Load configuration and functions
2. Load data, auto-detect & rename DRSP columns, score composites
3. Set up output directories, colors, axis metadata
4. Generate per-participant PDF reports
5. Generate overall sample summary PDF
6. Write CSV summary

## DRSP Composites

The following composites are automatically computed when their component items are all present:

| Composite | Domain | Component Items |
|-----------|--------|-----------------|
| `drsp_NA` (Negative Affect) | Affective | 1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17 |
| `drsp_distress` | Affective | 1, 2, 3, 4, 16, 17 |
| `drsp_labilsocsx` (Social/Affect Lability) | Affective | 5, 6, 7, 8 |
| `drsp_lowarousal` (Low Arousal) | Affective | 9, 10, 11, 14, 15 |
| `drsp_breastpain` (Breast Sx) | Physical | 18, 19 |
| `drsp_eatsx` (Eating Sx) | Physical | 12, 13 |

Each composite is computed as `rowMeans(component_items, na.rm = TRUE)`.

## GAM Model Details

- **Spline type**: Cyclic cubic spline (`bs = 'cc'`)
- **Basis functions**: `k = min(6, N - 1)` (at least 3 required)
- **Fitting method**: fREML with `discrete = TRUE` and `select = TRUE`
- **Transformation**: `log(y + 1)` → fit → back-transform with `exp(η) − 1`
- **Coverage rule**: All 4 cycle quarters must have ≥1 observation; at most 1 quarter may have < `sparse_quarter_cutoff` (default: 2) observations
- **Minimum observations**: Outcomes with fewer than `min_obs_for_gam` (default: 10) complete observations are skipped
- **Benchmark**: Per-person C-PASS-equivalent change criterion (default 30%) based on each participant's observed DRSP scale range

## Adapting for a New Repository

To move this template to a standalone repository:

1. Copy the entire `templates/drsp_gam_template/` folder
2. The template is self-contained — no dependencies on the HERA codebase
3. Update `drsp_gam_config.R` for your project's data paths
4. Consider adding your own `HERA_00_setup.R`-style initialization if needed

## Citation

If you use this template, please cite the HERA project and the original GAM analysis methodology developed by Tory Eisenlohr-Moul, PhD.
