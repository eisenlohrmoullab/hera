# DRSP GAM Template

A reusable R Markdown template for automated, participant-centric analysis of DRSP (Daily Record of Severity of Problems) symptom cyclicity using cyclic Generalized Additive Models (GAMs).

Originally derived from the [HERA project](https://github.com/eisenlohrmoullab/hera)'s `HERA_08_single_GAMs.Rmd`.

## Features

- **Auto-detects DRSP items**: Works with any subset of `drsp_1` through `drsp_21`. If your dataset only has some items, the template automatically adapts.
- **Automatic composite scoring**: Computes standard DRSP composites (Negative Affect, Distress, Social/Affect Lability, Low Arousal, Breast Sx, Eating Sx) from available items. Composites requiring missing items are skipped gracefully.
- **Per-participant PDF reports**: Landscape PDFs with raw data coverage, interpretation key, composite definitions, score distributions, overlaid GAM curves, significance tables, and individual outcome plots.
- **Overall sample summary PDF**: Key metrics, quarter coverage, per-outcome descriptives, range distributions, and significance rates across the full sample.
- **CSV audit file**: Per-ID × outcome × time-variable statistics for QC/triage.
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
| `drsp_1` … `drsp_21` | Some or all DRSP items (1–6 Likert scale) |
| A cyclic time variable | Scaled to [−1, 1], e.g., `cyclic_time_impute` |

The template will automatically detect which DRSP items are present and compute composites accordingly.

## Quick Start

1. **Copy the template folder** to your project:

   ```
   templates/drsp_gam_template/
   ├── DRSP_GAM_Template.Rmd     # Main analysis template
   ├── drsp_gam_config.R          # User configuration (edit this!)
   ├── drsp_gam_scoring.R         # DRSP composite scoring functions
   ├── drsp_gam_functions.R       # Core GAM fitting & plotting functions
   └── README.md                  # This file
   ```

2. **Edit `drsp_gam_config.R`**: Set your data file path, output directory, study label, and time variable names. At minimum, change:

   ```r
   data_file_path <- "path/to/your_data.rds"
   gam_output_base <- "path/to/output/"
   study_label <- "My Study"
   time_vars_to_run <- c("cyclic_time_impute")
   ```

3. **Knit `DRSP_GAM_Template.Rmd`** (or run chunk-by-chunk in RStudio). Reports will be generated in a dated subfolder under your output directory.

## File Descriptions

### `drsp_gam_config.R`

User-facing configuration file. Edit this to customize:

- **Data input**: Path to your `.rds` or `.csv` file
- **Output directory**: Where PDF reports and CSV summaries are saved
- **Study label**: Used in report titles
- **Time variables**: Cyclic time variable name(s) in your data
- **Cycle phasing**: Whether time is centered on menses or ovulation
- **GAM quality gates**: Sparse quarter cutoff, minimum observations
- **Benchmark settings**: C-PASS-equivalent change percentage (default 30%)
- **Report layout**: PDF dimensions, plots per page
- **Report toggles**: Which outputs to generate

### `drsp_gam_scoring.R`

DRSP composite scoring module. Provides:

- `score_drsp_composites(df)`: Detects DRSP items, computes composites, returns metadata
- `build_outcomes_map(items_found, composites_created)`: Builds the outcome label map
- `classify_outcomes(items_found, composite_specs)`: Classifies outcomes into affective/physical domains

### `drsp_gam_functions.R`

Core analysis functions:

- `build_drsp_color_map()`: Colorblind-safe color assignments
- `build_axis_meta()`: Cycle phase axis labels and reference lines
- `quarter_bin_counts()` / `check_quarter_coverage()`: Coverage quality checks
- `fit_outcome()`: Fits cyclic GAM with quality gates
- `create_annotated_plot()`: Individual outcome plot with annotations
- `plot_overlay_page()`: Multi-outcome overlay plot
- `expand_ymax()`: Dynamic Y-axis expansion

### `DRSP_GAM_Template.Rmd`

Main analysis template. Runs through:

1. Load configuration and functions
2. Load data and auto-score DRSP composites
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
- **Benchmark**: Per-person C-PASS-equivalent 30% change criterion based on each participant's observed DRSP scale range

## Adapting for a New Repository

To move this template to a standalone repository:

1. Copy the entire `templates/drsp_gam_template/` folder
2. The template is self-contained — no dependencies on the HERA codebase
3. Update `drsp_gam_config.R` for your project's data paths
4. Consider adding your own `HERA_00_setup.R`-style initialization if needed

## Citation

If you use this template, please cite the HERA project and the original GAM analysis methodology developed by Tory Eisenlohr-Moul, PhD.
