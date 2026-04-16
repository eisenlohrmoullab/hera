# =============================================================================
# DRSP GAM Template — User Configuration
# =============================================================================
# Edit this file to configure the template for your dataset.
# All paths, variable names, and analysis options are set here.
# =============================================================================

# ---- 1. DATA INPUT ----

# Path to your input data file (.rds or .csv)
# The data MUST contain:
#   - An 'id' column identifying each participant
#   - At least one cyclic time variable scaled to [-1, 1]
#   - Some or all DRSP items (1-21)
#
# DRSP columns are detected flexibly — ANY naming convention works as long as
# the column contains "drsp" (case-insensitive) and an item number.
# Examples that all map to drsp_1:
#   drsp_1, drsp1, DRSP1, DRSP_1, DRSP__1, drsp.1, DRSP1_depblue
#
# Composites will be computed automatically from available items.
data_file_path <- "path/to/your_data.rds"   # CHANGE THIS

# ---- 2. OUTPUT DIRECTORY ----

# Base directory where all reports will be saved.
# A study-specific, dated subfolder will be created automatically:
#   <gam_output_base>/<study_name>_GAM_<YYYYMMDD>/
gam_output_base <- "path/to/output/"   # CHANGE THIS

# ---- 3. STUDY IDENTIFICATION ----

# study_name: A short identifier/slug for your study (no spaces recommended).
#   This slug is used in output folder names and filenames.
#   Examples: "HERA", "TIDES", "MyStudy"
study_name <- "MyStudy"   # CHANGE THIS

# study_label: A human-readable label for your study.
#   Used in report titles, cover pages, and annotations.
#   Examples: "HERA Menstrual Cycle Study", "TIDES Project"
study_label <- "My Study"   # CHANGE THIS

# ---- 4. TIME VARIABLES ----

# Character vector of cyclic time variable names in your dataset.
# These must be scaled to [-1, 1] where the cycle phase is encoded.
# Common options: "cyclic_time", "cyclic_time_impute", "cyclic_time_ov"
time_vars_to_run <- c("cyclic_time_impute")   # CHANGE THIS

# ---- 5. CYCLE PHASING ----

# How is the cycle phased? This controls axis labels and reference lines.
# Options:
#   "menses"    — cycle centered on menses (0 = menses onset)
#   "ovulation" — cycle centered on ovulation (0 = estimated ovulation)
# If you have multiple time_vars with different phasing, set this to a named
# list, e.g.: list(cyclic_time = "ovulation", cyclic_time_impute = "menses")
# Otherwise, a single string applies to all time variables.
default_cycle_phasing <- "menses"   # CHANGE THIS if your phasing differs

# ---- 6. GAM QUALITY GATES ----

# Sparse-quarter cutoff for GAM inclusion.
# The cycle is divided into 4 equal quarters over [-1, 1].
# Inclusion rule: ALL 4 quarters must have >= 1 observation, AND at most
# 1 quarter may have < sparse_quarter_cutoff observations.
sparse_quarter_cutoff <- 2

# Minimum total observations required per outcome to attempt a GAM fit.
# Outcomes with fewer than this many complete (non-NA) observations are skipped.
min_obs_for_gam <- 10

# ---- 7. BENCHMARK SETTINGS ----

# Person-specific C-PASS-equivalent change benchmark.
# For each participant, computed as: (max - min of all raw DRSP items) × cpass_change_pct.
# A GAM range meeting or exceeding this benchmark is flagged.
cpass_change_pct <- 0.30

# ---- 8. REPORT LAYOUT ----

# PDF dimensions (landscape by default)
pdf_width  <- 11
pdf_height <- 8.5

# Plots per page in grid layout
plots_per_col <- 3
plots_per_row <- 3

# Columns per page in significance tables
table_cols_per_page <- 6

# ---- 9. WHICH REPORTS TO GENERATE ----

# Set to TRUE/FALSE to control which outputs are produced.
generate_per_id_reports     <- TRUE   # Individual participant PDF reports
generate_overall_summary    <- TRUE   # Overall sample summary PDF
generate_per_outcome_pdfs   <- TRUE   # Per-outcome exploration PDFs
generate_csv_summary        <- TRUE   # CSV of all GAM results

# Skip existing per-ID PDFs? Set FALSE to overwrite.
skip_existing_reports <- TRUE

# ---- 10. DOMAIN COLOR SCHEME ----

# Colorblind-safe domain colors (used for axis labels, fills, etc.)
DOMAIN_COL_AFFECTIVE <- "#0072B2"   # blue
DOMAIN_COL_PHYSICAL  <- "#D55E00"   # orange/vermillion
