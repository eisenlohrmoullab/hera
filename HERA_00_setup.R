
# =============================================================================
# SECTION 1: SETUP & CONFIGURATION
# =============================================================================
# This section loads all required packages and sets global options for the
# analysis. It uses the pacman package manager for efficient loading/installation.
#
# WHAT IT DOES:
# - Sets global display options (no scientific notation, 3 decimal places)
# - Loads ~50 packages for data wrangling, visualization, and modeling
# - Resolves namespace conflicts (e.g., dplyr::filter vs stats::filter)
#
# WHY IT'S NECESSARY:
# - Ensures all required functionality is available
# - Prevents conflicts between functions with the same name from different packages
# - Makes code more readable by avoiding scientific notation
#
# IMPORTANT PACKAGES:
# - tidyverse: Core data manipulation (dplyr, tidyr, ggplot2, etc.)
# - lubridate: Date handling
# - zoo: Rolling averages and time series operations
# - lme4/lmerTest: Mixed-effects models
# - janitor: Data cleaning utilities
# =============================================================================

# ---- 1. Global Options ----
options(
  scipen = 999,      # Suppress scientific notation
  digits = 3         # Set display precision
)

# ---- 2. Package Loading (via Pacman) ----
# Install pacman if it isn't already installed
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  # -- Core Data Science & Wrangling --
  tidyverse,       # Includes dplyr, tidyr, ggplot2, readr, stringr, forcats, purrr, tibble
  lubridate,       # Date handling
  janitor,         # Cleaning column names
  data.table,      # Fast data manipulation
  zoo,             # Rolling averages/time series
  beepr,           # Notifications
  readr,          # Reading data
  readxl,         # Reading Excel files
  writexl,        # Writing Excel files
  haven,          # Reading SPSS, Stata, SAS
  openxlsx,       # Advanced Excel operations
  glue,
  
  # -- Exploratory Data Analysis (EDA) --
  skimr,           # Summary statistics
  visdat,          # Visualizing missingness
  dlookr,          # Data diagnosis and quality
  psych,           # Psychometrics and descriptive stats
  
  # -- Matching & Linkage --
  fuzzyjoin,       # Fuzzy matching
  stringdist,      # String distance metrics
  MatchIt,         # Propensity score matching
  RecordLinkage,   # Deduplication
  powerjoin,       # Advanced joins
  
  # -- Modeling & Statistics --
  lme4,            # Mixed-effects models
  lmerTest,        # P-values for lme4
  nlme,            # Nonlinear mixed-effects
  mgcv,            # GAMs
  broom,           # Tidy model output
  broom.mixed,     # Tidy mixed/Bayesian model output
  emmeans,         # Estimated marginal means
  marginaleffects, # Marginal effects
  performance,     # Model performance metrics
  insight,         # Model information extraction
  rmcorr,          # Repeated measures correlation
  careless,        # Survey data quality
  responsePatterns,# Survey response analysis
  menstrualcycleR, # Menstrual cycle visualization functions
  
  # -- Visualization Extensions --
  ragg,          # High-quality rendering 
  patchwork,       # Arranging plots
  ggdist,          # Uncertainty viz
  ggrepel,         # Non-overlapping labels
  ggforce,         # Zooming and shapes
  ggExtra,         # Marginal histograms
  ggstatsplot,     # Stats-rich plots
  ggvenn,          # Venn diagrams
  sjPlot,          # Plotting model results
  see,             # Model diagnostic plots
  corrplot,        # Correlation matrices
  plotly,          # Interactive plots
  naniar,          # Missing data visualization
  ggpubr,      # Arranging ggplots
  
  # -- Project & Git --
  usethis,
  gitcreds,
  conflicted       # Handle namespace conflicts (Load last)
)

# ---- 3. Conflict Management ----
# Explicitly set preferences for common naming collisions
conflict_scout()

conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::lag,
  dplyr::first,
  dplyr::rename,
  dplyr::summarise, 
  tidyr::complete,
  tidyr::expand,
  lme4::lmer,
  psych::alpha,
  psych::ICC
)

# ---- 4. Confirmation ----
cat("SETUP COMPLETE: Packages installed/loaded via pacman, options set, and conflict preferences declared.\n")


# =============================================================================
# OUTPUT FOLDER SETUP
# =============================================================================
# Creates a date-stamped output folder for all analysis products. This ensures
# reproducibility by versioning outputs by date.
#
# WHAT IT DOES:
# - Creates folder name with format YYYYMMDD (e.g., "20251120")
# - Builds full path to Box folder for output
# - Creates folder if it doesn't exist
#
# OUTPUT LOCATION:
# - Box: "UKALC/03_analytic_projects/UKALC-Primary ms/02_datasets_output/YYYYMMDD/"
#
# SUBFOLDER STRUCTURE (created by individual scripts):
# - 01_data_preparation/    - Data prep diagnostics and intermediate outputs
# - 01b_hormone_qc/         - Hormone trajectory plots and QC outputs
# - 01c_batch_check/        - Assay batch comparison plots
# - 02_descriptives/        - Descriptive statistics and visualizations
# - 03_gamms/               - GAMM models (per-outcome subfolders)
# - 04_smms/                - SMM models and classification results
#
# WHY IT'S NECESSARY:
# - Prevents overwriting previous analysis runs
# - Makes it easy to track which outputs correspond to which code version
# - Organizes outputs by pipeline stage for better navigation
# - Facilitates collaboration by organizing outputs by date and analysis type
#
# TODO: Update the path below to match your Box folder location
# To find it: Box Desktop → Preferences → Account → "Open Box Folder"
# =============================================================================

# Create a new output folder with the current date
current_date <- format(Sys.Date(), "%Y%m%d")  # Produces date as YYYYMMDD

# ⚠️ CONFIGURE THIS PATH for your Box account:
output_folder <- paste0(
  "~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/03_analytic_projects/UKALC-Primary ms/02_datasets_output/",
  current_date
)

# State the output folder
cat("Output folder set as: ", output_folder, "\n")

# Create the output folder if needed
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# =============================================================================
# GRAPHICS CONFIGURATION
# =============================================================================
# Source centralized graphics configuration for consistent, publication-ready
# figures across all UKALC analyses.
# =============================================================================

# Source graphics configuration
source("scripts/graphics_config.R")
