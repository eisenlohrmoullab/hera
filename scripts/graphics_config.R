# =============================================================================
# UKALC CENTRALIZED GRAPHICS CONFIGURATION
# =============================================================================
# This file provides consistent, publication-ready graphics settings and
# helper functions for all UKALC visualizations.
#
# WHAT IT PROVIDES:
# 1. Graphics state reset (closes all open devices)
# 2. ragg device configuration (200 DPI for optimal balance, white background)
# 3. Publication-ready constants (10x6 inches, 200 DPI)
# 4. Custom theme (theme_ukalc) for consistent styling
# 5. Unified save functions (save_plot, save_png) with:
#    - ragg device and SVG support
#    - Concise console output (filename only, not full paths)
#    - Defensive validation to prevent text readability issues
# 6. Date scale helper (scale_x_date_all) to show all dates
# 7. SVG text overlap prevention (fix_text_size = TRUE for proper text sizing)
#
# OUTPUT FORMAT:
# - Concise messages: "Saved: filename.png + SVG" instead of verbose full paths
# - Makes console output easier to read during analysis runs
# =============================================================================

# ---- 1. Reset Graphics State ----
# Close any open devices and reset to clean state
while (!is.null(dev.list())) dev.off()
graphics.off()

# ---- 2. Configure ragg as Default Device ----
# Note: DPI set to 200 for optimal balance of quality and readability. Use 300 DPI only for print-quality exports.
options(
  device = function(...) ragg::agg_png(..., res = 200, bg = "white"),
  bitmapType = "cairo"
)

# ---- 3. Publication-Ready Constants ----
UKALC_GRAPHICS <- list(
  # Optimized dimensions for 200 DPI (better quality than 150, still readable text)
  width_single = 10,
  width_double = 14,
  height_standard = 6,
  height_tall = 8,
  
  # Resolution - 200 DPI balances quality and text readability
  dpi = 200,             # Increased from 150 for better resolution while keeping text readable
  
  # Typography (increased from 12 to 14 for better readability in saved plots)
  base_size = 14,
  
  # Color palettes
  colors_hormones = c(
    e2 = "#0072B2",  # Blue
    lh = "#009E73",  # Green
    p4 = "#D55E00"   # Orange
  ),
  color_menses = "#CC79A7",     # Pink
  color_ovulation = "#009E73",  # Green
  
  # Background
  bg = "white"
)

# ---- 4. Custom Theme: theme_ukalc() ----
#' Publication-Ready Theme for UKALC Plots
#'
#' Provides consistent styling across all UKALC visualizations with
#' clean backgrounds, rotated x-axis labels, and publication-ready fonts.
#'
#' @param base_size Base font size (default: 14)
#' @param rotate_x_labels Angle for x-axis labels (default: 45)
#' @return A ggplot2 theme object
theme_ukalc <- function(base_size = UKALC_GRAPHICS$base_size, 
                        rotate_x_labels = 45) {
  theme_minimal(base_size = base_size) +
    theme(
      # Panel styling
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      
      # Axis styling
      axis.text.x = element_text(
        angle = rotate_x_labels, 
        hjust = 1, 
        vjust = 1
      ),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold"),
      
      # Legend styling
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      
      # Plot title styling
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      
      # Margins
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ---- 5. Unified Save Function: save_plot() ----
#' Save Plot with Consistent Settings
#'
#' Unified function for saving plots with publication-ready settings.
#' Uses ragg device explicitly for high-quality output. Also saves an SVG
#' version for scalable vector graphics output with proper text sizing to
#' prevent overlap issues. Outputs concise save confirmation messages.
#'
#' @param plot ggplot2 plot object
#' @param filename File name (e.g., "plot.png")
#' @param folder Output folder path (default: output_folder from environment)
#' @param width Plot width in inches (default: 10)
#' @param height Plot height in inches (default: 6)
#' @param dpi Resolution (default: 200)
#' @param bg Background color (default: "white")
#' @return NULL (saves plot to file)
#' 
#' @details
#' SVG files use \code{fix_text_size = TRUE} parameter to ensure proper text
#' sizing and prevent overlap issues common in SVG outputs.
#' Prints concise output: "Saved: filename.png + filename.svg"
save_plot <- function(plot, 
                      filename, 
                      folder = if (exists("output_folder", envir = .GlobalEnv)) output_folder else getwd(),
                      width = UKALC_GRAPHICS$width_single,
                      height = UKALC_GRAPHICS$height_standard,
                      dpi = UKALC_GRAPHICS$dpi,
                      bg = UKALC_GRAPHICS$bg) {
  
  # Ensure directory exists
  full_path <- file.path(folder, filename)
  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  
  # Save PNG with ragg device explicitly
  ggsave(
    filename = full_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    device = ragg::agg_png,
    bg = bg
  )
  
  # Save SVG version with same dimensions
  # Use tools::file_path_sans_ext to handle any extension properly
  svg_filename <- paste0(tools::file_path_sans_ext(filename), ".svg")
  svg_path <- file.path(folder, svg_filename)
  
  ggsave(
    filename = svg_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = "svg",
    bg = bg,
    fix_text_size = TRUE  # Fixes text sizing issues in SVG to prevent overlap
  )
  
  # Concise output: only show filename, not full path
  cat("Saved:", basename(full_path), "+", basename(svg_path), "\n")
}

# ---- 5b. PNG Export Helper with Validation: save_png() ----
#' Save PNG (and optionally SVG) with Defensive Validation
#'
#' Helper function for saving PNG plots with validation to prevent common
#' mistakes that lead to unreadable text (e.g., pixel values interpreted as
#' inches, excessive DPI with small dimensions). Can optionally save an SVG 
#' version of the plot for scalable vector graphics output with proper text 
#' sizing to prevent overlap issues. Outputs concise save confirmation messages.
#'
#' @param plot ggplot2 plot object
#' @param filename File name (e.g., "plot.png")
#' @param folder Output folder path (default: output_folder from environment)
#' @param width Plot width in inches (default: 10)
#' @param height Plot height in inches (default: 6)
#' @param units Units for width/height (default: "in", always use "in" for PNG)
#' @param dpi Resolution (default: 200 for optimal balance, use 300 for print)
#' @param scale Scaling factor for plot elements (default: 1, increase to enlarge text)
#' @param save_svg Logical; whether to also save SVG version (default: TRUE)
#' @param ... Additional arguments passed to ggsave()
#' @return NULL (saves plot to file)
#'
#' @details
#' VALIDATION RULES:
#' 1. If units is missing and width/height > 50: STOP (likely pixel values as inches)
#' 2. If dpi >= 250 for PNG: WARN (high DPI may make text appear small)
#' 3. Always explicitly set units = "in"
#' 
#' By default, both PNG and SVG versions are saved. The SVG filename is generated
#' by removing the file extension from \code{filename} and adding \code{.svg}
#' (e.g., "plot.anyext" becomes "plot.svg"; using ".png" is recommended for clarity).
#' SVG files use \code{fix_text_size = TRUE} parameter to ensure proper text sizing
#' and prevent overlap issues common in SVG outputs. Set \code{save_svg = FALSE}
#' to save only PNG files.
#' Prints concise output: "Saved: filename.png + SVG" or "Saved: filename.png"
#'
#' @examples
#' # Basic usage with defaults (10x6 inches, 200 DPI, PNG + SVG)
#' save_png(my_plot, "figure1.png")
#' 
#' # PNG only, no SVG
#' save_png(my_plot, "figure1.png", save_svg = FALSE)
#' 
#' # Custom dimensions for larger figure
#' save_png(my_plot, "figure2.png", width = 12, height = 8)
#' 
#' # Print quality (use larger dimensions with high DPI)
#' save_png(my_plot, "figure3.png", width = 12, height = 8, dpi = 300)
save_png <- function(plot,
                     filename,
                     folder = if (exists("output_folder", envir = .GlobalEnv)) output_folder else getwd(),
                     width = 10,
                     height = 6,
                     units = "in",
                     dpi = 200,
                     scale = 1,
                     save_svg = TRUE,
                     ...) {
  
  # Validation 1: Check if units is missing and dimensions look like pixels
  if (missing(units) && (width > 50 || height > 50)) {
    stop(
      "ERROR: width or height > 50 without explicit units. ",
      "Are you passing pixel values? PNG exports should use inches (units = 'in'). ",
      "For 10x6 inch plot, use: width = 10, height = 6, units = 'in'"
    )
  }
  
  # Validation 2: Warn if using high DPI with default dimensions
  if (dpi >= 250) {
    warning(
      "Using DPI >= 250 for PNG export. High DPI may make text appear small. ",
      "Consider using dpi = 200 for screen viewing, or increase width/height if needed for print. ",
      "Current settings: width = ", width, ", height = ", height, ", dpi = ", dpi
    )
  }
  
  # Validation 3: Ensure units is always "in" for PNG
  if (units != "in") {
    warning(
      "Using units = '", units, "' for PNG export. ",
      "Recommended: always use units = 'in' for PNG exports to ensure consistent sizing."
    )
  }
  
  # Ensure directory exists
  full_path <- file.path(folder, filename)
  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  
  # Save PNG with ragg device for high-quality rendering
  ggsave(
    filename = full_path,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    scale = scale,
    device = ragg::agg_png,
    bg = "white",
    ...
  )
  
  # Concise output: only show filename, not full path
  if (save_svg) {
    cat("Saved:", basename(full_path), "+ SVG\n")
  } else {
    cat("Saved:", basename(full_path), "\n")
  }
  
  # Optionally save SVG version with same dimensions
  if (save_svg) {
    # Use tools::file_path_sans_ext to handle any extension properly
    svg_filename <- paste0(tools::file_path_sans_ext(filename), ".svg")
    svg_path <- file.path(folder, svg_filename)
    
    ggsave(
      filename = svg_path,
      plot = plot,
      width = width,
      height = height,
      units = units,
      scale = scale,
      device = "svg",
      bg = "white",  # Always use white background for SVG
      fix_text_size = TRUE,  # Fixes text sizing issues in SVG to prevent overlap
      ...
    )
  }
}

# ---- 6. Date Scale Helper: scale_x_date_all() ----
#' Date Scale with All Dates Visible
#'
#' Helper function to ensure all dates are visible on x-axis in hormone plots.
#' Automatically adjusts date breaks based on date range.
#'
#' @param data Data frame containing date column
#' @param date_col Name of date column (default: "date")
#' @return A ggplot2 scale_x_date object
scale_x_date_all <- function(data, date_col = "date") {
  date_range <- range(data[[date_col]], na.rm = TRUE)
  n_days <- as.numeric(diff(date_range))
  
  # Threshold of 45 days balances readability (prevents overcrowding of labels)
  # with completeness (shows daily resolution for typical menstrual cycle studies)
  if (n_days <= 45) {
    # For short date ranges (≤45 days), show every day
    scale_x_date(
      breaks = seq(date_range[1], date_range[2], by = "1 day"),
      date_labels = "%m/%d",
      expand = expansion(mult = 0.02)
    )
  } else {
    # For longer date ranges, show every 3 days
    scale_x_date(
      date_breaks = "3 days",
      date_labels = "%m/%d",
      expand = expansion(mult = 0.02)
    )
  }
}

# ---- Confirmation ----
cat("GRAPHICS CONFIGURATION LOADED:\n")
cat("- Graphics devices reset\n")
cat("- ragg configured as default (200 DPI, white background)\n")
cat("- Constants: UKALC_GRAPHICS (10x6 inches, 200 DPI)\n")
cat("- Functions: theme_ukalc(), save_plot(), save_png(), scale_x_date_all()\n")
cat("- Use save_png() for standardized PNG and SVG exports with validation\n")
cat("- Both PNG and SVG versions are saved automatically with concise output\n")
cat("- SVG text overlap prevention: fix_text_size = TRUE enabled\n")
