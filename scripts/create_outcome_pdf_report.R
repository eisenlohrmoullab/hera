#' Generate Comprehensive PDF Comparison Reports for SMM Results
#'
#' Creates multi-page PDF reports for each outcome, showing BIC comparisons and
#' GAM trajectory plots across different cycle centering methods (menses vs ovulation)
#' and group sizes. This enables quick visual comparison of model configurations
#' before selecting final group solutions for labeling.
#'
#' Each PDF report contains:
#' - Page 1 (Cover): BIC comparison plot showing both centering methods
#' - Page 2: Menses-centered GAM plots for all group sizes (g=1-4) in a 2×2 grid
#' - Page 3: Ovulation-centered GAM plots for all group sizes (g=1-4) in a 2×2 grid
#'
#' All plots on each page share the same Y-axis scale for direct visual comparison
#' of trajectory magnitudes across group solutions.
#'
#' @param base_save_dir Character string specifying the base directory where SMM
#'   outputs are stored. This should be the smm_output_folder path used in
#'   UKALC_06_smms.Rmd. Expected structure:
#'   base_save_dir/YYYYMMDD/{centering}_centered/outcome/
#' @param outcomes Character vector of outcome variable names (e.g., c("dep",
#'   "crave_alc_pm", "num_drinks_today")). One PDF will be generated per outcome.
#' @param groups_to_show Integer vector specifying which group sizes to include
#'   in the reports (default: 1:4). Use 1:5 if you fit 5-group models.
#' @param report_dir Character string specifying where to save PDF reports.
#'   If NULL (default), creates "outcome_comparison_reports" subdirectory within
#'   base_save_dir.
#'
#' @return A list with two elements:
#'   - report_dir: Path where PDF reports were saved
#'   - report_files: Character vector of generated PDF file paths
#'
#' @details
#' File Structure Expected:
#' - BIC files: base_save_dir/YYYYMMDD/{centering}_centered/outcome/{outcome}_{centering}_bic_comparison.csv
#' - GAM models: base_save_dir/YYYYMMDD/{centering}_centered/outcome/{outcome}_log_g{g}_GAM_model.rds
#' - For g=1: {outcome}_log_noGroup_GAM_model.rds
#'
#' Error Handling (Updated January 2026):
#' Instead of returning NULL and showing empty pages, the function now creates
#' informative error plots that display exactly what went wrong:
#' - Missing date folders: Shows the base_save_dir path where folders were expected
#' - Missing GAM model files: Shows the expected file path and directory location
#' - Loading/plotting errors: Shows the actual error message from R
#' 
#' This makes debugging much easier - you'll see on pages 2-3 exactly why a plot
#' couldn't be generated instead of just "No GAM models found".
#'
#' The function continues processing even when errors occur:
#' - Skips outcomes with no BIC files found
#' - Shows error plots for missing GAM model files
#' - Continues processing other outcomes even if one fails
#'
#' GAM Plot Generation (Updated January 2026):
#' - Uses marginaleffects::predictions() for robust population-level predictions
#' - Creates prediction grid spanning -1 to +1 in cyclic time
#' - Excludes random effects via exclude = "s(id)" parameter for population-level predictions
#' - Back-transforms from log(x+1) scale: exp(prediction) - 1
#' - Adds cycle phase shading (follicular/luteal for menses; peri-ovulatory for ovulation)
#' - Shows group labels with participant counts
#' - Simple, maintainable approach proven to work in cycle_pipeline_template
#'
#' @examples
#' # Generate reports for all analyzed outcomes
#' result <- create_outcome_pdf_report(
#'   base_save_dir = "output/04_smms/SMMs_Ov_and_Borderline",
#'   outcomes = c("num_drinks_today", "crave_alc_pm", "dep"),
#'   groups_to_show = 1:4
#' )
#'
#' # Check output
#' print(result$report_dir)
#' print(result$report_files)
#'
#' @seealso
#' \code{\link{run_smm_cyclic}} for fitting SMMs
#' \code{\link{compare_bic_cyclic}} for BIC comparison
#' \code{\link{model_plot_modx_gam_cyclic}} for GAM fitting
#'
#' @note
#' Required packages: dplyr, ggplot2, glue, gridExtra, grid, mgcv, marginaleffects
#' These should be installed before running this function.
#' 
#' WARNING: This function loads packages into the global namespace using library().
#' If you need to avoid namespace conflicts, ensure these packages are already loaded.
#'
#' UPDATED January 2026: 
#' - Switched to marginaleffects::predictions() for reliable population-level predictions
#' - Uses exclude = "s(id)" parameter to obtain population-level predictions (matching cycle_pipeline_template)
#' - Simpler than previous dynamic random effect detection approach
#' - Consistent with working implementation in cycle_pipeline_template
#' - Fixed multi-page PDF generation by adding grid.newpage() calls before each grid.draw()
#'   to ensure plots appear on separate pages instead of overlapping on a single page
#' - Changed grid.arrange() to grid.draw(arrangeGrob()) for consistent page control
#'
#' @export
create_outcome_pdf_report <- function(
    base_save_dir,
    outcomes,
    groups_to_show = 1:4,
    report_dir = NULL
) {
  
  # =============================================================================
  # 1. DEPENDENCY CHECKS
  # =============================================================================
  
  required_packages <- c("dplyr", "ggplot2", "glue", "gridExtra", "grid", "mgcv", "marginaleffects")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "),
               "\nInstall with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))"))
  }
  
  # Load required libraries
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(gridExtra)
  library(grid)
  library(mgcv)
  library(marginaleffects)
  
  # =============================================================================
  # 2. SETUP OUTPUT DIRECTORY
  # =============================================================================
  
  if (is.null(report_dir)) {
    report_dir <- file.path(base_save_dir, "outcome_comparison_reports")
  }
  
  if (!dir.exists(report_dir)) {
    dir.create(report_dir, recursive = TRUE)
    message(glue("📁 Created report directory: {report_dir}"))
  }
  
  # =============================================================================
  # 3. DEFINE VERSION CONFIGURATIONS (SIMPLIFIED FOR UKALC)
  # =============================================================================
  
  versions <- data.frame(
    centering = c("menses", "ovulation"),
    centering_folder = c("menses_centered", "ovulation_centered"),
    version_label = c("Menses-Centered", "Ovulation-Centered"),
    stringsAsFactors = FALSE
  )
  
  # =============================================================================
  # HELPER FUNCTIONS (defined internally to simplify passing common parameters)
  # =============================================================================
  # Note: These functions are defined inside create_outcome_pdf_report() rather than
  # as separate exported functions because:
  # 1. They share common parameters (base_save_dir, version_info, etc.)
  # 2. They are specific to this report generation workflow
  # 3. Keeping them internal reduces code complexity and parameter passing
  
  # =============================================================================
  # 4. HELPER FUNCTION: LOAD BIC DATA
  # =============================================================================
  
  load_bic_data <- function(outcome, version_info, base_save_dir) {
    # Find the most recent date folder
    date_folders <- list.dirs(base_save_dir, recursive = FALSE, full.names = FALSE)
    date_folders <- date_folders[grepl("^\\d{8}$", date_folders)]
    
    if (length(date_folders) == 0) {
      return(NULL)
    }
    
    # Use most recent date
    most_recent_date <- max(date_folders)
    
    version_dir <- file.path(
      base_save_dir,
      most_recent_date,
      version_info$centering_folder,
      outcome
    )
    
    bic_file <- file.path(version_dir, glue("{outcome}_{version_info$centering}_bic_comparison.csv"))
    
    if (!file.exists(bic_file)) {
      return(NULL)
    }
    
    bic_data <- read.csv(bic_file)
    bic_data$version <- version_info$version_label
    bic_data$centering <- version_info$centering
    
    return(bic_data)
  }
  
  # =============================================================================
  # 5. HELPER FUNCTION: CREATE BIC COVER PAGE
  # =============================================================================
  
  create_bic_cover_page <- function(outcome, all_bic_data) {
    if (is.null(all_bic_data) || nrow(all_bic_data) == 0) {
      # Create empty plot with message
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = "No BIC data available", 
                size = 8, color = "gray50") +
        theme_void()
      return(p)
    }
    
    # Note: We use direct column references for static columns (groups, BIC)
    # and .data[[]] pronoun for dynamic column names (time_var, estimate_original)
    # This follows ggplot2 best practices for both scenarios
    p <- ggplot(all_bic_data, aes(x = groups, y = BIC, color = version, group = version)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      labs(
        title = glue("{outcome}: BIC Comparison Across Configurations"),
        subtitle = "Lower BIC indicates better fit (penalized for complexity)",
        x = "Number of Groups",
        y = "BIC",
        color = "Configuration"
      ) +
      scale_x_continuous(breaks = sort(unique(all_bic_data$groups))) +
      scale_color_manual(values = c("Menses-Centered" = "red", 
                                     "Ovulation-Centered" = "#009E73")) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray30"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    return(p)
  }
  
  # =============================================================================
  # 6. HELPER FUNCTION: ADD COVARIATES TO PREDICTION DATA
  # =============================================================================
  
  add_covariates_to_pred_data <- function(pred_data, gam_model) {
    # Add covariates at reference levels if they exist in the model
    if ("day_of_week" %in% names(gam_model$model)) {
      pred_data$day_of_week <- levels(gam_model$model$day_of_week)[1]
    }
    if ("session_today" %in% names(gam_model$model)) {
      pred_data$session_today <- 0
    }
    return(pred_data)
  }
  
  # =============================================================================
  # 7. HELPER FUNCTION: LOAD GAM MODEL AND CREATE PLOT
  # =============================================================================
  
  load_and_plot_gam <- function(outcome, g, version_info, base_save_dir) {
    # Find the most recent date folder
    date_folders <- list.dirs(base_save_dir, recursive = FALSE, full.names = FALSE)
    date_folders <- date_folders[grepl("^\\d{8}$", date_folders)]
    
    if (length(date_folders) == 0) {
      # Create error plot instead of returning NULL
      error_msg <- glue("No date folders found in:\n{base_save_dir}")
      message(glue("  ⚠️  {error_msg}"))
      error_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = error_msg,
                size = 3, color = "red", hjust = 0.5, vjust = 0.5) +
        labs(title = glue("g={g} - No Date Folders")) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11,
                                        color = "red"))
      return(error_plot)
    }
    
    most_recent_date <- max(date_folders)
    
    version_dir <- file.path(
      base_save_dir,
      most_recent_date,
      version_info$centering_folder,
      outcome
    )
    
    # Determine model filename
    if (g == 1) {
      model_file <- file.path(version_dir, glue("{outcome}_log_noGroup_GAM_model.rds"))
    } else {
      model_file <- file.path(version_dir, glue("{outcome}_log_g{g}_GAM_model.rds"))
    }
    
    if (!file.exists(model_file)) {
      # Create error plot with detailed path information
      error_msg <- glue("Model file not found:\n{basename(model_file)}\n\nExpected in:\n{version_dir}")
      message(glue("  ⚠️  Model file not found: {model_file}"))
      error_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = error_msg,
                size = 3, color = "grey", hjust = 0.5, vjust = 0.5) +
        labs(title = glue("g={g} - File Missing")) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11,
                                        color = "grey"))
      return(error_plot)
    }
    
    # Load model
    tryCatch({
      gam_model <- readRDS(model_file)
      
      # Get time variable from model
      time_var <- if (version_info$centering == "menses") "cyclic_time_impute" else "cyclic_time_imp_ov"
      
      # Create prediction grid using direct data extraction
      pred_time <- seq(-1, 1, length.out = 200)
      
      if (g == 1) {
        # Build prediction grid for single group
        pred_grid <- expand.grid(
          temp_time = seq(-1, 1, length.out = 200),
          id = 0  # Placeholder for population-level predictions
        )
        names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
        
        # Add covariates at reference levels
        pred_grid <- add_covariates_to_pred_data(pred_grid, gam_model)
        
        # Generate predictions using marginaleffects
        pred <- marginaleffects::predictions(
          gam_model,
          newdata = pred_grid,
          type = "response",
          exclude = "s(id)"  # Exclude random effect for id
        )
        
        # Create result data frame with back-transformation
        pred_df <- data.frame(
          time = pred_grid[[time_var]],
          estimate_original = exp(pred$estimate) - 1  # Back-transform from log(x+1)
        )
        names(pred_df)[1] <- time_var
        
      } else {
        # For multiple groups
        if ("smm_group" %in% names(gam_model$model)) {
          group_levels <- levels(as.factor(gam_model$model$smm_group))
          
          # Build prediction grid with both time_var and smm_group
          pred_grid <- expand.grid(
            temp_time = seq(-1, 1, length.out = 200),
            smm_group = factor(group_levels, levels = group_levels),
            id = 0  # Placeholder for population-level predictions
          )
          names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
          
          # Add covariates at reference levels
          pred_grid <- add_covariates_to_pred_data(pred_grid, gam_model)
          
          # Generate predictions using marginaleffects
          pred <- marginaleffects::predictions(
            gam_model,
            newdata = pred_grid,
            type = "response",
            exclude = "s(id)"  # Exclude random effect for id
          )
          
          # Create result data frame with back-transformation
          pred_df <- data.frame(
            time = pred_grid[[time_var]],
            smm_group = pred_grid$smm_group,
            estimate_original = exp(pred$estimate) - 1  # Back-transform from log(x+1)
          )
          names(pred_df)[1] <- time_var
        } else {
          # Fallback if smm_group not found
          message(glue("  ⚠️  Expected smm_group variable for g={g} but not found in model. Using fallback prediction."))
          
          # Build prediction grid
          pred_grid <- expand.grid(
            temp_time = seq(-1, 1, length.out = 200),
            id = 0  # Placeholder for population-level predictions
          )
          names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
          
          # Add covariates at reference levels
          pred_grid <- add_covariates_to_pred_data(pred_grid, gam_model)
          
          # Generate predictions using marginaleffects
          pred <- marginaleffects::predictions(
            gam_model,
            newdata = pred_grid,
            type = "response",
            exclude = "s(id)"  # Exclude random effect for id
          )
          
          # Create result data frame with back-transformation
          pred_df <- data.frame(
            time = pred_grid[[time_var]],
            estimate_original = exp(pred$estimate) - 1  # Back-transform from log(x+1)
          )
          names(pred_df)[1] <- time_var
        }
      }
      
      # Create plot
      p <- ggplot()
      
      # Add phase shading based on centering
      if (version_info$centering == "menses") {
        # Luteal phase (Ov to Mens): light red
        p <- p + annotate("rect", xmin = -1, xmax = 0, ymin = -Inf, ymax = Inf, 
                         fill = "#FFE6F0", alpha = 0.5)
        # Follicular phase (mens to next ov): light pink
        p <- p + annotate("rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf, 
                         fill = "#E6F2FF", alpha = 0.5)
      } else {
        # Peri-ovulatory window: light green
        p <- p + annotate("rect", xmin = -0.3, xmax = 0.3, ymin = -Inf, ymax = Inf, 
                         fill = "#E6F7ED", alpha = 0.5)
      }
      
      # Add trajectory lines
      if (g == 1) {
        # For single group, create a simple line plot
        p <- p + geom_line(data = pred_df, 
                          aes(x = .data[[time_var]], y = estimate_original),
                          linewidth = 1.2, color = "steelblue")
        title_text <- glue("g={g} (No Groups)")
      } else {
        # Get group counts from model data
        # Count unique IDs per group (not total observations)
        # Convert to data.frame first to ensure standard data structure
        model_data <- as.data.frame(gam_model$model)
        group_counts <- model_data %>%
          group_by(smm_group) %>%
          summarise(n_id = n_distinct(id), .groups = "drop") %>%
          mutate(group_label = paste0("Group ", smm_group, " (N=", n_id, ")"))
        
        # Create named vector for easy lookup
        group_labels <- setNames(
          group_counts$group_label,
          as.character(group_counts$smm_group)
        )
        
        pred_df$smm_group <- factor(pred_df$smm_group)
        pred_df$group_label <- group_labels[as.character(pred_df$smm_group)]
        
        p <- p + geom_line(data = pred_df,
                          aes(x = .data[[time_var]], y = estimate_original, 
                              color = group_label, group = smm_group),
                          linewidth = 1.2)
        
        p <- p + scale_color_discrete(name = "")
        p <- p + theme(legend.position = "bottom",
                      legend.text = element_text(size = 8))
        
        title_text <- glue("g={g} ({g} Groups)")
      }
      
      # Add scale with proper cycle phase labels
      if (version_info$centering == "menses") {
        p <- p + scale_x_continuous(
          limits = c(-1, 1),
          breaks = seq(-1, 1, by = 0.50),
          labels = c("Ovulation", "50%L", "Menses Onset", "50%F", "Ovulation")
        )
      } else {
        p <- p + scale_x_continuous(
          limits = c(-1, 1),
          breaks = seq(-1, 1, by = 0.50),
          labels = c("Menses Onset", "50%F", "Ovulation", "50%L", "Menses Onset")
        )
      }
      
      # Add labels and theme
      p <- p + 
        labs(
          title = title_text,
          x = "",  # Empty x-axis label since the breaks provide context
          y = outcome
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),  # Angle labels for readability
          panel.grid.minor = element_blank()
        )
      
      return(p)
      
    }, error = function(e) {
      # Create error plot with the actual error message
      error_msg <- glue("Error loading/plotting model:\n{e$message}")
      message(glue("  ❌ Error for g={g}: {e$message}"))
      error_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                label = error_msg,
                size = 3, color = "grey", hjust = 0.5, vjust = 0.5) +
        labs(title = glue("g={g} - Error")) +
        theme_void() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11,
                                        color = "grey"))
      return(error_plot)
    })
  }
  
  # =============================================================================
  # 8. HELPER FUNCTION: CREATE GAM GRID PAGE
  # =============================================================================
  
  create_gam_grid_page <- function(outcome, version_info, base_save_dir, groups_to_show) {
    plots <- list()
    
    for (g in groups_to_show) {
      p <- load_and_plot_gam(outcome, g, version_info, base_save_dir)
      # Now load_and_plot_gam always returns a plot (either real or error plot)
      # so we don't need to check for NULL
      plots[[length(plots) + 1]] <- p
    }
    
    if (length(plots) == 0) {
      # Create empty plot with message (safety net - shouldn't happen now)
      empty_plot <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                label = glue("No GAM models found for {version_info$version_label}"),
                size = 6, color = "gray50") +
        theme_void()
      return(empty_plot)
    }
    
    # Find shared Y-axis limits across all plots for this page
    # Only extract y-axis from valid GAM plots (not error plots)
    y_min <- Inf
    y_max <- -Inf
    
    for (p in plots) {
      # Use ggplot_build to extract the actual plot data
      # Skip plots that are error messages (they won't have meaningful y data)
      tryCatch({
        built_plot <- ggplot_build(p)
        # Extract y values from all layers
        for (layer_data in built_plot$data) {
          if ("y" %in% names(layer_data)) {
            y_vals <- layer_data$y
            # Check if y values are finite numbers (not error plot text annotations)
            if (length(y_vals) > 0 && any(is.finite(y_vals))) {
              y_min <- min(y_min, min(y_vals, na.rm = TRUE), na.rm = TRUE)
              y_max <- max(y_max, max(y_vals, na.rm = TRUE), na.rm = TRUE)
            }
          }
        }
      }, error = function(e) {
        # Skip plots that can't be built or don't have expected structure
        NULL
      })
    }
    
    # Apply shared Y-axis only to valid plots (those with data layers)
    # Edge case: If all plots are error plots, y_min/y_max remain Inf/-Inf,
    # and this block is skipped (error plots are returned without y-axis scaling)
    if (is.finite(y_min) && is.finite(y_max)) {
      # Add some padding
      y_range <- y_max - y_min
      y_min <- y_min - 0.05 * y_range
      y_max <- y_max + 0.05 * y_range
      
      # Only apply ylim to plots that have actual data (not error plots)
      plots <- lapply(plots, function(p) {
        tryCatch({
          # Check if plot has geom_line or geom_ribbon (real data plots)
          if (length(p$layers) > 0) {
            # Check if any layer is not just annotation/text
            has_data_layer <- any(sapply(p$layers, function(layer) {
              !inherits(layer$geom, c("GeomText", "GeomLabel"))
            }))
            if (has_data_layer) {
              return(p + ylim(y_min, y_max))
            }
          }
          return(p)  # Return unchanged if it's an error plot
        }, error = function(e) {
          return(p)  # Return unchanged on any error
        })
      })
    }
    
    # Create grid layout
    # Add title as a text grob
    title_grob <- textGrob(
      glue("{outcome}: {version_info$version_label} GAM Trajectories"),
      gp = gpar(fontsize = 16, fontface = "bold")
    )
    
    # Arrange plots in 2x2 grid with title
    grid_plot <- arrangeGrob(
      grobs = plots,
      ncol = 2,
      top = title_grob
    )
    
    return(grid_plot)
  }
  
  # =============================================================================
  # 9. MAIN LOOP: GENERATE PDF FOR EACH OUTCOME
  # =============================================================================
  
  report_files <- character(0)
  
  for (outcome in outcomes) {
    message(glue("\n📊 Processing outcome: {outcome}"))
    
    # Load BIC data for all versions
    all_bic_data <- NULL
    
    for (i in 1:nrow(versions)) {
      version_info <- versions[i, ]
      bic_data <- load_bic_data(outcome, version_info, base_save_dir)
      
      if (!is.null(bic_data)) {
        all_bic_data <- rbind(all_bic_data, bic_data)
      }
    }
    
    if (is.null(all_bic_data)) {
      message(glue("  ⚠️  No BIC data found for {outcome} - skipping"))
      next
    }
    
    # Create PDF file
    pdf_file <- file.path(report_dir, glue("{outcome}_comparison_report.pdf"))
    
    tryCatch({
      pdf(pdf_file, width = 11, height = 8.5)
      
      # Page 1: BIC Cover Page
      message(glue("  📄 Creating BIC cover page..."))
      bic_plot <- create_bic_cover_page(outcome, all_bic_data)
      grid.newpage()
      grid.draw(arrangeGrob(bic_plot))
      
      # Page 2: Menses-centered GAM plots
      message(glue("  📄 Creating Menses-centered GAM plots..."))
      menses_version <- versions[versions$centering == "menses", ]
      menses_grid <- create_gam_grid_page(outcome, menses_version, base_save_dir, groups_to_show)
      grid.newpage()
      grid.draw(menses_grid)
      
      # Page 3: Ovulation-centered GAM plots
      message(glue("  📄 Creating Ovulation-centered GAM plots..."))
      ov_version <- versions[versions$centering == "ovulation", ]
      ov_grid <- create_gam_grid_page(outcome, ov_version, base_save_dir, groups_to_show)
      grid.newpage()
      grid.draw(ov_grid)
      
      dev.off()
      
      message(glue("  ✅ PDF saved: {basename(pdf_file)}"))
      report_files <- c(report_files, pdf_file)
      
    }, error = function(e) {
      message(glue("  ❌ Error creating PDF for {outcome}: {e$message}"))
      if (dev.cur() > 1) dev.off()  # Close PDF device if open
    })
  }
  
  # =============================================================================
  # 10. RETURN RESULTS
  # =============================================================================
  
  message(glue("\n✨ PDF generation complete!"))
  message(glue("📄 Generated {length(report_files)} PDF files in {basename(report_dir)}"))
  
  return(list(
    report_dir = report_dir,
    report_files = report_files
  ))
}
