#' Fit Group-Moderated GAMs with Cyclic Splines for Menstrual Cycle Data
#'
#' Fits generalized additive models (GAMs) to test whether SMM group membership
#' moderates the relationship between cycle phase and outcome variables. This
#' function iterates through multiple SMM group solutions, fitting a separate
#' GAM for each.
#'
#' For each group solution, the function:
#' 1. Merges SMM group assignments with outcome data
#' 2. Fits a GAM with group-specific cyclic smooths
#' 3. Generates model-implied trajectory plots by group
#' 4. Saves model objects, summaries, and plots
#'
#' Model structure includes:
#' - Random intercepts and slopes: s(id, bs='re') + s(time_var, id, bs=c('re','cc'))
#' - Group main effects: smm_group (categorical predictor)
#' - Group-specific cyclic smooths: s(time_var, by=smm_group, bs='cc')
#'
#' Cyclic splines (bs='cc') ensure smooth connections at cycle boundaries.
#'
#' @param data A dataframe containing:
#'   - id: Participant identifier
#'   - Time variable (specified by 'time_var' parameter)
#'   - Outcome variable (specified by 'outcome' parameter)
#'   - Derived variables: {outcome}_log (if log_var=TRUE)
#' @param outcome Character string naming the outcome variable (e.g., "dep", "crave_alc_pm").
#'   The function automatically detects binary outcomes (0/1 values) and uses 
#'   binomial(link="logit") family; otherwise uses Gaussian family.
#' @param time_var Character string naming the cyclic time variable (e.g., 
#'   "cyclic_time_impute", "ovtime_imputed"), typically ranging -1 to +1
#' @param smm_result List object returned by run_smm_cyclic() or run_smm_bernoulli() 
#'   with multiple g values. Must contain:
#'   - all_results: Named list of SMM results for each group size
#'   - Each result must have 'class' element with id and smm_group columns
#' @param centering Character string: "menses" or "ovulation" for plot labeling
#'   and phase shading (default: "menses")
#' @param save_dir Character string specifying base directory for saving outputs.
#'   Files organized as: save_dir/YYYYMMDD/{centering}_centered/outcome/
#' @param k_smooth Integer specifying number of basis functions for cyclic spline
#'   (default: 10). Higher values allow more flexible curves.
#' @param log_var Logical: use log-transformed outcome? (default: TRUE).
#'   If TRUE, models {outcome}_log; if FALSE, models {outcome}.
#'   Note: This parameter is ignored for binary outcomes (0/1 values), which always
#'   use raw values with binomial family.
#' @param show_CI Logical: display confidence intervals on trajectory plots?
#'   (default: TRUE). CIs are 95% confidence bands around predicted means.
#' @param covariates Character vector of covariate names to include as fixed effects
#'   (default: c("day_of_week", "session_today")). Set to NULL to exclude covariates.
#'   Must match the covariates used in the SMM fitting.
#'
#' @return invisible(NULL). The function is called for its side effects (saving files).
#'
#' @details
#' Output files saved for each group solution:
#' - {outcome}_log_g{g}_GAM_model.rds: Model object (for predictions/diagnostics)
#' - {outcome}_log_g{g}_GAM_summary.txt: Text summary (coefficients, smooths, fit stats)
#' - {outcome}_log_g{g}_GAM_plot.png: Trajectory plot with group-specific curves
#'
#' ERROR HANDLING:
#' The function includes robust error handling for common GAM fitting issues:
#' - Singular matrix errors: Automatically reduces basis functions (k) and retries
#' - Prediction failures: Skips plotting but preserves model and summary files
#' - Plot save errors: Ensures model/summary are saved even if plotting fails
#' 
#' When a singular matrix error occurs (e.g., "backsolve" error), the function
#' automatically tries progressively smaller k values (default, max(5, default - 2), 4, 3)
#' until the model fits successfully. This handles cases with insufficient data or
#' limited variation in some groups. If all attempts fail, the group size is
#' skipped with an informative message, and processing continues with other groups.
#'
#' Additional files:
#' - {outcome}_noGroup_GAM_model.rds: Baseline model without groups
#' - {outcome}_noGroup_GAM_summary.txt: Baseline model summary
#'
#' Trajectory plots include:
#' - Shaded background regions indicating cycle phases (luteal/follicular)
#' - Group-specific curves with 95% CIs (if show_CI=TRUE)
#' - Legend showing group labels with sample sizes
#' - X-axis labels marking key cycle events (menses, ovulation, 50% phase elapsed)
#'
#' Predictions use marginaleffects::predictions() with:
#' - Population-level effects (random effect for id excluded)
#' - type="response" for appropriate scale transformation:
#'   * Binary outcomes: Applies plogis() to convert from logit to probability scale
#'   * Continuous outcomes: Returns predictions on response scale
#' - Exponential transformation if log_var=TRUE for continuous outcomes (back-transforms 
#'   from log(x+1) to original scale using exp(y) - 1)
#'
#' @examples
#' # Fit SMMs with multiple group sizes
#' smm_result <- run_smm_cyclic(
#'   data = ukalc_data,
#'   outcome = "dep",
#'   time_var = "cyclic_time_impute",
#'   g = 2:5,
#'   centering = "menses",
#'   save_dir = "output/SMMs"
#' )
#'
#' # Fit group-moderated GAMs
#' model_plot_modx_gam_cyclic(
#'   data = ukalc_data,
#'   outcome = "dep",
#'   time_var = "cyclic_time_impute",
#'   smm_result = smm_result,
#'   centering = "menses",
#'   save_dir = "output/GAMs",
#'   log_var = TRUE,
#'   show_CI = TRUE
#' )
#'
#' # Load saved model for further analysis
#' best_model <- readRDS("output/GAMs/20260101/menses_centered/dep/dep_log_g3_GAM_model.rds")
#' summary(best_model)
#'
#' @seealso
#' \code{\link{run_smm_cyclic}} for fitting SMMs (prerequisite)
#' \code{\link[mgcv]{gam}} for the underlying GAM fitting
#' \code{\link[marginaleffects]{predictions}} for model predictions with CIs
#'
#' @note Requires packages: dplyr, tidyverse, mgcv, gamm4, ggplot2, zoo, marginaleffects, glue
#'
#' @export
model_plot_modx_gam_cyclic <- function(
    data,
    outcome,
    time_var, # New argument
    smm_result,
    centering = "menses",
    save_dir,
    k_smooth = 10, 
    log_var = TRUE,
    show_CI = TRUE,
    covariates = c("day_of_week", "session_today")  # NEW PARAMETER
) {
  library(glue)
  library(dplyr)
  library(ggplot2)
  library(mgcv)
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  ## --------- choose subfolder by centering for all saves ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    
    # New path: save_dir/YYYYMMDD/centering_folder/outcome
    sub_dir <- file.path(save_dir, date_folder, centering_folder, outcome)
    
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  
  ## Determine variable suffix and detect binary outcomes
  # Check if outcome is binary (0/1 values only; ignore missing values)
  is_binary <- all(data[[outcome]] %in% c(0, 1), na.rm = TRUE)
  
  if (is_binary) {
    full_outcome <- outcome  # Use raw binary outcome
    family_spec <- binomial(link = "logit")
    message(glue("✓ Binary outcome detected: using raw '{outcome}' with binomial family"))
  } else {
    var_suffix <- if (log_var) "_log" else ""
    full_outcome <- paste0(outcome, var_suffix)
    family_spec <- gaussian()
    message(glue("✓ Continuous outcome: using '{full_outcome}' with Gaussian family"))
  }
  
  ## 🔹 Baseline model without group
  base_formula <- as.formula(glue(
    "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + s({time_var}, bs = 'cc')"
  ))
  
  base_fit <- mgcv::gam(
    formula = base_formula,
    data = data,
    method = "REML",
    family = family_spec
  )
  
  # Save baseline model and summary
  if (!is.null(sub_dir)) {
    saveRDS(base_fit, file = file.path(sub_dir, glue("{full_outcome}_noGroup_GAM_model.rds")))
    writeLines(capture.output(summary(base_fit)), file.path(sub_dir, glue("{full_outcome}_noGroup_GAM_summary.txt")))
  }
  
  # Iterate over all group sizes in smm result
  all_g <- names(smm_result$all_results)
  
  for (g in all_g) {
    message(glue(">> Processing grouping for g = {g}"))
    
    # 1️⃣ Extract class assignments
    class_df <- smm_result$all_results[[g]]$class
    class_df <- unique(class_df)
    
    # 2️⃣ Merge onto main data
    # Include covariates if they exist in the data and are specified
    select_cols <- c("id", outcome, paste0(outcome, "_log"), full_outcome, time_var)
    
    # Add covariates to the selection if they are specified and exist in the data
    if (!is.null(covariates) && length(covariates) > 0) {
      available_covariates <- intersect(covariates, names(data))
      if (length(available_covariates) > 0) {
        select_cols <- c(select_cols, available_covariates)
      } else {
        message("⚠️ Warning: Specified covariates not found in data. Proceeding without covariates.")
        covariates <- NULL
      }
    }
    
    data_with_group <- data %>%
      dplyr::select(all_of(select_cols)) %>%
      left_join(class_df, by = "id") %>%
      filter(!is.na(smm_group))
    
    # Make sure group is a factor
    data_with_group$smm_group <- as.factor(data_with_group$smm_group)
    
    # 3️⃣ Fit GAM with error handling and automatic k reduction
    # 
    # PROBLEM: GAM fitting can fail with "singular matrix in backsolve" errors when:
    # - There's insufficient data variation within some groups
    # - Too many basis functions (k) relative to available data
    # - Perfect collinearity between predictors
    # 
    # SOLUTION: Progressive k reduction strategy
    # - Start with default k_smooth (typically 10)
    # - If singular matrix error, retry with max(5, k_smooth-2), then 4, then 3
    # - Each reduction simplifies the model, making it more stable
    # - Skip group size entirely if all attempts fail
    # 
    # WHY THIS WORKS: Fewer basis functions = simpler spline = less likely to overfit
    # sparse data or create numerical instability in matrix operations
    
    # Build covariate part of formula (must match what was used in run_smm_cyclic)
    if (!is.null(covariates) && length(covariates) > 0) {
      covariate_formula <- paste(covariates, collapse = " + ")
      covariate_formula <- paste0(" + ", covariate_formula)
    } else {
      covariate_formula <- ""
    }
    
    gam_formula <- as.formula(glue(
      "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + smm_group + s({time_var}, by = smm_group, bs = c('cc'), k = {k_smooth}){covariate_formula}"
    ))
    
    # Try fitting with default k, then reduce if singular matrix error occurs
    gam_fit <- NULL
    k_values <- unique(c(k_smooth, max(5, k_smooth - 2), 4, 3))  # Try progressively smaller k values without duplicates
    
    for (k_try in k_values) {
      if (k_try != k_smooth) {
        # Update formula with reduced k
        gam_formula <- as.formula(glue(
          "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + smm_group + s({time_var}, by = smm_group, bs = c('cc'), k = {k_try}){covariate_formula}"
        ))
      }
      
      gam_fit <- tryCatch(
        {
          mgcv::gam(
            formula = gam_formula,
            data = data_with_group,
            method = "REML",
            family = family_spec
          )
        },
        error = function(e) {
          if (grepl("singular matrix|backsolve", e$message, ignore.case = TRUE)) {
            message(glue("⚠️  Singular matrix error with k={k_try} for g={g}. ", 
                        ifelse(k_try == min(k_values), 
                               "Skipping this group size.", 
                               "Trying with reduced basis functions...")))
            return(NULL)
          } else {
            # Re-throw other errors
            stop(e)
          }
        }
      )
      
      # If fit succeeded, break out of loop
      if (!is.null(gam_fit)) {
        if (k_try != k_smooth) {
          message(glue("✓ Successfully fit GAM with reduced k={k_try} for g={g}"))
        }
        break
      }
    }
    
    # Skip this group size if all attempts failed
    if (is.null(gam_fit)) {
      message(glue("❌ Could not fit GAM for g={g} even with reduced basis functions. Skipping."))
      next
    }
    
    # 4️⃣ Save model as RDS
    if (!is.null(sub_dir)){
      saveRDS(gam_fit, file = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_model.rds")))
    }
    
    # 5️⃣ Save summary as TXT
    summ_text <- capture.output(summary(gam_fit))
    if (!is.null(sub_dir)){
      writeLines(summ_text, con = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_summary.txt")))
    }
    
    # 6️⃣ Build prediction grid
    group_levels = sort(unique(data_with_group$smm_group))
    pred_grid <- expand.grid(
      temp_time = seq(-1, 1, length.out = 100),
      smm_group = factor(group_levels, levels = group_levels),
      id = "new" # Use a placeholder for population-level predictions
    )
    names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
    
    # Add covariates to prediction grid at reference levels
    if (!is.null(covariates) && length(covariates) > 0) {
      for (cov in covariates) {
        if (cov %in% names(data_with_group)) {
          if (is.factor(data_with_group[[cov]])) {
            # Use the reference level for factors
            pred_grid[[cov]] <- levels(data_with_group[[cov]])[1]
          } else {
            # Use the most common value or 0 for numeric covariates
            pred_grid[[cov]] <- 0
          }
        }
      }
    }
    
    # 7️⃣ Predictions with error handling
    # Note: predictions are on the person-centered log scale
    # For log(x+1) transformation, back-transform is: exp(y) - 1
    pred <- tryCatch(
      {
        marginaleffects::predictions(
          gam_fit,
          newdata = pred_grid,
          type = "response",
          exclude = "s(id)" # Exclude random effect for id
        )
      },
      error = function(e) {
        message(glue("⚠️  Prediction error for g={g}: {e$message}"))
        message(glue("❌ Skipping plot generation for g={g}"))
        return(NULL)
      }
    )
    
    # Skip plotting if predictions failed
    if (is.null(pred)) {
      next
    }
    
    # Back-transformation logic based on outcome type
    # - Binary outcomes: type="response" already applies plogis(), no further transformation needed
    # - Continuous log(x+1) outcomes: need to apply exp(y) - 1 to reverse the transformation
    # - Continuous raw outcomes: use predictions as-is
    if (is_binary) {
      # Binary: predictions are already on probability scale [0,1] via plogis()
      pred_grid$estimate <- pred$estimate
      pred_grid$conf.low <- pred$conf.low
      pred_grid$conf.high <- pred$conf.high
    } else if (log_var) {
      # Continuous with log transformation: back-transform from log(x+1) scale
      pred_grid$estimate <- exp(pred$estimate) - 1
      pred_grid$conf.low <- exp(pred$conf.low) - 1
      pred_grid$conf.high <- exp(pred$conf.high) - 1
    } else {
      # Continuous without log transformation: use as-is
      pred_grid$estimate <- pred$estimate
      pred_grid$conf.low <- pred$conf.low
      pred_grid$conf.high <- pred$conf.high
    }
    
    
    # 8️⃣ Build prediction plot
    x_breaks <- seq(-1, 1, by = 0.5)
    x_labels <- if (centering == "menses") {
      c("Ovulation", "50%L", "Menses Onset", "50%F", "Ovulation")
    } else {
      c("Menses Onset", "50%F", "Ovulation", "50%L", "Menses Onset")
    }
    
    rect_data <- if (centering == "menses") {
      data.frame(
        xmin = c(-0.04, 0.92, -1),
        xmax = c(0.04, 1, -0.92),
        fill = c("grey70", "grey87", "grey87")
      )
    } else {
      data.frame(
        xmin = c(-0.04, -1, 0.92),
        xmax = c(0.04, -0.92, 1),
        fill = c("grey87", "grey70", "grey70")
      )
    }
    
    group_n <- data_with_group %>%
      group_by(smm_group) %>%
      summarise(n_id = n_distinct(id), .groups = "drop") %>%
      mutate(
        group_label = paste0("Group ", smm_group, " (N=", n_id, ")"),
        smm_group = as.factor(smm_group)
      )
    
    # Merge into prediction grid
    pred_grid <- pred_grid %>%
      left_join(group_n, by = "smm_group")
    
    p <- ggplot(pred_grid, aes(x = .data[[time_var]], y = estimate, color = group_label)) +
      geom_rect(
        data = rect_data,
        inherit.aes = FALSE,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = rect_data$fill,
        alpha = 0.2,
        color = "white",
        show.legend = FALSE
      ) +
      geom_ribbon(
        aes(ymin = conf.low, ymax = conf.high, fill = group_label),
        alpha = if (show_CI) 0.2 else 0,
        color = NA,
        show.legend = FALSE
      ) +
      geom_line(linewidth = 0.9) +
      scale_x_continuous(
        limits = c(-1, 1),
        breaks = x_breaks,
        labels = x_labels
      ) +
      labs(
        x = "",
        y = glue("{outcome}"),
        title = glue("Model-Implied Curves for {outcome} with Group Moderator (g={g})"),
        color = "Group",
        fill = "Group"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    
    # 9️⃣ Save prediction plot with error handling
    if (!is.null(sub_dir)){
      tryCatch(
        {
          save_png(
            plot = p,
            filename = glue("{full_outcome}_g{g}_GAM_plot.png"),
            folder = sub_dir
          )
          message(glue("✅ Done with g = {g}"))
        },
        error = function(e) {
          message(glue("⚠️  Error saving plot for g={g}: {e$message}"))
          message(glue("⚠️  Model and summary were saved, but plot generation failed"))
        }
      )
    } else {
      message(glue("✅ Done with g = {g}. Model fitted successfully (no save_dir specified)"))
    }
  }
  
  message("✅✅ All group GAMs processed and saved!")
  
  ## Return
  return(invisible(NULL))
}