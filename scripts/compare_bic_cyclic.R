#' Compare BIC Values Across Group Numbers for Cyclic SMMs
#'
#' Compares Bayesian Information Criterion (BIC) values for structural marginal
#' models (SMMs) with different numbers of latent groups, including a baseline
#' single-group GAM model (g=1). This function helps identify the optimal number
#' of trajectory groups in menstrual cycle data by visualizing and tabulating
#' BIC values across model complexity levels.
#'
#' The function:
#' 1. Fits a baseline cyclic GAM with random effects (g=1 model)
#' 2. Extracts BIC values from multi-group SMM results (g=2, 3, 4, ...)
#' 3. Combines into a single comparison table
#' 4. Creates a line plot showing BIC by number of groups
#' 5. Saves table (CSV) and plot (PNG) if save_dir is provided
#'
#' Lower BIC indicates better model fit penalized for complexity. The optimal
#' number of groups typically occurs at the minimum BIC value, though theoretical
#' considerations and interpretability also guide group selection.
#'
#' @param data A dataframe containing:
#'   - id: Participant identifier
#'   - Outcome variable (specified in 'outcome' parameter)
#'   - Time variable (specified in 'time_var' parameter)
#'   - Must have derived variable: {outcome}_log.d (log-transformed, person-centered)
#' @param outcome Character string naming the outcome variable (e.g., "dep", "crave_alc_pm")
#' @param time_var Character string naming the time variable for cyclic modeling
#'   (e.g., "cyclic_time_impute", "ovtime_imputed")
#' @param centering Character string: "menses" or "ovulation" to specify cycle centering
#'   (used for labeling and organizing output files)
#' @param smm_results List object returned by run_smm_cyclic() containing:
#'   - bic_table: Dataframe with BIC values for multi-group models
#' @param k_smooth Integer specifying number of basis functions for the cyclic smooth
#'   (default: 10). Higher values allow more flexible curves.
#' @param save_dir Character string specifying base directory for saving outputs.
#'   If NULL (default), outputs are not saved. Files are organized as:
#'   save_dir/YYYYMMDD/{centering}_centered/outcome/
#'
#' @return A list with two elements:
#'   - bic_table: Dataframe with columns 'groups' and 'BIC' for all models (g=1 through g=max)
#'   - bic_plot: ggplot2 object showing BIC values across group numbers
#'
#' @details
#' The baseline g=1 model is a cyclic GAM with formula:
#'   outcome_log.d ~ s(time_var, bs='cc', k=k_smooth)
#' with random effects: ~ (1 + time_var | id)
#'
#' Cyclic splines (bs='cc') constrain the smooth to connect smoothly at cycle
#' boundaries (-1 and +1), appropriate for menstrual cycle data where day -28
#' and day 0 represent consecutive days.
#'
#' BIC is calculated as: -2*log-likelihood + log(n)*effective_df
#'
#' @examples
#' # Compare BIC for depression across 1-5 groups (menses-centered)
#' smm_result <- run_smm_cyclic(data, "dep", "cyclic_time_impute", 
#'                               groups = 2:5, centering = "menses")
#' bic_comparison <- compare_bic_cyclic(data, "dep", "cyclic_time_impute",
#'                                      centering = "menses", 
#'                                      smm_results = smm_result,
#'                                      save_dir = "output/SMMs")
#'
#' # View results
#' print(bic_comparison$bic_table)
#' print(bic_comparison$bic_plot)
#'
#' @seealso 
#' \code{\link{run_smm_cyclic}} for fitting multi-group SMMs
#' \code{\link[gamm4]{gamm4}} for the underlying GAM fitting
#'
#' @note Requires packages: dplyr, tidyverse, mgcv, gamm4, ggplot2, zoo, glue
#'
#' @export
compare_bic_cyclic <- function(
    data,
    outcome,
    time_var, # New argument
    centering = "menses",
    smm_results,
    k_smooth = 10,
    save_dir = NULL
) {
  
  ## --------- choose subfolder by centering for all saves ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    
    # Final path: save_dir/YYYYMMDD/centering_folder/outcome
    sub_dir <- file.path(save_dir, date_folder, centering_folder, outcome)
    
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  message(glue(">> Computing baseline (g=1) model for outcome = {outcome}, centering = {centering}"))
  
  ## We now use the time_var passed directly into the function
  knots_list <- setNames(list(c(-1, 1)), time_var)
  
  ## Fit single-group model
  formula_g1 <- as.formula(glue("{outcome}_log.d ~ s({time_var},  bs = 'cc', k = {k_smooth})"))
  
  gamm_fit_g1 <- gamm4(
    formula_g1,
    random = as.formula(glue("~ (1 + {time_var} | id)")),
    data = data,
    na.action = na.omit, 
    knots = knots_list
  )
  
  # Compute log-likelihood, edf, and BIC
  llk_1group <- logLik(gamm_fit_g1$mer)
  df_1group <- sum(summary(gamm_fit_g1$gam)$edf) + 1
  n <- nrow(data)
  bic_1group <- -2 * llk_1group + log(n) * df_1group
  bic_1group_val <- as.numeric(bic_1group)
  message(glue(">> Baseline BIC (g=1): {round(bic_1group_val, 2)}"))
  
  ## Extract BIC values from smm_results
  if (is.null(smm_results$bic_table)) {
    stop("smm_results does not include bic_table. Did you pass the correct object?")
  }
  
  smm_table <- smm_results$bic_table %>% arrange(groups)
  
  ## Combine into one comparison table
  all_bic_table <- data.frame(
    groups = c(1, smm_table$groups),
    BIC = c(bic_1group_val, smm_table$best_BIC)
  )
  
  ## Plot
  bic_plot <- ggplot(all_bic_table, aes(x = groups, y = BIC)) +
    geom_line() + 
    geom_point(size = 2) +
    labs(
      title = glue("{outcome} SMM ({centering}-centered) Comparison by BIC"),
      x = "Number of Groups",
      y = "Minimum BIC"
    ) +
    scale_x_continuous(
      breaks = seq(min(all_bic_table$groups), max(all_bic_table$groups), by = 1)
    ) +
    theme_minimal(base_size = 14)
  
  print(bic_plot)
  
  ## Save outputs if directory specified
  if (!is.null(sub_dir)) {
    
    # Build base filename
    base_name <- glue("{outcome}_{centering}_bic_comparison")
    
    # Save CSV
    csv_path <- file.path(sub_dir, paste0(base_name, ".csv"))
    write.csv(all_bic_table, csv_path, row.names = FALSE)
    message(glue("✅ Saved BIC comparison: {base_name}.csv"))
    
    # Save plot (PNG only, no SVG)
    png_path <- file.path(sub_dir, paste0(base_name, ".png"))
    save_png(plot = bic_plot, filename = paste0(base_name, ".png"), folder = sub_dir, save_svg = FALSE)
  }
  
  ## Return both
  return(list(
    bic_table = all_bic_table,
    bic_plot = bic_plot
  ))
}