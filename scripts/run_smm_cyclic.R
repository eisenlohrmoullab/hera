#' Fit Smoothing Mixture Models (SMMs) for Cyclic Menstrual Data
#'
#' Implements expectation-maximization (EM) algorithm to identify latent groups
#' with distinct trajectory patterns across the menstrual cycle. This function
#' fits cyclic generalized additive mixed models (GAMMs) to daily outcome data,
#' allowing identification of subgroups with different cycle-related patterns
#' (e.g., perimenstrual symptoms vs. luteal symptoms vs. stable/asymptomatic).
#'
#' The algorithm:
#' 1. Initializes group assignments using k-means clustering of individual GAM trajectories
#' 2. Iteratively refits group-specific GAMMs and reassigns participants based on
#'    residual sum of squares (RSS)
#' 3. Converges when group assignments stabilize or maximum iterations reached
#' 4. Computes posterior probabilities for group membership based on likelihood
#' 5. Generates visualization plots showing group-specific trajectories
#'
#' The function can fit a single g-group solution or multiple solutions simultaneously
#' (e.g., g = 2:5) for model selection via BIC comparison.
#'
#' @param data A dataframe containing:
#'   - id: Participant identifier
#'   - Time variable (specified by 'time_var' parameter)
#'   - Outcome variable (specified by 'outcome' parameter)
#'   - Should include derived variables: {outcome}_log.d (log-centered) or {outcome}.d (centered)
#'   - day_of_week: Factor variable for day of week (if using covariates)
#'   - session_today: Binary 0/1 indicator for lab session presence (if using covariates)
#' @param outcome Character string naming the outcome variable (e.g., "dep", "crave_alc_pm")
#' @param time_var Character string naming the cyclic time variable (typically ranging -1 to +1,
#'   where 0 = cycle event like menses onset or ovulation)
#' @param plot_label Character string for plot titles (default: NULL, uses technical outcome name).
#'   Use human-readable labels (e.g., "Depression" instead of "dep")
#' @param g Integer or integer vector specifying number of latent groups to fit.
#'   If vector (e.g., 2:5), fits multiple models and returns BIC table for comparison.
#' @param d_inter Integer specifying maximum EM iterations (default: 20)
#' @param plot Logical: should trajectory plots be generated? (default: TRUE)
#' @param seed Integer for random seed to ensure reproducible k-means initialization (default: 123)
#' @param centering Character string: "menses" or "ovulation" for labeling plots and organizing
#'   output folders (default: "menses")
#' @param save_dir Character string specifying base directory for saving outputs. If NULL,
#'   outputs are not saved. Files organized as: save_dir/YYYYMMDD/{centering}_centered/outcome/
#' @param k_smooth Integer specifying number of basis functions for cyclic spline (default: 10)
#' @param log_var Logical: use log-transformed outcome? (default: TRUE, uses {outcome}_log.d).
#'   Note: This parameter is ignored for binary outcomes (0/1 values), which always
#'   use raw values with a Bernoulli distribution instead.
#' @param covariates Character vector of covariate names to include as fixed effects
#'   (default: c("day_of_week", "session_today")). Set to NULL to exclude covariates.
#'
#' @return If single g value:
#'   A list with elements:
#'   - class: Dataframe with id, smm_group (hard assignment), and prob_group* columns (posterior probabilities)
#'   - LLK: Vector of log-likelihoods for each EM iteration
#'   - BIC: Vector of BIC values for each EM iteration
#'   - summary: Group-level summary statistics (n, mean, sd of outcome)
#'   - plots: List containing ggplot2 objects (plot_roll, plot_centered, plot_mean)
#'
#' If multiple g values:
#'   A list with elements:
#'   - all_results: Named list of results for each g value
#'   - bic_table: Dataframe comparing BIC across group numbers
#'
#' @details
#' The cyclic spline (bs='cc') ensures smooth connection at cycle boundaries, appropriate
#' for menstrual cycle data where day -28 and day 0 are consecutive. Random effects
#' structure varies by outcome type: ~ (1 + time_var | id) for continuous outcomes
#' (person-specific intercepts and slopes), or ~ (1 | id) for binary outcomes
#' (intercept-only to prevent convergence issues with sparse data).
#'
#' Group assignment uses RSS as the criterion: each participant is assigned to the
#' group whose model produces the smallest residual sum of squares for their data.
#' For binary outcomes, RSS is calculated using Pearson residuals with binomial variance.
#'
#' Posterior probabilities are calculated using person-specific log-likelihoods and
#' softmax transformation, providing probabilistic (vs. hard) group assignments. The
#' likelihood function is outcome-specific:
#' - Binary outcomes: Binomial log-likelihood Σ[y*log(p) + (1-y)*log(1-p)]
#' - Continuous outcomes: Gaussian log-likelihood -0.5*n*log(σ²) - RSS/(2σ²)
#'
#' Participants need ≥10 non-missing observations to contribute to trajectory estimation.
#'
#' Plots show group trajectories three ways:
#' - Rolling average: Smoothed person-centered values
#' - Person-centered: Within-person deviations from personal mean
#' - Mean: Raw outcome values (grand mean scale)
#'
#' @examples
#' # Fit 3-group solution for depression (menses-centered)
#' result <- run_smm_cyclic(
#'   data = ukalc_data,
#'   outcome = "dep",
#'   time_var = "cyclic_time_impute",
#'   plot_label = "Depression",
#'   g = 3,
#'   centering = "menses",
#'   save_dir = "output/SMMs"
#' )
#'
#' # View group assignments and probabilities
#' head(result$class)
#'
#' # Check convergence
#' plot(result$BIC, type = "l", ylab = "BIC", xlab = "EM Iteration")
#'
#' # Fit multiple group solutions for model selection
#' multi_result <- run_smm_cyclic(
#'   data = ukalc_data,
#'   outcome = "crave_alc_pm",
#'   time_var = "ovtime_imputed",
#'   g = 2:5,
#'   centering = "ovulation",
#'   save_dir = "output/SMMs"
#' )
#'
#' # Compare BIC across group numbers
#' print(multi_result$bic_table)
#'
#' @seealso 
#' \code{\link{compare_bic_cyclic}} for comparing multi-group solutions
#' \code{\link{label_smm_groups}} for assigning meaningful labels to groups
#' \code{\link[gamm4]{gamm4}} for the underlying GAMM fitting
#'
#' @note Requires packages: glue, dplyr, mgcv, gamm4, ggplot2
#'
#' @export
run_smm_cyclic <- function(
    data,
    outcome,
    time_var,
    plot_label = NULL, # <-- New argument
    g = 2,
    d_inter = 20,
    plot = TRUE,
    seed = 123,
    centering = "menses",
    save_dir = NULL,
    k_smooth = 10,
    log_var = TRUE,
    covariates = c("day_of_week", "session_today")  # NEW PARAMETER
) {
  # Fallback to use the technical name if a pretty label isn't provided
  if (is.null(plot_label)) plot_label <- outcome

  set.seed(seed)
  `%>%` <- dplyr::`%>%`

  ## --------- create nested folder structure ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    sub_dir <- file.path(save_dir, date_folder, centering_folder, outcome)
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }

  ## Detect if outcome is binary (only contains 0, 1, or NA)
  # First check if there are any non-NA values
  non_na_values <- data[[outcome]][!is.na(data[[outcome]])]
  if (length(non_na_values) == 0) {
    stop(glue("Outcome '{outcome}' contains only NA values. Cannot proceed with analysis."))
  }
  
  is_binary <- all(data[[outcome]] %in% c(0, 1, NA), na.rm = TRUE)
  
  if (is_binary) {
    full_outcome <- outcome  # Use raw binary outcome
    family_spec <- binomial(link = "logit")
    message(glue("✓ Binary outcome detected: using raw '{outcome}' with Bernoulli distribution"))
  } else {
    # Continuous: use existing log + person-centered approach
    var_suffix <- if (log_var) "_log.d" else ".d"
    full_outcome <- paste0(outcome, var_suffix)
    family_spec <- gaussian(link = "identity")
    message(glue("✓ Continuous outcome: using '{full_outcome}' with Gaussian distribution"))
  }

  ## ========= MULTI-g WRAPPER ========= ##
  if (length(g) > 1) {
    message(glue(">> Running multiple group sizes: {paste(g, collapse=', ')}"))
    all_results <- list()
    bic_table <- data.frame(groups = integer(), best_BIC = numeric())
    for (gval in g) {
      message(glue(">>> Fitting SMM for g = {gval} <<<"))
      
      # Wrap SMM fitting in tryCatch to handle singular matrix errors gracefully
      # These patterns capture known gamm4/mgcv singularity / numerical instability errors
      # that arise when the smooths are not identifiable for a given g (group count).
      res <- tryCatch({
        run_smm_cyclic(
          data = data, outcome = outcome, time_var = time_var, plot_label = plot_label, g = gval,
          d_inter = d_inter, plot = plot, seed = seed, centering = centering,
          save_dir = save_dir, k_smooth = k_smooth, log_var = log_var, covariates = covariates
        )
      }, error = function(e) {
        if (grepl("singular matrix|backsolve|Downdated VtV is not positive definite", e$message, ignore.case = TRUE)) {
          message(glue("⚠️  Singular matrix error for g={gval}: {e$message}"))
          message(glue("⚠️  Skipping g={gval} and continuing with remaining group sizes..."))
          return(NULL)
        } else {
          # Re-throw other errors
          stop(e)
        }
      })
      
      all_results[[as.character(gval)]] <- res
      if (!is.null(res)) {
        best_BIC <- min(res$BIC, na.rm = TRUE)
        bic_table <- rbind(bic_table, data.frame(groups = gval, best_BIC = best_BIC))
        
        # Save outputs for this g value if save_dir is specified
        if (!is.null(sub_dir)) {
          # Save classification CSV with group assignments and probabilities
          write.csv(res$class,
                    file = file.path(sub_dir, glue("{outcome}_g{gval}_class.csv")),
                    row.names = FALSE)
          
          # Save plots if they exist
          plots_list <- res$plots
          if (!is.null(plots_list)) {
            # Save rolling average plot
            if (!is.null(plots_list$plot_roll)) {
              ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_roll.png")),
                     plots_list$plot_roll, width = 7, height = 5)
            }
            # Save person-centered plot
            if (!is.null(plots_list$plot_centered)) {
              ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_centered.png")),
                     plots_list$plot_centered, width = 7, height = 5)
            }
            # Save mean plot
            if (!is.null(plots_list$plot_mean)) {
              ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_mean.png")),
                     plots_list$plot_mean, width = 7, height = 5)
            }
          }
        }
      }
    }
    if (!is.null(sub_dir)) {
      write.csv(bic_table, file = file.path(sub_dir, glue("{outcome}_bic_table.csv")), row.names = FALSE)
    }
    return(list(all_results = all_results, bic_table = bic_table))
  }

  ## ========= SINGLE-g CASE ========= ##
  message(glue(">>> Fitting SMM for g = {g} <<<"))
  if (!time_var %in% names(data)) stop(glue("Time variable '{time_var}' not found in data!"))

  # Minimum variance threshold for Pearson residual calculation
  # Prevents division by zero when fitted probabilities are at boundaries (0 or 1)
  MIN_VARIANCE_THRESHOLD <- 1e-10

  # For binary outcomes, only need the raw outcome column
  # For continuous outcomes, need centered and rolling columns
  if (is_binary) {
    outcome_cols <- c(outcome)  # Binary: only raw outcome
  } else {
    centered_col <- paste0(outcome, if(log_var) "_log.d" else ".d")
    rolling_col <- paste0(outcome, if(log_var) "_log.d.roll" else ".d.roll")
    outcome_cols <- c(outcome, centered_col, rolling_col)
  }
  keep_cols <- intersect(c("id", time_var, outcome_cols, covariates), names(data))
  df <- dplyr::select(data, dplyr::all_of(keep_cols))
  
  # Build covariate part of formula
  if (!is.null(covariates) && length(covariates) > 0) {
    covariate_formula <- paste(covariates, collapse = " + ")
    covariate_formula <- paste0(covariate_formula, " + ")
  } else {
    covariate_formula <- ""
  }

  knots_list <- setNames(list(c(-1, 1)), time_var)
  grid <- setNames(data.frame(seq(-1, 1, length.out = 20)), time_var)
  id_vec <- unique(df$id)
  trajectory_matrix <- matrix(NA, nrow = length(id_vec), ncol = 20)
  for (i in seq_along(id_vec)) {
    person_data <- df %>% dplyr::filter(id == id_vec[i])
    if (nrow(person_data) >= 10) {
      # NOTE: For k-means initialization we fit individual-level GAMs using only
      # the smooth time effect. Covariates are excluded here so that prediction
      # on the grid (which only contains the time variable) is valid. Covariates
      # are incorporated later in the group-level GAMM fits.
      fit <- tryCatch(
        mgcv::gam(
          as.formula(glue("{full_outcome} ~ s({time_var}, k = {k_smooth})")),
          data = person_data,
          family = if (is_binary) binomial(link = "logit") else gaussian()
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        preds_for_kmeans <- predict(fit, newdata = grid)
        # For binary outcomes, convert to probability scale for clustering
        if (is_binary) {
          preds_for_kmeans <- plogis(preds_for_kmeans)
        }
        trajectory_matrix[i, ] <- preds_for_kmeans
      }
    }
  }
  valid_rows <- which(rowSums(is.na(trajectory_matrix)) < ncol(trajectory_matrix))
  trajectory_matrix <- trajectory_matrix[valid_rows, ]
  id_vec <- id_vec[valid_rows]
  if (length(id_vec) < g) {
    warning(glue("Not enough participants with valid trajectories for g = {g}. Needed {g}, got {length(id_vec)}. Returning NULL."))
    return(NULL)
  }
  km_res <- kmeans(scale(trajectory_matrix), centers = g)
  cluster_map <- data.frame(id = id_vec, smm_group = as.character(km_res$cluster))
  df <- dplyr::left_join(df, cluster_map, by = "id")
  LLK <- numeric(d_inter)
  BIC <- numeric(d_inter)
  for (i in 1:d_inter) {
    models <- list()
    preds <- matrix(NA, nrow = nrow(df), ncol = g)
    
    # Flag to track if iteration failed
    iteration_failed <- FALSE
    
    for (k in 1:g) {
      dat_k <- df %>% dplyr::filter(smm_group == as.character(k))
      if (dplyr::n_distinct(dat_k$id) < 2) {
        warning(glue("Group {k} has <2 participants. Stopping.")); return(NULL)
      }
      
      # Choose random effects structure based on outcome type
      # Binary outcomes need simpler structure due to sparse data and convergence issues
      if (is_binary) {
        random_formula <- as.formula(glue("~ (1 | id)"))  # Intercept-only for binary
        if (i == 1 && k == 1) {
          message("  Using intercept-only random effects (1 | id) for binary outcome")
        }
      } else {
        random_formula <- as.formula(glue("~ (1 + {time_var} | id)"))  # Full structure for continuous
        if (i == 1 && k == 1) {
          message("  Using random intercepts and slopes (1 + time | id) for continuous outcome")
        }
      }
      
      # Set optimizer control based on outcome type
      control_spec <- if (is_binary) {
        # Use robust optimizer for binary outcomes
        lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 1e5)
        )
      } else {
        # Default control for continuous outcomes
        lme4::lmerControl()
      }
      
      # Wrap gamm4 call in tryCatch to handle singular matrix errors
      # These patterns capture known gamm4/mgcv singularity / numerical instability errors
      # that arise when the smooths are not identifiable for a given g (group count).
      model_fit <- tryCatch({
        gamm4::gamm4(
          as.formula(glue("{full_outcome} ~ {covariate_formula}s({time_var}, bs = 'cc', k = {k_smooth})")), 
          random = random_formula,  # Use conditional random effects formula
          data = dat_k, 
          na.action = na.omit, 
          knots = knots_list,
          family = family_spec,  # Use detected family (binomial or gaussian)
          control = control_spec
        )
      }, error = function(e) {
        # Check for convergence-related errors specific to GAMM/binomial models
        if (grepl("singular matrix|backsolve|Downdated VtV is not positive definite", e$message, ignore.case = TRUE)) {
          message(glue("⚠️  Singular matrix error in EM iteration {i}, group {k} for g={g}: {e$message}"))
          message(glue("⚠️  Unable to fit model for g={g}. Returning NULL for this group size."))
          return(NULL)
        } else if (grepl("step-halving|PIRLS|convergence", e$message, ignore.case = TRUE)) {
          message(glue("⚠️  Convergence issue in EM iteration {i}, group {k} for g={g}"))
          message(glue("     Error: {e$message}"))
          message(glue("⚠️  Unable to fit model for g={g}. Returning NULL for this group size."))
          return(NULL)
        } else {
          # Re-throw other errors
          stop(e)
        }
      })
      
      # If model fitting failed, mark iteration as failed and break
      # Conservative approach: if ANY group fails in an EM iteration, we skip the entire g value
      # This prevents partial/incomplete results that could be misleading
      if (is.null(model_fit)) {
        iteration_failed <- TRUE
        break
      }
      
      models[[k]] <- model_fit
      preds[, k] <- predict(models[[k]]$gam, newdata = df)
    }
    
    # If iteration failed due to singular matrix, return NULL for this g value
    if (iteration_failed) {
      return(NULL)
    }
    
    # Calculate residuals based on outcome type
    if (is_binary) {
      # Bernoulli: Calculate Pearson residuals with binomial variance
      # Transform predictions from logit to probability scale
      fitted_probs <- plogis(preds)  # inverse logit: exp(p)/(1+exp(p))
      
      # Variance for Bernoulli: p*(1-p)
      variance <- fitted_probs * (1 - fitted_probs)
      
      # Avoid division by zero for probabilities at boundaries
      variance[variance < MIN_VARIANCE_THRESHOLD] <- MIN_VARIANCE_THRESHOLD
      
      # Pearson residual: (observed - expected)^2 / variance
      resids <- ((df[[full_outcome]] - fitted_probs)^2) / variance
    } else {
      # Gaussian: Use squared residuals (existing approach)
      resids <- (df[[full_outcome]] - preds)^2
    }
    resid_sums <- lapply(1:g, function(k) {
      agg <- stats::aggregate(resids[, k], by = list(id = df$id), FUN = sum, na.rm = TRUE)
      colnames(agg) <- c("id", paste0("RSS_group", k))
      return(agg)
    })
    resid_df <- Reduce(function(x, y) merge(x, y, by = "id"), resid_sums)
    group_map <- data.frame(id = resid_df$id, smm_group = as.character(apply(resid_df[, -1], 1, which.min)))
    df <- df %>% dplyr::select(-any_of("smm_group")) %>% dplyr::left_join(group_map, by = "id")
    logliks <- sapply(models, function(m) logLik(m$mer))
    LLK[i] <- sum(logliks)
    n_params <- sum(sapply(models, function(m) sum(summary(m$gam)$edf) + 1))
    BIC[i] <- -2 * LLK[i] + log(nrow(na.omit(df[, c("id", full_outcome, time_var)]))) * n_params
    if (any(table(df$smm_group) <= 20)) {
      warning("At least one group has ≤20 observations. Stopping early."); break
    }
  }
  
  ## Calculate posterior probabilities using log-likelihood and softmax
  rss_cols <- paste0("RSS_group", 1:g)
  rss_matrix <- as.matrix(resid_df[, rss_cols])
  
  ## Calculate person-specific log-likelihood based on outcome type
  n_obs_per_person <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")
  
  n_obs_aligned <- resid_df %>%
    dplyr::select(id) %>%
    dplyr::left_join(n_obs_per_person, by = "id")
  
  log_lik_matrix <- matrix(NA, nrow = nrow(rss_matrix), ncol = g)
  
  if (is_binary) {
    ## BINARY/BERNOULLI: Use binomial log-likelihood
    # For each person, calculate sum of log-likelihoods across observations
    for (k in 1:g) {
      person_logliks <- numeric(nrow(resid_df))
      
      for (person_idx in 1:nrow(resid_df)) {
        person_id <- resid_df$id[person_idx]
        person_data <- df %>% dplyr::filter(id == person_id)
        
        # Get predicted probabilities from group k model on logit scale
        person_preds_logit <- predict(models[[k]]$gam, newdata = person_data)
        person_fitted_probs <- plogis(person_preds_logit)
        
        # Binomial log-likelihood: Σ[y*log(p) + (1-y)*log(1-p)]
        # Handle edge cases where p = 0 or p = 1 to avoid log(0)
        person_fitted_probs <- pmax(pmin(person_fitted_probs, 1 - 1e-10), 1e-10)
        
        y <- person_data[[full_outcome]]
        log_lik_contributions <- y * log(person_fitted_probs) + 
                                 (1 - y) * log(1 - person_fitted_probs)
        
        person_logliks[person_idx] <- sum(log_lik_contributions, na.rm = TRUE)
      }
      
      log_lik_matrix[, k] <- person_logliks
    }
    
  } else {
    ## CONTINUOUS/GAUSSIAN: Use existing RSS-based approach
    group_variances <- numeric(g)
    for (k in 1:g) {
      group_k_rows <- which(df$smm_group == as.character(k))
      if (length(group_k_rows) > 0) {
        group_k_resids <- resids[group_k_rows, k]
        group_variances[k] <- mean(group_k_resids, na.rm = TRUE)
      } else {
        group_variances[k] <- 1e-10
      }
    }
    
    min_variance <- 1e-10
    for (k in 1:g) {
      sigma2_k <- max(group_variances[k], min_variance)
      log_lik_matrix[, k] <- -0.5 * n_obs_aligned$n_obs * log(sigma2_k) -
        rss_matrix[, k] / (2 * sigma2_k)
    }
  }
  
  ## Apply softmax transformation (log-sum-exp trick for stability)
  log_lik_max <- apply(log_lik_matrix, 1, max)
  log_lik_centered <- log_lik_matrix - log_lik_max
  exp_log_lik <- exp(log_lik_centered)
  prob_matrix <- exp_log_lik / rowSums(exp_log_lik)
  colnames(prob_matrix) <- paste0("prob_group", 1:g)
  
  ## Create final_class with hard assignments (argmax) and probabilities
  final_class <- data.frame(
    id = resid_df$id,
    smm_group = as.character(apply(prob_matrix, 1, which.max))
  )
  final_class <- cbind(final_class, prob_matrix)
  grp_counts <- df %>% dplyr::group_by(smm_group) %>% dplyr::summarise(n = dplyr::n(), N = dplyr::n_distinct(id), .groups = "drop")
  df <- df %>% dplyr::left_join(grp_counts, by = "smm_group") %>% dplyr::mutate(group_lab = factor(paste0(smm_group, " (N=", N, ", n=", n, ")"), levels = paste0(sort(unique(smm_group)), " (N=", grp_counts$N[match(sort(unique(smm_group)), grp_counts$smm_group)], ", n=", grp_counts$n[match(sort(unique(smm_group)), grp_counts$smm_group)], ")")))

  p_roll <- p_centered <- p_mean <- NULL
  if (plot) {
    df <- df %>% dplyr::mutate(cycleday_perc = (.data[[time_var]] + 1) / 2, cycleday_5perc = round(cycleday_perc / 0.05) * 0.05)
    x_breaks <- seq(0, 1, by = 0.05)
    x_labels <- if (centering == "menses") {
      c("Ovulation", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
    } else {
      c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "Menses Onset")
    }
    
    # Set y-axis labels and limits based on outcome type
    if (is_binary) {
      y_label_roll <- "Mean Probability"  # Binary: no rolling window, just mean probability
      y_label_centered <- "Mean Probability"
      y_label_mean <- "Mean Probability"
      y_limits <- c(0, 1.3)  # Match reference Bernoulli implementation
    } else {
      y_label_roll <- "Rolling Mean Centered"
      y_label_centered <- "Mean Centered"
      y_label_mean <- "Mean Outcome"
      y_limits <- NULL  # Auto-scale for continuous outcomes
    }
    
    # For binary outcomes, use raw outcome for all plots
    # For continuous outcomes, use centered/rolling columns as before
    if (is_binary) {
      summary_roll <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_roll = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
      summary_roll <- dplyr::bind_rows(summary_roll, summary_roll %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
      summary_centered <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_centered = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
      summary_centered <- dplyr::bind_rows(summary_centered, summary_centered %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
      summary_mean <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_outcome = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
      summary_mean <- dplyr::bind_rows(summary_mean, summary_mean %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
    } else {
      summary_roll <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_roll = mean(.data[[rolling_col]], na.rm = TRUE), se = sd(.data[[rolling_col]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[rolling_col]]))), .groups = "drop")
      summary_roll <- dplyr::bind_rows(summary_roll, summary_roll %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
      summary_centered <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_centered = mean(.data[[centered_col]], na.rm = TRUE), se = sd(.data[[centered_col]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[centered_col]]))), .groups = "drop")
      summary_centered <- dplyr::bind_rows(summary_centered, summary_centered %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
      summary_mean <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_outcome = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
      summary_mean <- dplyr::bind_rows(summary_mean, summary_mean %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
    }

    p_roll <- ggplot(summary_roll, aes(x = cycleday_5perc, y = mean_roll, color = group_lab, fill = group_lab)) +
      geom_line(linewidth = 0.9) +
      geom_ribbon(aes(ymin = mean_roll - se, ymax = mean_roll + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      {if (!is.null(y_limits)) scale_y_continuous(limits = y_limits)} +
      labs(title = paste(plot_label, "Rolling Avg (G =", g, ")"), x = "Percentage of Phase Elapsed", y = y_label_roll, color = "Group") +
      theme_minimal(base_size = 14)

    p_centered <- ggplot(summary_centered, aes(x = cycleday_5perc, y = mean_centered, color = group_lab, fill = group_lab)) +
      geom_line(linewidth = 0.9) +
      geom_ribbon(aes(ymin = mean_centered - se, ymax = mean_centered + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      {if (!is.null(y_limits)) scale_y_continuous(limits = y_limits)} +
      labs(
        title = if (is_binary) {
          paste(plot_label, ": Group Trajectories (G =", g, ")")
        } else {
          paste(plot_label, ": Person-Centered (G =", g, ")")
        },
        x = "Percentage of Phase Elapsed",
        y = y_label_centered,
        color = "Group"
      ) +
      theme_minimal(base_size = 14)

    p_mean <- ggplot(summary_mean, aes(x = cycleday_5perc, y = mean_outcome, color = group_lab, fill = group_lab)) +
      geom_line(linewidth = 0.9) +
      geom_ribbon(aes(ymin = mean_outcome - se, ymax = mean_outcome + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      {if (!is.null(y_limits)) scale_y_continuous(limits = y_limits)} +
      labs(title = paste(plot_label, ": Mean (G =", g, ")"), x = "Percentage of Phase Elapsed", y = y_label_mean, color = "Group") +
      theme_minimal(base_size = 14)
  }

  return(list(class = final_class, LLK = LLK, BIC = BIC, summary = df %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::group_by(smm_group) %>% dplyr::summarise(n = dplyr::n(), mean_outcome = mean(.data[[outcome]], na.rm = TRUE), sd_outcome = sd(.data[[outcome]], na.rm = TRUE), .groups = "drop"), plots = list(plot_roll = p_roll, plot_centered = p_centered, plot_mean = p_mean)))
}