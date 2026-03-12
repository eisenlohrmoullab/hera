#' Fit Smoothing Mixture Models (SMMs) for Binary Outcomes
#'
#' Implements expectation-maximization (EM) algorithm to identify latent groups
#' with distinct trajectory patterns for binary (0/1) outcomes. This is a
#' simplified version of run_smm_cyclic.R that ONLY handles Bernoulli distributions.
#'
#' The algorithm:
#' 1. Initializes group assignments using k-means clustering of individual GAM trajectories
#' 2. Iteratively refits group-specific GAMMs with binomial family (logit link)
#' 3. Reassigns participants based on Pearson residual sum of squares
#' 4. Converges when group assignments stabilize or maximum iterations reached
#' 5. Computes posterior probabilities using binomial log-likelihood
#'    (Σ[y*log(p) + (1-y)*log(1-p)]) and softmax transformation
#' 6. Generates visualization plots showing group-specific probability trajectories
#'
#' @param data A dataframe containing:
#'   - id: Participant identifier
#'   - Time variable (specified by 'time_var' parameter)
#'   - Outcome variable (specified by 'outcome' parameter) - MUST be binary (0/1)
#'   - day_of_week: Factor variable for day of week (if using covariates)
#'   - session_today: Binary 0/1 indicator for lab session presence (if using covariates)
#' @param outcome Character string naming the outcome variable (e.g., "drink_today_bin")
#'   IMPORTANT: This MUST be a binary variable with values 0, 1, or NA only.
#' @param time_var Character string naming the cyclic time variable (typically ranging -1 to +1,
#'   where 0 = cycle event like menses onset or ovulation)
#' @param plot_label Character string for plot titles (default: NULL, uses technical outcome name)
#' @param g Integer or integer vector specifying number of latent groups to fit.
#'   If vector (e.g., 2:5), fits multiple models and returns BIC table for comparison.
#' @param d_inter Integer specifying maximum EM iterations (default: 20)
#' @param plot Logical: should trajectory plots be generated? (default: TRUE)
#' @param seed Integer for random seed to ensure reproducible k-means initialization (default: 123)
#' @param centering Character string: "menses" or "ovulation" for labeling plots (default: "menses")
#' @param save_dir Character string specifying base directory for saving outputs. If NULL,
#'   outputs are not saved. Files organized as: save_dir/YYYYMMDD/{centering}_centered/outcome/
#' @param k_smooth Integer specifying number of basis functions for cyclic spline (default: 10)
#' @param covariates Character vector of covariate names to include as fixed effects
#'   (default: c("day_of_week", "session_today")). Set to NULL to exclude covariates.
#'
#' @return If single g value:
#'   A list with elements:
#'   - class: Dataframe with id, smm_group (hard assignment), and prob_group* columns (posterior probabilities)
#'   - LLK: Vector of log-likelihoods for each EM iteration
#'   - BIC: Vector of BIC values for each EM iteration
#'   - summary: Group-level summary statistics
#'   - plots: List containing ggplot2 objects (plot_roll, plot_centered, plot_mean)
#'
#' If multiple g values:
#'   A list with elements:
#'   - all_results: Named list of results for each g value
#'   - bic_table: Dataframe comparing BIC across group numbers
#'
#' @examples
#' # Fit 3-group solution for binary drinking outcome
#' result <- run_smm_bernoulli(
#'   data = ukalc_data,
#'   outcome = "drink_today_bin",
#'   time_var = "cyclic_time_impute",
#'   plot_label = "Any Drinking Today",
#'   g = 3,
#'   centering = "menses",
#'   save_dir = "output/SMMs_Binary"
#' )
#'
#' # Compare multiple group solutions
#' multi_result <- run_smm_bernoulli(
#'   data = ukalc_data,
#'   outcome = "fourplustoday_bin",
#'   time_var = "cyclic_time_impute",
#'   plot_label = "Binge Drinking (4+)",
#'   g = 2:4,
#'   save_dir = "output/SMMs_Binary"
#' )
#'
#' @seealso 
#' \code{\link{run_smm_cyclic}} for continuous outcomes and mixed outcome types
#' \code{\link{label_smm_groups}} for assigning meaningful labels to groups
#' \code{\link[gamm4]{gamm4}} for the underlying GAMM fitting
#'
#' @note Requires packages: glue, dplyr, mgcv, gamm4, ggplot2, lme4
#'
#' @export
run_smm_bernoulli <- function(
    data,
    outcome,
    time_var,
    plot_label = NULL,
    g = 2,
    d_inter = 20,
    plot = TRUE,
    seed = 123,
    centering = "menses",
    save_dir = NULL,
    k_smooth = 10,
    covariates = c("day_of_week", "session_today")
) {
  
  # Fallback to use the technical name if a pretty label isn't provided
  if (is.null(plot_label)) plot_label <- outcome
  set.seed(seed)
  `%>%` <- dplyr::`%>%`
  
  # Define error pattern for singular matrix errors (used in multiple tryCatch blocks)
  SINGULAR_MATRIX_PATTERN <- "singular matrix|backsolve|Downdated VtV is not positive definite"
  
  ## --------- create nested folder structure ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    sub_dir <- file.path(save_dir, date_folder, centering_folder, outcome)
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  ## VALIDATE: Outcome must be binary (0, 1, or NA only)
  non_na_values <- data[[outcome]][!is.na(data[[outcome]])]
  if (length(non_na_values) == 0) {
    stop(glue::glue("Outcome '{outcome}' contains only NA values. Cannot proceed with analysis."))
  }
  
  if (!all(non_na_values %in% c(0, 1))) {
    stop(glue::glue("Outcome '{outcome}' must be binary (0/1 only). Use run_smm_cyclic() for continuous outcomes."))
  }
  
  message(glue::glue("✓ Binary outcome validated: '{outcome}' with Bernoulli distribution"))
  
  ## ========= MULTI-g WRAPPER ========= ##
  if (length(g) > 1) {
    message(glue::glue(">> Running multiple group sizes: {paste(g, collapse=', ')}"))
    all_results <- list()
    bic_table <- data.frame(groups = integer(), best_BIC = numeric())
    for (gval in g) {
      message(glue::glue(">>> Fitting SMM for g = {gval} <<<"))
      
      # Wrap SMM fitting in tryCatch to handle singular matrix errors gracefully
      # These patterns capture known gamm4/mgcv singularity / numerical instability errors
      # that arise when the smooths are not identifiable for a given g (group count).
      res <- tryCatch({
        run_smm_bernoulli(
          data = data, outcome = outcome, time_var = time_var, plot_label = plot_label, g = gval,
          d_inter = d_inter, plot = plot, seed = seed, centering = centering,
          save_dir = save_dir, k_smooth = k_smooth, covariates = covariates
        )
      }, error = function(e) {
        if (grepl(SINGULAR_MATRIX_PATTERN, e$message, ignore.case = TRUE)) {
          message(glue::glue("⚠️  Singular matrix error for g={gval}: {e$message}"))
          message(glue::glue("⚠️  Skipping g={gval} and continuing with remaining group sizes..."))
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
                    file = file.path(sub_dir, glue::glue("{outcome}_g{gval}_class.csv")),
                    row.names = FALSE)
          
          # Save plots if they exist
          plots_list <- res$plots
          if (!is.null(plots_list)) {
            # Save rolling average plot
            if (!is.null(plots_list$plot_roll)) {
              ggplot2::ggsave(file.path(sub_dir, glue::glue("{outcome}_g{gval}_roll.png")),
                     plots_list$plot_roll, width = 7, height = 5)
            }
            # Save person-centered plot
            if (!is.null(plots_list$plot_centered)) {
              ggplot2::ggsave(file.path(sub_dir, glue::glue("{outcome}_g{gval}_centered.png")),
                     plots_list$plot_centered, width = 7, height = 5)
            }
            # Save mean plot
            if (!is.null(plots_list$plot_mean)) {
              ggplot2::ggsave(file.path(sub_dir, glue::glue("{outcome}_g{gval}_mean.png")),
                     plots_list$plot_mean, width = 7, height = 5)
            }
          }
        }
      }
    }
    if (!is.null(sub_dir)) {
      write.csv(bic_table, file = file.path(sub_dir, glue::glue("{outcome}_bic_table.csv")), row.names = FALSE)
    }
    return(list(all_results = all_results, bic_table = bic_table))
  }
  
  ## ========= SINGLE-g CASE ========= ##
  message(glue::glue(">>> Fitting SMM for g = {g} <<<"))
  if (!time_var %in% names(data)) stop(glue::glue("Time variable '{time_var}' not found in data!"))
  
  # Minimum variance threshold for Pearson residual calculation
  # Prevents division by zero when fitted probabilities are at boundaries (0 or 1)
  MIN_VARIANCE_THRESHOLD <- 1e-10
  
  # Binary outcomes: only need the raw outcome column
  full_outcome <- outcome
  family_spec <- binomial(link = "logit")
  
  keep_cols <- intersect(c("id", time_var, outcome, covariates), names(data))
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
  
  # Initialize group assignments using k-means clustering of individual GAM trajectories
  for (i in seq_along(id_vec)) {
    person_data <- df %>% dplyr::filter(id == id_vec[i])
    if (nrow(person_data) >= 10) {
      # NOTE: For k-means initialization we fit individual-level GAMs using only
      # the smooth time effect. Covariates are excluded here so that prediction
      # on the grid (which only contains the time variable) is valid. Covariates
      # are incorporated later in the group-level GAMM fits.
      fit <- tryCatch(
        mgcv::gam(
          as.formula(glue::glue("{full_outcome} ~ s({time_var}, k = {k_smooth})")),
          data = person_data,
          family = binomial(link = "logit")
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        preds_for_kmeans <- predict(fit, newdata = grid)
        # For binary outcomes, convert to probability scale for clustering
        preds_for_kmeans <- plogis(preds_for_kmeans)
        trajectory_matrix[i, ] <- preds_for_kmeans
      }
    }
  }
  
  valid_rows <- which(rowSums(is.na(trajectory_matrix)) < ncol(trajectory_matrix))
  trajectory_matrix <- trajectory_matrix[valid_rows, ]
  id_vec <- id_vec[valid_rows]
  if (length(id_vec) < g) {
    warning(glue::glue("Not enough participants with valid trajectories for g = {g}. Needed {g}, got {length(id_vec)}. Returning NULL."))
    return(NULL)
  }
  
  km_res <- kmeans(scale(trajectory_matrix), centers = g)
  cluster_map <- data.frame(id = id_vec, smm_group = as.character(km_res$cluster))
  df <- dplyr::left_join(df, cluster_map, by = "id")
  LLK <- numeric(d_inter)
  BIC <- numeric(d_inter)
  
  # EM algorithm: iterate to convergence
  for (i in 1:d_inter) {
    models <- list()
    preds <- matrix(NA, nrow = nrow(df), ncol = g)
    
    # Flag to track if iteration failed
    iteration_failed <- FALSE
    
    for (k in 1:g) {
      dat_k <- df %>% dplyr::filter(smm_group == as.character(k))
      if (dplyr::n_distinct(dat_k$id) < 2) {
        warning(glue::glue("Group {k} has <2 participants. Stopping.")); return(NULL)
      }
      
      # Binary outcomes need simpler random effects structure (intercept-only)
      random_formula <- as.formula(glue::glue("~ (1 | id)"))
      if (i == 1 && k == 1) {
        message("  Using intercept-only random effects (1 | id) for binary outcome")
      }
      
      # Use robust optimizer for binary outcomes
      control_spec <- lme4::glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 1e5)
      )
      
      # Wrap gamm4 call in tryCatch to handle singular matrix errors
      # These patterns capture known gamm4/mgcv singularity / numerical instability errors
      # that arise when the smooths are not identifiable for a given g (group count).
      model_fit <- tryCatch({
        gamm4::gamm4(
          as.formula(glue::glue("{full_outcome} ~ {covariate_formula}s({time_var}, bs = 'cc', k = {k_smooth})")), 
          random = random_formula,
          data = dat_k, 
          na.action = na.omit, 
          knots = knots_list,
          family = family_spec,
          control = control_spec
        )
      }, error = function(e) {
        # Check for convergence-related errors specific to GAMM/binomial models
        if (grepl(SINGULAR_MATRIX_PATTERN, e$message, ignore.case = TRUE)) {
          message(glue::glue("⚠️  Singular matrix error in EM iteration {i}, group {k} for g={g}: {e$message}"))
          message(glue::glue("⚠️  Unable to fit model for g={g}. Returning NULL for this group size."))
          return(NULL)
        } else if (grepl("step-halving|PIRLS|convergence", e$message, ignore.case = TRUE)) {
          message(glue::glue("⚠️  Convergence issue in EM iteration {i}, group {k} for g={g}"))
          message(glue::glue("     Error: {e$message}"))
          message(glue::glue("⚠️  Unable to fit model for g={g}. Returning NULL for this group size."))
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
    
    # Calculate Pearson residuals with binomial variance
    # Transform predictions from logit to probability scale
    fitted_probs <- plogis(preds)  # inverse logit: exp(p)/(1+exp(p))
    
    # Variance for Bernoulli: p*(1-p)
    variance <- fitted_probs * (1 - fitted_probs)
    
    # Avoid division by zero for probabilities at boundaries
    variance[variance < MIN_VARIANCE_THRESHOLD] <- MIN_VARIANCE_THRESHOLD
    
    # Pearson residual: (observed - expected)^2 / variance
    resids <- ((df[[full_outcome]] - fitted_probs)^2) / variance
    
    resid_sums <- lapply(1:g, function(k) {
      agg <- stats::aggregate(resids[, k], by = list(id = df$id), FUN = sum, na.rm = TRUE)
      colnames(agg) <- c("id", paste0("RSS_group", k))
      return(agg)
    })
    resid_df <- Reduce(function(x, y) merge(x, y, by = "id"), resid_sums)
    group_map <- data.frame(id = resid_df$id, smm_group = as.character(apply(resid_df[, -1], 1, which.min)))
    df <- df %>% dplyr::select(-dplyr::any_of("smm_group")) %>% dplyr::left_join(group_map, by = "id")
    logliks <- sapply(models, function(m) logLik(m$mer))
    LLK[i] <- sum(logliks)
    n_params <- sum(sapply(models, function(m) sum(summary(m$gam)$edf) + 1))
    BIC[i] <- -2 * LLK[i] + log(nrow(na.omit(df[, c("id", full_outcome, time_var)]))) * n_params
    if (any(table(df$smm_group) <= 20)) {
      warning("At least one group has ≤20 observations. Stopping early."); break
    }
  }
  
  ## Calculate posterior probabilities using binomial log-likelihood and softmax
  rss_cols <- paste0("RSS_group", 1:g)
  rss_matrix <- as.matrix(resid_df[, rss_cols])
  
  ## Calculate person-specific log-likelihood
  n_obs_per_person <- df %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")
  
  n_obs_aligned <- resid_df %>%
    dplyr::select(id) %>%
    dplyr::left_join(n_obs_per_person, by = "id")
  
  log_lik_matrix <- matrix(NA, nrow = nrow(rss_matrix), ncol = g)
  
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
    
    # Set y-axis labels and limits for binary outcomes
    y_label_roll <- "Mean Probability"
    y_label_centered <- "Mean Probability"
    y_label_mean <- "Mean Probability"
    y_limits <- c(0, 1.3)
    
    # For binary outcomes, use raw outcome for all plots
    summary_roll <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_roll = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
    summary_roll <- dplyr::bind_rows(summary_roll, summary_roll %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
    summary_centered <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_centered = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
    summary_centered <- dplyr::bind_rows(summary_centered, summary_centered %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
    summary_mean <- df %>% dplyr::group_by(group_lab, cycleday_5perc) %>% dplyr::summarise(mean_outcome = mean(.data[[outcome]], na.rm = TRUE), se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))), .groups = "drop")
    summary_mean <- dplyr::bind_rows(summary_mean, summary_mean %>% dplyr::filter(cycleday_5perc == 1) %>% dplyr::mutate(cycleday_5perc = 0))
    
    p_roll <- ggplot2::ggplot(summary_roll, ggplot2::aes(x = cycleday_5perc, y = mean_roll, color = group_lab, fill = group_lab)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_roll - se, ymax = mean_roll + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = y_limits) +
      ggplot2::labs(title = paste(plot_label, "Rolling Avg (G =", g, ")"), x = "Percentage of Phase Elapsed", y = y_label_roll, color = "Group") +
      ggplot2::theme_minimal(base_size = 14)
    
    p_centered <- ggplot2::ggplot(summary_centered, ggplot2::aes(x = cycleday_5perc, y = mean_centered, color = group_lab, fill = group_lab)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_centered - se, ymax = mean_centered + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = y_limits) +
      ggplot2::labs(
        title = paste(plot_label, ": Group Trajectories (G =", g, ")"),
        x = "Percentage of Phase Elapsed",
        y = y_label_centered,
        color = "Group"
      ) +
      ggplot2::theme_minimal(base_size = 14)
    
    p_mean <- ggplot2::ggplot(summary_mean, ggplot2::aes(x = cycleday_5perc, y = mean_outcome, color = group_lab, fill = group_lab)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_outcome - se, ymax = mean_outcome + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      ggplot2::scale_y_continuous(limits = y_limits) +
      ggplot2::labs(title = paste(plot_label, ": Mean (G =", g, ")"), x = "Percentage of Phase Elapsed", y = y_label_mean, color = "Group") +
      ggplot2::theme_minimal(base_size = 14)
  }
  
  return(list(class = final_class, LLK = LLK, BIC = BIC, summary = df %>% dplyr::distinct(id, .keep_all = TRUE) %>% dplyr::group_by(smm_group) %>% dplyr::summarise(n = dplyr::n(), mean_outcome = mean(.data[[outcome]], na.rm = TRUE), sd_outcome = sd(.data[[outcome]], na.rm = TRUE), .groups = "drop"), plots = list(plot_roll = p_roll, plot_centered = p_centered, plot_mean = p_mean)))
}
