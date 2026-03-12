#' Subset Data by Menstrual Cycle Phase with Minimum Observation Requirements
#'
#' Filters daily diary data to include only participants with sufficient
#' observations in both luteal and follicular cycle phases. This ensures
#' within-person phase comparisons are adequately powered and prevents
#' unreliable estimates from participants with sparse or phase-imbalanced data.
#'
#' The function categorizes observations into cycle phases based on a cyclic
#' time variable (typically ranging from -1 to +1, where 0 = menses or ovulation):
#' - Luteal phase: time ∈ [-1, 0)
#' - Follicular phase: time ∈ [0, 1]
#'
#' Participants must have at least 'min_obs' non-missing outcome observations
#' in BOTH phases to be retained.
#'
#' @param data A dataframe containing:
#'   - id: Participant identifier
#'   - Time variable (specified by 'time_var' parameter)
#'   - Outcome variable (specified by 'outcome' parameter)
#' @param outcome Character string naming the outcome variable to check for
#'   non-missing observations (e.g., "dep", "num_drinks")
#' @param time_var Character string naming the cyclic time variable used to
#'   define phases (default: "cyclic_time_impute"). Should range from -1 to +1.
#' @param min_obs Integer specifying minimum required observations per phase
#'   (default: 5). Participants with fewer than this many non-missing outcome
#'   values in either phase are excluded.
#'
#' @return A list with two elements:
#'   - data_subset: Dataframe containing only retained participants, with new
#'     'phase' column ("luteal" or "follicular")
#'   - excluded_ids: Vector of participant IDs that were excluded due to
#'     insufficient observations in one or both phases
#'
#' @details
#' Phase definitions:
#' - For menses-centered time: luteal = pre-menses, follicular = post-menses
#' - For ovulation-centered time: luteal = post-ovulation, follicular = pre-ovulation
#'
#' The function prints a message reporting the number of excluded IDs. If all
#' participants meet criteria, a success message is printed.
#'
#' Common use case: Ensuring adequate within-person statistical power before
#' fitting phase-comparison models or computing phase-specific descriptive statistics.
#'
#' @examples
#' # Require at least 5 observations per phase
#' result <- subset_by_phase_cyclic(ukalc_data, outcome = "dep", 
#'                                  time_var = "cyclic_time_impute", 
#'                                  min_obs = 5)
#'
#' # Use filtered dataset for phase comparisons
#' phase_comparison_data <- result$data_subset
#'
#' # Report which IDs were excluded
#' cat("Excluded IDs:", paste(result$excluded_ids, collapse = ", "))
#'
#' # Stricter criterion: require 10 observations per phase
#' result_strict <- subset_by_phase_cyclic(ukalc_data, outcome = "crave_alc_pm",
#'                                         min_obs = 10)
#'
#' @seealso 
#' \code{\link{run_smm_cyclic}} which may call this function internally
#'
#' @note Requires packages: tidyverse, dplyr, glue
#'
#' @export
subset_by_phase_cyclic <- function(data, outcome, time_var = "cyclic_time_impute", min_obs = 5) {
  
  # 1️⃣ Add phase variable
  data <- data %>%
    mutate(
      phase = case_when(
        .data[[time_var]] >= -1 & .data[[time_var]] < 0 ~ "luteal",
        .data[[time_var]] >= 0 & .data[[time_var]] <= 1 ~ "follicular",
        TRUE ~ NA_character_
      )
    )
  
  # 2️⃣ Count non-missing observations by phase
  id_counts <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(phase)) %>%
    group_by(id, phase) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    pivot_wider(names_from = phase, values_from = n_obs, values_fill = list(n_obs = 0))
  
  # Ensure both phase columns exist
  if (!"luteal" %in% names(id_counts)) {
    id_counts$luteal <- 0
  }
  if (!"follicular" %in% names(id_counts)) {
    id_counts$follicular <- 0
  }
  
  # 3️⃣ Keep only IDs with minimum observations in both phases
  valid_ids <- id_counts %>%
    filter(luteal >= min_obs, follicular >= min_obs) %>%
    pull(id)
  
  all_ids <- unique(data$id)
  excluded_ids <- setdiff(all_ids, valid_ids)
  
  # 4️⃣ Subset data
  data_subset <- data %>%
    filter(id %in% valid_ids)
  
  # 5️⃣ Report excluded IDs
  if (length(excluded_ids) > 0) {
    message(glue("⚠️ Excluded {length(excluded_ids)} IDs (insufficient observations in one or both phases)."))
  } else {
    message("✅ All IDs meet the minimum criteria!")
  }
  
  # 6️⃣ Return both
  return(list(
    data_subset = data_subset,
    excluded_ids = excluded_ids
  ))
}