#' Preprocess Outcome Variable for Modeling
#'
#' Applies a standardized preprocessing pipeline to an outcome variable,
#' including log transformation, person-mean centering, and 5-day rolling
#' average computation. This function is designed for count or skewed
#' continuous variables in daily diary data (e.g., number of drinks, craving
#' scores) to prepare them for mixed-effects modeling.
#'
#' The preprocessing steps are:
#' 1. Log transformation: log(x + 1) to reduce skewness
#' 2. Person-mean centering: Subtract each person's mean, isolating within-person variation
#' 3. 5-day rolling average: Smooth day-to-day fluctuations using centered window
#'
#' @param data A dataframe containing:
#'   - id: Participant identifier (converted to factor)
#'   - A column matching the 'outcome' parameter name
#'   - Data should be sorted by id and date for meaningful rolling averages
#' @param outcome Character string naming the outcome variable to preprocess
#'   (must be a column in 'data')
#'
#' @return The input dataframe with three new columns added:
#'   - {outcome}_log: log(outcome + 1) transformation
#'   - {outcome}_log.d: Person-mean centered log values (within-person deviation)
#'   - {outcome}_log.d.roll: 5-day centered rolling mean of person-centered values
#'
#' @details
#' The log(x+1) transformation handles zeros gracefully while reducing positive skew
#' common in count data. Person-mean centering removes between-person differences,
#' focusing statistical models on within-person changes over time.
#'
#' The 5-day rolling average uses a centered window (2 days before, current day,
#' 2 days after) with partial windows at edges (.partial = TRUE) and NA removal
#' (.na_rm = TRUE). This smoothing can reduce measurement error and day-to-day
#' noise while preserving longer-term patterns relevant to menstrual cycle effects.
#'
#' @examples
#' # Preprocess alcohol consumption for modeling
#' data <- preprocess_outcome(ukalc_data, "num_drinks")
#' 
#' # Now data contains: num_drinks_log, num_drinks_log.d, num_drinks_log.d.roll
#' 
#' # Preprocess craving score
#' data <- preprocess_outcome(data, "crave_alc_pm")
#'
#' @seealso 
#' \code{\link[slider]{slide_dbl}} for rolling window calculations
#' \code{\link[dplyr]{group_by}} for person-level operations
#'
#' @note Requires packages: dplyr, slider
#'
#' @export
preprocess_outcome <- function(data, outcome) {
  outcome_log <- paste0(outcome, "_log")
  outcome_log_d <- paste0(outcome, "_log.d")
  outcome_roll <- paste0(outcome, "_log.d.roll")
  
  data %>%
    mutate(id = as.factor(id)) %>%
    mutate(!!outcome_log := log(.data[[outcome]] + 1)) %>%
    group_by(id) %>%
    mutate(!!outcome_log_d := .data[[outcome_log]] - mean(.data[[outcome_log]], na.rm = TRUE)) %>%
    # Add the 5-day centered rolling average using slider with a partial mean.
    # The .before and .after arguments specify the window size (5 days total).
    mutate(!!outcome_roll := slider::slide_dbl(
      .x = .data[[outcome_log_d]],
      .f = mean,
      .before = 2,
      .after = 2,
      .partial = TRUE,
      .na_rm = TRUE
    )) %>%
    ungroup()
}