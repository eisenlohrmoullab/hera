#' Verify Hormone Data Exclusion for Flagged Participants
#'
#' Validates that hormone data (E2, P4, LH) and all derived hormone variables
#' are properly set to NA for participants flagged with Excl_Horm = TRUE. This
#' verification function should be run after completing UKALC_01_data_prep.Rmd
#' to ensure data quality control measures are correctly implemented.
#'
#' The function checks all hormone-related variables including:
#' - Raw assay values (e2, p4, lh)
#' - Winsorized values (*_w)
#' - Rolling averages (*.3roll, *.5roll)
#' - Person-centered values (*.d, *.zd)
#' - Sample-standardized values (*.szd)
#'
#' Participants with Excl_Horm = TRUE are flagged due to data quality issues
#' (e.g., irregular cycle patterns, assay problems, insufficient temporal coverage)
#' and should not contribute hormone data to analyses, though their survey data
#' may still be valid.
#'
#' @param df A dataframe containing:
#'   - Excl_Horm: Logical flag for hormone exclusion
#'   - id: Participant identifier
#'   - Hormone variables (e2, p4, lh and derived versions)
#'
#' @return Logical value:
#'   - TRUE if all hormone variables are properly NA for excluded participants
#'   - FALSE if any hormone variables have non-missing data for excluded participants
#'   
#'   Side effect: Prints detailed verification report to console including:
#'   - List of excluded participant IDs
#'   - Count of checked variables
#'   - Specific failures (variables with data leakage)
#'
#' @details
#' Flagged participants (IDs 152, 181, 185 in current data) should have:
#' - Excl_Horm = TRUE (set during cycle data merge)
#' - All hormone variables = NA (enforced after hormone merge)
#' - Survey data intact (cravings, mood, etc.)
#'
#' If verification fails, it indicates that hormone exclusion logic needs to be
#' reapplied or that new derived variables were created without applying exclusions.
#'
#' @examples
#' # Load processed dataset
#' df <- readRDS("output/20251201/ukalc_20251201.rds")
#' 
#' # Run verification
#' is_valid <- verify_hormone_exclusion(df)
#' 
#' # Check result
#' if (is_valid) {
#'   message("Hormone exclusions verified - safe to proceed with analyses")
#' } else {
#'   stop("Hormone exclusions failed - review UKALC_01_data_prep.Rmd")
#' }
#'
#' @seealso 
#' The "Hormone Exclusion Fix" section in UKALC_01_data_prep.Rmd where
#' exclusions are applied
#'
#' @note This function is for quality control only and does not modify data
#'
#' @export
verify_hormone_exclusion <- function(df) {
  cat("\n========================================\n")
  cat("HORMONE EXCLUSION VERIFICATION\n")
  cat("========================================\n\n")
  
  # Check if Excl_Horm column exists
  if (!"Excl_Horm" %in% names(df)) {
    stop("ERROR: Excl_Horm column not found in dataframe!")
  }
  
  # Get IDs with Excl_Horm = TRUE
  excluded_ids <- df %>%
    filter(Excl_Horm == TRUE) %>%
    pull(id) %>%
    unique()
  
  cat("Participants with Excl_Horm = TRUE:", paste(excluded_ids, collapse = ", "), "\n")
  cat("Number of excluded participants:", length(excluded_ids), "\n\n")
  
  if (length(excluded_ids) == 0) {
    cat("✓ No participants have Excl_Horm = TRUE\n")
    return(TRUE)
  }
  
  # Filter to excluded participants
  excluded_df <- df %>% filter(Excl_Horm == TRUE)
  
  # List of all hormone variables to check
  hormone_vars <- c(
    # Raw hormones
    "e2", "p4", "lh",
    # Winsorized
    "e2_w", "p4_w", "lh_w",
    # Rolling averages
    "e2.3roll", "e2.5roll", "p4.3roll", "p4.5roll", "lh.3roll", "lh.5roll",
    "e2_w.3roll", "e2_w.5roll", "p4_w.3roll", "p4_w.5roll", "lh_w.3roll", "lh_w.5roll",
    # Person-centered
    "e2.d", "e2.zd", "p4.d", "p4.zd", "lh.d", "lh.zd",
    "e2_w.d", "e2_w.zd", "p4_w.d", "p4_w.zd", "lh_w.d", "lh_w.zd",
    # Sample-standardized
    "e2.szd", "p4.szd", "lh.szd",
    "e2_w.szd", "p4_w.szd", "lh_w.szd",
    # Special rolling standardized
    "e2.3roll.szd", "p4.3roll.szd", "lh.3roll.szd"
  )
  
  # Check which variables exist in the dataframe
  existing_vars <- hormone_vars[hormone_vars %in% names(df)]
  missing_vars <- hormone_vars[!hormone_vars %in% names(df)]
  
  if (length(missing_vars) > 0) {
    cat("Note: Some hormone variables not found (may not be created yet):\n")
    cat("  ", paste(head(missing_vars, 10), collapse = ", "), "\n")
    if (length(missing_vars) > 10) {
      cat("  ... and", length(missing_vars) - 10, "more\n")
    }
    cat("\n")
  }
  
  # Check each existing variable
  cat("Checking", length(existing_vars), "hormone variables...\n\n")
  
  failures <- list()
  
  for (var in existing_vars) {
    n_nonmissing <- sum(!is.na(excluded_df[[var]]))
    
    if (n_nonmissing > 0) {
      failures[[var]] <- n_nonmissing
      cat("✗ FAIL:", var, "has", n_nonmissing, "non-missing values\n")
    }
  }
  
  # Summary
  cat("\n========================================\n")
  if (length(failures) == 0) {
    cat("✓ SUCCESS: All hormone variables properly excluded\n")
    cat("  Checked", length(existing_vars), "variables\n")
    cat("  0 variables have data for Excl_Horm = TRUE participants\n")
    cat("========================================\n\n")
    return(TRUE)
  } else {
    cat("✗ FAILURE: Some hormone variables not properly excluded\n")
    cat("  Failed variables:", length(failures), "out of", length(existing_vars), "\n")
    cat("  Variables with leakage:\n")
    for (var in names(failures)) {
      cat("    -", var, ":", failures[[var]], "non-missing values\n")
    }
    cat("========================================\n\n")
    return(FALSE)
  }
}

# Example usage (uncomment after running UKALC_01_data_prep.Rmd):
# df <- readRDS("path/to/ukalc_YYYYMMDD.rds")
# verify_hormone_exclusion(df)
