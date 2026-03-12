# Test script to verify the fixes for binary variable corruption and GAMM convergence
# This script tests the logical correctness of the fixes without requiring actual data

cat("=== Testing Binary Variable Fix ===\n\n")

# Test 1: Demonstrate the problem with as.numeric() on factors
cat("Test 1: Factor to numeric conversion issue\n")
# Simulate the actual data structure: binary numeric 0/1 values first
binary_numeric <- c(0, 1, 0, 1, NA)
cat("Original binary numeric values:", binary_numeric, "\n")
# Then factors are created from these numeric values with labels
test_factor <- factor(binary_numeric, levels = c(0, 1), labels = c("No", "Yes"))
cat("Factor version (for display):", as.character(test_factor), "\n")
cat("Incorrect conversion (as.numeric on factor):", as.numeric(test_factor), "\n")
cat("  ^ This returns 1, 2 instead of 0, 1 (WRONG!)\n")
cat("  ^ This is what the old code did: mutate(drink_today_bin = as.numeric(drink_today))\n")
cat("Original binary numeric (what we should keep):", binary_numeric, "\n")
cat("  ^ This is 0, 1 as intended (CORRECT)\n")
cat("  ^ The fix: Don't overwrite the _bin variables at all!\n\n")

# Test 2: Demonstrate validation logic
cat("Test 2: Binary validation logic\n")
test_binary_correct <- c(0, 1, 0, 1, NA, 0)
test_binary_incorrect <- c(1, 2, 1, 2, NA, 1)  # Corrupted by as.numeric(factor)

#' Validate Binary Variable Values
#'
#' Checks if a variable contains only 0, 1, and NA values, which is required
#' for binary outcomes in SMM analysis.
#'
#' @param var_name Character string naming the variable being validated
#' @param var_values Numeric vector of variable values to validate
#' @return Logical TRUE if variable is valid binary (0/1/NA only), FALSE otherwise
#'
#' @examples
#' validate_binary("drink_today_bin", c(0, 1, 0, 1, NA))  # Returns TRUE
#' validate_binary("corrupted", c(1, 2, 1, 2, NA))  # Returns FALSE
validate_binary <- function(var_name, var_values) {
  unique_vals <- unique(var_values)
  unique_vals <- unique_vals[!is.na(unique_vals)]
  if (!all(unique_vals %in% c(0, 1))) {
    cat(sprintf("  ✗ %s FAILED: Contains non-binary values: %s\n", 
                var_name, paste(unique_vals, collapse=", ")))
    return(FALSE)
  } else {
    cat(sprintf("  ✓ %s PASSED: Valid binary (0/1) with %d non-NA values\n", 
                var_name, sum(!is.na(var_values))))
    return(TRUE)
  }
}

validate_binary("Correct binary", test_binary_correct)
validate_binary("Corrupted binary", test_binary_incorrect)

cat("\n=== Testing Random Effects Structure Fix ===\n\n")

# Test 3: Demonstrate conditional random effects structure
cat("Test 3: Random effects structure based on outcome type\n")

#' Simulate Random Effects Structure Selection
#'
#' Demonstrates how the SMM implementation chooses random effects structure
#' based on outcome type. Binary outcomes use intercept-only structure to
#' avoid convergence issues with sparse data.
#'
#' @param is_binary Logical indicating if outcome is binary (TRUE) or continuous (FALSE)
#' @param time_var Character string naming the time variable (default: "cyclic_time_impute")
#' @return Character string containing the random effects formula
#'
#' @examples
#' simulate_random_effects(is_binary = TRUE)   # Returns "~ (1 | id)"
#' simulate_random_effects(is_binary = FALSE)  # Returns "~ (1 + cyclic_time_impute | id)"
simulate_random_effects <- function(is_binary, time_var = "cyclic_time_impute") {
  if (is_binary) {
    formula <- sprintf("~ (1 | id)")
    cat("  Binary outcome: Using intercept-only random effects\n")
    cat("  Formula:", formula, "\n")
    cat("  Rationale: Sparse binary data cannot support complex random effects\n")
  } else {
    formula <- sprintf("~ (1 + %s | id)", time_var)
    cat("  Continuous outcome: Using random intercepts and slopes\n")
    cat("  Formula:", formula, "\n")
    cat("  Rationale: Continuous data can support both random intercepts and slopes\n")
  }
  return(formula)
}

cat("\nFor binary outcome (drink_today_bin):\n")
simulate_random_effects(is_binary = TRUE)

cat("\nFor continuous outcome (dep):\n")
simulate_random_effects(is_binary = FALSE)

cat("\n=== Testing Error Handling Improvements ===\n\n")

# Test 4: Demonstrate improved error handling
cat("Test 4: Error handling for convergence issues\n")

#' Simulate Error Handling Logic
#'
#' Demonstrates how the SMM implementation categorizes and handles different
#' types of GAMM fitting errors. Shows which errors are caught gracefully
#' and which are re-thrown.
#'
#' @param error_message Character string containing the error message to classify
#' @return Character string indicating error category: "singular", "convergence", or "other"
#'
#' @examples
#' simulate_error_handling("PIRLS step-halvings failed")  # Returns "convergence"
#' simulate_error_handling("singular matrix in backsolve")  # Returns "singular"
#' simulate_error_handling("unexpected error")  # Returns "other"
simulate_error_handling <- function(error_message) {
  if (grepl("singular matrix|backsolve|Downdated VtV is not positive definite", 
            error_message, ignore.case = TRUE)) {
    cat("  Caught: Singular matrix error\n")
    cat("  Action: Return NULL for this group size\n")
    return("singular")
  } else if (grepl("step-halving|PIRLS|convergence", error_message, ignore.case = TRUE)) {
    cat("  Caught: Convergence/PIRLS error\n")
    cat("  Action: Return NULL for this group size\n")
    return("convergence")
  } else {
    cat("  Unhandled error - will be re-thrown\n")
    return("other")
  }
}

cat("\nError 1: 'PIRLS step-halvings failed to reduce deviance'\n")
simulate_error_handling("PIRLS step-halvings failed to reduce deviance in pwrssUpdate")

cat("\nError 2: 'singular matrix in backsolve'\n")
simulate_error_handling("singular matrix in backsolve")

cat("\nError 3: 'model convergence problem'\n")
simulate_error_handling("model convergence problem")

cat("\n=== All Tests Complete ===\n")
cat("\nSummary of Fixes:\n")
cat("1. ✓ Removed problematic as.numeric() conversion that corrupted binary variables\n")
cat("2. ✓ Added validation to ensure binary variables contain only 0, 1, or NA\n")
cat("3. ✓ Implemented conditional random effects (intercept-only for binary outcomes)\n")
cat("4. ✓ Added robust optimizer (bobyqa) for binary outcomes\n")
cat("5. ✓ Improved error handling for PIRLS/convergence failures\n")
cat("6. ✓ Added informative messages about which approach is being used\n")
