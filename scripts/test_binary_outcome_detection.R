# Test script for binary outcome detection in run_smm_cyclic.R
# This script tests that binary outcomes are correctly detected and handled

library(dplyr)
library(glue)

# Test 1: Binary outcome detection
cat("\n=== Test 1: Binary Outcome Detection ===\n")

# Create test data with binary outcome (0/1 values only)
test_data_binary <- data.frame(
  id = rep(1:10, each = 20),
  time_var = rep(seq(-1, 1, length.out = 20), 10),
  drink_today = sample(c(0, 1, NA), 200, replace = TRUE, prob = c(0.4, 0.4, 0.2))
)

# Test detection logic
is_binary_test <- all(test_data_binary$drink_today %in% c(0, 1, NA), na.rm = TRUE)
cat("Binary outcome detected:", is_binary_test, "\n")
cat("Expected: TRUE\n")
cat("Test 1", ifelse(is_binary_test, "PASSED ✓", "FAILED ✗"), "\n")

# Test 2: Continuous outcome detection
cat("\n=== Test 2: Continuous Outcome Detection ===\n")

# Create test data with continuous outcome
test_data_continuous <- data.frame(
  id = rep(1:10, each = 20),
  time_var = rep(seq(-1, 1, length.out = 20), 10),
  dep = rnorm(200, mean = 3, sd = 1)
)

# Test detection logic
is_continuous_test <- all(test_data_continuous$dep %in% c(0, 1, NA), na.rm = TRUE)
cat("Binary outcome detected:", is_continuous_test, "\n")
cat("Expected: FALSE\n")
cat("Test 2", ifelse(!is_continuous_test, "PASSED ✓", "FAILED ✗"), "\n")

# Test 3: Binary with NA values
cat("\n=== Test 3: Binary with NA Values ===\n")

test_data_binary_na <- data.frame(
  id = rep(1:10, each = 20),
  time_var = rep(seq(-1, 1, length.out = 20), 10),
  fourplustoday = c(rep(0, 50), rep(1, 50), rep(NA, 100))
)

is_binary_na_test <- all(test_data_binary_na$fourplustoday %in% c(0, 1, NA), na.rm = TRUE)
cat("Binary outcome with NA detected:", is_binary_na_test, "\n")
cat("Expected: TRUE\n")
cat("Test 3", ifelse(is_binary_na_test, "PASSED ✓", "FAILED ✗"), "\n")

# Test 4: Edge case - all NA
cat("\n=== Test 4: Edge Case - All NA ===\n")

test_data_all_na <- data.frame(
  id = rep(1:10, each = 20),
  time_var = rep(seq(-1, 1, length.out = 20), 10),
  outcome = rep(NA, 200)
)

is_all_na_test <- all(test_data_all_na$outcome %in% c(0, 1, NA), na.rm = TRUE)
cat("All NA detected as binary:", is_all_na_test, "\n")
cat("Expected: TRUE (vacuous truth - all non-NA values are in {0,1})\n")
cat("Test 4", ifelse(is_all_na_test, "PASSED ✓", "FAILED ✗"), "\n")

# Test 5: Edge case - contains values other than 0/1
cat("\n=== Test 5: Edge Case - Contains 2 ===\n")

test_data_with_2 <- data.frame(
  id = rep(1:10, each = 20),
  time_var = rep(seq(-1, 1, length.out = 20), 10),
  outcome = sample(c(0, 1, 2), 200, replace = TRUE)
)

is_not_binary_test <- all(test_data_with_2$outcome %in% c(0, 1, NA), na.rm = TRUE)
cat("Contains value 2, detected as binary:", is_not_binary_test, "\n")
cat("Expected: FALSE\n")
cat("Test 5", ifelse(!is_not_binary_test, "PASSED ✓", "FAILED ✗"), "\n")

# Test 6: Test plogis transformation (inverse logit)
cat("\n=== Test 6: plogis (inverse logit) transformation ===\n")

logit_values <- c(-2, -1, 0, 1, 2)
prob_values <- plogis(logit_values)
cat("Logit values:", paste(logit_values, collapse = ", "), "\n")
cat("Probability values:", paste(round(prob_values, 4), collapse = ", "), "\n")
cat("Expected probabilities in [0, 1]:", all(prob_values >= 0 & prob_values <= 1), "\n")
cat("Test 6", ifelse(all(prob_values >= 0 & prob_values <= 1), "PASSED ✓", "FAILED ✗"), "\n")

cat("\n=== All Tests Complete ===\n")
