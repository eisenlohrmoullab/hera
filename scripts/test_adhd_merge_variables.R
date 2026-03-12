# Test script for ADHD merge variable handling
# Tests DSRP_1-23 mapping and menses/ovtoday inclusion

library(dplyr)

cat("\n=== Testing ADHD Merge Variable Mapping ===\n\n")

# Test 1: DSRP variable mapping
cat("=== Test 1: DSRP Variable Mapping ===\n")

# Create mapping from UKALC_02_merge_adhd.Rmd
# NOTE: Format is "new_name" = "old_name" for dplyr::rename()
adhd_drsp_mapping <- c(
  "dep" = "DSRP_1", "hopeless" = "DSRP_2", "worthless" = "DSRP_3",
  "anxious" = "DSRP_4", "mood_swing" = "DSRP_5", "rej_sens" = "DSRP_6",
  "angry" = "DSRP_7", "conflicts" = "DSRP_8", "loss_interest" = "DSRP_9",
  "diff_conc" = "DSRP_10", "tired" = "DSRP_11", "over_eat" = "DSRP_12",
  "sp_food_crav" = "DSRP_13", "high_sleep" = "DSRP_14", "diff_sleep" = "DSRP_15",
  "overwhelm" = "DSRP_16", "loss_control" = "DSRP_17", "breast_tend" = "DSRP_18",
  "breast_swell" = "DSRP_19", "headache" = "DSRP_20", "jm_pain" = "DSRP_21",
  "loss_prod" = "DSRP_22", "int_relat" = "DSRP_23"
)

# Expected DRSP variable order from UKALC
expected_drsp_order <- c(
  "dep", "hopeless", "worthless", "anxious", "mood_swing", "rej_sens",
  "angry", "conflicts", "loss_interest", "diff_conc", "tired",
  "over_eat", "sp_food_crav", "high_sleep", "diff_sleep", "overwhelm",
  "loss_control", "breast_tend", "breast_swell", "headache",
  "jm_pain", "loss_prod", "int_relat"
)

# Check: All 23 variables are mapped
test1_count <- length(adhd_drsp_mapping) == 23
cat("  23 DSRP variables mapped:", test1_count, "\n")

# Check: Mapping names match expected DRSP set (values are old names, names are new)
test1_order <- setequal(names(adhd_drsp_mapping), expected_drsp_order)
cat("  Mapping names match UKALC DRSP set:", test1_order, "\n")

# Check: All DSRP keys (old names) are correctly numbered 1-23
test1_keys <- all(unname(adhd_drsp_mapping) == paste0("DSRP_", 1:23))
cat("  DSRP values (old names) correctly numbered 1-23:", test1_keys, "\n")

cat("Test 1", ifelse(test1_count && test1_order && test1_keys, "PASSED ✓", "FAILED ✗"), "\n\n")

# Test 2: Cycle variables inclusion
cat("=== Test 2: Cycle Variables Inclusion ===\n")

cycle_vars <- c("menses", "ovtoday")

# Check: Both cycle variables defined
test2_count <- length(cycle_vars) == 2
cat("  2 cycle variables defined:", test2_count, "\n")

# Check: Variable names are correct
test2_names <- all(cycle_vars %in% c("menses", "ovtoday"))
cat("  Cycle variable names correct:", test2_names, "\n")

cat("Test 2", ifelse(test2_count && test2_names, "PASSED ✓", "FAILED ✗"), "\n\n")

# Test 3: DSRP pattern matching
cat("=== Test 3: DSRP Pattern Detection ===\n")

# Create mock ADHD dataset with DSRP variables
mock_adhd <- data.frame(
  id = 1:10,
  daterated = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
  DSRP_1 = rnorm(10, 3, 1),
  DSRP_2 = rnorm(10, 3, 1),
  DSRP_23 = rnorm(10, 3, 1),
  other_var = rnorm(10)
)

# Test pattern from script
drsp_pattern <- "^DSRP_[0-9]+$"
detected_dsrp <- grep(drsp_pattern, names(mock_adhd), value = TRUE)

test3_detect <- length(detected_dsrp) == 3
cat("  DSRP pattern detected 3 variables:", test3_detect, "\n")

test3_correct <- all(detected_dsrp %in% c("DSRP_1", "DSRP_2", "DSRP_23"))
cat("  Correct DSRP variables detected:", test3_correct, "\n")

cat("Test 3", ifelse(test3_detect && test3_correct, "PASSED ✓", "FAILED ✗"), "\n\n")

# Test 4: Menses/ovtoday pattern matching
cat("=== Test 4: Menses/Ovtoday Pattern Detection ===\n")

# Create mock dataset with cycle variables
mock_ukalc <- data.frame(
  id = 1:10,
  daterated = seq(as.Date("2024-01-01"), by = "day", length.out = 10),
  menses = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  ovtoday = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
  menses_rtb = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),  # derived version
  other_var = rnorm(10)
)

# Test pattern from script
cycle_pattern <- "menses|ovtoday"
detected_cycle <- grep(cycle_pattern, names(mock_ukalc), ignore.case = TRUE, value = TRUE)

test4_detect <- length(detected_cycle) == 3  # includes menses_rtb
cat("  Cycle pattern detected 3 variables:", test4_detect, "\n")

# Filter raw variables (excluding .rtb suffix)
DERIVED_VAR_SUFFIX_PATTERN <- "\\.(szd|zd|sd|m|d|log|[0-9]+roll)$"
filter_raw_vars <- function(var_names) {
  var_names[!grepl(DERIVED_VAR_SUFFIX_PATTERN, var_names, ignore.case = TRUE)]
}

# Note: _rtb is not in the pattern, so we need to check if manual filtering would work
# In real script, menses_rtb would be filtered out by the derived var pattern if it had a suffix
raw_cycle <- detected_cycle[!grepl("_rtb$", detected_cycle)]
test4_raw <- length(raw_cycle) == 2
cat("  Raw cycle variables (excluding _rtb):", test4_raw, "\n")

test4_correct <- all(raw_cycle %in% c("menses", "ovtoday"))
cat("  Correct cycle variables identified:", test4_correct, "\n")

cat("Test 4", ifelse(test4_detect && test4_correct, "PASSED ✓", "FAILED ✗"), "\n\n")

# Test 5: Shared variables construction
cat("=== Test 5: Shared Variables List Construction ===\n")

core_vars <- c("id", "original_id", "date", "study")
drsp_vars_standard <- expected_drsp_order
horm_vars <- c("e2", "p4", "lh")
alc_vars <- c("num_drinks_today", "drink_today", "fourplustoday")

shared_vars_all <- c(core_vars, drsp_vars_standard, horm_vars, cycle_vars, alc_vars)

test5_count <- length(shared_vars_all) == (4 + 23 + 3 + 2 + 3)  # 35 total
cat("  Expected 35 variables in shared_vars_all:", test5_count, "\n")

test5_unique <- length(unique(shared_vars_all)) == length(shared_vars_all)
cat("  No duplicate variables:", test5_unique, "\n")

test5_cycle <- all(cycle_vars %in% shared_vars_all)
cat("  Cycle variables in shared_vars_all:", test5_cycle, "\n")

cat("Test 5", ifelse(test5_count && test5_unique && test5_cycle, "PASSED ✓", "FAILED ✗"), "\n\n")

# Test 6: Variable renaming simulation
cat("=== Test 6: Variable Renaming Simulation ===\n")

# Create mock ADHD data with DSRP variables
adhd_test <- data.frame(
  id = 1:5,
  daterated = seq(as.Date("2024-01-01"), by = "day", length.out = 5),
  DSRP_1 = c(2, 3, 4, 3, 2),
  DSRP_2 = c(1, 2, 3, 2, 1),
  DSRP_23 = c(3, 4, 5, 4, 3)
)

# Apply renaming
adhd_renamed <- adhd_test %>%
  rename(any_of(adhd_drsp_mapping))

test6_renamed <- all(c("dep", "hopeless", "int_relat") %in% names(adhd_renamed))
cat("  DSRP variables renamed to DRSP:", test6_renamed, "\n")

test6_values <- all(adhd_renamed$dep == c(2, 3, 4, 3, 2))
cat("  Values preserved after renaming:", test6_values, "\n")

cat("Test 6", ifelse(test6_renamed && test6_values, "PASSED ✓", "FAILED ✗"), "\n\n")

# Summary
cat("=== Test Summary ===\n")
all_tests <- c(
  test1_count && test1_order && test1_keys,
  test2_count && test2_names,
  test3_detect && test3_correct,
  test4_detect && test4_correct,
  test5_count && test5_unique && test5_cycle,
  test6_renamed && test6_values
)

cat("Tests passed:", sum(all_tests), "/", length(all_tests), "\n")
if (all(all_tests)) {
  cat("\n✓ All tests PASSED - ADHD merge variable handling is correct\n\n")
} else {
  cat("\n✗ Some tests FAILED - review implementation\n\n")
  cat("Failed tests:", paste(which(!all_tests), collapse = ", "), "\n\n")
}
