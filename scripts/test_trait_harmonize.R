# Test script for trait harmonization functions
# Tests import_clean_trait_csv(), recode_ukalc_traits(), recode_adhd_traits(),
# merge_trait_datasets(), and apply_codebook() from scripts/harmonize_traits.R

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(readr)
  library(janitor)
})

source("scripts/harmonize_traits.R")

cat("\n=== Testing Trait Harmonization Functions ===\n\n")

# Paths to test data
ukalc_path <- "testdata/traits/ukalc_trait_example.csv"
adhd_path  <- "testdata/traits/adhd_trait_example.csv"

all_pass <- TRUE

# Helper to record test outcomes
record <- function(name, condition) {
  status <- if (isTRUE(condition)) "PASSED \u2713" else "FAILED \u2717"
  cat(sprintf("  %-60s %s\n", name, status))
  if (!isTRUE(condition)) all_pass <<- FALSE
}


# =============================================================================
# Test 1: import_clean_trait_csv()
# =============================================================================
cat("=== Test 1: import_clean_trait_csv() ===\n")

ukalc_raw <- import_clean_trait_csv(ukalc_path)
adhd_raw  <- import_clean_trait_csv(adhd_path)

record("UKALC CSV loads without error",        is.data.frame(ukalc_raw))
record("ADHD CSV loads without error",         is.data.frame(adhd_raw))

# Column names should be snake_case (no uppercase, no spaces)
record("UKALC column names are snake_case",
       all(!grepl("[A-Z ]", names(ukalc_raw))))
record("ADHD column names are snake_case",
       all(!grepl("[A-Z ]", names(adhd_raw))))

# No rows that are entirely NA
record("UKALC has no fully-blank rows",
       !any(apply(ukalc_raw, 1, function(r) all(is.na(r)))))
record("ADHD has no fully-blank rows",
       !any(apply(adhd_raw, 1, function(r) all(is.na(r)))))

# Blank cells converted to NA (row 7 of ukalc_example has whitespace-only values)
ukalc_race_7 <- ukalc_raw$race[nrow(ukalc_raw)]
record("Whitespace-only strings become NA",    is.na(ukalc_race_7))

cat("\n")


# =============================================================================
# Test 2: recode_ukalc_traits()
# =============================================================================
cat("=== Test 2: recode_ukalc_traits() ===\n")

ukalc_recoded <- recode_ukalc_traits(ukalc_raw)

# ID prefixing
record("id column has ALC_ prefix (matching daily pipeline)",
       all(grepl("^ALC_", ukalc_recoded$id[!is.na(ukalc_recoded$id)])))
record("original_id column preserved",
       "original_id" %in% names(ukalc_recoded))
record("source column equals 'UKALC'",
       all(ukalc_recoded$source == "UKALC"))

# Date parsing
record("trait_firststudyperiod is a Date",
       inherits(ukalc_recoded$trait_firststudyperiod, "Date"))

# Race recodes
races <- ukalc_recoded$race[!is.na(ukalc_recoded$race)]
record("'white' (lowercase) recoded to 'White'",
       "White" %in% races)
record("Uppercase 'WHITE' normalized to 'White'",
       "White" %in% races && !"WHITE" %in% races)
record("'Black or African American' recoded to 'Black'",
       all(races[grepl("(?i)^black$|african american", ukalc_raw$race[!is.na(ukalc_raw$race)], perl = TRUE)] == "Black"))
record("'multiracial' recoded to 'Multiracial'",
       "Multiracial" %in% races)

# sexorient recodes
sexorients <- ukalc_recoded$sexorient[!is.na(ukalc_recoded$sexorient)]
record("'straight' recoded to 'Heterosexual'",
       "Heterosexual" %in% sexorients && !"straight" %in% sexorients)
record("'bisexual' recoded to 'Bisexual/pansexual'",
       "Bisexual/pansexual" %in% sexorients)
record("'gay' recoded to 'Gay/lesbian'",
       "Gay/lesbian" %in% sexorients && !"gay" %in% sexorients)
record("'homosexual' recoded to 'Gay/lesbian'",
       all(sexorients[tolower(ukalc_raw$sexorient[!is.na(ukalc_raw$sexorient)]) == "homosexual"] == "Gay/lesbian"))
record("'queer' preserved as-is (sentence-case 'queer' → left unchanged)",
       "queer" %in% tolower(sexorients))

# Medication text
record("current_meds_text column exists",
       "current_meds_text" %in% names(ukalc_recoded))
record("current_meds_text is NA when med_user != 1",
       all(is.na(ukalc_recoded$current_meds_text[
         !is.na(ukalc_recoded$med_user) & as.integer(as.character(ukalc_recoded$med_user)) != 1
       ])))
record("current_meds_text has value when med_user == 1",
       any(!is.na(ukalc_recoded$current_meds_text[
         !is.na(ukalc_recoded$med_user) & as.integer(as.character(ukalc_recoded$med_user)) == 1
       ])))

cat("\n")


# =============================================================================
# Test 3: recode_adhd_traits()
# =============================================================================
cat("=== Test 3: recode_adhd_traits() ===\n")

adhd_recoded <- recode_adhd_traits(adhd_raw)

# ID prefixing
record("id column has ADHD_ prefix",
       all(grepl("^ADHD_", adhd_recoded$id[!is.na(adhd_recoded$id)])))
record("id is NOT zero-padded (matches daily pipeline: ADHD_1 not ADHD_001)",
       all(grepl("^ADHD_[0-9]+$", adhd_recoded$id[!is.na(adhd_recoded$id)]) &
           !any(grepl("^ADHD_0", adhd_recoded$id[!is.na(adhd_recoded$id)]))))
record("original_id column preserved",
       "original_id" %in% names(adhd_recoded))
record("source column equals 'ADHD'",
       all(adhd_recoded$source == "ADHD"))

# Date parsing
record("firstperiod is a Date",
       inherits(adhd_recoded$firstperiod, "Date"))

# Unit conversions
record("height_cm column created",          "height_cm" %in% names(adhd_recoded))
record("height column removed",             !"height" %in% names(adhd_recoded))
record("height_in column retained",         "height_in" %in% names(adhd_recoded))
record("weight_kg column created",          "weight_kg" %in% names(adhd_recoded))
record("weight column removed",             !"weight" %in% names(adhd_recoded))
record("weight_lb column retained",         "weight_lb" %in% names(adhd_recoded))

# Sanity check for conversions (64.5 in → ~163.8 cm; 135 lb → ~61.2 kg)
first_ht_cm <- adhd_recoded$height_cm[1]
first_wt_kg <- adhd_recoded$weight_kg[1]
record("height conversion is correct (64.5 in ≈ 163.8 cm)",
       abs(first_ht_cm - 64.5 * 2.54) < 0.1)
record("weight conversion is correct (135 lb ≈ 61.23 kg)",
       abs(first_wt_kg - 135 * 0.453592) < 0.01)
record("all non-NA height_cm > 100 cm (sanity)",
       all(adhd_recoded$height_cm[!is.na(adhd_recoded$height_cm)] > 100))
record("all non-NA weight_kg > 30 kg (sanity)",
       all(adhd_recoded$weight_kg[!is.na(adhd_recoded$weight_kg)] > 30))

# Codebook recodes
record("gender_label column created",           "gender_label" %in% names(adhd_recoded))
record("ethnicity_race_label column created",   "ethnicity_race_label" %in% names(adhd_recoded))
record("sexuality_label column created",        "sexuality_label" %in% names(adhd_recoded))
record("education_label column created",        "education_label" %in% names(adhd_recoded))

record("gender code 1 → 'Female'",
       adhd_recoded$gender_label[adhd_recoded$gender == 1][1] == "Female")
record("ethnicity_race code 1 → 'White'",
       adhd_recoded$ethnicity_race_label[!is.na(adhd_recoded$ethnicity_race) & adhd_recoded$ethnicity_race == 1][1] == "White")
record("sexuality code 2 → 'Bisexual/pansexual'",
       adhd_recoded$sexuality_label[!is.na(adhd_recoded$sexuality) & adhd_recoded$sexuality == 2][1] == "Bisexual/pansexual")
record("education code 4 → 'Graduated 4-year college'",
       adhd_recoded$education_label[!is.na(adhd_recoded$education) & adhd_recoded$education == 4][1] == "Graduated 4-year college")

# Original code columns kept alongside labels
record("gender code column retained alongside label",    "gender" %in% names(adhd_recoded))
record("sexuality code column retained alongside label", "sexuality" %in% names(adhd_recoded))

cat("\n")


# =============================================================================
# Test 4: apply_codebook()
# =============================================================================
cat("=== Test 4: apply_codebook() ===\n")

mapping <- c("1" = "Female", "2" = "Nonbinary", "3" = "Agender")
result  <- apply_codebook(c(1, 2, NA, 3), mapping, col_name = "gender")

record("apply_codebook maps 1 → 'Female'",   result[1] == "Female")
record("apply_codebook maps 2 → 'Nonbinary'", result[2] == "Nonbinary")
record("apply_codebook propagates NA",        is.na(result[3]))
record("apply_codebook maps 3 → 'Agender'",  result[4] == "Agender")

# Unknown code warning
got_warning <- FALSE
result_warn <- withCallingHandlers(
  apply_codebook(c("9"), mapping, col_name = "gender"),
  warning = function(w) {
    got_warning <<- TRUE
    invokeRestart("muffleWarning")
  }
)
record("apply_codebook warns on unknown code", isTRUE(got_warning))
record("apply_codebook returns NA for unknown code", is.na(result_warn[1]))

cat("\n")


# =============================================================================
# Test 5: merge_trait_datasets()
# =============================================================================
cat("=== Test 5: merge_trait_datasets() ===\n")

merged <- merge_trait_datasets(ukalc_recoded, adhd_recoded)

record("merged list has 'common' element",     "common" %in% names(merged))
record("merged list has 'union' element",      "union"  %in% names(merged))

n_ukalc <- nrow(ukalc_recoded)
n_adhd  <- nrow(adhd_recoded)
record("union row count = UKALC + ADHD rows",
       nrow(merged$union) == n_ukalc + n_adhd)
record("common row count = UKALC + ADHD rows",
       nrow(merged$common) == n_ukalc + n_adhd)

record("common columns subset of union columns",
       all(names(merged$common) %in% names(merged$union)))
record("common has fewer or equal columns than union",
       ncol(merged$common) <= ncol(merged$union))

# Required columns present in both
for (col in c("id", "original_id", "source")) {
  record(paste0("'", col, "' in common form"), col %in% names(merged$common))
  record(paste0("'", col, "' in union form"),  col %in% names(merged$union))
}

record("No duplicate IDs in common form",   !any(duplicated(merged$common$id)))
record("No duplicate IDs in union form",    !any(duplicated(merged$union$id)))

record("source values in union are 'UKALC' or 'ADHD'",
       all(merged$union$source %in% c("UKALC", "ADHD")))

# UKALC-only columns (e.g. sexorient) should be NA for ADHD rows in union
if ("sexorient" %in% names(merged$union)) {
  adhd_rows <- merged$union$source == "ADHD"
  record("UKALC-only 'sexorient' is NA for ADHD rows in union",
         all(is.na(merged$union$sexorient[adhd_rows])))
}

cat("\n")


# =============================================================================
# Summary
# =============================================================================
cat("=== Test Summary ===\n")
if (all_pass) {
  cat("\n\u2713 All tests PASSED\n\n")
} else {
  cat("\n\u2717 Some tests FAILED - review output above\n\n")
  stop("Trait harmonization tests failed.")
}
