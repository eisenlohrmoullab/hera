#' Verify UKALC Pipeline Setup
#'
#' This script validates that the UKALC analysis pipeline is correctly configured
#' before running the main analysis. It checks:
#' - Required R packages are installed
#' - Box Desktop paths are accessible
#' - Output folder can be created
#'
#' @return Invisible TRUE if all checks pass, stops with error message if any check fails
#'
#' @examples
#' # Run this script after completing the setup instructions in README.md
#' source("scripts/verify_setup.R")

verify_setup <- function() {
  
  cat("\n=== UKALC Pipeline Setup Verification ===\n\n")
  
  # Track whether all checks passed
  all_passed <- TRUE
  
  ## 1. Check R version
  cat("1. Checking R version... ")
  r_version <- getRversion()
  if (r_version >= "4.0.0") {
    cat("✓ R", as.character(r_version), "(minimum 4.0.0 required)\n")
  } else {
    cat("✗ FAILED\n")
    cat("   Your R version is", as.character(r_version), "but 4.0.0+ is required\n")
    cat("   Download from: https://www.r-project.org/\n")
    all_passed <- FALSE
  }
  
  ## 2. Check critical packages
  cat("\n2. Checking critical packages... ")
  critical_packages <- c("tidyverse", "lme4", "mgcv", "readxl")
  missing_packages <- critical_packages[!sapply(critical_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) == 0) {
    cat("✓ All critical packages installed\n")
  } else {
    cat("✗ FAILED\n")
    cat("   Missing packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("   Run: source('UKALC_00_setup.R') to install\n")
    all_passed <- FALSE
  }
  
  ## 3. Check output folder path
  cat("\n3. Checking output folder configuration... ")
  if (exists("output_folder")) {
    cat("✓ Variable 'output_folder' is defined\n")
    cat("   Path:", output_folder, "\n")
    
    # Check if parent directory exists (Box folder)
    parent_dir <- dirname(output_folder)
    if (dir.exists(parent_dir)) {
      cat("   ✓ Parent directory accessible (Box folder found)\n")
    } else {
      cat("   ✗ WARNING: Parent directory not found\n")
      cat("      Expected:", parent_dir, "\n")
      cat("      Is Box Desktop running and synced?\n")
      cat("      See README.md Step 4 for path configuration\n")
      all_passed <- FALSE
    }
    
    # Try to create output folder
    if (!dir.exists(output_folder)) {
      tryCatch({
        dir.create(output_folder, recursive = TRUE)
        cat("   ✓ Output folder created successfully\n")
      }, error = function(e) {
        cat("   ✗ FAILED to create output folder\n")
        cat("      Error:", e$message, "\n")
        all_passed <<- FALSE
      })
    } else {
      cat("   ✓ Output folder already exists\n")
    }
  } else {
    cat("✗ FAILED\n")
    cat("   Variable 'output_folder' not found\n")
    cat("   Run: source('UKALC_00_setup.R') first\n")
    all_passed <- FALSE
  }
  
  ## 4. Check for temporary files
  cat("\n4. Checking for temporary files... ")
  temp_files <- list.files(pattern = "^\\.RDataTmp.*", all.files = TRUE)
  if (length(temp_files) > 0) {
    cat("⚠️  WARNING\n")
    cat("   Found temporary files:", paste(temp_files, collapse = ", "), "\n")
    cat("   These should not be committed to git\n")
  } else {
    cat("✓ No temporary .RDataTmp files found\n")
  }
  
  ## Summary
  cat("\n=== Summary ===\n")
  if (all_passed) {
    cat("✓ All checks passed! Ready to run the pipeline.\n")
    cat("\nNext steps:\n")
    cat("  1. Open UKALC_01_data_prep.Rmd\n")
    cat("  2. Update data input paths (import-survey-data chunk)\n")
    cat("  3. Click 'Knit' to run data preparation\n\n")
  } else {
    cat("✗ Some checks failed. Please fix the issues above before proceeding.\n")
    cat("   See README.md for setup instructions\n\n")
    stop("Setup verification failed. See messages above for details.")
  }
  
  invisible(TRUE)
}

# Run verification if sourced
if (!interactive()) {
  verify_setup()
}

# If in interactive mode, just define the function
if (interactive()) {
  cat("Setup verification function loaded. Run: verify_setup()\n")
}
