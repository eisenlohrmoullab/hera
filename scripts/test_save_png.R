#!/usr/bin/env Rscript
# Test script for save_png() validation and functionality

# Source the setup and graphics config
source("UKALC_00_setup.R")

# Create a temporary output directory for tests
test_output <- tempdir()
cat("Test output directory:", test_output, "\n\n")

# ============================================================================
# TEST 1: Basic save_png with defaults
# ============================================================================
cat("TEST 1: Basic save_png with defaults (10x6 inches, 200 DPI)\n")
test_plot <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  labs(title = "Test Plot: Default Settings",
       subtitle = "10x6 inches @ 200 DPI") +
  theme_minimal(base_size = 14)

tryCatch({
  save_png(test_plot, "test1_defaults.png", folder = test_output)
  cat("✅ PASS: Default settings work\n\n")
}, error = function(e) {
  cat("❌ FAIL:", e$message, "\n\n")
})

# ============================================================================
# TEST 2: Custom dimensions
# ============================================================================
cat("TEST 2: Custom dimensions (12x8 inches)\n")
tryCatch({
  save_png(test_plot, "test2_custom_dims.png", 
           folder = test_output, width = 12, height = 8)
  cat("✅ PASS: Custom dimensions work\n\n")
}, error = function(e) {
  cat("❌ FAIL:", e$message, "\n\n")
})

# ============================================================================
# TEST 3: High DPI warning (should warn but not fail)
# ============================================================================
cat("TEST 3: High DPI (300) - should warn\n")
tryCatch({
  suppressWarnings({
    result <- save_png(test_plot, "test3_high_dpi.png", 
                       folder = test_output, dpi = 300)
  })
  # Check if warning was generated
  warning_result <- tryCatch({
    save_png(test_plot, "test3_high_dpi_check.png", 
             folder = test_output, dpi = 300)
    "no_warning"
  }, warning = function(w) {
    if (grepl("DPI >= 250", w$message)) {
      return("correct_warning")
    }
    return("wrong_warning")
  })
  
  if (warning_result == "correct_warning") {
    cat("✅ PASS: High DPI warning triggered correctly\n\n")
  } else {
    cat("⚠️  WARNING: High DPI check may not be working\n\n")
  }
}, error = function(e) {
  cat("❌ FAIL:", e$message, "\n\n")
})

# ============================================================================
# TEST 4: Validation - pixel values as inches (should stop)
# ============================================================================
cat("TEST 4: Validation - pixel values (1920x1080) should stop\n")
error_caught <- FALSE
tryCatch({
  # Try to pass pixel dimensions without units (should fail)
  save_png(test_plot, "test4_pixels.png", 
           folder = test_output, width = 1920, height = 1080)
}, error = function(e) {
  if (grepl("width or height > 50", e$message)) {
    error_caught <<- TRUE
  }
})

if (error_caught) {
  cat("✅ PASS: Pixel value validation works (correctly stopped)\n\n")
} else {
  cat("❌ FAIL: Should have stopped for pixel values\n\n")
}

# ============================================================================
# TEST 5: last_plot() usage
# ============================================================================
cat("TEST 5: Using last_plot()\n")
ggplot(mtcars, aes(x = hp, y = qsec)) +
  geom_point() +
  labs(title = "Test Plot: last_plot()") +
  theme_minimal(base_size = 14)

tryCatch({
  save_png(plot = last_plot(), "test5_lastplot.png", folder = test_output)
  cat("✅ PASS: last_plot() works\n\n")
}, error = function(e) {
  cat("❌ FAIL:", e$message, "\n\n")
})

# ============================================================================
# TEST 6: Verify files were created
# ============================================================================
cat("TEST 6: Verify output files exist\n")
expected_files <- c("test1_defaults.png", "test2_custom_dims.png", 
                   "test3_high_dpi.png", "test5_lastplot.png")
files_found <- file.exists(file.path(test_output, expected_files))

if (all(files_found)) {
  cat("✅ PASS: All expected files created\n\n")
  
  # Check file sizes
  cat("File sizes:\n")
  for (f in expected_files) {
    path <- file.path(test_output, f)
    size_kb <- round(file.info(path)$size / 1024, 1)
    cat(sprintf("  %s: %s KB\n", f, size_kb))
  }
  cat("\n")
} else {
  cat("❌ FAIL: Some files missing:", 
      paste(expected_files[!files_found], collapse = ", "), "\n\n")
}

# ============================================================================
# SUMMARY
# ============================================================================
cat("=====================================\n")
cat("TEST SUMMARY\n")
cat("=====================================\n")
cat("All basic functionality tests completed.\n")
cat("Check test output files in:", test_output, "\n")
cat("\nExpected behavior:\n")
cat("- Default settings: 10x6 inches @ 200 DPI\n")
cat("- High DPI (250+) triggers warning\n")
cat("- Pixel values (>50) without units cause error\n")
cat("- All files saved with ragg device\n")
cat("=====================================\n")
