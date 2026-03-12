# standardize_tubenumber.R
# Helper function to standardize tube_number to TubeNumber for consistency
# Used in both AM and PM survey processing, and could be used elsewhere

#' Standardize TubeNumber column name
#'
#' Renames tube_number to TubeNumber for consistency with hormone data
#' This ensures consistency across all data sources
#'
#' @param df A dataframe that may contain a tube_number column
#' @param verbose Logical, whether to print informational messages (default: TRUE)
#' @return The dataframe with tube_number renamed to TubeNumber (if present)
#'
#' @examples
#' df <- standardize_tubenumber(df)
#' df <- standardize_tubenumber(df, verbose = FALSE)
standardize_tubenumber <- function(df, verbose = TRUE) {
  # Check if tube_number exists and TubeNumber doesn't (to avoid overwrites)
  if ("tube_number" %in% names(df) && !"TubeNumber" %in% names(df)) {
    df <- df %>% dplyr::rename(TubeNumber = tube_number)
    if (verbose) {
      cat("✓ Renamed tube_number to TubeNumber\n")
    }
  } else if ("TubeNumber" %in% names(df)) {
    if (verbose) {
      cat("✓ TubeNumber already present (no change needed)\n")
    }
  } else {
    if (verbose) {
      cat("ℹ No tube_number column found (skipping)\n")
    }
  }
  
  return(df)
}
