#' Count Rows and Unique IDs in a Dataframe
#'
#' Provides a diagnostic summary of dataset size, including total number of rows,
#' count of unique participant IDs, and a complete list of ID values. This function
#' is particularly useful for tracking participant data through the UKALC pipeline
#' and verifying data integrity after merges or filtering operations.
#'
#' The function assumes the dataframe contains an 'id' column representing
#' participant identifiers.
#'
#' @param df A dataframe or tibble containing an 'id' column
#'
#' @return NULL. The function prints diagnostic information to console and returns nothing.
#'   Output includes:
#'   - Total number of rows (observations)
#'   - Number of unique participant IDs
#'   - Comma-separated list of all unique ID values
#'
#' @examples
#' # Check sample size after importing AM survey data
#' count_rows_ids(am_rc)
#'
#' # Verify IDs after merging AM and PM surveys
#' count_rows_ids(merged_df)
#'
#' # Quick diagnostic after filtering step
#' count_rows_ids(ukalc_final)
#'
#' @seealso \code{\link[dplyr]{n_distinct}} for counting unique values
count_rows_ids <- function(df) {
  cat("-------The number of unique IDs in the raw dataset-------")
  cat("\n", "Number of rows: ", nrow(df), "\n")
  cat("\n", "Number of unique ids: ", n_distinct(df$id), "\n")
  
  # Get a list of unique IDs in the dataset
  id_list <- unique(df$id)
  
  # Print the number of unique IDs and the list of unique IDs in a sentence
  cat("\n", "The id values are:", "\n", "\n", paste(id_list, collapse = ", "), "\n", "\n")
}
