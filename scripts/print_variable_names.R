#' Print Variable Names from a Dataframe
#'
#' Displays all column names from a dataframe in a formatted, comma-separated list.
#' This is a utility function for quick inspection of dataframe structure during
#' interactive analysis sessions.
#'
#' @param df A dataframe or tibble whose column names should be printed
#'
#' @return NULL. The function prints to console via cat() and returns nothing.
#'
#' @examples
#' # Print column names from the AM survey data
#' print.variable.names(am_rc)
#'
#' # Quick check of merged dataset structure
#' print.variable.names(ukalc_merged)
#'
#' @seealso \code{\link{names}} for getting variable names without printing
print.variable.names <- function(df) {
  variable_names <- names(df)
  formatted_list <- paste(variable_names, collapse = ", ")
  cat("\n", "-------- Names of variables in the dataset --------   ", "\n", formatted_list)
}
