#' Plot Day-of-Week Effects on Outcome Variables
#'
#' Creates three types of visualizations for examining day-of-week patterns in
#' outcome variables: sine transformation smooths, cosine transformation smooths,
#' and weekly bar plots. This function is designed to help identify cyclical
#' patterns in daily survey data that may be confounded with day-of-week effects.
#'
#' The function generates and saves plots to the output folder specified in the
#' global environment variable 'output_folder'. For each outcome variable, three
#' plots are created:
#' 1. Loess smooth of outcome vs day_of_week_sin
#' 2. Loess smooth of outcome vs day_of_week_cos  
#' 3. Bar plot of mean outcome by day of week
#'
#' Day-of-week is treated as an ordered factor (Sunday through Saturday), and
#' sine/cosine transformations allow for modeling cyclical weekly patterns.
#'
#' @param df A dataframe containing:
#'   - day_of_week: Day names (will be converted to ordered factor if not already)
#'   - day_of_week_sin: Sine-transformed day of week values
#'   - day_of_week_cos: Cosine-transformed day of week values
#'   - Columns matching names in outcome_vars parameter
#' @param outcome_vars Character vector of column names to plot (e.g., c("num_drinks", "crave_alc"))
#'
#' @return Character string "Plots generated and saved successfully."
#'
#' @details
#' The sine and cosine transformations are calculated as:
#' - sin(2π * day_number / 7) where day_number = 0 (Sunday) to 6 (Saturday)
#' - cos(2π * day_number / 7)
#' 
#' These transformations allow weekly patterns to be modeled as smooth cyclical
#' functions rather than treating each day as a separate category.
#'
#' Output files are saved with naming convention:
#' - day_of_week_sin_{outcome}.png
#' - day_of_week_cos_{outcome}.png
#' - weekly_summary_{outcome}.png
#'
#' @examples
#' # Plot day-of-week effects for alcohol variables
#' plot_day_of_week_effects(ukalc_data, c("num_drinks", "drink_today", "crave_alc_pm"))
#'
#' # Plot effects for mood variables
#' plot_day_of_week_effects(ukalc_data, c("dep", "anxious", "angry"))
#'
#' @seealso \code{\link{save_plot}} for the underlying plot saving mechanism
plot_day_of_week_effects <- function(df, outcome_vars) {
  # Set output directory path from the environment variable
  output_folder_path <- output_folder
  
  # Ensure day_of_week is an ordered factor
  df <- df %>%
    mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
  
  # Define sine and cosine positions for each day (without labels)
  label_positions <- data.frame(
    day_of_week_sin = sin(2 * pi * (0:6) / 7),
    day_of_week_cos = cos(2 * pi * (0:6) / 7)
  )
  
  # Create output directory if it doesn’t exist
  if (!dir.exists(output_folder_path)) {
    dir.create(output_folder_path, recursive = TRUE)
  }
  
  # Helper function to plot smooth effect (sin/cos) for a given outcome
  plot_smooth_effect <- function(df, outcome_var, x_var, title_suffix, output_filename) {
    outcome_sym <- sym(outcome_var)
    x_sym <- sym(x_var)
    
    plot <- ggplot(df %>% filter(!is.na(!!x_sym) & !is.na(!!outcome_sym)), 
                   aes(x = !!x_sym, y = !!outcome_sym)) +
      geom_smooth(method = "loess") +
      labs(
        title = paste("Effects of Day of Week on", outcome_var, title_suffix),
        x = paste("Day of Week", title_suffix),
        y = outcome_var
      ) +
      theme_minimal()
    
    # Save the plot
    save_plot(plot = plot, 
              filename = output_filename,
              folder = output_folder_path,
              width = 10, 
              height = 6)
  }
  
  # Loop over each outcome variable to generate the plots
  for (outcome_var in outcome_vars) {
    # Plot for day_of_week_sin
    plot_smooth_effect(df, outcome_var, "day_of_week_sin", "(sin)",
                       paste0("day_of_week_sin_", outcome_var, ".png"))
    
    # Plot for day_of_week_cos
    plot_smooth_effect(df, outcome_var, "day_of_week_cos", "(cos)",
                       paste0("day_of_week_cos_", outcome_var, ".png"))
    
    # Calculate weekly average or proportion for bar plot
    weekly_summary <- df %>%
      group_by(day_of_week) %>%
      summarize(mean_outcome = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
      ungroup()
    
    # Plot the weekly average/proportion by day of the week (bar plot)
    plot_weekly <- ggplot(weekly_summary, aes(x = day_of_week, y = mean_outcome)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = paste("Weekly Average of", outcome_var, "by Day of the Week"),
        x = "Day of the Week",
        y = paste("Average", outcome_var)
      ) +
      theme_minimal()
    
    # Save the bar plot for weekly summary by day of the week
    save_plot(plot = plot_weekly, 
              filename = paste0("weekly_summary_", outcome_var, ".png"),
              folder = output_folder_path,
              width = 10, 
              height = 6)
  }
  
  return("Plots generated and saved successfully.")
}
