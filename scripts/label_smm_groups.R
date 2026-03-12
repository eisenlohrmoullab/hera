## ============================================================================
## SMM GROUP LABELING FUNCTIONS
## ============================================================================
## Interactive functionality for labeling structural marginal model (SMM) 
## groups with meaningful, interpretable names based on their trajectory patterns.
##
## After running SMM analyses (via run_smm_cyclic), groups are numbered 
## arbitrarily (1, 2, 3, ...). These functions allow researchers to:
## 1. View SMM trajectory plots
## 2. Assign descriptive labels (e.g., "Perimenstrual-Onset", "Luteal-Peak")
## 3. Save label mappings for reproducibility
## 4. Apply labels to classification dataframes
##
## This makes results more interpretable and ensures consistent labeling across
## manuscripts, presentations, and follow-up analyses.
##
## WORKFLOW:
## 1. Run SMM analysis: result <- run_smm_cyclic(...)
## 2. View plots in output folder
## 3. Assign labels: labels <- label_smm_groups(...)
## 4. Save labels: save_group_labels(labels, ...)
## 5. Apply to data: class_labeled <- apply_group_labels(result$class, labels)
##
## Labels are saved as CSV files alongside SMM results for reproducibility.
## Future analyses can load saved labels via load_group_labels().
##
## Usage: source("scripts/label_smm_groups.R")
## See: scripts/example_label_smm_groups.R for detailed examples
## ============================================================================

library(tidyverse)
library(glue)

#' Get Centering Folder Name
#'
#' Internal helper function to standardize folder naming based on cycle centering.
#' Returns "ovulation_centered" or "menses_centered".
#'
#' @param centering Character string: "menses" or "ovulation"
#' @return Character string with folder name
#' 
#' @keywords internal
.get_centering_folder <- function(centering) {
  if (centering == "ovulation") "ovulation_centered" else "menses_centered"
}

#' Label SMM Groups Interactively
#'
#' Displays SMM trajectory plots and prompts user to assign meaningful labels 
#' to each latent group. This interactive function streamlines the labeling 
#' process by:
#' 1. Locating the relevant SMM plots based on outcome/centering/group number
#' 2. Listing available plot files for review
#' 3. Prompting for a descriptive label for each group (validated non-empty)
#' 4. Returning a dataframe mapping group numbers to labels
#'
#' The function operates in two modes:
#' - Interactive (default): Prompts user for labels after viewing plots
#' - Template: Returns unlabeled dataframe for manual completion
#'
#' Common label patterns by trajectory shape:
#' - "Perimenstrual-Onset": Symptoms peak around menses
#' - "Luteal-Peak": Symptoms peak in luteal phase (premenstrual)
#' - "Follicular-Increase": Symptoms increase post-menses
#' - "Stable" or "Low-Symptom": Minimal cycle variation
#' - "High-Symptom": Elevated throughout cycle
#' - "U-Shaped": High at both menses and ovulation
#'
#' @param base_save_dir Character string specifying base directory where SMM 
#'   results are saved (e.g., "output/SMMs")
#' @param outcome Character string with outcome variable technical name 
#'   (e.g., "dep", "crave_alc_pm")
#' @param centering Character string: "menses" or "ovulation" to specify cycle centering
#' @param g Integer specifying number of groups in the SMM solution to label
#' @param date_folder Character string with date folder name in YYYYMMDD format
#'   (default: today's date via Sys.Date())
#' @param interactive Logical flag:
#'   - TRUE (default): Prompts user interactively for labels
#'   - FALSE: Returns template dataframe with NA labels for manual filling
#'
#' @return Dataframe with columns:
#'   - smm_group: Character vector of group numbers ("1", "2", ...)
#'   - group_label: Character vector of assigned labels (or NA if non-interactive)
#'
#' @details
#' File organization: SMM plots are expected in:
#'   base_save_dir/YYYYMMDD/{centering}_centered/outcome/
#'
#' Plot naming convention:
#'   {outcome}_{plot_type}_g{g}.png
#'   (e.g., "dep_centered_g3.png", "dep_roll_g3.png")
#'
#' Label validation: Empty strings are rejected; user must provide non-empty label.
#'
#' Labels should be:
#' - Descriptive of the trajectory pattern
#' - Concise (ideally 2-4 words)
#' - Consistent with terminology in manuscripts
#' - Theoretically meaningful (not just "Group 1", "Group 2")
#'
#' @examples
#' # Interactive labeling (default)
#' labels <- label_smm_groups(
#'   base_save_dir = "output/SMMs_Ov_and_Borderline",
#'   outcome = "dep",
#'   centering = "menses",
#'   g = 3
#' )
#' # User is prompted to view plots and enter labels for Groups 1, 2, 3
#'
#' # Template mode (for programmatic labeling)
#' template <- label_smm_groups(
#'   base_save_dir = "output/SMMs",
#'   outcome = "anxious",
#'   centering = "ovulation",
#'   g = 4,
#'   interactive = FALSE
#' )
#' # Returns: data.frame(smm_group = c("1","2","3","4"), group_label = c(NA, NA, NA, NA))
#' # User fills in labels manually, then uses save_group_labels()
#'
#' # Labeling for different date
#' labels <- label_smm_groups(
#'   base_save_dir = "output/SMMs",
#'   outcome = "crave_alc_pm",
#'   centering = "menses",
#'   g = 2,
#'   date_folder = "20251215"
#' )
#'
#' @seealso
#' \code{\link{save_group_labels}} to save labels to CSV
#' \code{\link{load_group_labels}} to load previously saved labels
#' \code{\link{apply_group_labels}} to apply labels to classification dataframe
#' \code{\link{run_smm_cyclic}} for fitting SMMs
#'
#' @export
label_smm_groups <- function(
    base_save_dir,
    outcome,
    centering = "menses",
    g = 2,
    date_folder = format(Sys.Date(), "%Y%m%d"),
    interactive = TRUE
) {
  # Build path to SMM results folder
  centering_folder <- .get_centering_folder(centering)
  smm_folder <- file.path(base_save_dir, date_folder, centering_folder, outcome)
  
  # Check if folder exists
  if (!dir.exists(smm_folder)) {
    stop(glue("SMM folder not found: {smm_folder}"))
  }
  
  # Look for plot files - expecting three plot types
  plot_files <- list.files(smm_folder, pattern = glue("^{outcome}_.*_g{g}\\.png$"), full.names = TRUE)
  
  if (length(plot_files) == 0) {
    warning(glue("No plots found for g={g} in {smm_folder}"))
    warning(glue("Example expected pattern: {outcome}_centered_g{g}.png or {outcome}_mean_g{g}.png"))
  }
  
  # Create a template dataframe with group numbers
  labels_df <- data.frame(
    smm_group = as.character(1:g),
    group_label = rep(NA_character_, g),
    stringsAsFactors = FALSE
  )
  
  # If not interactive, return template
  if (!interactive) {
    message(glue("Created template label dataframe for {g} groups."))
    message("Fill in the 'group_label' column and use save_group_labels() to save.")
    return(labels_df)
  }
  
  # Interactive mode: display plots and prompt for labels
  cat("\n")
  cat("================================================================================\n")
  cat(glue("LABELING SMM GROUPS: {outcome} ({centering}-centered, g={g})"))
  cat("\n")
  cat("================================================================================\n")
  cat("\n")
  cat("Please view the SMM plots in:\n")
  cat(glue("  {smm_folder}"))
  cat("\n\n")
  
  if (length(plot_files) > 0) {
    cat("Available plots:\n")
    for (pf in plot_files) {
      cat(glue("  - {basename(pf)}"))
      cat("\n")
    }
    cat("\n")
  }
  
  cat("After viewing the plots, assign a meaningful label to each group.\n")
  cat("Examples: 'Perimenstrual-Onset', 'Luteal-Peak', 'Stable', 'High-Symptom'\n")
  cat("\n")
  
  # Prompt for each group
  for (i in 1:g) {
    repeat {
      label <- readline(prompt = glue("Enter label for Group {i}: "))
      label <- trimws(label)
      
      # Validate non-empty label
      if (nchar(label) > 0) {
        labels_df$group_label[i] <- label
        break
      } else {
        cat("Label cannot be empty. Please enter a meaningful label.\n")
      }
    }
  }
  
  cat("\n")
  cat("Labels assigned:\n")
  print(labels_df)
  cat("\n")
  
  return(labels_df)
}


#' Save Group Labels to CSV
#'
#' Saves SMM group label mappings to a CSV file for reproducibility and future
#' reference. The CSV file is stored alongside SMM results (plots, BIC tables)
#' in the structured output directory.
#'
#' This function ensures that group labels are documented and can be reused in:
#' - Follow-up analyses using the same SMM solution
#' - Manuscripts and presentations (consistent terminology)
#' - Collaborative projects (shared label definitions)
#'
#' The saved CSV contains two columns:
#' - smm_group: Numeric group identifiers as character ("1", "2", ...)
#' - group_label: Descriptive text labels assigned by researcher
#'
#' @param labels_df Dataframe with columns:
#'   - smm_group: Character vector of group numbers
#'   - group_label: Character vector of meaningful labels
#'   (Typically output from label_smm_groups())
#' @param base_save_dir Character string specifying base directory where SMM 
#'   results are saved
#' @param outcome Character string with outcome variable technical name
#' @param centering Character string: "menses" or "ovulation"
#' @param g Integer specifying number of groups
#' @param date_folder Character string with date folder name (YYYYMMDD format)
#'
#' @return Path to saved CSV file (invisibly). Side effect: CSV file written to disk.
#'
#' @details
#' Output path structure:
#'   base_save_dir/YYYYMMDD/{centering}_centered/outcome/group_labels_g{g}.csv
#'
#' The function creates the output directory if it doesn't exist.
#'
#' @examples
#' # After labeling groups interactively
#' labels <- label_smm_groups(
#'   base_save_dir = "output/SMMs",
#'   outcome = "dep",
#'   centering = "menses",
#'   g = 3
#' )
#'
#' # Save labels for future use
#' save_group_labels(
#'   labels_df = labels,
#'   base_save_dir = "output/SMMs",
#'   outcome = "dep",
#'   centering = "menses",
#'   g = 3,
#'   date_folder = "20260101"
#' )
#' # Output: output/SMMs/20260101/menses_centered/dep/group_labels_g3.csv
#'
#' # Programmatic labeling workflow
#' template <- label_smm_groups(..., interactive = FALSE)
#' template$group_label <- c("Luteal-Peak", "Stable", "Perimenstrual")
#' save_group_labels(template, ...)
#'
#' @seealso
#' \code{\link{label_smm_groups}} to create label mappings
#' \code{\link{load_group_labels}} to load saved labels
#'
#' @export
save_group_labels <- function(labels_df, base_save_dir, outcome, centering, g, date_folder) {
  # Build path to SMM results folder
  centering_folder <- .get_centering_folder(centering)
  smm_folder <- file.path(base_save_dir, date_folder, centering_folder, outcome)
  
  # Create folder if it doesn't exist
  if (!dir.exists(smm_folder)) {
    dir.create(smm_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Save CSV
  csv_path <- file.path(smm_folder, glue("group_labels_g{g}.csv"))
  write.csv(labels_df, file = csv_path, row.names = FALSE)
  
  message(glue("Group labels saved: group_labels_g{g}.csv"))
  
  invisible(csv_path)
}


#' Load Group Labels from CSV
#'
#' Loads previously saved SMM group label mappings from CSV file. This enables
#' reproducible labeling across multiple analysis sessions and ensures consistent
#' terminology when revisiting saved SMM results.
#'
#' The function expects a CSV file created by save_group_labels() with columns:
#' - smm_group: Character vector of group numbers
#' - group_label: Character vector of descriptive labels
#'
#' This is particularly useful for:
#' - Reproducing manuscript analyses with saved label definitions
#' - Sharing label conventions across collaborators
#' - Applying consistent labels when rerunning analyses with updated data
#'
#' @param base_save_dir Character string specifying base directory where SMM 
#'   results are saved
#' @param outcome Character string with outcome variable technical name
#' @param centering Character string: "menses" or "ovulation"
#' @param g Integer specifying number of groups
#' @param date_folder Character string with date folder name (YYYYMMDD format)
#'
#' @return Dataframe with columns:
#'   - smm_group: Character vector of group numbers ("1", "2", ...)
#'   - group_label: Character vector of loaded descriptive labels
#'
#' @details
#' Expected file path:
#'   base_save_dir/YYYYMMDD/{centering}_centered/outcome/group_labels_g{g}.csv
#'
#' The function stops with an error if the file doesn't exist, providing the
#' expected path for troubleshooting.
#'
#' @examples
#' # Load labels saved from previous session
#' labels <- load_group_labels(
#'   base_save_dir = "output/SMMs",
#'   outcome = "dep",
#'   centering = "menses",
#'   g = 3,
#'   date_folder = "20260101"
#' )
#'
#' # Use loaded labels to classify new data
#' class_labeled <- apply_group_labels(new_smm_result$class, labels)
#'
#' # Load labels from specific analysis date
#' old_labels <- load_group_labels(
#'   base_save_dir = "output/SMMs",
#'   outcome = "crave_alc_pm",
#'   centering = "ovulation",
#'   g = 4,
#'   date_folder = "20251215"
#' )
#'
#' @seealso
#' \code{\link{save_group_labels}} to save labels
#' \code{\link{label_smm_groups}} to create new labels
#' \code{\link{apply_group_labels}} to apply loaded labels to data
#'
#' @export
load_group_labels <- function(base_save_dir, outcome, centering, g, date_folder) {
  # Build path to CSV file
  centering_folder <- .get_centering_folder(centering)
  csv_path <- file.path(base_save_dir, date_folder, centering_folder, outcome, glue("group_labels_g{g}.csv"))
  
  # Check if file exists
  if (!file.exists(csv_path)) {
    stop(glue("Label file not found: {csv_path}"))
  }
  
  # Load CSV
  labels_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # Ensure smm_group is character
  labels_df$smm_group <- as.character(labels_df$smm_group)
  
  message(glue("Loaded group labels from: {csv_path}"))
  
  return(labels_df)
}


#' Apply Group Labels to SMM Classification Dataframe
#'
#' Adds meaningful group labels to an SMM classification dataframe by joining
#' with a label mapping. This function preserves all existing columns (including
#' posterior probability columns) while adding a new 'group_label' column that
#' provides interpretable names for latent groups.
#'
#' The resulting labeled dataframe can be:
#' - Merged with main analysis dataset for group-stratified analyses
#' - Used to create demographic tables by trajectory group
#' - Exported for manuscript tables with interpretable group names
#' - Visualized with meaningful legend labels instead of numbers
#'
#' The function ensures compatibility by:
#' - Converting smm_group to character in both inputs (handles factor/numeric/character)
#' - Removing existing group_label column if present (prevents duplicate columns)
#' - Warning if any IDs lack matching labels (data quality check)
#'
#' @param class_df Classification dataframe from run_smm_cyclic() containing:
#'   - id: Participant identifier
#'   - smm_group: Group assignment (numeric or character)
#'   - prob_group1, prob_group2, ... : Posterior probabilities (optional but preserved)
#' @param labels_df Label mapping dataframe with columns:
#'   - smm_group: Group numbers as character ("1", "2", ...)
#'   - group_label: Descriptive text labels
#'   (Typically from label_smm_groups() or load_group_labels())
#'
#' @return Modified dataframe with added 'group_label' column. All original
#'   columns (id, smm_group, posterior probabilities) are preserved.
#'
#' @details
#' The join is a left join on smm_group, so all rows in class_df are retained
#' even if some lack matching labels (these get NA for group_label, triggering
#' a warning).
#'
#' Best practices:
#' - Verify labels_df has labels for all groups present in class_df
#' - Check for NA group_labels after applying (indicates missing label mapping)
#' - Use labeled data immediately or save for reproducibility
#'
#' @examples
#' # Standard workflow
#' smm_result <- run_smm_cyclic(data = df, outcome = "dep", g = 3, ...)
#' labels <- label_smm_groups(base_save_dir = "output/SMMs", outcome = "dep", g = 3)
#' class_labeled <- apply_group_labels(smm_result$class, labels)
#'
#' # Merge with main dataset for group comparisons
#' df_with_groups <- df %>%
#'   left_join(class_labeled %>% select(id, group_label), by = "id")
#'
#' # Create demographic table by trajectory group
#' df_with_groups %>%
#'   group_by(group_label) %>%
#'   summarise(mean_age = mean(age), n = n_distinct(id))
#'
#' # Use in plotting with meaningful labels
#' ggplot(df_with_groups, aes(x = cycleday, y = dep, color = group_label)) +
#'   geom_smooth()
#'
#' # Apply previously saved labels
#' old_labels <- load_group_labels(base_save_dir = "output/SMMs", 
#'                                 outcome = "anxious", g = 4, date_folder = "20251201")
#' new_class_labeled <- apply_group_labels(new_smm$class, old_labels)
#'
#' @seealso
#' \code{\link{run_smm_cyclic}} for generating class_df
#' \code{\link{label_smm_groups}} for creating labels_df
#' \code{\link{load_group_labels}} for loading saved labels
#'
#' @export
apply_group_labels <- function(class_df, labels_df) {
  # Validate inputs
  if (!"smm_group" %in% names(class_df)) {
    stop("class_df must have an 'smm_group' column")
  }
  
  if (!all(c("smm_group", "group_label") %in% names(labels_df))) {
    stop("labels_df must have 'smm_group' and 'group_label' columns")
  }
  
  # Ensure smm_group is character in both dataframes
  class_df$smm_group <- as.character(class_df$smm_group)
  labels_df$smm_group <- as.character(labels_df$smm_group)
  
  # Remove group_label column if it already exists
  if ("group_label" %in% names(class_df)) {
    class_df <- class_df %>% select(-group_label)
  }
  
  # Join labels
  class_df <- class_df %>%
    left_join(labels_df %>% select(smm_group, group_label), by = "smm_group")
  
  # Check for missing labels
  n_missing <- sum(is.na(class_df$group_label))
  if (n_missing > 0) {
    warning(glue("{n_missing} rows have no matching label in labels_df"))
  }
  
  message("Group labels applied successfully.")
  
  return(class_df)
}
