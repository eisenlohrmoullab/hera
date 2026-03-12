## ============================================================================
## EXAMPLE: Using SMM Group Labeling Functions
## ============================================================================
## This example demonstrates how to use the label_smm_groups.R functions
## to interactively label SMM groups and apply those labels to results.
##
## Source this file or run code chunks interactively after running SMM analyses
## ============================================================================

# Load required libraries
library(tidyverse)
library(glue)

# Source the labeling functions
source("scripts/label_smm_groups.R")

## ============================================================================
## EXAMPLE 1: Interactive Labeling Workflow
## ============================================================================

# After running SMM analyses in UKALC_06_smms.Rmd, you can interactively label groups:

# 1. Run interactive labeling for a specific outcome and group size
# This will prompt you to view plots and enter labels for each group
labels_dep_g3 <- label_smm_groups(
  base_save_dir = "output/SMMs_Ov_and_Borderline",  # Your SMM output folder
  outcome = "dep",                                   # Outcome variable
  centering = "menses",                              # "menses" or "ovulation"
  g = 3,                                            # Number of groups
  date_folder = "20260101",                         # Date folder (YYYYMMDD)
  interactive = TRUE                                # Interactive mode
)

# 2. Save the labels for future use
save_group_labels(
  labels_df = labels_dep_g3,
  base_save_dir = "output/SMMs_Ov_and_Borderline",
  outcome = "dep",
  centering = "menses",
  g = 3,
  date_folder = "20260101"
)

# 3. Later, load the saved labels
labels_dep_g3 <- load_group_labels(
  base_save_dir = "output/SMMs_Ov_and_Borderline",
  outcome = "dep",
  centering = "menses",
  g = 3,
  date_folder = "20260101"
)

# 4. Apply labels to the classification dataframe from your SMM results
# Assuming you have stored your SMM results in all_smm_results
smm_result <- all_smm_results[["dep_menses"]]
class_df <- smm_result$all_results[["3"]]$class  # Get classification for g=3

# Apply labels - this preserves all columns including posterior probabilities
class_labeled <- apply_group_labels(class_df, labels_dep_g3)

# View the result
head(class_labeled)


## ============================================================================
## EXAMPLE 2: Non-Interactive (Template) Mode
## ============================================================================

# If you want to create labels programmatically or edit them manually:

# 1. Create a template dataframe
labels_template <- label_smm_groups(
  base_save_dir = "output/SMMs_Ov_and_Borderline",
  outcome = "anxious",
  centering = "menses",
  g = 2,
  date_folder = "20260101",
  interactive = FALSE  # Non-interactive mode
)

# 2. Manually fill in labels
labels_template$group_label[1] <- "High-Anxiety"
labels_template$group_label[2] <- "Low-Anxiety"

# 3. Save the labels
save_group_labels(
  labels_df = labels_template,
  base_save_dir = "output/SMMs_Ov_and_Borderline",
  outcome = "anxious",
  centering = "menses",
  g = 2,
  date_folder = "20260101"
)


## ============================================================================
## EXAMPLE 3: Batch Labeling Multiple Outcomes
## ============================================================================

# Define labels for multiple outcomes at once
# This is useful when you want consistent labeling across similar patterns

outcomes_to_label <- c("dep", "anxious", "angry", "mood_swing")
g <- 3
date_folder <- "20260101"

# Create a list to store all labels
all_labels <- list()

for (outcome in outcomes_to_label) {
  message(glue("\n--- Labeling {outcome} ---"))
  
  # Interactive labeling
  labels <- label_smm_groups(
    base_save_dir = "output/SMMs_Ov_and_Borderline",
    outcome = outcome,
    centering = "menses",
    g = g,
    date_folder = date_folder,
    interactive = TRUE
  )
  
  # Save labels
  save_group_labels(
    labels_df = labels,
    base_save_dir = "output/SMMs_Ov_and_Borderline",
    outcome = outcome,
    centering = "menses",
    g = g,
    date_folder = date_folder
  )
  
  # Store in list
  all_labels[[outcome]] <- labels
}


## ============================================================================
## EXAMPLE 4: Apply Labels to All Results
## ============================================================================

# After labeling, apply to all your SMM classification results

# Assuming all_smm_results is your list of SMM results from UKALC_06_smms.Rmd
date_folder <- "20260101"
g <- 3

for (outcome in names(all_smm_results)) {
  # Parse outcome and centering from list name (e.g., "dep_menses")
  parts <- strsplit(outcome, "_")[[1]]
  outcome_name <- parts[1]
  centering <- parts[2]
  
  # Check if labels exist
  labels_path <- file.path(
    "output/SMMs_Ov_and_Borderline",
    date_folder,
    ifelse(centering == "ovulation", "ovulation_centered", "menses_centered"),
    outcome_name,
    glue("group_labels_g{g}.csv")
  )
  
  if (file.exists(labels_path)) {
    # Load labels
    labels <- load_group_labels(
      base_save_dir = "output/SMMs_Ov_and_Borderline",
      outcome = outcome_name,
      centering = centering,
      g = g,
      date_folder = date_folder
    )
    
    # Apply to classification dataframe
    class_df <- all_smm_results[[outcome]]$all_results[[as.character(g)]]$class
    class_labeled <- apply_group_labels(class_df, labels)
    
    # Store back or save
    all_smm_results[[outcome]]$all_results[[as.character(g)]]$class_labeled <- class_labeled
    
    message(glue("✓ Applied labels to {outcome}"))
  } else {
    message(glue("⚠ No labels found for {outcome}"))
  }
}


## ============================================================================
## EXAMPLE 5: Using Labels in Further Analyses
## ============================================================================

# Once labels are applied, you can use them in analyses and visualizations

# Example: Merge labeled classifications with your main dataframe
class_labeled <- all_smm_results[["dep_menses"]]$all_results[["3"]]$class_labeled

# Merge with main data
df_with_labels <- df %>%
  left_join(
    class_labeled %>% select(id, smm_group, group_label),
    by = "id"
  )

# Now you can use meaningful labels in plots and analyses
df_with_labels %>%
  group_by(group_label) %>%
  summarise(
    n = n_distinct(id),
    mean_dep = mean(dep, na.rm = TRUE),
    sd_dep = sd(dep, na.rm = TRUE)
  )

# Create a plot with meaningful labels
ggplot(df_with_labels, aes(x = cyclic_time_impute, y = dep, color = group_label)) +
  geom_smooth(method = "loess") +
  labs(
    title = "Depression by SMM Group",
    x = "Cycle Time",
    y = "Depression Score",
    color = "Pattern Type"
  ) +
  theme_minimal()


## ============================================================================
## TIPS AND NOTES
## ============================================================================

# 1. Label naming conventions:
#    - Use descriptive names that capture the pattern (e.g., "Luteal-Peak")
#    - Be consistent across related outcomes
#    - Avoid spaces (use hyphens or underscores)
#    - Keep labels concise but meaningful

# 2. When to label:
#    - After reviewing SMM plots and understanding group patterns
#    - Before sharing results or creating final visualizations
#    - When preparing manuscripts or presentations

# 3. Label files are saved as CSV in the same folder as SMM results:
#    {base_save_dir}/{date_folder}/{centering_folder}/{outcome}/group_labels_g{g}.csv

# 4. Labels are backward compatible:
#    - Existing SMM analyses work without labels
#    - Labels are optional and can be added anytime
#    - Original smm_group numbers are preserved

# 5. Posterior probabilities are preserved:
#    - apply_group_labels() keeps all prob_group* columns
#    - You can use these for sensitivity analyses
