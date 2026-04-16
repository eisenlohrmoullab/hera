# =============================================================================
# DRSP GAM Template — Composite Scoring
# =============================================================================
# Automatically detects which DRSP items are present in the dataset using
# flexible column-name matching. Any column containing "drsp" or "DRSP" plus
# a number 1-21 is recognized and renamed to the canonical form drsp_N.
#
# Supported formats (all map to drsp_1):
#   drsp_1, drsp1, DRSP1, DRSP_1, DRSP__1, drsp.1, DRSP1_depblue, etc.
#
# After renaming, composite scores are computed for any composite whose
# component items are all available. Composites requiring missing items
# are skipped with a message.
#
# Usage:
#   source("drsp_gam_scoring.R")
#   result <- score_drsp_composites(df)
#   df <- result$data
#   available_composites <- result$composites_created
#   available_items      <- result$items_found
#   rename_log           <- result$columns_renamed
# =============================================================================

# --- Master DRSP item labels (1-6 Likert scale) ---
drsp_item_labels <- c(
  drsp_1  = "Depression",
  drsp_2  = "Hopelessness",
  drsp_3  = "Worthlessness",
  drsp_4  = "Anxiety",
  drsp_5  = "Mood Swings",
  drsp_6  = "Rejection Sensitivity",
  drsp_7  = "Anger/Irritability",
  drsp_8  = "Interpersonal Conflicts",
  drsp_9  = "Loss of Interest",
  drsp_10 = "Difficulty Concentrating",
  drsp_11 = "Fatigue/Lethargy",
  drsp_12 = "Overeating",
  drsp_13 = "Specific Food Cravings",
  drsp_14 = "Hypersomnia",
  drsp_15 = "Insomnia",
  drsp_16 = "Feeling Overwhelmed",
  drsp_17 = "Feeling Out of Control",
  drsp_18 = "Breast Tenderness",
  drsp_19 = "Breast Swelling",
  drsp_20 = "Headache",
  drsp_21 = "Joint/Muscle Pain"
)

# --- Composite definitions ---
# Each composite is the rowMean (na.rm = TRUE) of its component items.
drsp_composite_defs <- list(
  drsp_NA = list(
    label     = "DRSP Negative Affect Mean",
    domain    = "AFFECTIVE",
    items     = c("drsp_1", "drsp_2", "drsp_3", "drsp_4", "drsp_5", "drsp_6",
                  "drsp_7", "drsp_8", "drsp_9", "drsp_16", "drsp_17"),
    components_display = c(
      "Depression (1)", "Hopelessness (2)", "Worthlessness (3)",
      "Anxiety (4)", "Mood Swings (5)", "Rejection Sensitivity (6)",
      "Anger/Irritability (7)", "Interpersonal Conflicts (8)",
      "Loss of Interest (9)", "Feeling Overwhelmed (16)",
      "Feeling Out of Control (17)"
    )
  ),
  drsp_distress = list(
    label     = "Distress (DRSP)",
    domain    = "AFFECTIVE",
    items     = c("drsp_1", "drsp_2", "drsp_3", "drsp_4", "drsp_16", "drsp_17"),
    components_display = c(
      "Depression (1)", "Hopelessness (2)", "Worthlessness (3)",
      "Anxiety (4)", "Feeling Overwhelmed (16)",
      "Feeling Out of Control (17)"
    )
  ),
  drsp_labilsocsx = list(
    label     = "Social/Affect Lability (DRSP)",
    domain    = "AFFECTIVE",
    items     = c("drsp_5", "drsp_6", "drsp_7", "drsp_8"),
    components_display = c(
      "Mood Swings (5)", "Rejection Sensitivity (6)",
      "Anger/Irritability (7)", "Interpersonal Conflicts (8)"
    )
  ),
  drsp_lowarousal = list(
    label     = "Low Arousal Sx (DRSP)",
    domain    = "AFFECTIVE",
    items     = c("drsp_9", "drsp_10", "drsp_11", "drsp_14", "drsp_15"),
    components_display = c(
      "Loss of Interest (9)", "Difficulty Concentrating (10)",
      "Fatigue/Lethargy (11)", "Hypersomnia (14)", "Insomnia (15)"
    )
  ),
  drsp_breastpain = list(
    label     = "Breast Sx (DRSP)",
    domain    = "PHYSICAL",
    items     = c("drsp_18", "drsp_19"),
    components_display = c(
      "Breast Tenderness (18)", "Breast Swelling (19)"
    )
  ),
  drsp_eatsx = list(
    label     = "Eating Sx",
    domain    = "PHYSICAL",
    items     = c("drsp_12", "drsp_13"),
    components_display = c(
      "Overeating (12)", "Specific Food Cravings (13)"
    )
  ),
  drsp_total = list(
    label     = "DRSP Total Mean",
    domain    = "AFFECTIVE",
    items     = paste0("drsp_", 1:21),
    components_display = paste0(
      c("Depression", "Hopelessness", "Worthlessness", "Anxiety",
        "Mood Swings", "Rejection Sensitivity", "Anger/Irritability",
        "Interpersonal Conflicts", "Loss of Interest",
        "Difficulty Concentrating", "Fatigue/Lethargy",
        "Overeating", "Specific Food Cravings", "Hypersomnia",
        "Insomnia", "Feeling Overwhelmed", "Feeling Out of Control",
        "Breast Tenderness", "Breast Swelling", "Headache",
        "Joint/Muscle Pain"),
      " (", 1:21, ")"
    )
  )
)

# --- Physical symptom items (for domain classification) ---
physical_symptom_items <- c("drsp_12", "drsp_13", "drsp_14", "drsp_15",
                            "drsp_18", "drsp_19", "drsp_20", "drsp_21")

# =============================================================================
# detect_and_rename_drsp_columns()
# =============================================================================
# Scans all column names in a data.frame for any that look like DRSP items
# (contain "drsp" or "DRSP" plus a number 1-21) and renames them to the
# canonical format drsp_N.
#
# The regex is case-insensitive and tolerant of any separator (or none)
# between "drsp" and the item number.  Examples of recognized patterns:
#   drsp_1, drsp1, DRSP1, DRSP_1, DRSP__1, drsp.1,
#   DRSP1_depblue, drsp_1_raw, drsp.01, DRSP_01
#
# When a column's suffix (everything after the number) is non-empty, the
# function assumes the first matching column is the item of interest.
# If multiple columns map to the same drsp_N, only the first is kept and
# the user is warned.
#
# Arguments:
#   df — data.frame whose columns may contain DRSP items in any naming format
#
# Returns a list:
#   $data            — the data.frame with DRSP columns renamed to drsp_N
#   $columns_renamed — named character vector (original_name → canonical_name)
#                      for columns that were renamed (excludes already-canonical)
#   $columns_already_canonical — character vector of columns that were already drsp_N
# =============================================================================
detect_and_rename_drsp_columns <- function(df) {

  col_names <- names(df)
  columns_renamed <- character(0)
  columns_already_canonical <- character(0)

  # Pattern: "drsp" (case-insensitive), then any non-digit separators,
  # then a number (1-21), optionally followed by a non-digit or end-of-string
  # so that "drsp_2" doesn't match as item 2 in "drsp_21".
  #
  # We capture the item number and use a word-boundary-like approach:
  # the number must NOT be followed by another digit.
  drsp_regex <- "(?i)^.*drsp[^0-9]*(\\d+)(?!\\d).*$"

  # Track which canonical names have been claimed (to handle duplicates)
  claimed <- list()   # canonical_name → original_col_name

  for (col in col_names) {
    if (!grepl(drsp_regex, col, perl = TRUE)) next

    # Extract the item number
    item_num <- as.integer(sub(drsp_regex, "\\1", col, perl = TRUE))

    # Only recognize items 1-21
    if (is.na(item_num) || item_num < 1 || item_num > 21) next

    canonical <- paste0("drsp_", item_num)

    # Check if this column IS already canonical
    if (col == canonical) {
      columns_already_canonical <- c(columns_already_canonical, col)
      claimed[[canonical]] <- col
      next
    }

    # Check for duplicates: another column already maps to the same canonical
    if (!is.null(claimed[[canonical]])) {
      existing <- claimed[[canonical]]
      warning(sprintf(
        "DRSP Detection: Multiple columns map to %s: '%s' (kept) and '%s' (skipped). ",
        canonical, existing, col
      ), call. = FALSE)
      next
    }

    # Also check if the canonical name already exists as a different column
    if (canonical %in% col_names && canonical != col) {
      # The canonical column already exists natively — skip this fuzzy match
      warning(sprintf(
        "DRSP Detection: Column '%s' looks like %s, but '%s' already exists natively. Skipping rename.",
        col, canonical, canonical
      ), call. = FALSE)
      next
    }

    # Rename: copy data to canonical name and drop the original
    names(df)[names(df) == col] <- canonical
    columns_renamed[col] <- canonical
    claimed[[canonical]] <- col

    message(sprintf("DRSP Detection: Renamed '%s' -> '%s'", col, canonical))
  }

  n_found <- length(columns_renamed) + length(columns_already_canonical)
  if (n_found > 0) {
    message(sprintf(
      "DRSP Detection: Found %d DRSP item column(s) (%d renamed, %d already canonical)",
      n_found, length(columns_renamed), length(columns_already_canonical)
    ))
  }

  list(
    data                      = df,
    columns_renamed           = columns_renamed,
    columns_already_canonical = columns_already_canonical
  )
}

# =============================================================================
# score_drsp_composites()
# =============================================================================
# Detects which DRSP items are in the dataset, coerces them to numeric,
# computes composites where all component items are present, and returns
# metadata about what was found/created.
#
# Arguments:
#   df              — data.frame with DRSP item columns
#   compute_total   — logical; also compute drsp_total? (default FALSE, since
#                     not always useful and requires all 21 items)
#
# Returns a list with:
#   $data              — the modified data.frame with new composite columns
#   $items_found       — character vector of DRSP items found in data
#   $items_missing     — character vector of DRSP items NOT found
#   $composites_created — character vector of composite names added
#   $composites_skipped — named list: name → character vector of missing items
#   $composite_specs   — list of specs for created composites (for downstream use)
# =============================================================================
score_drsp_composites <- function(df, compute_total = FALSE) {

  all_possible_items <- paste0("drsp_", 1:21)
  items_found   <- intersect(all_possible_items, names(df))
  items_missing <- setdiff(all_possible_items, names(df))

  if (length(items_found) == 0) {
    stop("No DRSP items (drsp_1 ... drsp_21) found in the dataset. ",
         "Please ensure your data contains columns named drsp_N where N is 1-21.")
  }

  message(sprintf("DRSP Scoring: Found %d of 21 items: %s",
                  length(items_found), paste(items_found, collapse = ", ")))
  if (length(items_missing) > 0) {
    message(sprintf("DRSP Scoring: Missing %d items: %s",
                    length(items_missing), paste(items_missing, collapse = ", ")))
  }

  # Coerce found items to numeric
  for (item in items_found) {
    df[[item]] <- suppressWarnings(as.numeric(df[[item]]))
  }

  # Determine which composites to compute
  composites_to_try <- drsp_composite_defs
  if (!compute_total) {
    composites_to_try[["drsp_total"]] <- NULL
  }


  composites_created <- character(0)
  composites_skipped <- list()
  composite_specs    <- list()

  for (comp_name in names(composites_to_try)) {
    spec <- composites_to_try[[comp_name]]
    needed_items  <- spec$items
    missing_items <- setdiff(needed_items, items_found)

    if (length(missing_items) > 0) {
      composites_skipped[[comp_name]] <- missing_items
      message(sprintf("DRSP Scoring: Skipping %s — missing items: %s",
                      comp_name, paste(missing_items, collapse = ", ")))
      next
    }

    # Compute rowMeans with na.rm = TRUE
    df[[comp_name]] <- rowMeans(df[, needed_items, drop = FALSE], na.rm = TRUE)
    composites_created <- c(composites_created, comp_name)
    composite_specs[[comp_name]] <- spec
    message(sprintf("DRSP Scoring: Created %s (%s) from %d items",
                    comp_name, spec$label, length(needed_items)))
  }

  list(
    data               = df,
    items_found        = items_found,
    items_missing      = items_missing,
    composites_created = composites_created,
    composites_skipped = composites_skipped,
    composite_specs    = composite_specs
  )
}

# =============================================================================
# build_outcomes_map()
# =============================================================================
# Builds the named vector of outcome_variable → label for all available
# DRSP items and composites. Composites are listed first, then individual items.
#
# Arguments:
#   items_found        — character vector of drsp_N items present in data
#   composites_created — character vector of composite names that were computed
#
# Returns:
#   Named character vector (names = variable names, values = labels)
# =============================================================================
build_outcomes_map <- function(items_found, composites_created) {
  outcomes <- character(0)

  # Add composites first (excluding drsp_total which is informational)
  for (comp_name in composites_created) {
    if (comp_name == "drsp_total") next
    spec <- drsp_composite_defs[[comp_name]]
    if (!is.null(spec)) {
      outcomes[comp_name] <- spec$label
    }
  }

  # Add individual items in numeric order
  item_nums <- as.integer(sub("^drsp_", "", items_found))
  items_sorted <- items_found[order(item_nums)]
  for (item in items_sorted) {
    if (item %in% names(drsp_item_labels)) {
      outcomes[item] <- drsp_item_labels[[item]]
    } else {
      outcomes[item] <- item
    }
  }

  outcomes
}

# =============================================================================
# classify_outcomes()
# =============================================================================
# Classify outcomes into affective vs physical domains.
#
# Returns a list with:
#   $physical_items    — physical DRSP items found
#   $affective_items   — affective DRSP items found
#   $physical_composites — physical composite names created
#   $affective_composites — affective composite names created
#   $all_physical      — all physical outcomes (items + composites)
#   $all_affective     — all affective outcomes (items + composites)
# =============================================================================
classify_outcomes <- function(items_found, composite_specs) {
  phys_items <- intersect(items_found, physical_symptom_items)
  aff_items  <- setdiff(items_found, physical_symptom_items)

  phys_comps <- character(0)
  aff_comps  <- character(0)
  for (cname in names(composite_specs)) {
    if (composite_specs[[cname]]$domain == "PHYSICAL") {
      phys_comps <- c(phys_comps, cname)
    } else {
      aff_comps <- c(aff_comps, cname)
    }
  }

  list(
    physical_items      = phys_items,
    affective_items     = aff_items,
    physical_composites = phys_comps,
    affective_composites = aff_comps,
    all_physical        = c(phys_items, phys_comps),
    all_affective       = c(aff_items, aff_comps)
  )
}
