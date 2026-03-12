# =============================================================================
# TRAIT HARMONIZATION HELPER FUNCTIONS
# =============================================================================
# Functions for importing, cleaning, recoding, and merging the UKALC and
# ADHD-Cycle participant-level trait datasets.
#
# WHAT IT DOES:
# - Provides a consistent CSV import and cleaning routine
# - Recodes UKALC race/sexorient/medication variables
# - Recodes ADHD categorical codes to labeled text using project codebooks
# - Converts ADHD anthropometric units (height inches→cm; weight lbs→kg)
# - Prefixes participant IDs and tracks dataset source
#
# USAGE:
#   source("scripts/harmonize_traits.R")
#   ukalc_traits <- import_clean_trait_csv(ukalc_path)
#   adhd_traits  <- import_clean_trait_csv(adhd_path)
#   ukalc_recoded <- recode_ukalc_traits(ukalc_traits)
#   adhd_recoded  <- recode_adhd_traits(adhd_traits)
#   outputs <- merge_trait_datasets(ukalc_recoded, adhd_recoded)
#   # outputs$common  — intersecting columns only
#   # outputs$union   — all columns (bind_rows)
# =============================================================================

# ---------------------------------------------------------------------------
# CODEBOOKS
# ---------------------------------------------------------------------------

#' ADHD-Cycle Categorical Codebooks
#'
#' Named lists mapping raw numeric codes to human-readable labels for the four
#' ADHD-Cycle trait variables that are stored as integers in the source CSV.
#' These codebooks are used by \code{\link{recode_adhd_traits}}.
#'
#' @format A named list with elements \code{Gender}, \code{EthnicityRace},
#'   \code{Sexuality}, and \code{Education}. Each element is a named character
#'   vector where names are the numeric codes (as strings) and values are the
#'   corresponding labels.
#'
#' @export
ADHD_CODEBOOKS <- list(
  Gender = c(
    "1" = "Female",
    "2" = "Nonbinary",
    "3" = "Agender"
  ),
  EthnicityRace = c(
    "1" = "White",
    "2" = "Black",
    "3" = "Hispanic",
    "4" = "Asian Indian",
    "5" = "Hispanic/white",
    "6" = "Black/white",
    "7" = "Black/American Indian",
    "8" = "Multiracial",
    "9" = "Middle Eastern / North African"
  ),
  Sexuality = c(
    "1" = "Heterosexual",
    "2" = "Bisexual/pansexual",
    "3" = "Gay/lesbian",
    "4" = "Asexual",
    "5" = "Queer"
  ),
  Education = c(
    "1" = "Graduated high school/high school equivalent",
    "2" = "Some college/trade school",
    "3" = "Graduated 2-year college/trade school",
    "4" = "Graduated 4-year college",
    "5" = "Some graduate/professional school",
    "6" = "Completed graduate/professional school"
  )
)


# ---------------------------------------------------------------------------
# IMPORT & CLEAN
# ---------------------------------------------------------------------------

#' Import and Clean a Trait CSV File
#'
#' Reads a CSV file containing participant-level (trait) data and applies a
#' standard battery of cleaning steps used throughout the UKALC pipeline:
#' \itemize{
#'   \item Standardise column names to snake_case via \code{janitor::clean_names()}.
#'   \item Trim leading/trailing whitespace from all character columns.
#'   \item Convert blank strings (\code{""} and whitespace-only strings) to
#'     \code{NA}.
#'   \item Drop rows that are entirely \code{NA} (fully blank rows).
#' }
#'
#' @param path Character. Absolute or relative path to the CSV file.
#' @param ... Additional arguments passed to \code{readr::read_csv()} (e.g.
#'   \code{col_types}, \code{locale}).
#'
#' @return A tibble with cleaned column names and no blank rows.
#'
#' @examples
#' df <- import_clean_trait_csv("testdata/traits/ukalc_trait_example.csv")
#'
#' @seealso \code{\link{recode_ukalc_traits}}, \code{\link{recode_adhd_traits}}
import_clean_trait_csv <- function(path, ...) {

  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  df <- readr::read_csv(
    path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    name_repair = "unique_quiet",
    ...
  )

  # Strip UTF-8 BOM from column names if present
  names(df) <- sub("^\ufeff", "", names(df))

  # Standardise to snake_case
  df <- janitor::clean_names(df)

  # Trim whitespace and convert blank strings to NA across all columns
  df <- dplyr::mutate(df, dplyr::across(
    dplyr::where(is.character),
    ~ dplyr::na_if(stringr::str_trim(.), "")
  ))

  # Drop rows that are entirely NA
  df <- dplyr::filter(df, !dplyr::if_all(dplyr::everything(), is.na))

  df
}


# ---------------------------------------------------------------------------
# UKALC RECODING
# ---------------------------------------------------------------------------

#' Recode UKALC Trait Variables
#'
#' Applies UKALC-specific recodes to a cleaned trait data frame returned by
#' \code{\link{import_clean_trait_csv}}:
#'
#' \describe{
#'   \item{ID prefixing}{Adds \code{"ALC_"} prefix to match the established
#'     convention in \code{UKALC_02_merge_adhd.Rmd}; preserves the original
#'     numeric ID in \code{original_id}.  Missing \code{id} values remain
#'     \code{NA} (never produce the string \code{"ALC_NA"}).}
#'   \item{source column}{Sets \code{source = "UKALC"}.}
#'   \item{trait_firststudyperiod}{Parses the date column using
#'     \code{lubridate::mdy()} (expected format: MM/DD/YYYY).}
#'   \item{race normalisation}{
#'     Case-insensitive matching:
#'     \itemize{
#'       \item "white" → \code{"White"}
#'       \item "black" (and variants like "Black or African American") → \code{"Black"}
#'       \item "multiracial" / "multi-racial" / "multiracial/multiethnic" →
#'         \code{"Multiracial"}
#'       \item All other non-NA values → \code{stringr::str_to_sentence()} (first
#'         letter capitalised, rest lower).
#'     }
#'   }
#'   \item{sexorient harmonisation}{
#'     Case-insensitive matching:
#'     \itemize{
#'       \item "straight" → \code{"Heterosexual"}
#'       \item "bisexual" → \code{"Bisexual/pansexual"}
#'       \item "gay", "lesbian", or "homosexual" → \code{"Gay/lesbian"}
#'       \item All other non-NA values preserved as-is (e.g. "Asexual", "Queer").
#'     }
#'   }
#'   \item{medication text}{If \code{med_user == 1}, copies
#'     \code{medication_description} into a new column
#'     \code{current_meds_text}; otherwise \code{NA_character_}.  Both
#'     \code{med_user} and \code{medication_description} must exist; if either is
#'     absent the column is created as all-\code{NA}.}
#' }
#'
#' @param df A tibble / data frame produced by
#'   \code{\link{import_clean_trait_csv}} for the UKALC trait CSV.
#'
#' @return The same data frame with recoded/added columns.
#'
#' @examples
#' df <- import_clean_trait_csv("testdata/traits/ukalc_trait_example.csv")
#' df_recoded <- recode_ukalc_traits(df)
#' # IDs become "ALC_101", "ALC_102", etc.; original stored in original_id
#'
#' @seealso \code{\link{recode_adhd_traits}}, \code{\link{merge_trait_datasets}}
recode_ukalc_traits <- function(df) {

  # --- ID prefix -----------------------------------------------------------
  # Use "ALC_" prefix to match the daily-level pipeline convention
  # (UKALC_02_merge_adhd.Rmd uses ALC_ for UKALC participants).
  # NA ids are preserved as NA rather than producing "ALC_NA".
  df <- dplyr::mutate(df,
    original_id = id,
    id          = dplyr::if_else(is.na(id), NA_character_, paste0("ALC_", id)),
    source      = "UKALC"
  )

  # --- Date parsing --------------------------------------------------------
  if ("trait_firststudyperiod" %in% names(df)) {
    df <- dplyr::mutate(df,
      trait_firststudyperiod = lubridate::mdy(trait_firststudyperiod)
    )
  }

  # --- Race normalisation --------------------------------------------------
  if ("race" %in% names(df)) {
    # Pre-compute lower-case once to avoid redundant str_to_lower() calls
    df <- dplyr::mutate(df, race_lc_ = stringr::str_to_lower(race))
    df <- dplyr::mutate(df,
      race = dplyr::case_when(
        stringr::str_detect(race_lc_, "^white$") ~ "White",
        stringr::str_detect(race_lc_, "white") &
          !stringr::str_detect(race_lc_, "black|hispanic|asian|indian|multiracial|multi") ~ "White",
        stringr::str_detect(race_lc_, "^black$|^black or african american$|^african american$") ~ "Black",
        stringr::str_detect(race_lc_, "^multiracial$|^multi-racial$|^multiracial/multiethnic$") ~ "Multiracial",
        is.na(race) ~ NA_character_,
        TRUE ~ stringr::str_to_sentence(race)
      )
    )
    df <- dplyr::select(df, -race_lc_)
  }

  # --- Sexual orientation harmonisation ------------------------------------
  if ("sexorient" %in% names(df)) {
    df <- dplyr::mutate(df,
      sexorient = dplyr::case_when(
        stringr::str_to_lower(sexorient) == "straight"   ~ "Heterosexual",
        stringr::str_to_lower(sexorient) == "bisexual"   ~ "Bisexual/pansexual",
        stringr::str_to_lower(sexorient) %in% c("gay", "lesbian", "homosexual") ~ "Gay/lesbian",
        is.na(sexorient) ~ NA_character_,
        TRUE ~ sexorient
      )
    )
  }

  # --- Medication text ------------------------------------------------------
  has_med_user  <- "med_user"              %in% names(df)
  has_med_desc  <- "medication_description" %in% names(df)

  if (has_med_user && has_med_desc) {
    df <- dplyr::mutate(df,
      current_meds_text = dplyr::if_else(
        !is.na(med_user) & as.integer(as.character(med_user)) == 1L,
        medication_description,
        NA_character_
      )
    )
  } else {
    df <- dplyr::mutate(df, current_meds_text = NA_character_)
  }

  df
}


# ---------------------------------------------------------------------------
# ADHD RECODING
# ---------------------------------------------------------------------------

#' Apply a Codebook Mapping to an Integer or Character Column
#'
#' Looks up each value of \code{x} in the codebook named character vector
#' \code{mapping} (where names are the codes and values are the labels).
#' Unrecognised non-\code{NA} codes are returned as \code{NA} with a warning.
#'
#' @param x A vector of values to recode (will be coerced to character for
#'   lookup).
#' @param mapping A named character vector: \code{c("1" = "Female", "2" = "Nonbinary", ...)}.
#' @param col_name Character. Column name used in any warning messages.
#'
#' @return A character vector of the same length as \code{x} with labels
#'   substituted for codes.
apply_codebook <- function(x, mapping, col_name = "column") {
  # Convert to character but keep NA as NA_character_ (not the string "NA")
  x_char <- as.character(x)
  x_char[is.na(x)] <- NA_character_

  result <- mapping[x_char]
  names(result) <- NULL   # drop names from index lookup

  unknown <- !is.na(x_char) & is.na(result)
  if (any(unknown)) {
    warning(
      "apply_codebook: unrecognised code(s) in '", col_name, "': ",
      paste(unique(x_char[unknown]), collapse = ", "),
      call. = FALSE
    )
  }

  result
}


#' Recode ADHD-Cycle Trait Variables
#'
#' Applies ADHD-Cycle-specific recodes to a cleaned trait data frame returned
#' by \code{\link{import_clean_trait_csv}}:
#'
#' \describe{
#'   \item{ID prefixing}{Adds \code{"ADHD_"} prefix without zero-padding,
#'     matching the convention in \code{UKALC_02_merge_adhd.Rmd}
#'     (e.g. ID 1 → \code{"ADHD_1"}).  Preserves original numeric ID in
#'     \code{original_id}.  Missing \code{id} values remain \code{NA}
#'     (never produce the string \code{"ADHD_NA"}).}
#'   \item{source column}{Sets \code{source = "ADHD"}.}
#'   \item{firstperiod}{Parses the date column using \code{lubridate::ymd()}.
#'     \emph{Format assumption:} the ADHD-Cycle CSV stores this field as
#'     ISO 8601 (YYYY-MM-DD).  If parsing fails for any rows, those values
#'     become \code{NA} and a warning is issued.}
#'   \item{Unit conversions}{
#'     \itemize{
#'       \item \code{height} (inches) → \code{height_cm}: multiply by 2.54.
#'       \item \code{weight} (pounds) → \code{weight_kg}: multiply by 0.453592.
#'       \item Originals retained as \code{height_in} and \code{weight_lb}.
#'     }
#'   }
#'   \item{Categorical recoding}{Each codebook variable is recoded to labeled
#'     text using \code{\link{ADHD_CODEBOOKS}}.  The original integer column is
#'     kept alongside its labeled counterpart with suffix \code{_label}:
#'     \itemize{
#'       \item \code{gender} → \code{gender_label}
#'       \item \code{ethnicity_race} → \code{ethnicity_race_label}
#'       \item \code{sexuality} → \code{sexuality_label}
#'       \item \code{education} → \code{education_label}
#'     }
#'   }
#' }
#'
#' @param df A tibble / data frame produced by
#'   \code{\link{import_clean_trait_csv}} for the ADHD-Cycle trait CSV.
#'   Column names are expected to be in snake_case (as produced by
#'   \code{janitor::clean_names()}).
#'
#' @return The same data frame with recoded/added columns.
#'
#' @examples
#' df <- import_clean_trait_csv("testdata/traits/adhd_trait_example.csv")
#' df_recoded <- recode_adhd_traits(df)
#' # IDs become "ADHD_1", "ADHD_2", etc.; original stored in original_id
#'
#' @seealso \code{\link{recode_ukalc_traits}}, \code{\link{merge_trait_datasets}},
#'   \code{\link{ADHD_CODEBOOKS}}
recode_adhd_traits <- function(df) {

  # --- ID prefix -----------------------------------------------------------
  # Use "ADHD_" prefix without zero-padding to match the daily-level pipeline
  # convention (UKALC_02_merge_adhd.Rmd uses paste0("ADHD_", id) directly).
  # NA ids are preserved as NA rather than producing "ADHD_NA".
  df <- dplyr::mutate(df,
    original_id = id,
    id          = dplyr::if_else(is.na(id), NA_character_, paste0("ADHD_", id)),
    source      = "ADHD"
  )

  # --- Date parsing --------------------------------------------------------
  # ADHD-Cycle stores firstperiod in ISO 8601 (YYYY-MM-DD) format.
  if ("firstperiod" %in% names(df)) {
    df <- dplyr::mutate(df,
      firstperiod = lubridate::ymd(firstperiod)
    )
  }

  # --- Unit conversions ----------------------------------------------------
  if ("height" %in% names(df)) {
    df <- dplyr::mutate(df,
      height_in = height,
      height_cm = round(height * 2.54, 1)
    ) %>% dplyr::select(-height)
  }

  if ("weight" %in% names(df)) {
    df <- dplyr::mutate(df,
      weight_lb = weight,
      weight_kg = round(weight * 0.453592, 2)
    ) %>% dplyr::select(-weight)
  }

  # --- Categorical codebook recodes ----------------------------------------
  # Map snake_case column name → codebook key
  codebook_cols <- c(
    gender         = "Gender",
    ethnicity_race = "EthnicityRace",
    sexuality      = "Sexuality",
    education      = "Education"
  )

  for (col in names(codebook_cols)) {
    if (col %in% names(df)) {
      cb_key   <- codebook_cols[[col]]
      label_col <- paste0(col, "_label")
      df <- dplyr::mutate(df,
        !!label_col := apply_codebook(.data[[col]], ADHD_CODEBOOKS[[cb_key]], col_name = col)
      )
    }
  }

  df
}


# ---------------------------------------------------------------------------
# MERGE
# ---------------------------------------------------------------------------

#' Merge UKALC and ADHD-Cycle Trait Datasets
#'
#' Combines the two recoded trait data frames (returned by
#' \code{\link{recode_ukalc_traits}} and \code{\link{recode_adhd_traits}})
#' into two output forms:
#'
#' \describe{
#'   \item{common}{Contains only the columns that appear in \emph{both}
#'     datasets (intersection).  Produced by selecting the intersecting column
#'     names then using \code{dplyr::bind_rows()}.}
#'   \item{union}{Contains \emph{all} columns from both datasets.  Columns
#'     absent in one dataset are filled with \code{NA}.  Produced with
#'     \code{dplyr::bind_rows()}.}
#' }
#'
#' Both outputs are sorted by \code{source} then \code{id} for reproducibility.
#'
#' @param ukalc_df A recoded UKALC trait data frame (output of
#'   \code{\link{recode_ukalc_traits}}).
#' @param adhd_df A recoded ADHD-Cycle trait data frame (output of
#'   \code{\link{recode_adhd_traits}}).
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{common}}{Tibble with intersection columns only.}
#'     \item{\code{union}}{Tibble with all columns (union).}
#'   }
#'
#' @examples
#' ukalc_df  <- recode_ukalc_traits(import_clean_trait_csv(
#'   "testdata/traits/ukalc_trait_example.csv"))
#' adhd_df   <- recode_adhd_traits(import_clean_trait_csv(
#'   "testdata/traits/adhd_trait_example.csv"))
#' merged <- merge_trait_datasets(ukalc_df, adhd_df)
#' # merged$common  — intersecting columns only
#' # merged$union   — all columns
#'
#' @seealso \code{\link{recode_ukalc_traits}}, \code{\link{recode_adhd_traits}}
merge_trait_datasets <- function(ukalc_df, adhd_df) {

  common_cols <- intersect(names(ukalc_df), names(adhd_df))

  merged_common <- dplyr::bind_rows(
    dplyr::select(ukalc_df, dplyr::all_of(common_cols)),
    dplyr::select(adhd_df,  dplyr::all_of(common_cols))
  ) %>% dplyr::arrange(source, id)

  merged_union <- dplyr::bind_rows(ukalc_df, adhd_df) %>%
    dplyr::arrange(source, id)

  list(common = merged_common, union = merged_union)
}
