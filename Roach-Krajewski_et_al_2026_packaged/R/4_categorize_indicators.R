# categorize_indicators.R
# 
# Output:
# This script reads the finalized evidence map workbook,
# matches raw indicator names to grouped indicator categories
# using the "indicator_categories" sheet, appends the new
# indicator_cat_1 / indicator_cat_2 / indicator_cat_3 columns
# to the study data, and writes a new Excel workbook:
#
# data/4_final_study_data/evidence_map_data_with_indicator_categories.xlsx
#
# This script should be saved in:
# Roach-Krajewski_et_al_2026_packaged/R/
#
# Expected folder structure:
# Roach-Krajewski_et_al_2026_packaged/
# ├── data/
# │   ├── 4_final_study_data/
# │   │   ├── evidence_map_data.xlsx
# │   │   └── evidence_map_data_with_indicator_categories.xlsx
# │   └── 5_extra_figure_data/
# ├── R/
# │   └── FIGURES/
# └── figures/


# STEP 1: SET UP THE PROJECT ROOT AND LOAD PACKAGES ====================

## 1a) Find the project root folder --------------------------------
find_project_root <- function(expected_folder = "Roach-Krajewski_et_al_2026_packaged") {
  
  current_path <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  
  repeat {
    if (basename(current_path) == expected_folder) {
      return(current_path)
    }
    
    parent_path <- dirname(current_path)
    
    if (parent_path == current_path) {
      stop(
        paste0(
          "Could not find the project root folder named '",
          expected_folder,
          "'.\n",
          "Please make sure you are running this script from inside the packaged folder structure."
        ),
        call. = FALSE
      )
    }
    
    current_path <- parent_path
  }
}

## 1b) Store the project root path ---------------------------------
proj_root <- find_project_root()

## 1c) Set the working directory to the project root --------------
setwd(proj_root)

## 1d) Confirm the root folder in the console ----------------------
message("Project root set to: ", proj_root)

## 1e) Load required packages --------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(tibble)
  library(writexl)
})

## 1f) Define input and output paths -------------------------------
input_file      <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
study_sheet     <- "5_study_data"
category_sheet  <- "7_indicator_categories"
output_file     <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data_with_indicator_categories.xlsx")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Normalize semicolon-separated text --------------------------
# This keeps semicolon-separated values consistent before splitting.
split_semis <- function(x) {
  x %>%
    as.character() %>%
    replace_na("") %>%
    str_replace_all("\\s*;\\s*", ";")
}

## 2b) Collapse unique values back into one semicolon string -------
collapse_unique_values <- function(x) {
  x <- x %>%
    as.character() %>%
    str_squish()
  
  x <- x[!is.na(x) & x != ""]
  x <- unique(x)
  
  if (length(x) == 0) {
    return(NA_character_)
  } else {
    return(paste(x, collapse = "; "))
  }
}


# STEP 3: IMPORT THE SOURCE DATA =======================================

## 3a) Check that the input workbook exists ------------------------
if (!file.exists(input_file)) {
  stop(
    paste0(
      "The input file was not found:\n",
      input_file,
      "\n\nPlease confirm that the packaged folder structure is correct."
    ),
    call. = FALSE
  )
}

## 3b) Read the main study-data sheet ------------------------------
study_data <- read_excel(
  path         = input_file,
  sheet        = study_sheet,
  skip         = 1,
  na           = c("", "NA"),
  col_types    = "text",
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))

## 3c) Read the indicator-category sheet ---------------------------
indicator_categories <- read_excel(
  path         = input_file,
  sheet        = category_sheet,
  skip         = 0,
  na           = c("", "NA"),
  col_types    = "text",
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))

## 3d) Check that required columns are present ---------------------
required_study_cols <- c("study_id", "indicators_1", "indicators_2", "indicators_3")
required_cat_cols   <- c("Category_name", "Included_indicators")

missing_study_cols <- setdiff(required_study_cols, names(study_data))
missing_cat_cols   <- setdiff(required_cat_cols, names(indicator_categories))

if (length(missing_study_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the study-data sheet:\n",
      paste(missing_study_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (length(missing_cat_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the indicator-category sheet:\n",
      paste(missing_cat_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: BUILD THE INDICATOR LOOKUP TABLE ============================

## 4a) Expand the included indicator tokens ------------------------
# Each row in the category sheet may contain multiple indicator names
# separated by semicolons. This step creates one token per row.
indicator_lookup <- indicator_categories %>%
  transmute(
    Category_name        = str_squish(Category_name),
    Included_indicators  = split_semis(Included_indicators)
  ) %>%
  separate_rows(Included_indicators, sep = ";") %>%
  mutate(
    token      = str_squish(Included_indicators),
    token_norm = str_to_lower(token)
  ) %>%
  filter(
    !is.na(Category_name), Category_name != "",
    !is.na(token),         token != ""
  ) %>%
  distinct(token_norm, Category_name, token)

## 4b) Identify tokens that map to more than one category ----------
lookup_duplicates <- indicator_lookup %>%
  count(token_norm, name = "n_categories") %>%
  filter(n_categories > 1) %>%
  arrange(desc(n_categories), token_norm)


# STEP 5: MATCH RAW INDICATORS TO CATEGORIES ==========================

## 5a) Convert the three indicator columns to long format ----------
indicator_long <- study_data %>%
  select(study_id, indicators_1, indicators_2, indicators_3) %>%
  pivot_longer(
    cols      = starts_with("indicators_"),
    names_to  = "slot",
    values_to = "indicators_raw"
  ) %>%
  mutate(
    indicators_raw = split_semis(indicators_raw)
  ) %>%
  separate_rows(indicators_raw, sep = ";") %>%
  mutate(
    indicator  = str_squish(indicators_raw),
    token_norm = str_to_lower(indicator)
  ) %>%
  filter(!is.na(indicator), indicator != "")

## 5b) Join the lookup table to assign categories ------------------
indicator_long <- indicator_long %>%
  left_join(
    indicator_lookup %>% select(token_norm, Category_name),
    by = "token_norm"
  )

## 5c) Collapse matched categories back to one value per slot -----
indicator_categories_by_slot <- indicator_long %>%
  group_by(study_id, slot) %>%
  summarise(
    category_joined = collapse_unique_values(Category_name),
    .groups = "drop"
  ) %>%
  mutate(slot_num = str_extract(slot, "\\d+")) %>%
  select(study_id, slot_num, category_joined) %>%
  pivot_wider(
    names_from  = slot_num,
    values_from = category_joined,
    names_glue  = "indicator_cat_{slot_num}"
  )

## 5d) Make sure all 3 output category columns exist --------------
for (col_name in c("indicator_cat_1", "indicator_cat_2", "indicator_cat_3")) {
  if (!col_name %in% names(indicator_categories_by_slot)) {
    indicator_categories_by_slot[[col_name]] <- NA_character_
  }
}

indicator_categories_by_slot <- indicator_categories_by_slot %>%
  select(study_id, indicator_cat_1, indicator_cat_2, indicator_cat_3)

## 5e) Attach the new category columns to the full study data -----
study_data_with_indicator_cats <- study_data %>%
  left_join(indicator_categories_by_slot, by = "study_id") %>%
  relocate(indicator_cat_1, indicator_cat_2, indicator_cat_3, .after = indicators_3)

## 5f) Collect unmapped indicator tokens for review ---------------
# should be zero!
unmapped_indicators <- indicator_long %>%
  filter(is.na(Category_name)) %>%
  distinct(indicator) %>%
  arrange(indicator)


# STEP 6: EXPORT THE CATEGORIZED DATA ================================

## 6a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 6b) Build export tables -----------------------------------------
lookup_export <- indicator_lookup %>%
  arrange(Category_name, token) %>%
  select(Category_name, token)

## 6c) Write the new Excel workbook --------------------------------
# The workbook contains:
# - the updated study data
# - the list of unmapped indicator tokens
# - the final lookup table used
write_xlsx(
  x = list(
    study_data_with_indicator_cats = study_data_with_indicator_cats,
    unmapped_indicators            = unmapped_indicators,
    indicator_lookup              = lookup_export
  ),
  path = output_file
)


# STEP 7: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 7a) Count rows with at least one mapped indicator category ------
rows_with_any_category <- study_data_with_indicator_cats %>%
  transmute(
    any_category = !is.na(indicator_cat_1) |
      !is.na(indicator_cat_2) |
      !is.na(indicator_cat_3)
  ) %>%
  summarise(n = sum(any_category)) %>%
  pull(n)

## 7b) Count unique mapped categories used -------------------------
unique_categories_used <- study_data_with_indicator_cats %>%
  select(indicator_cat_1, indicator_cat_2, indicator_cat_3) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "slot",
    values_to = "indicator_cat"
  ) %>%
  mutate(indicator_cat = split_semis(indicator_cat)) %>%
  separate_rows(indicator_cat, sep = ";") %>%
  mutate(indicator_cat = str_squish(indicator_cat)) %>%
  filter(!is.na(indicator_cat), indicator_cat != "") %>%
  summarise(n = n_distinct(indicator_cat)) %>%
  pull(n)

## 7c) Count mapped and unmapped raw indicator tokens -------------
n_raw_indicator_tokens <- nrow(indicator_long)

n_mapped_indicator_tokens <- indicator_long %>%
  filter(!is.na(Category_name)) %>%
  nrow()

n_unmapped_indicator_tokens <- indicator_long %>%
  filter(is.na(Category_name)) %>%
  nrow()

## 7d) Create a consistent summary table ---------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input workbook",
    "Study-data sheet",
    "Indicator-category sheet",
    "Rows imported from study-data sheet",
    "Unique studies imported",
    "Unique lookup tokens",
    "Lookup tokens mapped to multiple categories",
    "Raw indicator tokens parsed",
    "Mapped raw indicator tokens",
    "Unmapped raw indicator tokens",
    "Unique unmapped indicator names",
    "Rows with any mapped indicator category",
    "Unique indicator categories used",
    "Output workbook"
  ),
  value = c(
    proj_root,
    input_file,
    study_sheet,
    category_sheet,
    nrow(study_data),
    n_distinct(study_data$study_id),
    nrow(indicator_lookup),
    nrow(lookup_duplicates),
    n_raw_indicator_tokens,
    n_mapped_indicator_tokens,
    n_unmapped_indicator_tokens,
    nrow(unmapped_indicators),
    rows_with_any_category,
    unique_categories_used,
    output_file
  )
)

## 7e) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n===== LOOKUP TOKENS MAPPED TO MULTIPLE CATEGORIES =====\n")
if (nrow(lookup_duplicates) > 0) {
  print(lookup_duplicates, n = Inf)
} else {
  cat("No lookup tokens map to more than one category.\n")
}

cat("\n===== UNMAPPED INDICATORS =====\n")
if (nrow(unmapped_indicators) > 0) {
  print(unmapped_indicators, n = Inf)
} else {
  cat("All indicators were mapped successfully.\n")
}


