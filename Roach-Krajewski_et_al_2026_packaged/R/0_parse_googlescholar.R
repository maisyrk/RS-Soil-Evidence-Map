# 0_parse_googlescholar.R
# ============================================================
# Script purpose:
# This script reads the raw Google Scholar search results file,
# keeps the first 300 records by Google Scholar rank, standardizes
# the main bibliographic fields, creates study IDs, and exports
# a cleaned database file for later merging and screening steps
# in the evidence-map workflow.
#
# Input:
# data/0_initial_search/googlescholar.csv
#
# Output:
# data/1_parsed_databases/parsed_googlescholar.csv
#
# Expected folder structure:
# Roach-Krajewski_et_al_2026_packaged/
# ├── data/
# │   ├── 0_initial_search/
# │   ├── 1_parsed_databases/
# │   ├── 2_merged_databases/
# │   ├── 3_snowball_search/
# │   ├── 4_final_study_data/
# │   └── 5_extra_figure_data/
# ├── R/
# │   └── FIGURES/
# └── figures/
#
# This script is designed so that a user can download the full
# packaged folder and run the script from anywhere inside it.
# ============================================================


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
  library(readr)
  library(janitor)
  library(stringr)
  library(purrr)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "0_initial_search", "googlescholar.csv")
output_file <- file.path(proj_root, "data", "1_parsed_databases", "parsed_googlescholar.csv")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Split the author string into a clean author vector ----------
# Google Scholar authors are separated by commas in this export.
split_authors <- function(x) {
  if (is.na(x) || str_squish(x) == "") {
    return(character(0))
  }
  
  x %>%
    str_split(",") %>%
    pluck(1) %>%
    str_squish() %>%
    discard(~ .x == "")
}

## 2b) Extract the surname of a selected author position ----------
# Surname is taken as the last word in the author string.
get_author_surname <- function(author_vector, index) {
  if (length(author_vector) < index) {
    return(NA_character_)
  }
  
  author_vector[index] %>%
    str_squish() %>%
    str_split("\\s+") %>%
    pluck(1) %>%
    tail(1) %>%
    .[[1]]
}

## 2c) Convert a DOI to a URL if needed ---------------------------
normalize_doi_url <- function(x) {
  case_when(
    is.na(x) | str_squish(x) == "" ~ NA_character_,
    str_detect(x, regex("^https?://", ignore_case = TRUE)) ~ x,
    TRUE ~ paste0("https://doi.org/", str_squish(x))
  )
}

## 2d) Build a study ID from up to three authors and year ---------
build_study_id <- function(n_authors, first_author, second_author, third_author, year) {
  case_when(
    n_authors == 1 ~ paste0(first_author, "_", year),
    n_authors == 2 ~ paste0(first_author, "_", second_author, "_", year),
    n_authors == 3 ~ paste0(first_author, "_", second_author, "_", third_author, "_", year),
    n_authors > 3  ~ paste0(first_author, "_", second_author, "_", third_author, "_etal_", year),
    TRUE ~ NA_character_
  )
}


# STEP 3: IMPORT THE RAW GOOGLE SCHOLAR FILE ==========================

## 3a) Check that the input file exists ----------------------------
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

## 3b) Read the raw Google Scholar export --------------------------
raw_googlescholar <- read_csv(
  file            = input_file,
  guess_max       = Inf,
  show_col_types  = FALSE
)

## 3c) Clean the column names --------------------------------------
raw_googlescholar <- raw_googlescholar %>%
  clean_names()

## 3d) Check that required columns are present ---------------------
required_cols <- c(
  "gsrank",
  "authors",
  "year",
  "title",
  "source",
  "type",
  "doi",
  "article_url"
)

missing_cols <- setdiff(required_cols, names(raw_googlescholar))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the Google Scholar file:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: FILTER AND STANDARDIZE THE MAIN FIELDS ======================

## 4a) Keep only the first 300 records by Google Scholar rank -----
googlescholar_base <- raw_googlescholar %>%
  filter(gsrank <= 300)

## 4b) Create the cleaned bibliographic table ----------------------
googlescholar <- googlescholar_base %>%
  mutate(
    doi = normalize_doi_url(doi)
  ) %>%
  select(
    authors,
    year,
    title,
    source,
    type,
    doi,
    link_url = article_url
  ) %>%
  mutate(
    type = str_replace(str_to_lower(type), " ", "-"),
    list_authors = map(authors, split_authors),
    n_authors    = map_int(list_authors, length),
    first_author = map_chr(list_authors, get_author_surname, index = 1),
    second_author = map_chr(list_authors, get_author_surname, index = 2),
    third_author  = map_chr(list_authors, get_author_surname, index = 3),
    study_id = build_study_id(
      n_authors     = n_authors,
      first_author  = first_author,
      second_author = second_author,
      third_author  = third_author,
      year          = year
    ),
    dbase = "Google Scholar"
  ) %>%
  select(
    study_id,
    first_author,
    everything(),
    -n_authors,
    -list_authors,
    -second_author,
    -third_author
  )


# STEP 5: REMOVE DUPLICATES AND KNOWN PROBLEM ROWS ====================

## 5a) Remove exact duplicate rows ---------------------------------
googlescholar <- googlescholar %>%
  distinct()

## 5b) Remove known duplicate/problem records ----------------------
googlescholar <- googlescholar %>%
  filter(
    !(study_id == "Abdi_Uusitalo_Kivinen_2022" & is.na(source))
  )


# STEP 6: EXPORT THE PARSED DATABASE ==================================

## 6a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 6b) Write the cleaned Google Scholar file -----------------------
write_csv(
  x    = googlescholar,
  file = output_file,
  na   = ""
)


# STEP 7: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 7a) Check for any remaining duplicate study IDs -----------------
duplicate_study_ids <- googlescholar %>%
  get_dupes(study_id)

## 7b) Check for duplicated DOI URLs among non-missing values ------
duplicate_dois <- googlescholar %>%
  filter(!is.na(doi), doi != "") %>%
  get_dupes(doi)

## 7c) Summarize key counts ----------------------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Output file",
    "Rows imported from raw file",
    "Rows kept after GSRank <= 300 filter",
    "Rows exported after cleaning",
    "Unique study IDs exported",
    "Rows with missing study_id",
    "Rows with missing year",
    "Rows with missing DOI",
    "Remaining duplicate study IDs",
    "Remaining duplicate DOI URLs"
  ),
  value = c(
    proj_root,
    input_file,
    output_file,
    nrow(raw_googlescholar),
    nrow(googlescholar_base),
    nrow(googlescholar),
    n_distinct(googlescholar$study_id, na.rm = TRUE),
    sum(is.na(googlescholar$study_id) | googlescholar$study_id == ""),
    sum(is.na(googlescholar$year) | googlescholar$year == ""),
    sum(is.na(googlescholar$doi) | googlescholar$doi == ""),
    nrow(duplicate_study_ids),
    nrow(duplicate_dois)
  )
)

## 7d) Summarize document types ------------------------------------
type_totals <- googlescholar %>%
  count(type, name = "n") %>%
  arrange(desc(n), type)

## 7e) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n============= DOCUMENT TYPES =============\n")
print(type_totals, n = Inf)

cat("\n========= DUPLICATE STUDY IDs (IF ANY) =========\n")
if (nrow(duplicate_study_ids) > 0) {
  print(duplicate_study_ids, n = Inf)
} else {
  cat("No duplicate study IDs remain.\n")
}

cat("\n========= DUPLICATE DOI URLS (IF ANY) =========\n")
if (nrow(duplicate_dois) > 0) {
  print(duplicate_dois, n = Inf)
} else {
  cat("No duplicate DOI URLs remain.\n")
}

cat("\nParsed Google Scholar export complete.\n")

