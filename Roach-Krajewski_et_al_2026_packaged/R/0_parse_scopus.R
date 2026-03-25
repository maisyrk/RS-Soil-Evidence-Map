# 0_parse_scopus.R
# ============================================================
# Script purpose:
# This script reads the raw Scopus search results file,
# standardizes the main bibliographic fields, creates study IDs,
# and exports a cleaned database file for later merging and
# screening steps in the evidence-map workflow.
#
# Input:
# data/0_initial_search/scopus.csv
#
# Output:
# data/1_parsed_databases/parsed_scopus.csv
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
input_file  <- file.path(proj_root, "data", "0_initial_search", "scopus.csv")
output_file <- file.path(proj_root, "data", "1_parsed_databases", "parsed_scopus.csv")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Split the author string into a clean author vector ----------
# In the Scopus export, authors are separated by semicolons.
split_authors <- function(x) {
  if (is.na(x) || str_squish(x) == "") {
    return(character(0))
  }
  
  x %>%
    str_split(";") %>%
    pluck(1) %>%
    str_squish() %>%
    discard(~ .x == "")
}

## 2b) Extract the surname of a selected author position ----------
get_author_surname <- function(author_vector, index) {
  if (length(author_vector) < index || is.na(author_vector[index])) {
    return(NA_character_)
  }
  
  author_vector[index] %>%
    str_squish() %>%
    str_split("\\s+") %>%
    pluck(1) %>%
    head(1) %>%
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


# STEP 3: IMPORT THE RAW SCOPUS FILE ==================================

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

## 3b) Read the raw Scopus export ----------------------------------
raw_scopus <- read_csv(
  file           = input_file,
  guess_max      = Inf,
  show_col_types = FALSE
)

## 3c) Clean the column names --------------------------------------
raw_scopus <- raw_scopus %>%
  clean_names()

## 3d) Check that required columns are present ---------------------
required_cols <- c(
  "authors",
  "year",
  "title",
  "source_title",
  "document_type",
  "doi",
  "link"
)

missing_cols <- setdiff(required_cols, names(raw_scopus))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the Scopus file:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: PARSE AND STANDARDIZE THE MAIN FIELDS =======================

## 4a) Create the cleaned bibliographic table ----------------------
scopus <- raw_scopus %>%
  select(
    authors,
    year,
    title,
    source = source_title,
    type   = document_type,
    doi,
    link_url = link
  ) %>%
  mutate(
    type = str_replace(str_to_lower(type), " ", "-"),
    doi  = normalize_doi_url(doi),
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
    dbase = "Scopus"
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
scopus <- scopus %>%
  distinct()

## 5b) Remove known duplicate/problem records ----------------------
scopus <- scopus %>%
  filter(
    !(study_id == "Awange_2012" &
        source == "Environmental Science and Engineering (Subseries: Environmental Science)"),
    !(study_id == "Souza_Roberts_Monteiro_2005" & is.na(doi)),
    !(study_id == "Sundaram_1996" & is.na(doi)),
    !(study_id == "Wang_Qi_Cochrane_2005" & is.na(doi)),
    !(study_id == "Mangel_Bradford_Singha_2019" & year == 2019)
  )


# STEP 6: EXPORT THE PARSED DATABASE ==================================

## 6a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 6b) Write the cleaned Scopus file -------------------------------
write_csv(
  x    = scopus,
  file = output_file,
  na   = ""
)


# STEP 7: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 7a) Check for any remaining duplicate study IDs -----------------
duplicate_study_ids <- scopus %>%
  get_dupes(study_id)

## 7b) Check for duplicated DOI URLs among non-missing values ------
duplicate_dois <- scopus %>%
  filter(!is.na(doi), doi != "") %>%
  get_dupes(doi)

## 7c) Summarize key counts ----------------------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Output file",
    "Rows imported from raw file",
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
    nrow(raw_scopus),
    nrow(scopus),
    n_distinct(scopus$study_id, na.rm = TRUE),
    sum(is.na(scopus$study_id) | scopus$study_id == ""),
    sum(is.na(scopus$year) | scopus$year == ""),
    sum(is.na(scopus$doi) | scopus$doi == ""),
    nrow(duplicate_study_ids),
    nrow(duplicate_dois)
  )
)

## 7d) Summarize document types ------------------------------------
type_totals <- scopus %>%
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

cat("\nParsed Scopus export complete.\n")

