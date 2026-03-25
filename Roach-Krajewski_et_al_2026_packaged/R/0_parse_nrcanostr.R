# 0_parse_nrcanostr.R
# ============================================================
# Script purpose:
# This script reads the raw NRCan OSTR search results file,
# retrieves selected bibliographic metadata from Crossref using
# DOI values, standardizes the main bibliographic fields,
# creates study IDs, and exports a cleaned database file for
# later merging and screening steps in the evidence-map workflow.
#
# Input:
# data/0_initial_search/nrcanostr.csv
#
# Output:
# data/1_parsed_databases/parsed_nrcanostr.csv
#
# Important note:
# This script requires an internet connection when it runs,
# because it queries Crossref through the rcrossref package.
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
  library(rcrossref)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "0_initial_search", "nrcanostr.csv")
output_file <- file.path(proj_root, "data", "1_parsed_databases", "parsed_nrcanostr.csv")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Prepare DOI text for querying Crossref ----------------------
# Crossref works best with plain DOI strings, not full doi.org URLs.
clean_doi_for_query <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    TRUE ~ x %>%
      str_replace(regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE), "")
  )
}

## 2b) Convert a DOI to a URL for the output table -----------------
normalize_doi_url <- function(x) {
  x <- clean_doi_for_query(x)
  
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    TRUE ~ paste0("https://doi.org/", x)
  )
}

## 2c) Query Crossref safely ---------------------------------------
# If a DOI is missing or the query fails, return NULL instead of stopping.
fetch_crossref_record <- function(doi_query) {
  if (is.na(doi_query) || doi_query == "") {
    return(NULL)
  }
  
  tryCatch(
    rcrossref::cr_cn(dois = doi_query, format = "citeproc-json-ish"),
    error = function(e) NULL
  )
}

## 2d) Extract publication year from a Crossref record ------------
extract_crossref_year <- function(x) {
  if (is.null(x) || is.null(x$published) || is.null(x$published$`date-parts`)) {
    return(NA_real_)
  }
  
  out <- suppressWarnings(as.numeric(x$published$`date-parts`[1, 1]))
  ifelse(length(out) == 0, NA_real_, out)
}

## 2e) Extract source title from a Crossref record -----------------
extract_crossref_source <- function(x) {
  if (is.null(x) || is.null(x$`container-title`) || is.list(x$`container-title`)) {
    return(NA_character_)
  }
  
  as.character(x$`container-title`)
}

## 2f) Extract volume from a Crossref record -----------------------
extract_crossref_volume <- function(x) {
  if (is.null(x) || is.null(x$volume)) {
    return(NA_real_)
  }
  
  suppressWarnings(readr::parse_number(as.character(x$volume)))
}

## 2g) Split the author string into a clean author vector ----------
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

## 2h) Extract the surname of a selected author position ----------
get_author_surname <- function(author_vector, index) {
  if (length(author_vector) < index || is.na(author_vector[index])) {
    return(NA_character_)
  }
  
  author_vector[index] %>%
    str_split(",") %>%
    pluck(1) %>%
    .[[1]] %>%
    str_squish()
}

## 2i) Build a study ID from up to three authors and year ---------
build_study_id <- function(n_authors, first_author, second_author, third_author, year) {
  case_when(
    n_authors == 1 ~ paste0(first_author, "_", year),
    n_authors == 2 ~ paste0(first_author, "_", second_author, "_", year),
    n_authors == 3 ~ paste0(first_author, "_", second_author, "_", third_author, "_", year),
    n_authors > 3  ~ paste0(first_author, "_", second_author, "_", third_author, "_etal_", year),
    TRUE ~ NA_character_
  )
}


# STEP 3: IMPORT THE RAW NRCAN OSTR FILE ==============================

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

## 3b) Read the raw NRCan OSTR export ------------------------------
raw_nrcanostr <- read_csv(
  file           = input_file,
  guess_max      = Inf,
  show_col_types = FALSE
)

## 3c) Clean the column names --------------------------------------
raw_nrcanostr <- raw_nrcanostr %>%
  clean_names()

## 3d) Check that required columns are present ---------------------
required_cols <- c(
  "authors",
  "date_issued",
  "title",
  "type",
  "doi",
  "uri"
)

missing_cols <- setdiff(required_cols, names(raw_nrcanostr))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the NRCan OSTR file:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: QUERY CROSSREF USING DOI ====================================

## 4a) Prepare DOI values for querying -----------------------------
nrcanostr_crossref <- raw_nrcanostr %>%
  mutate(
    doi_query = clean_doi_for_query(doi)
  )

## 4b) Retrieve Crossref records -----------------------------------
# This step may take some time and requires internet access.
nrcanostr_crossref <- nrcanostr_crossref %>%
  mutate(
    crossref_json = map(doi_query, fetch_crossref_record)
  )


# STEP 5: PARSE AND STANDARDIZE THE MAIN FIELDS =======================

## 5a) Create the cleaned bibliographic table ----------------------
nrcanostr <- nrcanostr_crossref %>%
  mutate(
    year_local    = readr::parse_number(date_issued),
    year_crossref = map_dbl(crossref_json, extract_crossref_year),
    year          = if_else(is.na(year_crossref), year_local, year_crossref),
    source        = map_chr(crossref_json, extract_crossref_source),
    volume        = map_dbl(crossref_json, extract_crossref_volume),
    doi           = normalize_doi_url(doi)
  ) %>%
  select(
    authors,
    year,
    title,
    source,
    type,
    doi,
    link_url = uri
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
    dbase = "NRCan OSTR"
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


# STEP 6: REMOVE DUPLICATES AND KNOWN PROBLEM ROWS ====================

## 6a) Remove exact duplicate rows ---------------------------------
nrcanostr <- nrcanostr %>%
  distinct()

## 6b) Remove known duplicate/problem records ----------------------
nrcanostr <- nrcanostr %>%
  filter(
    !(study_id == "Chen_Chen_Cihlar_2000" & type == "book-chapter"),
    !(study_id == "Chen_Ju_Cihlar_etal_2003" & type == "book-chapter"),
    !(study_id == "Krcmar-nozic_Wilson_Arthur_2000" &
        title == "L’impact potentiel des ravageurs forestiers exotiques en Amerique du Nord : synthèse de la recherche"),
    !(study_id == "Nadeau_Pare_Girardin_etal_2020" &
        title == "In Brief from the Canadian Forest Service, Laurentian Forestry Centre. No. 58.")
  )


# STEP 7: EXPORT THE PARSED DATABASE ==================================

## 7a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 7b) Write the cleaned NRCan OSTR file ---------------------------
write_csv(
  x    = nrcanostr,
  file = output_file,
  na   = ""
)


# STEP 8: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 8a) Check for any remaining duplicate study IDs -----------------
duplicate_study_ids <- nrcanostr %>%
  get_dupes(study_id)

## 8b) Check for duplicated DOI URLs among non-missing values ------
duplicate_dois <- nrcanostr %>%
  filter(!is.na(doi), doi != "") %>%
  get_dupes(doi)

## 8c) Count Crossref query success --------------------------------
crossref_summary <- nrcanostr_crossref %>%
  transmute(
    doi_query,
    crossref_found = map_lgl(crossref_json, ~ !is.null(.x))
  )

n_doi_queries <- sum(!is.na(crossref_summary$doi_query) & crossref_summary$doi_query != "")
n_crossref_hits <- sum(crossref_summary$crossref_found, na.rm = TRUE)

## 8d) Summarize key counts ----------------------------------------
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
    "DOIs queried in Crossref",
    "Successful Crossref records returned",
    "Remaining duplicate study IDs",
    "Remaining duplicate DOI URLs"
  ),
  value = c(
    proj_root,
    input_file,
    output_file,
    nrow(raw_nrcanostr),
    nrow(nrcanostr),
    n_distinct(nrcanostr$study_id, na.rm = TRUE),
    sum(is.na(nrcanostr$study_id) | nrcanostr$study_id == ""),
    sum(is.na(nrcanostr$year)),
    sum(is.na(nrcanostr$doi) | nrcanostr$doi == ""),
    n_doi_queries,
    n_crossref_hits,
    nrow(duplicate_study_ids),
    nrow(duplicate_dois)
  )
)

## 8e) Summarize document types ------------------------------------
type_totals <- nrcanostr %>%
  count(type, name = "n") %>%
  arrange(desc(n), type)

## 8f) Print quality checks to the console -------------------------
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

cat("\nParsed NRCan OSTR export complete.\n")

