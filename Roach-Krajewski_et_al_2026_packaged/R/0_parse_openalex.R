# 0_parse_openalex.R
# ============================================================
# Script purpose:
# This script reads the raw OpenAlex search results file,
# keeps only English and French records, standardizes the main
# bibliographic fields, creates study IDs, and exports a cleaned
# database file for later merging and screening steps in the
# evidence-map workflow.
#
# Input:
# data/0_initial_search/openalex.csv
#
# Output:
# data/1_parsed_databases/parsed_openalex.csv
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
input_file  <- file.path(proj_root, "data", "0_initial_search", "openalex.csv")
output_file <- file.path(proj_root, "data", "1_parsed_databases", "parsed_openalex.csv")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Split the author string into a clean author vector ----------
# In the OpenAlex export, authors are separated by "|".
split_authors <- function(x) {
  if (is.na(x) || str_squish(x) == "") {
    return(character(0))
  }
  
  x %>%
    str_replace_all("\\|", ";") %>%
    str_split(";") %>%
    pluck(1) %>%
    str_squish() %>%
    discard(~ .x == "")
}

## 2b) Extract the surname of a selected author position ----------
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


# STEP 3: IMPORT THE RAW OPENALEX FILE ================================

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

## 3b) Read the raw OpenAlex export -------------------------------
raw_openalex <- read_csv(
  file           = input_file,
  guess_max      = Inf,
  show_col_types = FALSE
)

## 3c) Clean the column names --------------------------------------
raw_openalex <- raw_openalex %>%
  clean_names()

## 3d) Check that required columns are present ---------------------
required_cols <- c(
  "language",
  "authorships_author_display_name",
  "publication_year",
  "display_name",
  "primary_location_source_display_name",
  "type",
  "doi",
  "primary_location_landing_page_url"
)

missing_cols <- setdiff(required_cols, names(raw_openalex))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the OpenAlex file:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: FILTER AND STANDARDIZE THE MAIN FIELDS ======================

## 4a) Keep only English and French records ------------------------
openalex_base <- raw_openalex %>%
  filter(language %in% c("en", "fr"))

## 4b) Create the cleaned bibliographic table ----------------------
openalex <- openalex_base %>%
  select(
    authors = authorships_author_display_name,
    year    = publication_year,
    title   = display_name,
    source  = primary_location_source_display_name,
    type,
    doi,
    link_url = primary_location_landing_page_url
  ) %>%
  mutate(
    authors = str_replace_all(authors, "\\|", ";"),
    doi     = normalize_doi_url(doi),
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
    dbase = "OpenAlex"
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
openalex <- openalex %>%
  distinct()

## 5b) Remove known duplicate/problem records ----------------------
openalex <- openalex %>%
  filter(
    !(study_id == "Zahura_Bisht_Li_etal_2024" & type == "preprint"),
    !(study_id == "Ayres_Colliander_Cosh_etal_2021" & type == "preprint"),
    !(study_id == "Hilgartner_Nejako_Casey_2009" & is.na(source)),
    !(study_id == "Abdallah_2007" & type == "dissertation"),
    !(study_id == "Ayadiuno_Ndulue_Mozie_2022" & is.na(source)),
    !(study_id == "Ågren_Larson_Paul_etal_2021" & is.na(source)),
    !(study_id == "Barlow_Silveira_Mestre_etal_2012" &
        doi == "https://doi.org/10.1371/annotation/8013f95e-71f4-4ec0-afe9-00cea6627685"),
    !(study_id == "Baumgras_LeDoux_Sherar_1993" & is.na(source)),
    !(study_id == "Bauwens_Stavrakou_Müller_etal_2016" & is.na(source)),
    !(study_id == "Belenguer-Plomer_Chuvieco_Tanase_2019" & is.na(source)),
    !(study_id == "Bergamaschi_Bernknopf_Clow_etal_2010" & type == "review"),
    !(study_id == "Bowering_Edwards_Ziegler_2023" & is.na(source)),
    !(study_id == "Buschman_Hoitink_Jong_etal_2011" & is.na(doi)),
    !(study_id == "DeGroote_Mercer_Fisher_etal_2007" &
        doi == "https://doi.org/10.1603/0022-2585(2007)44[1139:sioamd]2.0.co;2"),
    !(study_id == "Deveci_Koru_Sakarya_etal_2016" &
        doi == "https://doi.org/10.5194/isprsarchives-xli-b8-1341-2016"),
    !(study_id == "Flores_2016" & is.na(doi)),
    !(study_id == "Glaser_Lewis_Wong_2000" & type == "article"),
    !(study_id == "Hale_2012" &
        title == "Rainfall-runoff response following the 2010 Bull Fire in southern Sequoia National Forest, California - eScholarship"),
    !(study_id == "Higgins_Asner_Perez_etal_2014" & is.na(source)),
    !(study_id == "Imen_Chang_Yang_2014" & is.na(source)),
    !(study_id == "Issa_2010" & title == "Cadre de gestion environmental et sociale"),
    !(study_id == "Jones_Murphy_2023" & source == "Ecological Solutions and Evidence"),
    !(study_id == "Klerk_Tildesley_Labuschagne_etal_2024" & type == "preprint"),
    !(study_id == "Larson_Lidberg_Ågren_etal_2022" & type == "preprint"),
    !(study_id == "Maranguit_2017" & is.na(doi)),
    !(study_id == "McCuen_2009" & title == "<i>Book Reviews</i>"),
    !(study_id == "Movasat_Tomac_2020" & is.na(source)),
    !(study_id == "Naciri_Boom_Payne_etal_2023" & is.na(source)),
    !(study_id == "Naus_Domingues_Krol_etal_2022" & is.na(source)),
    !(study_id == "Parker_Boesch_Wooster_etal_2016" & is.na(source)),
    !(study_id == "Radeloff_Mladenoff_Boyce_2000" &
        doi == "https://doi.org/10.2307/2640998"),
    !(study_id == "Rodriguez-Jimenez_Lorenzo_Novo_etal_2023" & type == "preprint"),
    !(study_id == "Sames_Klein_Kim_etal_2009" &
        doi == "https://doi.org/10.3376/038.034.0208"),
    !(study_id == "Shroff_2023" & source == "Research Square (Research Square)"),
    !(study_id == "Srivastava_Dobre_Lew_etal_2023" &
        doi == "https://doi.org/10.13031/soil.2023076"),
    !(study_id == "Sykas_Zografakis_Demestichas_2024" & type == "preprint"),
    !(study_id == "Tampekis_Samara_Sakellariou_etal_2015" & type == "preprint"),
    !(study_id == "Thompson_Brokaw_Zimmerman_etal_2002" &
        doi == "https://doi.org/10.2307/3099976"),
    !(study_id == "Wang_Qu_Hao_2012" &
        doi == "https://doi.org/10.1201/b11279-27"),
    !(study_id == "Whitman_Whitman_Woolet_etal_2019" & type == "preprint"),
    !(study_id == "Xu_Zhuang_Zhao_etal_2024" & is.na(source))
  )


# STEP 6: EXPORT THE PARSED DATABASE ==================================

## 6a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 6b) Write the cleaned OpenAlex file -----------------------------
write_csv(
  x    = openalex,
  file = output_file,
  na   = ""
)


# STEP 7: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 7a) Check for any remaining duplicate study IDs -----------------
duplicate_study_ids <- openalex %>%
  get_dupes(study_id)

## 7b) Check for duplicated DOI URLs among non-missing values ------
duplicate_dois <- openalex %>%
  filter(!is.na(doi), doi != "") %>%
  get_dupes(doi)

## 7c) Summarize key counts ----------------------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Output file",
    "Rows imported from raw file",
    "Rows kept after language filter",
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
    nrow(raw_openalex),
    nrow(openalex_base),
    nrow(openalex),
    n_distinct(openalex$study_id, na.rm = TRUE),
    sum(is.na(openalex$study_id) | openalex$study_id == ""),
    sum(is.na(openalex$year) | openalex$year == ""),
    sum(is.na(openalex$doi) | openalex$doi == ""),
    nrow(duplicate_study_ids),
    nrow(duplicate_dois)
  )
)

## 7d) Summarize document types ------------------------------------
type_totals <- openalex %>%
  count(type, name = "n") %>%
  arrange(desc(n), type)

## 7e) Summarize language counts from the raw file -----------------
language_totals <- raw_openalex %>%
  count(language, name = "n") %>%
  arrange(desc(n), language)

## 7f) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n============= RAW LANGUAGE COUNTS =============\n")
print(language_totals, n = Inf)

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

cat("\nParsed OpenAlex export complete.\n")

