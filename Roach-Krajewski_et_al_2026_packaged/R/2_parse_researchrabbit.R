# 2_parse_researchrabbit.R
# ============================================================
# Script purpose:
# This script reads the ResearchRabbit snowball-search results,
# cleans and standardizes the main bibliographic fields, corrects
# problematic author lists, creates study IDs, removes obvious
# and near-duplicate titles, and exports a cleaned database file
# for later merging and screening steps in the evidence-map workflow.
#
# Input:
# data/3_snowball_search/3_research_rabbit.csv
#
# Output:
# data/1_parsed_databases/parsed_researchrabbit.csv
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
  library(stringdist)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "3_snowball_search", "3_research_rabbit.csv")
output_file <- file.path(proj_root, "data", "1_parsed_databases", "parsed_researchrabbit.csv")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Split the author string into a clean author vector ----------
# ResearchRabbit author names are separated by commas in the raw file.
split_authors_rr <- function(x) {
  if (is.na(x) || str_squish(x) == "") {
    return(character(0))
  }
  
  x %>%
    str_replace_all(",", ";") %>%
    str_split(";") %>%
    pluck(1) %>%
    str_squish() %>%
    discard(~ .x == "")
}

## 2b) Remove perfect duplicate names from an author list ----------
remove_exact_duplicate_authors <- function(authors_list) {
  if (is.null(authors_list) || length(authors_list) == 0) {
    return(authors_list)
  }
  unique(authors_list)
}

## 2c) Remove near-duplicate names from an author list ------------
# Near-duplicates are identified using Jaro-Winkler similarity.
remove_near_duplicates_list <- function(authors_list, threshold = 0.70) {
  if (is.null(authors_list) || length(authors_list) == 0) {
    return(authors_list)
  }
  
  authors_list <- trimws(tolower(authors_list))
  unique_authors <- unique(authors_list)
  
  if (length(unique_authors) <= 1) {
    return(unique_authors)
  }
  
  for (i in seq_len(length(unique_authors) - 1)) {
    for (j in seq.int(i + 1, length(unique_authors))) {
      if (!is.na(unique_authors[i]) &&
          !is.na(unique_authors[j]) &&
          stringdist::stringsim(unique_authors[i], unique_authors[j], method = "jw") > threshold) {
        unique_authors[j] <- unique_authors[i]
      }
    }
  }
  
  unique(unique_authors)
}

## 2d) Extract the surname of a selected author position ----------
get_author_surname_lastword <- function(author_vector, index) {
  if (length(author_vector) < index || is.na(author_vector[index])) {
    return(NA_character_)
  }
  
  author_vector[index] %>%
    str_split("\\s+") %>%
    pluck(1) %>%
    tail(1) %>%
    .[[1]]
}

## 2e) Build a study ID from up to three authors and year ---------
build_study_id <- function(n_authors, first_author, second_author, third_author, year) {
  case_when(
    n_authors == 1 ~ paste0(coalesce(first_author, ""), "_", year),
    n_authors == 2 ~ paste0(coalesce(first_author, ""), "_", coalesce(second_author, ""), "_", year),
    n_authors == 3 ~ paste0(coalesce(first_author, ""), "_", coalesce(second_author, ""), "_", coalesce(third_author, ""), "_", year),
    n_authors > 3  ~ paste0(coalesce(first_author, ""), "_", coalesce(second_author, ""), "_", coalesce(third_author, ""), "_etal_", year),
    TRUE ~ NA_character_
  )
}

## 2f) Convert an author list column to a semicolon string --------
collapse_author_list <- function(x) {
  paste(x, collapse = ";")
}


# STEP 3: IMPORT THE RAW RESEARCHRABBIT FILE ==========================

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

## 3b) Read the raw ResearchRabbit export --------------------------
raw_rr <- read_csv(
  file           = input_file,
  guess_max      = Inf,
  show_col_types = FALSE
) %>%
  clean_names()

## 3c) Check that required columns are present ---------------------
required_cols <- c("authors", "year", "title", "journal", "doi")

missing_cols <- setdiff(required_cols, names(raw_rr))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the ResearchRabbit file:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: PARSE THE MAIN BIBLIOGRAPHIC FIELDS =========================

## 4a) Keep the columns needed for this workflow -------------------
rr <- raw_rr %>%
  transmute(
    authors,
    year,
    title,
    source = journal,
    doi
  )

## 4b) Create and clean the author list ----------------------------
rr <- rr %>%
  mutate(
    list_authors = map(authors, split_authors_rr),
    n_authors    = map_dbl(list_authors, length),
    list_authors = map(list_authors, remove_exact_duplicate_authors),
    list_authors = map(list_authors, remove_near_duplicates_list)
  )

## 4c) Extract first, second, and third author surnames -----------
rr <- rr %>%
  mutate(
    first_author  = map_chr(list_authors, get_author_surname_lastword, index = 1),
    second_author = map_chr(list_authors, get_author_surname_lastword, index = 2),
    third_author  = map_chr(list_authors, get_author_surname_lastword, index = 3)
  ) %>%
  mutate(
    first_author  = str_to_title(first_author),
    second_author = str_to_title(second_author),
    third_author  = str_to_title(third_author)
  )

## 4d) Build the initial study ID ----------------------------------
rr <- rr %>%
  mutate(
    study_id = build_study_id(
      n_authors     = n_authors,
      first_author  = first_author,
      second_author = second_author,
      third_author  = third_author,
      year          = year
    )
  )


# STEP 5: FLAG AUTHOR LISTS THAT NEED MANUAL CORRECTION ==============

## 5a) Flag rows where first and second authors are identical ------
rr <- rr %>%
  mutate(
    flag_equal_authors = if_else(first_author == second_author, TRUE, FALSE, missing = FALSE)
  )

## 5b) Split the flagged and unflagged rows ------------------------
false_rows <- rr %>%
  filter(flag_equal_authors == FALSE)

true_rows <- rr %>%
  filter(flag_equal_authors == TRUE)

## 5c) Store the number of flagged rows for QC ---------------------
nrow_equal_authors <- true_rows %>%
  summarise(count = n()) %>%
  pull(count)


# STEP 6: APPLY MANUAL AUTHOR CORRECTIONS ===============

## 6a) Define the corrected author lists ---------------------------
corrections <- list(
  c("sang-kyun han", "deborah s. page‐dumroese", "leonard r. johnson"),
  c("karl-johan jansson"),
  c("hung-chun huang", "haiyan huang", "david p. roy", "luigi boschetti", "lin yan", "sundeep kumar",
    "sanath sathyachandran kumar", "josé gómez-dans", "jian li"),
  c("t.f. fwa", "h.r. pasindu", "g. b. ong", "ghim ping ong"),
  c("guido fernando botta", "alfredo tolón becerra", "f. bellora tourn"),
  c("carsten warneke", "j. a. de gouw", "andreas stohl", "o. r. cooper", "p. d. goldan", "w. c. kuster",
    "j. s. holloway", "e. j. williams", "b. m. lerner", "s. a. mckeen", "m. trainer", "f. c. fehsenfeld",
    "elliot l. atlas", "v. stroud", "a. lueb", "shingo kato"),
  c("johnny m. grace", "r. w. skaggs", "d. k. cassel"),
  c("philippe teillet", "b. guindon", "d. g. goodenough"),
  c("pablo rosso", "susan l. ustin", "alan hastings"),
  c("richard a. earl"),
  c("p. m. teillet"),
  c("sang‐woo lee", "young‐geun lee", "myoung-soo won", "jong-jin kim", "sung–kwon hong"),
  c("victoria m. boulin", "margaret r. schmidt", "chuck bulmer", "maja kržić"),
  c("matthew j. wood", "paul a. carling", "a. j. moffat"),
  c("j. r. landis", "gary g. koch"),
  c("h.j. lutz"),
  c("nivedita sairam", "sudhagar nagarajan", "scott ornitz"),
  c("t f fwa", "l. chu", "kong-bing tan"),
  c("yichang tsai", "zhaohua wang", "feng li"),
  c("udayalakshmi vepakomma", "denis cormier"),
  c("patrick k. poon", "a. m. kinoshita"),
  c("p teluguntla", "prasad s. thenkabail", "adam oliphant", "jian xiong", "murali krishna gumma", "russell g. congalton",
    "kamini yadav", "alfredo huete"),
  c("udayalakshmi vepakomma", "denis cormier"),
  c("yanjun wang", "qi chen", "qing zhu", "lin liu", "chaokui li", "dunyong zheng"),
  c("pedro h. c. camargo", "flavio fortes camargo", "edson eyji sano", "cláudia maria de almeida",
    "josé cláudio mura", "tati de almeida"),
  c("tao long", "tengfei long", "zhaoming zhang", "guojin he", "weili jiao", "bingfang wu", "xiaomei zhang",
    "guizhou wang", "ranyu yin"),
  c("alexandre novo", "h. gonzález-jorge", "j. martínez-sánchez", "henrique lorenzo"),
  c("herbert j. lutz"),
  c("herbert j. lutz"),
  c("mostafa elhashash", "hessah albanwan", "rongjun qin")
)

## 6b) Confirm that the number of corrections matches the rows ----
if (nrow(true_rows) != length(corrections)) {
  stop(
    paste0(
      "The number of flagged rows (", nrow(true_rows), 
      ") does not match the number of manual corrections (", length(corrections), ")."
    ),
    call. = FALSE
  )
}

## 6c) Replace the flagged author lists with the corrected ones ----
if (nrow(true_rows) > 0) {
  true_rows$list_authors <- corrections
}

## 6d) Rebuild author fields and study IDs for corrected rows ------
true_rows <- true_rows %>%
  mutate(
    first_author  = map_chr(list_authors, get_author_surname_lastword, index = 1),
    second_author = map_chr(list_authors, get_author_surname_lastword, index = 2),
    third_author  = map_chr(list_authors, get_author_surname_lastword, index = 3)
  ) %>%
  mutate(
    first_author  = str_to_title(first_author),
    second_author = str_to_title(second_author),
    third_author  = str_to_title(third_author),
    study_id = build_study_id(
      n_authors     = n_authors,
      first_author  = first_author,
      second_author = second_author,
      third_author  = third_author,
      year          = year
    )
  )

## 6e) Merge corrected and unflagged rows back together -----------
rr_corrected <- bind_rows(true_rows, false_rows)


# STEP 7: FINALIZE THE AUTHOR FIELDS =================================

## 7a) Collapse the author list back to a semicolon string ---------
rr_corrected <- rr_corrected %>%
  mutate(
    list_authors = map_chr(list_authors, collapse_author_list),
    authors      = list_authors
  )

## 7b) Add standard output columns and remove helper columns -------
rr_corrected <- rr_corrected %>%
  mutate(
    authors   = str_to_title(authors),
    dbase     = "ResearchRabbit",
    link_url  = NA_character_,
    type      = NA_character_,
    study_id  = gsub("_+", "_", study_id)
  ) %>%
  select(
    study_id,
    first_author,
    authors,
    year,
    title,
    source,
    type,
    doi,
    link_url,
    dbase,
    everything(),
    -n_authors,
    -list_authors,
    -second_author,
    -third_author,
    -flag_equal_authors
  )


# STEP 8: REMOVE EXACT AND NEAR-DUPLICATE TITLES =====================

## 8a) Remove obvious duplicate titles -----------------------------
rr_corrected_distinct <- rr_corrected %>%
  distinct(title, .keep_all = TRUE)

## 8b) Build the title distance matrix -----------------------------
dist_matrix <- stringdistmatrix(
  rr_corrected_distinct$title,
  rr_corrected_distinct$title,
  method = "lv"
)

## 8c) Convert the matrix to suspected duplicate pairs ------------
similar_pairs <- which(dist_matrix <= 5 & dist_matrix > 0, arr.ind = TRUE)

suspected_duplicates <- tibble(
  index1   = similar_pairs[, 1],
  index2   = similar_pairs[, 2],
  title1   = rr_corrected_distinct$title[similar_pairs[, 1]],
  title2   = rr_corrected_distinct$title[similar_pairs[, 2]],
  distance = dist_matrix[similar_pairs]
) %>%
  filter(index1 < index2)

## 8d) Apply the original manual near-duplicate title removals ----
rr_final <- rr_corrected_distinct %>%
  filter(
    !(study_id == "Lutz_1956" & title == "Ecological effects of forest fires in the interior of Alaska."),
    !(study_id == "Markham_1985" & is.na(doi)),
    !(study_id == "Rouse_1973" & year == "1973"),
    !(study_id == "Rouse_Haas_Schell_etal_1973" & year == "1993")
  )


# STEP 9: EXPORT THE PARSED DATABASE ==================================

## 9a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 9b) Write the cleaned ResearchRabbit file -----------------------
write_csv(
  x    = rr_final %>% select(study_id, first_author, authors, year, title, source, type, doi, link_url, dbase),
  file = output_file,
  na   = ""
)


# STEP 10: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 10a) Check for any remaining duplicate study IDs ---------------
duplicate_study_ids <- rr_final %>%
  janitor::get_dupes(study_id)

## 10b) Check for duplicated DOI values among non-missing rows ----
duplicate_dois <- rr_final %>%
  filter(!is.na(doi), doi != "") %>%
  janitor::get_dupes(doi)

## 10c) Summarize key counts --------------------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Output file",
    "Rows imported from raw file",
    "Rows flagged for equal first/second author",
    "Rows after exact title de-duplication",
    "Rows exported after manual title cleanup",
    "Unique study IDs exported",
    "Rows with missing DOI",
    "Suspected near-duplicate title pairs",
    "Remaining duplicate study IDs",
    "Remaining duplicate DOI values"
  ),
  value = c(
    proj_root,
    input_file,
    output_file,
    nrow(raw_rr),
    nrow_equal_authors,
    nrow(rr_corrected_distinct),
    nrow(rr_final),
    n_distinct(rr_final$study_id, na.rm = TRUE),
    sum(is.na(rr_final$doi) | rr_final$doi == ""),
    nrow(suspected_duplicates),
    nrow(duplicate_study_ids),
    nrow(duplicate_dois)
  )
)

## 10d) Print quality checks to the console -----------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n===== FLAGGED ROWS WITH EQUAL FIRST/SECOND AUTHOR =====\n")
cat(nrow_equal_authors, "row(s) were manually corrected.\n")

cat("\n===== SUSPECTED NEAR-DUPLICATE TITLES =====\n")
if (nrow(suspected_duplicates) > 0) {
  print(suspected_duplicates, n = Inf)
} else {
  cat("No suspected near-duplicate title pairs were found.\n")
}

cat("\n========= DUPLICATE STUDY IDs (IF ANY) =========\n")
if (nrow(duplicate_study_ids) > 0) {
  print(duplicate_study_ids, n = Inf)
} else {
  cat("No duplicate study IDs remain.\n")
}

cat("\n========= DUPLICATE DOI VALUES (IF ANY) =========\n")
if (nrow(duplicate_dois) > 0) {
  print(duplicate_dois, n = Inf)
} else {
  cat("No duplicate DOI values remain.\n")
}

cat("\nParsed ResearchRabbit export complete.\n")

