# 3_merged_snowball.R
# ============================================================
# Script purpose:
# This script merges the main bibliography with the parsed
# ResearchRabbit snowball-search records, removes duplicates,
# and exports a final distinct ResearchRabbit-only file for
# later snowball-search screening and tracking.
#
# Inputs:
# data/2_merged_databases/2_merged_biblio_to_be_inspected.csv
# data/1_parsed_databases/parsed_researchrabbit.csv
#
# Output:
# data/3_snowball_search/researchrabbit_distinct.csv
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
  library(stringdist)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
merged_file <- file.path(
  proj_root,
  "data",
  "2_merged_databases",
  "2_merged_biblio_to_be_inspected.csv"
)

researchrabbit_file <- file.path(
  proj_root,
  "data",
  "1_parsed_databases",
  "parsed_researchrabbit.csv"
)

output_file <- file.path(
  proj_root,
  "data",
  "3_snowball_search",
  "researchrabbit_distinct.csv"
)


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Check that input files exist --------------------------------
check_input_files <- function(paths) {
  missing_files <- paths[!file.exists(paths)]
  
  if (length(missing_files) > 0) {
    stop(
      paste0(
        "The following required input files were not found:\n",
        paste(missing_files, collapse = "\n"),
        "\n\nPlease confirm that the packaged folder structure is correct ",
        "and that the earlier parsing/merging scripts have already been run."
      ),
      call. = FALSE
    )
  }
}

## 2b) Read one bibliography file ----------------------------------
read_biblio_file <- function(path) {
  readr::read_csv(
    file           = path,
    guess_max      = Inf,
    show_col_types = FALSE
  )
}


# STEP 3: IMPORT AND COMBINE THE BIBLIOGRAPHIES =======================

## 3a) Confirm that both input files exist -------------------------
check_input_files(c(merged_file, researchrabbit_file))

## 3b) Read the merged bibliography and ResearchRabbit file --------
merged_bib <- read_biblio_file(merged_file)
researchrabbit_bib <- read_biblio_file(researchrabbit_file)

## 3c) Check that key columns are present --------------------------
required_cols <- c("study_id", "title", "doi", "dbase")

missing_merged_cols <- setdiff(required_cols, names(merged_bib))
missing_rr_cols     <- setdiff(required_cols, names(researchrabbit_bib))

if (length(missing_merged_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the merged bibliography:\n",
      paste(missing_merged_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (length(missing_rr_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from parsed_researchrabbit.csv:\n",
      paste(missing_rr_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

## 3d) Combine the two bibliography files --------------------------
combined_bib <- bind_rows(researchrabbit_bib, merged_bib)


# STEP 4: REMOVE COMPLETE DUPLICATES ==================================

## 4a) Remove fully identical rows ---------------------------------
bib_stage1 <- combined_bib %>%
  distinct()


# STEP 5: INSPECT AND REMOVE DOI DUPLICATES ===========================

## 5a) Record DOI duplicates before manual filtering ---------------
doi_dupes_before <- bib_stage1 %>%
  filter(!is.na(doi), doi != "") %>%
  janitor::get_dupes(doi)

## 5b) Apply manual DOI duplicate rules --------------
bib_stage2 <- bib_stage1 %>%
  filter(
    # Chamen_Alakukku_Pires_etal_2003, remove part 2 version
    !(doi == "10.1016/s0167-1987(03)00108-9" &
        title == "Prevention strategies for field traffic-induced subsoil compaction: a review: Part 2. Equipment and field practices"),
    # 10.1080/13658816.2017.1346255, remove version with incorrect date
    !(doi == "10.1080/13658816.2017.1346255" & year == "2020")
  )

## 5c) Recheck DOI duplicates after filtering ----------------------
doi_dupes_after <- bib_stage2 %>%
  filter(!is.na(doi), doi != "") %>%
  janitor::get_dupes(doi)


# STEP 6: INSPECT AND REMOVE STUDY-ID DUPLICATES ======================

## 6a) Record study ID duplicates before manual filtering ----------
id_dupes_before <- bib_stage2 %>%
  janitor::get_dupes(study_id)

## 6b) Apply manual study-ID duplicate rules ---------
bib_stage3 <- bib_stage2 %>%
  filter(
    # Alonzo_Morton_Cook_etal_2017, dbase dupe - remove RR version (no link)
    !(study_id == "Alonzo_Morton_Cook_etal_2017" & dbase == "ResearchRabbit"),
    # Arnett_Coops_Daniels_etal_2015, dbase dupe - remove RR version (no link)
    !(study_id == "Arnett_Coops_Daniels_etal_2015" & dbase == "ResearchRabbit"),
    # Bagheri_Jalali_etal_2013, RR dupe - remove version with missing source
    !(study_id == "Bagheri_Jalali_etal_2013" & is.na(source)),
    # Bradstock_Hammill_Collins_etal_2010, dbase dupe - remove RR version (no link)
    !(study_id == "Bradstock_Hammill_Collins_etal_2010" & dbase == "ResearchRabbit"),
    # Chafer_2008, dbase dupe - remove RR version (no link)
    !(study_id == "Chafer_2008" & dbase == "ResearchRabbit"),
    # Fassnacht_Latifi_Ghosh_etal_2014, dbase dupe - remove RR version (no link)
    !(study_id == "Fassnacht_Latifi_Ghosh_etal_2014" & dbase == "ResearchRabbit"),
    # Jun_Way_Löfgren_etal_2004, dbase dupe - remove RR version (no link)
    !(study_id == "Jun_Way_Löfgren_etal_2004" & dbase == "ResearchRabbit"),
    # Keeley_2009, dbase dupe - remove RR version (no link)
    !(study_id == "Keeley_2009" & dbase == "ResearchRabbit"),
    # Lasaponara_2006, dbase dupe - remove RR version (no link)
    !(study_id == "Lasaponara_2006" & dbase == "ResearchRabbit"),
    # Lazzeri_Frodella_Rossi_etal_2021, dbase dupe - remove RR version (no link)
    !(study_id == "Lazzeri_Frodella_Rossi_etal_2021" & dbase == "ResearchRabbit"),
    # Michalek_French_Kasischke_etal_2000, dbase dupe - remove RR version (no link)
    !(study_id == "Michalek_French_Kasischke_etal_2000" & dbase == "ResearchRabbit"),
    # Mohieddinne_Brasseur_Spicher_etal_2019, dbase dupe - remove RR version (no link)
    !(study_id == "Mohieddinne_Brasseur_Spicher_etal_2019" & dbase == "ResearchRabbit"),
    # Murphy_Ogilvie_Meng_etal_2011, dbase dupe - remove RR version (no link)
    !(study_id == "Murphy_Ogilvie_Meng_etal_2011" & dbase == "ResearchRabbit"),
    # Simpson_Wooster_Smith_etal_2016, dbase dupe - remove RR version (no link)
    !(study_id == "Simpson_Wooster_Smith_etal_2016" & dbase == "ResearchRabbit"),
    # Wang_Quan_He_etal_2019, dbase dupe - remove RR version (no link)
    !(study_id == "Wang_Quan_He_etal_2019" & dbase == "ResearchRabbit"),
    # White_Ogilvie_Campbell_etal_2012, dbase dupe - remove RR version (no link)
    !(study_id == "White_Ogilvie_Campbell_etal_2012" & dbase == "ResearchRabbit"),
    # White_Ryan_Key_etal_1996, dbase dupe - remove RR version (no link)
    !(study_id == "White_Ryan_Key_etal_1996" & dbase == "ResearchRabbit")
  )

## 6c) Recheck study ID duplicates after filtering -----------------
id_dupes_after <- bib_stage3 %>%
  janitor::get_dupes(study_id)


# STEP 7: INSPECT AND REMOVE LOWERCASE TITLE DUPLICATES ==============

## 7a) Record lowercase title duplicates before manual filtering ---
title_dupes_before <- bib_stage3 %>%
  mutate(lower_title = str_to_lower(title)) %>%
  janitor::get_dupes(lower_title)

## 7b) Apply manual title duplicate rules ------------
bib_stage4 <- bib_stage3 %>%
  filter(
    # Rogan_Franklin_Roberts_2002, dbase dupe - remove RR version (wrong id, no link)
    !(study_id == "Rogan_Franklin_Roberts_etal_2002" & dbase == "ResearchRabbit"),
    # Fernández–Manso_Roberts_Quintano_2016, dbase dupe - remove RR version (wrong id, no link)
    !(study_id == "Fernández–Manso_Roberts_Quintano_etal_2016" & dbase == "ResearchRabbit"),
    # Bond-Lamberty_Peckham_Ahl_etal_2007, dbase dupe - remove RR version (no link)
    !(study_id == "Bond-Lamberty_Peckham_Ahl_etal_2007" & dbase == "ResearchRabbit"),
    # Alves_Llovería_Pérez‐Cabello_2018, dbase dupe - remove RR version (wrong id, no link)
    !(study_id == "Alves_Llovería_Pérez‐Cabello_etal_2018" & dbase == "ResearchRabbit"),
    # Melander_Einola_Ritala_2019, dbase dupe - remove RR version (no link)
    !(study_id == "Melander_Einola_Ritala_etal_2019" & dbase == "ResearchRabbit"),
    # Zenner_Berger_etal_2008, dbase dupe - remove RR version (no link)
    !(study_id == "Zenner_Berger_etal_2008" & dbase == "ResearchRabbit"),
    # Epting_Verbyla_etal_2005, dbase dupe - remove RR version (no link)
    !(study_id == "Epting_Verbyla_etal_2005" & dbase == "ResearchRabbit"),
    # Fernández‐Guisuraga_Suárez‐Seoane_Calvo_etal_2019, dbase dupe - remove RR version (no link)
    !(study_id == "Fernández‐Guisuraga_Suárez‐Seoane_Calvo_etal_2019" & dbase == "ResearchRabbit"),
    # "monitoring vegetation systems in the great plains with erts", remove copy with incorrect year
    !(study_id == "Rouse_Haas_Schell_etal_1973" & dbase == "ResearchRabbit"),
    # Abrahamson_1984, remove version with missing date
    !(study_id == "Abrahamson_NA" & dbase == "ResearchRabbit"),
    # Barrett_Mcguire_Hoy_etal_2011, dbase dupe - remove RR version (no link)
    !(study_id == "Barrett_Mcguire_Hoy_etal_2011" & dbase == "ResearchRabbit"),
    # Fernández‐Guisuraga_Suárez‐Seoane_Calvo_etal_2021, dbase dupe - remove RR version (no link)
    !(study_id == "Fernández‐Guisuraga_Suárez‐Seoane_Calvo_etal_2021" & dbase == "ResearchRabbit"),
    # Dozier_Frew_1990, remove copy with wrong date
    !(study_id == "Dozier_Frew_1989" & dbase == "ResearchRabbit"),
    # Poon_Kinoshita_etal_2018, dbase dupe - remove RR version (no link)
    !(study_id == "Poon_Kinoshita_etal_2018" & dbase == "ResearchRabbit"),
    # Heyerdahl_Brubaker_Agee_2001, remove version with missing age
    !(study_id == "Heyerdahl_Brubaker_Agee_etal_NA" & dbase == "ResearchRabbit"),
    # "spatial optimization of ground-based primary extraction routes using the bestway decision support system", remove RR version (wrong date)
    !(study_id == "Flisberg_Rönnqvist_Willén_etal_2020" & dbase == "ResearchRabbit"),
    # Schweier_Magagnotti_Jourgholami_etal_2019, dbase dupe - remove RR version (wrong id, no link)
    !(study_id == "Schweier_Magagnotti_Jourgholami_etal_2019" & dbase == "ResearchRabbit"),
    # Mattila_Tokola_etal_2019, dbase dupe - remove RR version (wrong id, no link)
    !(study_id == "Mattila_Tokola_etal_2019" & dbase == "ResearchRabbit"),
    # Slesak_Kaebisch_etal_2016, dbase dupe - remove RR version (wrong id, no link)
    !(study_id == "Slesak_Kaebisch_etal_2016" & dbase == "ResearchRabbit"),
    # Bond‐Lamberty_Peckham_Ahl_etal_2007, dbase dupe - remove RR version
    !(study_id == "Bond‐Lamberty_Peckham_Ahl_etal_2007" & dbase == "ResearchRabbit")
  )

## 7c) Recheck lowercase title duplicates after filtering ----------
title_dupes_after <- bib_stage4 %>%
  mutate(lower_title = str_to_lower(title)) %>%
  janitor::get_dupes(lower_title)


# STEP 8: INSPECT AND REMOVE NEAR-DUPLICATE TITLES ===================

## 8a) Build the title distance matrix -----------------------------
# Levenshtein distance <= 5 was used to flag likely near-duplicate 
# titles for manual inspection.
dist_matrix <- stringdist::stringdistmatrix(
  bib_stage4$title,
  bib_stage4$title,
  method = "lv"
)

## 8b) Convert the matrix to suspected duplicate pairs ------------
similar_pairs <- which(dist_matrix <= 5 & dist_matrix > 0, arr.ind = TRUE)

suspected_duplicates <- tibble(
  index1   = similar_pairs[, 1],
  index2   = similar_pairs[, 2],
  title1   = bib_stage4$title[similar_pairs[, 1]],
  title2   = bib_stage4$title[similar_pairs[, 2]],
  distance = dist_matrix[similar_pairs]
) %>%
  filter(index1 < index2)

## 8c) Apply manual near-duplicate rules -------------
bib_final <- bib_stage4 %>%
  filter(
    # Bottinelli_Hallaire_Pousse_etal_2014, dbase dupe - remove RR version (no link)
    !(study_id == "Bottinelli_Hallaire_Pousse_etal_2014" & dbase == "ResearchRabbit"),
    # Murphy_Ogilvie_etal_2009, dbase dupe - remove RR version
    !(study_id == "Murphy_Ogilvie_etal_2009" & dbase == "ResearchRabbit"),
    # Rogan_Yool_etal_2001, dbase dupe - remove RR version
    !(study_id == "Rogan_Yool_etal_2001" & dbase == "ResearchRabbit"),
    # Kankare_Saarinen_Peuhkurinen_etal_2019, dbase dupe - remove RR version
    !(study_id == "Kankare_Saarinen_Peuhkurinen_etal_2019" & dbase == "ResearchRabbit")
  )

## 8d) Keep only the final distinct ResearchRabbit subset ----------
rr_final <- bib_final %>%
  filter(dbase == "ResearchRabbit")


# STEP 9: EXPORT THE FINAL RESEARCHRABBIT FILE =======================

## 9a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 9b) Write the final ResearchRabbit-only file --------------------
readr::write_csv(
  x    = rr_final,
  file = output_file,
  na   = ""
)


# STEP 10: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 10a) Count rows removed at each major stage ---------------------
rows_removed_exact <- nrow(combined_bib) - nrow(bib_stage1)
rows_removed_doi   <- nrow(bib_stage1) - nrow(bib_stage2)
rows_removed_id    <- nrow(bib_stage2) - nrow(bib_stage3)
rows_removed_title <- nrow(bib_stage3) - nrow(bib_stage4)
rows_removed_near  <- nrow(bib_stage4) - nrow(bib_final)

## 10b) Summarize final database counts ----------------------------
final_dbase_totals <- bib_final %>%
  count(dbase, name = "n") %>%
  arrange(desc(n), dbase)

## 10c) Check duplicates remaining in final RR subset -------------
rr_duplicate_study_ids <- rr_final %>%
  janitor::get_dupes(study_id)

rr_duplicate_dois <- rr_final %>%
  filter(!is.na(doi), doi != "") %>%
  janitor::get_dupes(doi)

## 10d) Create a consistent summary table --------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Merged bibliography input file",
    "ResearchRabbit input file",
    "Output file",
    "Rows imported from merged bibliography",
    "Rows imported from parsed ResearchRabbit",
    "Rows combined before de-duplication",
    "Rows removed as exact duplicates",
    "Rows removed by DOI duplicate rules",
    "Rows removed by study-ID duplicate rules",
    "Rows removed by lowercase title duplicate rules",
    "Rows removed by near-duplicate title rules",
    "Final rows across all databases",
    "Final ResearchRabbit rows exported",
    "DOI duplicates before manual DOI filtering",
    "DOI duplicates after manual DOI filtering",
    "Study-ID duplicates before manual ID filtering",
    "Study-ID duplicates after manual ID filtering",
    "Lowercase title duplicates before manual title filtering",
    "Lowercase title duplicates after manual title filtering",
    "Suspected near-duplicate title pairs",
    "Remaining duplicate study IDs in final RR subset",
    "Remaining duplicate DOI values in final RR subset"
  ),
  value = c(
    proj_root,
    merged_file,
    researchrabbit_file,
    output_file,
    nrow(merged_bib),
    nrow(researchrabbit_bib),
    nrow(combined_bib),
    rows_removed_exact,
    rows_removed_doi,
    rows_removed_id,
    rows_removed_title,
    rows_removed_near,
    nrow(bib_final),
    nrow(rr_final),
    nrow(doi_dupes_before),
    nrow(doi_dupes_after),
    nrow(id_dupes_before),
    nrow(id_dupes_after),
    nrow(title_dupes_before),
    nrow(title_dupes_after),
    nrow(suspected_duplicates),
    nrow(rr_duplicate_study_ids),
    nrow(rr_duplicate_dois)
  )
)

## 10e) Print quality checks to the console -----------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n============= FINAL DATABASE COUNTS =============\n")
print(final_dbase_totals, n = Inf)

cat("\n===== DOI DUPLICATES BEFORE MANUAL FILTERING =====\n")
if (nrow(doi_dupes_before) > 0) {
  print(doi_dupes_before, n = Inf)
} else {
  cat("No DOI duplicates were found before manual filtering.\n")
}

cat("\n===== STUDY-ID DUPLICATES BEFORE MANUAL FILTERING =====\n")
if (nrow(id_dupes_before) > 0) {
  print(id_dupes_before, n = Inf)
} else {
  cat("No study-ID duplicates were found before manual filtering.\n")
}

cat("\n===== LOWERCASE TITLE DUPLICATES BEFORE MANUAL FILTERING =====\n")
if (nrow(title_dupes_before) > 0) {
  print(title_dupes_before, n = Inf)
} else {
  cat("No lowercase title duplicates were found before manual filtering.\n")
}

cat("\n===== SUSPECTED NEAR-DUPLICATE TITLES =====\n")
if (nrow(suspected_duplicates) > 0) {
  print(suspected_duplicates, n = Inf)
} else {
  cat("No suspected near-duplicate title pairs were found.\n")
}

cat("\n===== FINAL RR DUPLICATE STUDY IDs (IF ANY) =====\n")
if (nrow(rr_duplicate_study_ids) > 0) {
  print(rr_duplicate_study_ids, n = Inf)
} else {
  cat("No duplicate study IDs remain in the final ResearchRabbit subset.\n")
}

cat("\n===== FINAL RR DUPLICATE DOI VALUES (IF ANY) =====\n")
if (nrow(rr_duplicate_dois) > 0) {
  print(rr_duplicate_dois, n = Inf)
} else {
  cat("No duplicate DOI values remain in the final ResearchRabbit subset.\n")
}

cat("\nFinal ResearchRabbit snowball-search export complete.\n")

