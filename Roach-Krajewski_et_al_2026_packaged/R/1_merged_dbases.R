# 1_merged_dbases.R
# ============================================================
# Script purpose:
# This script combines the parsed bibliographic databases,
# removes duplicate records using DOI- and ID-based rules,
# and exports a merged bibliography file for manual inspection.
#
# Inputs:
# data/1_parsed_databases/parsed_ebscohost.csv
# data/1_parsed_databases/parsed_googlescholar.csv
# data/1_parsed_databases/parsed_nrcanostr.csv
# data/1_parsed_databases/parsed_openalex.csv
# data/1_parsed_databases/parsed_scopus.csv
#
# Output:
# data/2_merged_databases/2_merged_biblio_to_be_inspected.csv
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
  library(tibble)
  library(tidystringdist)
})

## 1f) Define input and output paths -------------------------------
input_dir <- file.path(proj_root, "data", "1_parsed_databases")

input_files <- c(
  parsed_ebscohost      = file.path(input_dir, "parsed_ebscohost.csv"),
  parsed_googlescholar  = file.path(input_dir, "parsed_googlescholar.csv"),
  parsed_nrcanostr      = file.path(input_dir, "parsed_nrcanostr.csv"),
  parsed_openalex       = file.path(input_dir, "parsed_openalex.csv"),
  parsed_scopus         = file.path(input_dir, "parsed_scopus.csv")
)

output_file <- file.path(
  proj_root,
  "data",
  "2_merged_databases",
  "2_merged_biblio_to_be_inspected.csv"
)


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Check that all required input files exist -------------------
check_input_files <- function(paths) {
  missing_files <- paths[!file.exists(paths)]
  
  if (length(missing_files) > 0) {
    stop(
      paste0(
        "The following required input files were not found:\n",
        paste(missing_files, collapse = "\n"),
        "\n\nPlease confirm that the packaged folder structure is correct ",
        "and that the parsing scripts have already been run."
      ),
      call. = FALSE
    )
  }
}

## 2b) Read one parsed database file -------------------------------
read_parsed_database <- function(path) {
  readr::read_csv(
    file           = path,
    guess_max      = Inf,
    show_col_types = FALSE
  )
}


# STEP 3: IMPORT AND COMBINE THE PARSED DATABASES =====================

## 3a) Confirm that all parsed input files exist -------------------
check_input_files(input_files)

## 3b) Read and bind the parsed databases --------------------------
biblio_data_combo <- dplyr::bind_rows(
  read_parsed_database(input_files["parsed_ebscohost"]),
  read_parsed_database(input_files["parsed_googlescholar"]),
  read_parsed_database(input_files["parsed_nrcanostr"]),
  read_parsed_database(input_files["parsed_openalex"]),
  read_parsed_database(input_files["parsed_scopus"])
)

## 3c) Check that key columns are present --------------------------
required_cols <- c("study_id", "year", "title", "doi", "dbase")

missing_cols <- setdiff(required_cols, names(biblio_data_combo))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing after combining the parsed databases:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: REMOVE OBVIOUS DOI/ID DUPLICATES ============================

## 4a) Prefer the alphabetically last database within each DOI+ID --
biblio_data_stage1 <- biblio_data_combo %>%
  mutate(ordered_dbase = as.ordered(dbase)) %>%
  group_by(doi, study_id) %>%
  filter(ordered_dbase == max(ordered_dbase)) %>%
  ungroup()


# STEP 5: APPLY MANUAL DOI-BASED DUPLICATE RULES =====================

biblio_data_stage2 <- biblio_data_stage1 %>%
  filter(
    !(doi == "https://doi.org/10.1002/gj.4589" & year != 2022),
    !(doi == "https://doi.org/10.1002/ldr.2214" & year != 2013),
    !(doi == "https://doi.org/10.1002/ldr.3533" & year != 2019),
    !(doi == "https://doi.org/10.1002/ldr.4553" & year != 2022),
    !(doi == "https://doi.org/10.1016/j.catena.2016.02.017" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1111/1365-2745.12504" & year != 2015),
    !(doi == "https://doi.org/10.1111/j.1529-8817.2003.00772.x" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1186/s13021-019-0121-0" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.3390/hydrology10010007" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1002/eap.1439" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1002/hyp.13882" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1002/hyp.14139" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1002/met.1425" & year != 2013),
    !(doi == "https://doi.org/10.1002/ppp.2001" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1002/ppp.2218" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1002/rse2.328" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1007/978-3-030-50930-9_21" & year != 2020),
    !(doi == "https://doi.org/10.1007/978-981-19-4200-6_17" & year != 2023),
    !(doi == "https://doi.org/10.1007/s12665-016-6349-z" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1007/s13280-017-0984-9" & year != 2017),
    !(doi == "https://doi.org/10.1007/s40725-021-00153-8" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/S0378-1127(00)00310-8" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.agrformet.2022.109022" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1016/j.cageo.2017.04.007" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.catena.2006.04.006" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.catena.2007.12.005" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.catena.2008.01.003" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.catena.2019.104320" & year != 2020),
    !(doi == "https://doi.org/10.1016/j.earscirev.2020.103414" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.enggeo.2004.01.005" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.envc.2021.100151" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1016/j.epsl.2010.07.040" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.foreco.2011.06.009" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.foreco.2011.06.009" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.foreco.2017.02.049" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.foreco.2023.121395" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.foreco.2024.122323" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.foreco.2024.122490" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.geoderma.2017.12.031" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.geoderma.2021.115280" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.geoderma.2022.116290" & year != 2023),
    !(doi == "https://doi.org/10.1016/j.jag.2024.103720" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1016/j.jenvman.2020.110491" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.jenvman.2020.111510" & year != 2021),
    !(doi == "https://doi.org/10.1016/j.jhydrol.2021.126009" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.landusepol.2021.105776" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1016/j.palaeo.2018.07.001" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.pce.2022.103246" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.1016/j.rse.2006.10.020" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.rse.2019.111539" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.rse.2022.113239" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1016/j.scitotenv.2022.156852" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.still.2018.10.013" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1016/j.tfp.2024.100648" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1034/j.1600-0889.2003.00036.x" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1038/s41598-021-82527-3" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1046/j.1365-2486.2000.06022.x" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1073/pnas.1523397113" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/01431160110109589" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/01431160152558279" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/01431160600976061" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/01431161.2013.796096" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/01431161.2018.1479790" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/01431169608949126" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/014311697219114" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/014311699213505" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/02827581.2017.1296181" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1080/02827581.2017.1339121" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1080/02827581.2017.1418421" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1080/09640568.2022.2027747" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1080/10106040802488542" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1080/14942119.2018.1498687" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1080/14942119.2022.2044724" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1080/17550874.2012.716086" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1088/1748-9326/10/12/125006" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1088/1748-9326/6/3/035203" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1088/1748-9326/acd6a8" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1088/1755-1315/1254/1/012132" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1088/1755-1315/47/1/012030" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1088/2632-2153/aba480" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1093/forestry/cpab024" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1093/forsci/fxz065" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1109/36.285204" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1109/36.739096" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1109/comeas.1995.472396" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1111/aec.12008" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1111/avsc.70006" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1111/ecog.03860" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1111/j.1365-2699.2006.01527.x" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1111/j.1466-8238.2011.00689.x" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1111/j.1600-0706.2011.19777.x" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1111/jbi.14302" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1111/rec.13827" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1117/12.2685554" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1117/12.462357" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1127/0372-8854/2011/0055S2-0045" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1139/X08-065" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1139/cjz-2022-0062" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.11591/eei.v13i3.6578" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1186/s13071-024-06446-8" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1186/s13717-020-0214-4" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1201/b18077-36" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.1371/journal.pone.0280187" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.1890/1051-0761(2006)016[1148:ROFSAS]2.0.CO;2" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.2113/EEG-D-21-00021" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.2136/sssaj2018.08.0302" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.2136/vzj2011.0102" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.2136/vzj2018.04.0081" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.24057/2071-9388-2021-139" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.2478/tar-2024-0019" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.2489/jswc.74.1.12" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.30638/eemj.2013.260" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3390/app11125423" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/f10110992" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3390/f12020179" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/f15060923" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3390/fire5040106" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3390/fire6080301" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3390/fire7070238" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/land11081172" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/rs11161868" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/rs13193873" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/rs16101718" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3390/rs16101728" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3390/su11010003" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.3832/ifor1779-008" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.3832/ifor2069-009" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.5194/egusphere-egu2020-18080" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.5194/egusphere-egu21-13140" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.5194/egusphere-egu21-15880" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.5194/egusphere-egu22-239" & dbase != "OpenAlex"),
    !(doi == "https://doi.org/10.5194/egusphere-egu23-14216" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.5194/egusphere-egu23-16066" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.5552/crojfe.2021.837" & dbase != "Scopus"),
    !(doi == "https://doi.org/10.5194/hess-16-4191-2012" & dbase != "EBSCOhost"),
    !(doi == "https://doi.org/10.5552/crojfe.2022.1727" & dbase != "Scopus")
  )


# STEP 6: APPLY MANUAL ID-BASED DUPLICATE RULES ======================

biblio_data_stage3 <- biblio_data_stage2 %>%
  filter(
    !(study_id == "Whitman_Parisien_Holsinger_etal_2020" & dbase != "OpenAlex"),
    !(study_id == "Ahjolia_Jariwala_Naik_etal_2022" & dbase != "Scopus"),
    !(study_id == "Allen_Sorbel_2008" & dbase != "OpenAlex"),
    !(study_id == "Alonso-Arroyo_Forte_Camps_etal_2013" & dbase != "Scopus"),
    !(study_id == "Andreoli_Pitscheider_Rozzoni_etal_2022" & dbase != "Scopus"),
    !(study_id == "Arai_Shimabukuro_Dutra_etal_2019" & dbase != "OpenAlex"),
    !(study_id == "Aranha_Enes_Calvão_etal_2020" & dbase != "Scopus"),
    !(study_id == "Ayres_Colliander_Cosh_etal_2021" & dbase != "OpenAlex"),
    !(study_id == "Banfai_Bowman_2007" & dbase != "OpenAlex"),
    !(study_id == "Berberian_Lopez_Avouris_etal_2023" & dbase != "OpenAlex"),
    !(study_id == "Bour_Danneyrolles_Boucher_etal_2021" & dbase != "Scopus"),
    !(study_id == "Bowman_1991" & dbase != "OpenAlex"),
    !(study_id == "Broadbent_Zarin_Asner_etal_2006" & dbase != "OpenAlex"),
    !(study_id == "Camps_Forte_Ramos_etal_2012" & dbase != "Scopus"),
    !(study_id == "Ceca_Ferreyra_Diez_etal_2016" & dbase != "Scopus"),
    !(study_id == "Chen_Ju_Cihlar_etal_2003" & dbase != "OpenAlex"),
    !(study_id == "Costa_Gardiner_Locatelli_etal_2023" & dbase != "Scopus"),
    !(study_id == "DeGroote_Mercer_Fisher_etal_2007" & dbase != "OpenAlex"),
    !(study_id == "Diamond_Ross_2016" & dbase != "OpenAlex"),
    !(study_id == "Dubé_Berch_2013" & dbase != "OpenAlex"),
    !(study_id == "Emili_Price_Fitzgerald_2006" & dbase != "OpenAlex"),
    !(study_id == "Epting_Verbyla_2005" & dbase != "Scopus"),
    !(study_id == "Farahmand_Stavros_Reager_etal_2020" & dbase != "OpenAlex"),
    !(study_id == "Guilinger_Foufoula-Georgiou_Gray_etal_2023" & dbase != "Scopus"),
    !(study_id == "Han_Abitew_Bazrkar_etal_2024" & dbase != "Scopus"),
    !(study_id == "Hazlett_Broad_Gordon_etal_2008" & dbase != "OpenAlex"),
    !(study_id == "Hilgartner_Nejako_Casey_2009" & dbase != "OpenAlex"),
    !(study_id == "Holder_2004" & dbase != "OpenAlex"),
    !(study_id == "Hoy_French_Turetsky_etal_2008" & dbase != "OpenAlex"),
    !(study_id == "Huang_Asner_2010" & dbase != "OpenAlex"),
    !(study_id == "Jain_Saxena_Sharma_etal_2021" & dbase != "OpenAlex"),
    !(study_id == "Keeley_2009" & dbase != "OpenAlex"),
    !(study_id == "Keeton_Franklin_2004" & dbase != "OpenAlex"),
    !(study_id == "Kesari_Bhunia_Kumar_etal_2011" & dbase != "OpenAlex"),
    !(study_id == "Khadke_Ghosh_2024" & dbase != "OpenAlex"),
    !(study_id == "Laneve_Bernini_Fusilli_etal_2016" & dbase != "Scopus"),
    !(study_id == "Lavalle_2017" & dbase != "Scopus"),
    !(study_id == "Leblon_2021" & dbase != "Scopus"),
    !(study_id == "Lewis_Hudak_Ottmar_etal_2011" & dbase != "OpenAlex"),
    !(study_id == "Lin_Yan_Wang_etal_2014" & dbase != "Scopus"),
    !(study_id == "Liu_Jafarov_Schaefer_etal_2014" & dbase != "OpenAlex"),
    !(study_id == "Liu_Kirkpatrick_2023" & dbase != "OpenAlex"),
    !(study_id == "Mamgain_Karnatak_Roy_etal_2022" & dbase != "Scopus"),
    !(study_id == "Martinis_Caspard_Plank_etal_2017" & dbase != "Scopus"),
    !(study_id == "Massman_Frank_2004" & dbase != "OpenAlex"),
    !(study_id == "Matheussen_Kirschbaum_Goodman_etal_2000" & dbase != "OpenAlex"),
    !(study_id == "Mohammad_Bhuvaneswari_Koteswari_etal_2022" & dbase != "OpenAlex"),
    !(study_id == "Mohod_Thakare_Bhoyar_etal_2022" & dbase != "Scopus"),
    !(study_id == "Moya_Certini_Fulé_2019" & dbase != "Scopus"),
    !(study_id == "Norovsuren_Mart_Natsagdorj_etal_2023" & dbase != "Scopus"),
    !(study_id == "Pavlova_Danzanova_2020" & dbase != "Scopus"),
    !(study_id == "Psomiadis_Soulis_Efthimiou_2020" & dbase != "OpenAlex"),
    !(study_id == "Radeloff_Mladenoff_Boyce_2000" & dbase != "Scopus"),
    !(study_id == "Reba_Rosli_Makhfuz_etal_2013" & dbase != "Scopus"),
    !(study_id == "Rebertus_Kitzberger_Veblen_etal_1997" & dbase != "OpenAlex"),
    !(study_id == "Rengers_Tucker_Moody_etal_2016" & dbase != "OpenAlex"),
    !(study_id == "Rigden_Powell_Trevino_etal_2020" & dbase != "OpenAlex"),
    !(study_id == "Rios_Santana_Lense_etal_2024" & doi != "https://doi.org/10.17707/AgricultForest.70.1.23"),
    !(study_id == "Rouse_2000" & dbase != "OpenAlex"),
    !(study_id == "Roy_Naceur_McDonald_etal_2022" & dbase != "Scopus"),
    !(study_id == "Santi_Pettinato_Comite_etal_2023" & dbase != "Scopus"),
    !(study_id == "Schneider_Hirsch_Bonhage_etal_2020" & doi != "https://doi.org/10.1016/j.geoderma.2020.114241"),
    !(study_id == "Servidoni_Silva_Santana_etal_2021" & dbase != "Scopus"),
    !(study_id == "Shimabukuro_Beuchle_Grecchi_etal_2014" & dbase != "OpenAlex"),
    !(study_id == "Silva_Santilli_Sano_etal_2019" & dbase != "Scopus"),
    !(study_id == "Souza_Roberts_Monteiro_2005" & dbase != "OpenAlex"),
    !(study_id == "Swetnam_Lynch_Falk_etal_2015" & dbase != "OpenAlex"),
    !(study_id == "Tanase_Panciera_Lowell_etal_2015" & dbase != "OpenAlex"),
    !(study_id == "Thompson_Brokaw_Zimmerman_etal_2002" & dbase != "Scopus"),
    !(study_id == "Torquato_Will_Zhang_etal_2020" & dbase != "OpenAlex"),
    !(study_id == "Wang_Qi_Cochrane_2005" & dbase != "OpenAlex"),
    !(study_id == "White_Host_2008" & dbase != "OpenAlex"),
    !(study_id == "White_Ryan_Key_etal_1996" & dbase != "OpenAlex"),
    !(study_id == "Wolter_Berkley_Peckham_etal_2014" & dbase != "OpenAlex"),
    !(study_id == "Yahia_Ghabi_Karoui_2023" & dbase != "Scopus"),
    !(study_id == "Zhang_Homayouni_Zhao_etal_2023" & dbase != "OpenAlex"),
    !(study_id == "Zhang_Xu_Li_etal_2012" & dbase != "Scopus"),
    !(study_id == "Zhao_Chen_Bakian-Dogaheh_etal_2022" & dbase != "Scopus"),
    !(study_id == "Zong_Tian_2022" & dbase != "OpenAlex"),
    !(study_id == "Zope_Dadlani_Matai_etal_2020" & dbase != "Scopus")
  )


# STEP 7: APPLY EXTRA DOI/TITLE CLEANUP RULES ========================

biblio_data_stage4 <- biblio_data_stage3 %>%
  filter(
    !(doi == "https://doi.org/10.1002/2015WR017738"),
    !(doi == "https://doi.org/10.1017/S0032247407006535"),
    !(doi == "https://doi.org/10.1029/2007GL031387"),
    !(doi == "https://doi.org/10.1029/2023GL104626"),
    !(doi == "https://doi.org/10.1061/(ASCE)HE.1943-5584.0000441"),
    !(doi == "https://doi.org/10.1071/BT9810081"),
    !(doi == "https://doi.org/10.1071/WF06138"),
    !(doi == "https://doi.org/10.1071/WF07175"),
    !(doi == "https://doi.org/10.1071/WF16217"),
    !(doi == "https://doi.org/10.1109/APMC.2016.7931453"),
    !(doi == "https://doi.org/10.1109/ELECSYM.2016.7861032"),
    !(doi == "https://doi.org/10.1109/igarss.2006.503"),
    !(doi == "https://doi.org/10.1109/IGARSS.2007.4423895"),
    !(doi == "https://doi.org/10.1109/igarss.2008.4779486"),
    !(doi == "https://doi.org/10.1109/iswrep.2011.5893403"),
    !(doi == "https://doi.org/10.1109/tsmc.2024.3352660"),
    !(doi == "https://doi.org/10.1117/1.jrs.16.014526"),
    !(doi == "https://doi.org/10.1134/s1995425523020154"),
    !(doi == "https://doi.org/10.1175/1520-0477(1996)077<0305:rofeao>2.0.co;2"),
    !(doi == "https://doi.org/10.12775/eq.2016.011"),
    !(doi == "https://doi.org/10.17221/122/2021-JFS"),
    !(doi == "https://doi.org/10.17707/agricultforest.70.1.23"),
    !(doi == "https://doi.org/10.1890/ES14-00488.1"),
    !(doi == "https://doi.org/10.3233/ATDE231070"),
    !(doi == "https://doi.org/10.3390/f11080790"),
    !(doi == "https://doi.org/10.3390/RS12152380"),
    !(doi == "https://doi.org/10.5194/isprs-archives-XLII-4-W18-435-2019")
  )

## 7b) Apply extra DOI/title cleanup rules from the original script
biblio_data_stage5 <- biblio_data_stage4 %>%
  filter(
    !(doi == "https://doi.org/10.1029/2008GM000847"),
    !(doi == "https://doi.org/10.52151/jae2018551.1676"),
    !(doi == "https://doi.org/10.5194/nhessd-3-3733-2015"),
    !(doi == "https://doi.org/10.21203/rs.3.rs-3681359/v1"),
    !(doi == "https://doi.org/10.2139/ssrn.3985464"),
    !(doi == "https://doi.org/10.1109/TGRS.2009.2021613"),
    !(doi == "https://doi.org/10.5194/egusphere-egu23-3103"),
    !(doi == "https://doi.org/10.1101/2024.01.31.578258"),
    !(doi == "https://doi.org/10.5194/bgd-10-16371-2013"),
    !(doi == "https://doi.org/10.1117/1.JRS.7.073541.full"),
    !(doi == "https://doi.org/10.5194/bg-2021-239"),
    !(doi == "https://doi.org/10.1101/2022.11.29.518357"),
    !(doi == "https://doi.org/10.22541/au.166256297.71210800/v1"),
    !(doi == "https://doi.org/10.1101/756163"),
    !(doi == "https://doi.org/10.5194/egusphere-egu24-2558"),
    !(doi == "https://doi.org/10.5194/icg2022-259"),
    !(doi == "https://doi.org/10.15760/etd.3489")
  )

biblio_data <- biblio_data_stage5 %>%
  filter(
    !(doi == "https://doi.org/10.20886/jppdas.v1i1.2502.g2073"),
    !(doi == "https://doi.org/10.21203/rs.3.rs-3272108/v1"),
    !(doi == "https://doi.org/10.1016/j.foreco.2017.11.038"),
    !(doi == "https://doi.org/10.5194/hess-2020-479"),
    !(doi == "https://doi.org/10.31219/osf.io/2meuw"),
    !(doi == "https://doi.org/10.1130/abs/2021AM-365999"),
    !(doi == "https://doi.org/10.2139/ssrn.3985888"),
    !(doi == "https://doi.org/10.1186/s40490-016-0073-z"),
    !(doi == "https://doi.org/10.21203/rs.3.rs-2596492/v1"),
    !(doi == "https://doi.org/10.1109/IGARSS.2008.4779486"),
    !(doi == "https://doi.org/10.21203/rs.3.rs-1938292/v1"),
    !(doi == "https://doi.org/10.5194/bg-2022-207"),
    !(doi == "https://doi.org/10.48550/arxiv.2211.01958"),
    !(doi == "https://doi.org/10.3233/978-1-61499-834-1-90")
  ) %>%
  select(-ordered_dbase)


# STEP 8: BUILD INSPECTION TABLES =====================================

## 8a) Create exact duplicate check tables -------------------------
duplicate_study_ids <- biblio_data %>%
  janitor::get_dupes(study_id)

duplicate_dois <- biblio_data %>%
  filter(!is.na(doi), doi != "") %>%
  janitor::get_dupes(doi)

duplicate_lower_dois <- biblio_data %>%
  filter(!is.na(doi), doi != "") %>%
  mutate(lower_doi = str_to_lower(doi)) %>%
  janitor::get_dupes(lower_doi)

duplicate_lower_titles <- biblio_data %>%
  mutate(lower_title = str_to_lower(title)) %>%
  janitor::get_dupes(lower_title)

## 8b) Create title string-distance table for manual inspection ----
# This replaces the interactive View() step with a reproducible table.
title_string_distances_head <- biblio_data %>%
  distinct(title) %>%
  tidy_comb_all(title) %>%
  tidy_stringdist() %>%
  arrange(lv, V1, V2) %>%
  slice_head(n = 10000) %>%
  mutate(
    v1_length = str_count(V1),
    v2_length = str_count(V2)
  ) %>%
  arrange(desc(v1_length))


# STEP 9: EXPORT THE MERGED BIBLIOGRAPHY ==============================

## 9a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 9b) Write the merged bibliography file --------------------------
readr::write_csv(
  x    = biblio_data,
  file = output_file,
  na   = ""
)


# STEP 10: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 10a) Count rows removed at each major step ----------------------
rows_removed_obvious <- nrow(biblio_data_combo)  - nrow(biblio_data_stage1)
rows_removed_doi     <- nrow(biblio_data_stage1) - nrow(biblio_data_stage2)
rows_removed_id      <- nrow(biblio_data_stage2) - nrow(biblio_data_stage3)
rows_removed_extra   <- nrow(biblio_data_stage3) - nrow(biblio_data)

## 10b) Summarize databases represented in the final file ----------
dbase_totals <- biblio_data %>%
  count(dbase, name = "n") %>%
  arrange(desc(n), dbase)

## 10c) Create a consistent summary table --------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Output file",
    "Rows imported across parsed databases",
    "Rows after initial DOI+ID de-duplication",
    "Rows exported after all manual rules",
    "Rows removed in initial DOI+ID de-duplication",
    "Rows removed in DOI-based manual rules",
    "Rows removed in ID-based manual rules",
    "Rows removed in extra DOI/title cleanup rules",
    "Unique study IDs exported",
    "Rows with missing DOI",
    "Remaining duplicate study IDs",
    "Remaining duplicate DOI values",
    "Remaining duplicate lowercase DOI values",
    "Remaining duplicate lowercase titles",
    "Title-pair rows in inspection table"
  ),
  value = c(
    proj_root,
    output_file,
    nrow(biblio_data_combo),
    nrow(biblio_data_stage1),
    nrow(biblio_data),
    rows_removed_obvious,
    rows_removed_doi,
    rows_removed_id,
    rows_removed_extra,
    n_distinct(biblio_data$study_id, na.rm = TRUE),
    sum(is.na(biblio_data$doi) | biblio_data$doi == ""),
    nrow(duplicate_study_ids),
    nrow(duplicate_dois),
    nrow(duplicate_lower_dois),
    nrow(duplicate_lower_titles),
    nrow(title_string_distances_head)
  )
)

## 10d) Print quality checks to the console ------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n============= FINAL DATABASE COUNTS =============\n")
print(dbase_totals, n = Inf)

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
  cat("No exact duplicate DOI values remain.\n")
}

cat("\n===== DUPLICATE LOWERCASE DOI VALUES (IF ANY) =====\n")
if (nrow(duplicate_lower_dois) > 0) {
  print(duplicate_lower_dois, n = Inf)
} else {
  cat("No lowercase DOI duplicates remain.\n")
}

cat("\n======== DUPLICATE LOWERCASE TITLES (IF ANY) ========\n")
if (nrow(duplicate_lower_titles) > 0) {
  print(duplicate_lower_titles, n = Inf)
} else {
  cat("No lowercase title duplicates remain.\n")
}

cat("\n===== CLOSEST TITLE STRING-DISTANCE PAIRS (TOP 50) =====\n")
print(head(title_string_distances_head, 50), n = 50)

cat("\nMerged bibliography export complete.\n")

