# FIG_1_intervention_count_by_pub_year.R =======================================
# 
# Figure output:
# This script creates a stacked bar chart showing the number of
# intervention cases by publication year.
#
# Each study can contribute up to three intervention categories.
# Publication year is extracted from the last 4 characters of
# the study_id field.
#
# Expected folder structure:
# Roach-Krajewski_et_al_2026_packaged/
# ├── data/
# │   ├── 4_final_study_data/
# │   │   └── evidence_map_data.xlsx
# │   └── 5_extra_figure_data/
# ├── R/
# │   └── FIGURES/
# └── figures/
#
# This script is designed so that a user can download the full
# packaged folder and run the script from anywhere inside it.



# STEP 1: SET UP THE PROJECT ROOT AND LOAD PACKAGES ====================

## 1a) Find the project root folder --------------------------------
# This function searches upward from the current working directory
# until it finds the expected packaged project folder.
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
# This keeps all relative paths consistent for the rest of the script.
setwd(proj_root)

## 1d) Confirm the root folder in the console ----------------------
message("Project root set to: ", proj_root)

## 1e) Load required packages --------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
  library(scales)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
sheet_name  <- "5_study_data"
output_file <- file.path(proj_root, "figures", "FIG_1_intervention_count_by_pub_year.png")

## 1g) Define plot settings ----------------------------------------
start_year     <- 1995
end_year       <- 2026
major_step     <- 5
max_y          <- 10
base_font_size <- 16
plot_width     <- 12
plot_height    <- 7
plot_dpi       <- 200


# STEP 2: IMPORT THE STUDY DATA ========================================

## 2a) Check that the Excel file exists ----------------------------
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

## 2b) Read the finalized Excel sheet ------------------------------
study_data <- read_excel(
  path         = input_file,
  sheet        = sheet_name,
  skip         = 1,
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))


# STEP 3: PREPARE THE PUBLICATION YEAR ================================

## 3a) Extract the publication year from study_id ------------------
# The year is assumed to be stored in the last 4 characters of study_id.
study_data <- study_data %>%
  mutate(
    pub_year_chr = str_sub(study_id, start = -4),
    pub_year     = suppressWarnings(as.integer(pub_year_chr)),
    pub_year     = if_else(
      !is.na(pub_year) & pub_year >= 1900 & pub_year <= 2099,
      pub_year,
      NA_integer_
    )
  )


# STEP 4: PREPARE THE INTERVENTION DATA ===============================

## 4a) Keep only the columns needed for this figure ----------------
interventions_long <- study_data %>%
  select(
    study_id,
    pub_year,
    intervention_category_1,
    intervention_category_2,
    intervention_category_3
  )

## 4b) Convert the 3 intervention columns to long format ----------
# This creates one row per study x intervention combination.
interventions_long <- interventions_long %>%
  pivot_longer(
    cols      = starts_with("intervention_category_"),
    names_to  = "slot",
    values_to = "intervention"
  )

## 4c) Clean the intervention labels -------------------------------
# This step:
# - removes extra spaces
# - turns blank values and "NA" text into true missing values
# - converts labels to sentence case
interventions_long <- interventions_long %>%
  mutate(
    intervention = str_squish(intervention),
    intervention = na_if(intervention, ""),
    intervention = na_if(intervention, "NA"),
    intervention = if_else(
      is.na(intervention),
      NA_character_,
      str_to_sentence(str_to_lower(intervention))
    )
  )

## 4d) Remove incomplete and duplicate records ---------------------
# A study should only contribute once to a given intervention in a year.
interventions_long <- interventions_long %>%
  filter(!is.na(pub_year), !is.na(intervention)) %>%
  distinct(study_id, pub_year, intervention)


# STEP 5: SUMMARIZE THE DATA FOR PLOTTING =============================

## 5a) Count intervention cases by year and intervention ----------
counts_yi <- interventions_long %>%
  count(pub_year, intervention, name = "n")

## 5b) Order the legend by total intervention frequency -----------
intervention_order <- counts_yi %>%
  group_by(intervention) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(intervention)

counts_yi <- counts_yi %>%
  mutate(intervention = factor(intervention, levels = intervention_order))


# STEP 6: DEFINE AXIS BREAKS AND COLOURS ==============================

## 6a) Create x-axis breaks ---------------------------------------
x_major_breaks <- seq(start_year, end_year, by = major_step)
x_minor_breaks <- seq(start_year, end_year, by = 1)

## 6b) Create a colour palette ------------------------------------
pal_cb <- scales::hue_pal()(length(levels(counts_yi$intervention)))
names(pal_cb) <- levels(counts_yi$intervention)


# STEP 7: BUILD THE FIGURE ============================================

## 7a) Create the stacked bar chart --------------------------------
p_fig1 <- ggplot(counts_yi, aes(x = pub_year, y = n, fill = intervention)) +
  geom_col(width = 0.85) +
  scale_fill_manual(
    values = pal_cb,
    name   = "Disturbance type: ",
    breaks = intervention_order
  ) +
  scale_x_continuous(
    limits       = c(start_year, end_year),
    breaks       = x_major_breaks,
    minor_breaks = x_minor_breaks,
    labels       = function(x) format(x, scientific = FALSE)
  ) +
  scale_y_continuous(
    breaks = seq(0, max_y, by = 2),
    limits = c(0, max_y),
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Publication year",
    y = "Number of cases"
  ) +
  theme_minimal(base_size = base_font_size) +
  theme(
    legend.position = "top",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

## 7b) Display the figure in the plotting window -------------------
print(p_fig1)


# STEP 8: EXPORT THE FIGURE ===========================================

## 8a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 8b) Save the figure ---------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_fig1,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi
)


# STEP 9: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 9a) Create a consistent summary table ---------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Rows imported",
    "Unique studies imported",
    "Studies with valid publication year",
    "Unique interventions plotted",
    "Total intervention cases plotted",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    sheet_name,
    nrow(study_data),
    n_distinct(study_data$study_id),
    n_distinct(interventions_long$study_id),
    n_distinct(interventions_long$intervention),
    sum(counts_yi$n),
    output_file
  )
)

## 9b) Count total cases by intervention ---------------------------
intervention_totals <- counts_yi %>%
  group_by(intervention) %>%
  summarise(total_cases = sum(n), .groups = "drop") %>%
  arrange(desc(total_cases))

## 9c) Count total cases by publication year ----------------------
year_totals <- counts_yi %>%
  group_by(pub_year) %>%
  summarise(total_cases = sum(n), .groups = "drop") %>%
  arrange(pub_year)

## 9d) Create a compact intervention summary string ---------------
totals_str <- intervention_totals %>%
  mutate(label = paste0(intervention, " (", total_cases, ")")) %>%
  pull(label) %>%
  paste(collapse = "; ")

## 9e) Print quality checks to the console ------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n========== TOTAL CASES BY INTERVENTION ==========\n")
print(intervention_totals, n = Inf)

cat("\n======== TOTAL CASES BY PUBLICATION YEAR ========\n")
print(year_totals, n = Inf)

cat("\n===== COMPACT INTERVENTION TOTALS STRING =====\n")
cat(totals_str, "\n")

