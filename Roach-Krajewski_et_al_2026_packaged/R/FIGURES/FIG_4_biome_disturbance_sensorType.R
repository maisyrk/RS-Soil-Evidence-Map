# FIG_4_biome_disturbance_sensorType.R =========================================
# 
# Figure output:
# This script creates a faceted horizontal bar chart showing the
# number of cases for each biome × sensor type combination,
# grouped by disturbance category.
#
# Each panel represents one disturbance category, and within each
# panel the bars show how often different sensor types were used
# across biomes.
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
  library(ggplot2)
  library(readxl)
  library(forcats)
  library(stringr)
  library(scales)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
sheet_name  <- "5_study_data"
output_file <- file.path(proj_root, "figures", "FIG_4_biome_disturbance_sensorType.png")

## 1g) Define plot settings ----------------------------------------
plot_width      <- 10
plot_height     <- 10
plot_dpi        <- 350
base_font_size  <- 13


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
  na           = c("", "NA"),
  col_types    = "text",
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))

## 2c) Check that required columns are present ---------------------
required_cols <- c(
  "study_id",
  "biome",
  "sensor_type",
  "intervention_category_1",
  "intervention_category_2",
  "intervention_category_3"
)

missing_cols <- setdiff(required_cols, names(study_data))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the input sheet:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 3: DEFINE HELPER FUNCTIONS =====================================

## 3a) Standardize semicolon-separated fields ----------------------
# This function normalizes semicolons and surrounding spaces only.
# It intentionally does NOT modify commas.
split_semis <- function(x) {
  x %>%
    coalesce("") %>%
    str_replace_all("\\s*;\\s*", ";")
}

## 3b) Standardize disturbance labels ------------------------------
# Only the main disturbance categories used in the figure are kept.
# Any other value is converted to NA and removed later.
canon_disturbance <- function(x) {
  xl <- str_to_lower(str_squish(x))
  
  case_when(
    xl %in% c("wildfire", "fire", "burn", "forest fire")            ~ "Wildfire",
    xl %in% c("logging", "harvesting", "clearcut", "clear cut")     ~ "Harvesting",
    xl %in% c("windthrow", "wind throw", "wind storm")              ~ "Windthrow",
    xl %in% c("insect outbreak", "insect", "beetle", "bark beetle") ~ "Insect outbreak",
    xl %in% c("mining", "mine")                                     ~ "Mining",
    xl %in% c("off-road vehicles", "off road vehicles", "orv")      ~ "Off-road vehicles",
    TRUE                                                            ~ NA_character_
  )
}

## 3c) Standardize sensor labels ----------------------------------
# This corrects capitalization and a few common spelling variants.
canon_sensor <- function(x) {
  xl <- str_to_lower(str_squish(x))
  
  case_when(
    xl %in% c("lidar", "li dar")                                ~ "LiDAR",
    xl %in% c("rgb", "rbg")                                     ~ "RGB",
    xl %in% c("sar")                                            ~ "SAR",
    xl %in% c("multispectral", "multi-spectral", "mulispectral")~ "Multispectral",
    xl %in% c("hyperspectral", "hyper-spectral")                ~ "Hyperspectral",
    xl %in% c("stereo")                                         ~ "Stereo",
    xl %in% c("thermal", "tirs")                                ~ "Thermal",
    TRUE                                                        ~ str_to_title(xl)
  )
}


# STEP 4: PREPARE THE BIOME × SENSOR × DISTURBANCE DATA ==============

## 4a) Keep only the columns needed for this figure ----------------
bio_base <- study_data %>%
  transmute(
    study_id,
    biome       = split_semis(biome),
    sensor_type = split_semis(sensor_type),
    intervention_category_1,
    intervention_category_2,
    intervention_category_3
  )

## 4b) Pivot the 3 intervention fields into one column ------------
bio_base <- bio_base %>%
  pivot_longer(
    cols      = starts_with("intervention_category_"),
    names_to  = "slot",
    values_to = "intervention_raw"
  )

## 4c) Separate semicolon-delimited biome and sensor entries ------
# This creates one row per study × biome × sensor × intervention
# combination before filtering and deduplication.
bio_base <- bio_base %>%
  separate_rows(biome, sep = ";") %>%
  separate_rows(sensor_type, sep = ";")

## 4d) Clean the labels and keep only valid rows -------------------
bio_base <- bio_base %>%
  mutate(
    biome        = str_squish(biome),
    sensor_type  = canon_sensor(sensor_type),
    intervention = canon_disturbance(intervention_raw)
  ) %>%
  filter(
    biome != "",
    sensor_type != "",
    !is.na(intervention)
  ) %>%
  distinct(study_id, biome, sensor_type, intervention)

## 4e) Stop if there are no valid rows to plot ---------------------
if (nrow(bio_base) == 0) {
  stop("No valid biome × sensor × disturbance cases were found for plotting.", call. = FALSE)
}


# STEP 5: SUMMARIZE THE DATA FOR PLOTTING =============================

## 5a) Count cases by biome, sensor type, and disturbance ---------
tally_BSI <- bio_base %>%
  count(biome, sensor_type, intervention, name = "count")

## 5b) Create a biome-within-panel label key -----------------------
# This is used so that each facet can have its own biome ordering.
tally_BSI <- tally_BSI %>%
  mutate(biome_int = paste0(biome, "_", intervention)) %>%
  group_by(biome_int) %>%
  mutate(count_biome_int = sum(count)) %>%
  ungroup() %>%
  arrange(count_biome_int) %>%
  mutate(biome_int = fct_reorder(biome_int, row_number()))

custom_labels <- tally_BSI %>%
  distinct(biome, biome_int)

## 5c) Calculate total counts per biome within each disturbance ----
panel_totals <- tally_BSI %>%
  group_by(intervention, biome_int) %>%
  summarise(total = sum(count), .groups = "drop")

## 5d) Force the same x-axis range in every facet ------------------
max_total <- max(panel_totals$total, na.rm = TRUE)

dummy_max <- panel_totals %>%
  group_by(intervention) %>%
  summarise(biome_int = dplyr::first(biome_int), .groups = "drop") %>%
  mutate(total = max_total)

## 5e) Order disturbance panels by total number of cases ----------
facet_order <- tally_BSI %>%
  group_by(intervention) %>%
  summarise(total_cases = sum(count), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  pull(intervention)

tally_BSI <- tally_BSI %>%
  mutate(intervention = factor(intervention, levels = facet_order))

panel_totals <- panel_totals %>%
  mutate(intervention = factor(intervention, levels = facet_order))

dummy_max <- dummy_max %>%
  mutate(intervention = factor(intervention, levels = facet_order))

custom_labels <- custom_labels %>%
  mutate(biome_int = factor(biome_int, levels = levels(tally_BSI$biome_int)))


# STEP 6: BUILD THE FIGURE ============================================

## 6a) Create the faceted horizontal bar chart ---------------------
p_fig4 <- ggplot(
  tally_BSI,
  aes(x = count, y = biome_int)
) +
  geom_blank(data = panel_totals, aes(x = total, y = biome_int)) +
  geom_blank(data = dummy_max, aes(x = total, y = biome_int)) +
  geom_col(aes(fill = sensor_type)) +
  facet_grid(
    rows = vars(intervention),
    scales = "free_y",
    space  = "free",
    labeller = labeller(intervention = identity)
  ) +
  scale_y_discrete(
    breaks = custom_labels$biome_int,
    labels = custom_labels$biome
  ) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_continuous(
    limits = c(0, max_total),
    breaks = scales::pretty_breaks(),
    expand = c(0, 0)
  ) +
  labs(
    x    = "Number of cases",
    y    = "Biome",
    fill = "Sensor type"
  ) +
  theme_bw(base_size = base_font_size) +
  theme(
    legend.position       = "right",
    strip.placement       = "outside",
    strip.background      = element_rect(fill = "grey92", color = NA),
    strip.text.y.right    = element_text(angle = 0, margin = margin(l = 6, r = 2)),
    panel.spacing.y       = grid::unit(6, "pt"),
    plot.margin           = margin(10, 10, 10, 10)
  )

## 6b) Display the figure in the plotting window -------------------
print(p_fig4)


# STEP 7: EXPORT THE FIGURE ===========================================

## 7a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 7b) Save the figure ---------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_fig4,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi,
  units    = "in"
)


# STEP 8: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 8a) Count total cases and unique combination types -------------
total_cases <- nrow(bio_base)
total_cases_from_tally <- sum(tally_BSI$count)

n_unique_triplets <- bio_base %>%
  distinct(biome, intervention, sensor_type) %>%
  nrow()

n_unique_dist_sensor <- bio_base %>%
  distinct(intervention, sensor_type) %>%
  nrow()

n_unique_biome_sensor <- bio_base %>%
  distinct(biome, sensor_type) %>%
  nrow()

## 8b) Create pairwise summary tables ------------------------------
bd <- bio_base %>%
  count(biome, intervention, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), biome, intervention)

ds <- bio_base %>%
  count(intervention, sensor_type, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), intervention, sensor_type)

bs <- bio_base %>%
  count(biome, sensor_type, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), biome, sensor_type)

## 8c) Create the three-way combination summary -------------------
bsi_top <- bio_base %>%
  count(biome, intervention, sensor_type, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), biome, intervention, sensor_type)

## 8d) Create totals by disturbance, sensor, and biome ------------
disturbance_totals <- bio_base %>%
  count(intervention, name = "total_cases") %>%
  arrange(desc(total_cases), intervention)

sensor_totals <- bio_base %>%
  count(sensor_type, name = "total_cases") %>%
  arrange(desc(total_cases), sensor_type)

biome_totals <- bio_base %>%
  count(biome, name = "total_cases") %>%
  arrange(desc(total_cases), biome)

## 8e) Create a consistent summary table ---------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Rows imported",
    "Unique studies imported",
    "Valid biome × sensor × disturbance cases",
    "Total cases from plot tally",
    "Unique biome × disturbance × sensor types",
    "Unique disturbance × sensor types",
    "Unique biome × sensor types",
    "Number of disturbance panels",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    sheet_name,
    nrow(study_data),
    n_distinct(study_data$study_id),
    total_cases,
    total_cases_from_tally,
    n_unique_triplets,
    n_unique_dist_sensor,
    n_unique_biome_sensor,
    nlevels(tally_BSI$intervention),
    output_file
  )
)

## 8f) Confirm that the case totals are internally consistent -----
stopifnot(total_cases == total_cases_from_tally)
stopifnot(sum(bd$n) == total_cases)
stopifnot(sum(ds$n) == total_cases)
stopifnot(sum(bs$n) == total_cases)

## 8g) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n========== TOTAL CASES BY DISTURBANCE ==========\n")
print(disturbance_totals, n = Inf)

cat("\n============ TOTAL CASES BY SENSOR =============\n")
print(sensor_totals, n = Inf)

cat("\n============= TOTAL CASES BY BIOME =============\n")
print(biome_totals, n = Inf)

cat("\n===== BIOME × DISTURBANCE COUNTS (TOP 20) =====\n")
print(head(bd, 20), n = 20)

cat("\n===== DISTURBANCE × SENSOR COUNTS (TOP 20) =====\n")
print(head(ds, 20), n = 20)

cat("\n===== BIOME × SENSOR COUNTS (TOP 20) =====\n")
print(head(bs, 20), n = 20)

cat("\n===== TOP THREE-WAY COMBINATIONS (TOP 30) =====\n")
print(head(bsi_top, 30), n = 30)


