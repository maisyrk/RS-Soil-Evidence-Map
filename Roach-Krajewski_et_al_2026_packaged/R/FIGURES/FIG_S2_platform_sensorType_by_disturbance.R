# FIG_S2_platform_sensorType_by_disturbance.R
# 
# Figure output:
# This script creates a faceted horizontal stacked bar chart
# showing the number of cases for each platform × sensor type
# combination, grouped by disturbance category.
#
# Each panel represents one disturbance category. Within each
# panel, the y-axis shows platforms and the stacked bars show
# how often different sensor types were used.
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
  library(purrr)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
sheet_name  <- "5_study_data"
output_file <- file.path(proj_root, "figures", "FIG_S2_platform_sensorType_by_disturbance.png")

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
  "platform",
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
# This normalizes semicolons and surrounding spaces only.
# It intentionally does NOT modify commas.
split_semis <- function(x) {
  x %>%
    coalesce("") %>%
    str_replace_all("\\s*;\\s*", ";")
}

## 3b) Count the number of semicolon-separated items --------------
# This is used to compare how many platforms and sensors were
# reported for each study.
count_items <- function(x) {
  x_clean <- str_squish(x)
  is_empty <- is.na(x_clean) | x_clean == ""
  out <- ifelse(is_empty, 0L, str_count(x_clean, ";") + 1L)
  as.integer(out)
}

## 3c) Standardize disturbance labels ------------------------------
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

## 3d) Standardize sensor labels ----------------------------------
canon_sensor <- function(x) {
  xl <- str_to_lower(str_squish(x))
  
  case_when(
    xl %in% c("lidar", "li dar")                                   ~ "LiDAR",
    xl %in% c("rgb", "rbg")                                        ~ "RGB",
    xl %in% c("sar")                                               ~ "SAR",
    xl %in% c("multispectral", "multi-spectral", "mulispectral")   ~ "Multispectral",
    xl %in% c("hyperspectral", "hyper-spectral")                   ~ "Hyperspectral",
    xl %in% c("stereo")                                            ~ "Stereo",
    xl %in% c("thermal", "tirs")                                   ~ "Thermal",
    TRUE                                                           ~ str_to_title(xl)
  )
}

## 3e) Standardize platform labels --------------------------------
canon_platform <- function(x) {
  xl <- str_to_lower(str_squish(x))
  
  case_when(
    xl %in% c("uav", "uas", "drone")                          ~ "UAV",
    xl %in% c("airborne", "aircraft", "plane", "helicopter") ~ "Airborne",
    xl %in% c("satellite", "spaceborne", "space-borne")      ~ "Satellite",
    xl %in% c("terrestrial", "ground", "in situ", "in-situ") ~ "Terrestrial",
    TRUE                                                     ~ str_to_title(xl)
  )
}

## 3f) Pair platform and sensor entries positionally --------------
# Platform and sensor fields can each contain semicolon-separated
# values. This function keeps the first platform with the first
# sensor, the second platform with the second sensor, and so on.
pair_platform_sensor <- function(platform_str, sensor_str) {
  
  p <- platform_str %>%
    split_semis() %>%
    str_split(";", simplify = FALSE) %>%
    .[[1]] %>%
    str_squish() %>%
    discard(~ .x == "") %>%
    canon_platform()
  
  s <- sensor_str %>%
    split_semis() %>%
    str_split(";", simplify = FALSE) %>%
    .[[1]] %>%
    str_squish() %>%
    discard(~ .x == "") %>%
    canon_sensor()
  
  n <- min(length(p), length(s))
  
  if (n == 0) {
    return(tibble(
      platform    = character(),
      sensor_type = character()
    ))
  }
  
  tibble(
    platform    = p[seq_len(n)],
    sensor_type = s[seq_len(n)]
  )
}


# STEP 4: PREPARE PLATFORM-SENSOR PAIRS ===============================

## 4a) Keep the platform and sensor columns needed for pairing -----
pair_base <- study_data %>%
  select(study_id, platform, sensor_type) %>%
  mutate(
    platform    = str_squish(platform),
    sensor_type = str_squish(sensor_type),
    platform    = na_if(platform, ""),
    sensor_type = na_if(sensor_type, ""),
    n_platform  = count_items(platform),
    n_sensor    = count_items(sensor_type)
  )

## 4b) Save rows where platform and sensor counts do not match -----
# This is just to flag any mismatches. Should be 0. 
pair_mismatches <- pair_base %>%
  filter(n_platform != n_sensor & (n_platform > 0 | n_sensor > 0)) %>%
  select(study_id, platform, sensor_type, n_platform, n_sensor)

## 4c) Keep only rows with valid one-to-one pairing ----------------
pair_clean <- pair_base %>%
  filter(n_platform == n_sensor, n_platform > 0)

## 4d) Expand to one row per platform-sensor pair ------------------
platform_sensor_pairs <- pair_clean %>%
  mutate(ps_pairs = map2(platform, sensor_type, pair_platform_sensor)) %>%
  select(study_id, ps_pairs) %>%
  unnest(ps_pairs) %>%
  filter(platform != "", sensor_type != "") %>%
  distinct(study_id, platform, sensor_type)


# STEP 5: PREPARE DISTURBANCE DATA ====================================

## 5a) Pivot the 3 disturbance columns into one column ------------
disturbance_long <- study_data %>%
  select(
    study_id,
    intervention_category_1,
    intervention_category_2,
    intervention_category_3
  ) %>%
  pivot_longer(
    cols      = starts_with("intervention_category_"),
    names_to  = "slot",
    values_to = "disturbance_raw"
  ) %>%
  mutate(
    disturbance = canon_disturbance(disturbance_raw)
  ) %>%
  filter(!is.na(disturbance)) %>%
  distinct(study_id, disturbance)


# STEP 6: BUILD THE PLATFORM × SENSOR × DISTURBANCE CASE TABLE =======

## 6a) Join paired platforms/sensors to disturbances --------------
ps_base <- disturbance_long %>%
  inner_join(platform_sensor_pairs, by = "study_id") %>%
  distinct(study_id, platform, sensor_type, disturbance)

## 6b) Stop if there are no valid rows to plot ---------------------
if (nrow(ps_base) == 0) {
  stop("No valid platform × sensor × disturbance cases were found for plotting.", call. = FALSE)
}


# STEP 7: SUMMARIZE THE DATA FOR PLOTTING =============================

## 7a) Count cases by platform, sensor type, and disturbance -------
tally_PSD <- ps_base %>%
  count(platform, sensor_type, disturbance, name = "count") %>%
  mutate(platform_dist = paste0(platform, "_", disturbance))

## 7b) Order the platform labels within each disturbance panel -----
panel_totals <- tally_PSD %>%
  group_by(disturbance, platform_dist) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(total, platform_dist)

platform_order <- panel_totals$platform_dist

tally_PSD <- tally_PSD %>%
  mutate(platform_dist = factor(platform_dist, levels = platform_order))

custom_labels <- tally_PSD %>%
  distinct(platform, platform_dist)

## 7c) Order the sensor legend by total frequency ------------------
sensor_order <- tally_PSD %>%
  group_by(sensor_type) %>%
  summarise(total_cases = sum(count), .groups = "drop") %>%
  arrange(desc(total_cases), sensor_type) %>%
  pull(sensor_type)

tally_PSD <- tally_PSD %>%
  mutate(sensor_type = factor(sensor_type, levels = sensor_order))

## 7d) Force the same x-axis range in every facet ------------------
max_total <- max(panel_totals$total, na.rm = TRUE)

dummy_max <- panel_totals %>%
  group_by(disturbance) %>%
  summarise(platform_dist = dplyr::first(platform_dist), .groups = "drop") %>%
  mutate(total = max_total)

## 7e) Order disturbance panels by total number of cases ----------
facet_order <- tally_PSD %>%
  group_by(disturbance) %>%
  summarise(total_cases = sum(count), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  pull(disturbance)

tally_PSD <- tally_PSD %>%
  mutate(disturbance = factor(disturbance, levels = facet_order))

panel_totals <- panel_totals %>%
  mutate(disturbance = factor(disturbance, levels = facet_order))

dummy_max <- dummy_max %>%
  mutate(disturbance = factor(disturbance, levels = facet_order))

custom_labels <- custom_labels %>%
  mutate(platform_dist = factor(platform_dist, levels = levels(tally_PSD$platform_dist)))


# STEP 8: BUILD THE FIGURE ============================================

## 8a) Create the faceted horizontal stacked bar chart ------------
p_figS2 <- ggplot(
  tally_PSD,
  aes(x = count, y = platform_dist)
) +
  geom_blank(data = panel_totals, aes(x = total, y = platform_dist)) +
  geom_blank(data = dummy_max, aes(x = total, y = platform_dist)) +
  geom_col(aes(fill = sensor_type)) +
  facet_grid(
    rows = vars(disturbance),
    scales = "free_y",
    space  = "free",
    labeller = labeller(disturbance = identity)
  ) +
  scale_y_discrete(
    breaks = custom_labels$platform_dist,
    labels = custom_labels$platform
  ) +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_continuous(
    limits = c(0, max_total),
    breaks = scales::pretty_breaks(),
    expand = c(0, 0)
  ) +
  labs(
    x    = "Number of cases",
    y    = "Platform",
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

## 8b) Display the figure in the plotting window -------------------
print(p_figS2)


# STEP 9: EXPORT THE FIGURE ===========================================

## 9a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 9b) Save the figure ---------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_figS2,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi,
  units    = "in"
)


# STEP 10: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 10a) Count total cases and unique combination types ------------
total_cases <- nrow(ps_base)
total_cases_from_tally <- sum(tally_PSD$count)

n_unique_triplets <- ps_base %>%
  distinct(platform, disturbance, sensor_type) %>%
  nrow()

n_unique_dist_sensor <- ps_base %>%
  distinct(disturbance, sensor_type) %>%
  nrow()

n_unique_platform_sensor <- ps_base %>%
  distinct(platform, sensor_type) %>%
  nrow()

## 10b) Create pairwise summary tables -----------------------------
platform_disturbance <- ps_base %>%
  count(platform, disturbance, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), platform, disturbance)

disturbance_sensor <- ps_base %>%
  count(disturbance, sensor_type, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), disturbance, sensor_type)

platform_sensor <- ps_base %>%
  count(platform, sensor_type, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), platform, sensor_type)

## 10c) Create the three-way combination summary ------------------
psd_top <- ps_base %>%
  count(platform, disturbance, sensor_type, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), platform, disturbance, sensor_type)

## 10d) Create totals by disturbance, sensor, and platform --------
disturbance_totals <- ps_base %>%
  count(disturbance, name = "total_cases") %>%
  arrange(desc(total_cases), disturbance)

sensor_totals <- ps_base %>%
  count(sensor_type, name = "total_cases") %>%
  arrange(desc(total_cases), sensor_type)

platform_totals <- ps_base %>%
  count(platform, name = "total_cases") %>%
  arrange(desc(total_cases), platform)

## 10e) Create a consistent summary table --------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Rows imported",
    "Unique studies imported",
    "Rows with matched platform/sensor counts",
    "Rows with mismatched platform/sensor counts",
    "Valid platform × sensor × disturbance cases",
    "Total cases from plot tally",
    "Unique platform × disturbance × sensor types",
    "Unique disturbance × sensor types",
    "Unique platform × sensor types",
    "Number of disturbance panels",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    sheet_name,
    nrow(study_data),
    n_distinct(study_data$study_id),
    nrow(pair_clean),
    nrow(pair_mismatches),
    total_cases,
    total_cases_from_tally,
    n_unique_triplets,
    n_unique_dist_sensor,
    n_unique_platform_sensor,
    nlevels(tally_PSD$disturbance),
    output_file
  )
)

## 10f) Confirm that the case totals are internally consistent ----
stopifnot(total_cases == total_cases_from_tally)
stopifnot(sum(platform_disturbance$n) == total_cases)
stopifnot(sum(disturbance_sensor$n) == total_cases)
stopifnot(sum(platform_sensor$n) == total_cases)

## 10g) Print quality checks to the console ------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n===== PLATFORM/SENSOR COUNT MISMATCHES (IF ANY) =====\n")
if (nrow(pair_mismatches) > 0) {
  print(pair_mismatches, n = Inf)
} else {
  cat("No platform/sensor count mismatches were found.\n")
}

cat("\n========== TOTAL CASES BY DISTURBANCE ==========\n")
print(disturbance_totals, n = Inf)

cat("\n============ TOTAL CASES BY SENSOR =============\n")
print(sensor_totals, n = Inf)

cat("\n=========== TOTAL CASES BY PLATFORM ============\n")
print(platform_totals, n = Inf)

cat("\n===== PLATFORM × DISTURBANCE COUNTS (TOP 20) =====\n")
print(head(platform_disturbance, 20), n = 20)

cat("\n===== DISTURBANCE × SENSOR COUNTS (TOP 20) =====\n")
print(head(disturbance_sensor, 20), n = 20)

cat("\n===== PLATFORM × SENSOR COUNTS (TOP 20) =====\n")
print(head(platform_sensor, 20), n = 20)

cat("\n===== TOP THREE-WAY COMBINATIONS (TOP 30) =====\n")
print(head(psd_top, 30), n = 30)


