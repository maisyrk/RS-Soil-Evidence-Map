# FIG_2_sensor-platform_by_pub_year.R =============================================
# 
# Figure output:
# This script creates a stacked bar chart showing the number of
# remote sensing system cases by publication year.
#
# A "remote sensing system" is defined here as a paired
# sensor_type + platform combination (for example:
# Satellite multispectral or Airborne lidar).
#
# The colours are grouped by platform family, with lighter/darker
# shades used to distinguish sensor types within each platform.
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
setwd(proj_root)

## 1d) Confirm the root folder in the console ----------------------
message("Project root set to: ", proj_root)

## 1e) Load required packages --------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(scales)
  library(stringr)
  library(purrr)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
sheet_name  <- "5_study_data"
output_file <- file.path(proj_root, "figures", "FIG_2_sensor-platform_by_pub_year.png")

## 1g) Define plot settings ----------------------------------------
start_year     <- 1995
end_year       <- 2026
major_step     <- 5
max_y          <- 12
base_font_size <- 16
plot_width     <- 14
plot_height    <- 5
plot_dpi       <- 200

## 1h) Define base colours for platform groups ---------------------
base_cols <- c(
  ground    = "#b8860b",
  uav       = "darkgreen",
  airborne  = "steelblue4",
  satellite = "tomato3",
  other     = "#808080"
)

platform_levels <- c("ground", "uav", "airborne", "satellite", "other")


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Count the number of semicolon-separated items --------------
# This is used to compare how many sensors and platforms were
# entered for each study.
count_items <- function(x) {
  x_clean <- str_squish(x)
  is_empty <- is.na(x_clean) | x_clean == ""
  out <- ifelse(is_empty, 0L, str_count(x_clean, ";") + 1L)
  as.integer(out)
}

## 2b) Lighten a colour -------------------------------------------
# This creates lighter shades within each platform colour family.
lighten_color <- function(col, factor = 0.5) {
  rgb_col <- grDevices::col2rgb(col) / 255
  rgb_new <- rgb_col + (1 - rgb_col) * factor
  grDevices::rgb(rgb_new[1], rgb_new[2], rgb_new[3])
}

## 2c) Classify a platform into a broad platform group ------------
classify_platform <- function(x) {
  x_clean <- str_to_lower(str_squish(x))
  
  case_when(
    str_detect(x_clean, "ground")                             ~ "ground",
    str_detect(x_clean, "uav|drone")                          ~ "uav",
    str_detect(x_clean, "airborne|aircraft|plane|helicopter") ~ "airborne",
    str_detect(x_clean, "satellite|spaceborne")               ~ "satellite",
    TRUE                                                      ~ "other"
  )
}

## 2d) Format legend labels for sensor-platform combinations ------
# This converts labels such as "uav multispectral" to "UAV multispectral"
label_sensor_platform <- function(x) {
  map_chr(x, ~ {
    if (is.na(.x) || .x == "") return(.x)
    
    parts <- str_split_fixed(.x, " ", n = 2)
    platform_raw <- parts[, 1]
    rest         <- parts[, 2]
    
    platform_label <- if (str_to_lower(platform_raw) == "uav") {
      "UAV"
    } else {
      str_to_sentence(str_to_lower(platform_raw))
    }
    
    out <- paste(platform_label, rest)
    str_squish(out)
  })
}


# STEP 3: IMPORT THE STUDY DATA ========================================

## 3a) Check that the Excel file exists ----------------------------
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

## 3b) Read the finalized Excel sheet ------------------------------
study_data <- read_excel(
  path         = input_file,
  sheet        = sheet_name,
  skip         = 1,
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))


# STEP 4: PREPARE THE PUBLICATION YEAR ================================

## 4a) Extract the publication year from study_id ------------------
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


# STEP 5: PREPARE SENSOR AND PLATFORM DATA ============================

## 5a) Keep the columns needed for this figure ---------------------
sensor_platform_base <- study_data %>%
  filter(!is.na(pub_year)) %>%
  select(study_id, pub_year, sensor_type, platform) %>%
  mutate(
    sensor_type_raw = sensor_type,
    platform_raw    = platform,
    sensor_type     = str_squish(sensor_type),
    platform        = str_squish(platform),
    sensor_type     = na_if(sensor_type, ""),
    platform        = na_if(platform, "")
  )

## 5b) Count how many sensor and platform entries each row has -----
# These counts are used to identify mismatches before pairing.
sensor_platform_base <- sensor_platform_base %>%
  mutate(
    n_sensor   = count_items(sensor_type),
    n_platform = count_items(platform)
  )

## 5c) Save rows where the number of sensors and platforms differ --
# These rows are not used in the plot, just for flagging mismatches 
sensor_platform_mismatches <- sensor_platform_base %>%
  filter(n_sensor != n_platform & (n_sensor > 0 | n_platform > 0)) %>%
  select(study_id, pub_year, sensor_type_raw, platform_raw, n_sensor, n_platform)

## 5d) Keep only rows with valid one-to-one pairing ----------------
sensor_platform_clean <- sensor_platform_base %>%
  filter(n_sensor == n_platform, n_sensor > 0)


# STEP 6: EXPAND TO ONE ROW PER SENSOR-PLATFORM PAIR =================

## 6a) Separate semicolon-delimited entries into paired rows -------
sensor_platform_pairs <- sensor_platform_clean %>%
  separate_rows(sensor_type, platform, sep = ";") %>%
  mutate(
    sensor_type     = str_squish(sensor_type),
    platform        = str_squish(platform),
    sensor_platform = paste(platform, sensor_type),
    platform_group  = classify_platform(platform)
  ) %>%
  filter(!is.na(sensor_platform), sensor_platform != "") %>%
  distinct(study_id, pub_year, sensor_platform, platform_group)


# STEP 7: SUMMARIZE THE DATA FOR PLOTTING ============================

## 7a) Count cases by publication year and sensor-platform ---------
counts_ysp <- sensor_platform_pairs %>%
  count(pub_year, sensor_platform, platform_group, name = "n")

## 7b) Order sensor-platform combinations within platform groups ---
pair_order_df <- counts_ysp %>%
  group_by(sensor_platform, platform_group) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  mutate(platform_group = factor(platform_group, levels = platform_levels)) %>%
  arrange(platform_group, desc(total))


# STEP 8: BUILD THE PLATFORM-BASED COLOUR PALETTE ====================

## 8a) Create a shade sequence within each platform group ----------
color_df <- pair_order_df %>%
  group_by(platform_group) %>%
  mutate(
    color = {
      base_col <- base_cols[as.character(first(platform_group))]
      
      if (n() == 1L) {
        base_col
      } else {
        grDevices::colorRampPalette(
          c(lighten_color(base_col, 0.7), base_col)
        )(n())
      }
    }
  ) %>%
  ungroup()

## 8b) Store the final plotting order and named colour palette -----
pair_order <- color_df$sensor_platform

counts_ysp <- counts_ysp %>%
  mutate(
    sensor_platform = factor(sensor_platform, levels = pair_order),
    platform_group  = factor(platform_group, levels = platform_levels)
  )

pal_cb_sp <- color_df$color
names(pal_cb_sp) <- color_df$sensor_platform


# STEP 9: DEFINE AXIS BREAKS ==========================================

## 9a) Create x-axis break positions --------------------------------
x_major_breaks <- seq(start_year, end_year, by = major_step)
x_minor_breaks <- seq(start_year, end_year, by = 1)


# STEP 10: BUILD THE FIGURE ===========================================

## 10a) Create the stacked bar chart --------------------------------
p_fig2 <- ggplot(counts_ysp, aes(x = pub_year, y = n, fill = sensor_platform)) +
  geom_col(width = 0.85) +
  scale_fill_manual(
    values = pal_cb_sp,
    name   = "Remote sensing system: ",
    breaks = pair_order,
    labels = label_sensor_platform(pair_order)
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
    legend.position       = "right",
    legend.title.position = "top",
    axis.text.x           = element_text(angle = 45, hjust = 1)
  ) +
  guides(
    fill = guide_legend(
      ncol  = 1,
      byrow = TRUE
    )
  )

## 10b) Display the figure in the plotting window ------------------
print(p_fig2)


# STEP 11: EXPORT THE FIGURE ==========================================

## 11a) Create the output folder if needed -------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 11b) Save the figure --------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_fig2,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi
)


# STEP 12: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 12a) Create a consistent summary table --------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Rows imported",
    "Unique studies imported",
    "Studies with valid publication year",
    "Rows with matched sensor/platform counts",
    "Rows with mismatched sensor/platform counts",
    "Unique sensor-platform combinations plotted",
    "Total sensor-platform cases plotted",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    sheet_name,
    nrow(study_data),
    n_distinct(study_data$study_id),
    sum(!is.na(study_data$pub_year)),
    nrow(sensor_platform_clean),
    nrow(sensor_platform_mismatches),
    n_distinct(sensor_platform_pairs$sensor_platform),
    sum(counts_ysp$n),
    output_file
  )
)

## 12b) Count total cases by sensor-platform combination ----------
sensor_platform_totals <- counts_ysp %>%
  group_by(sensor_platform) %>%
  summarise(total_cases = sum(n), .groups = "drop") %>%
  arrange(desc(total_cases))

## 12c) Count total cases by publication year ---------------------
year_totals <- counts_ysp %>%
  group_by(pub_year) %>%
  summarise(total_cases = sum(n), .groups = "drop") %>%
  arrange(pub_year)

## 12d) Create a compact totals string -----------------------------
totals_str_sp <- sensor_platform_totals %>%
  mutate(label = paste0(sensor_platform, " (", total_cases, ")")) %>%
  pull(label) %>%
  paste(collapse = "; ")

## 12e) Recreate the case table for results-text summaries --------
# This section keeps the same case definitions used in the original script.
cases_long <- sensor_platform_clean %>%
  separate_rows(sensor_type, platform, sep = ";") %>%
  mutate(
    sensor_type    = str_to_lower(str_squish(sensor_type)),
    platform       = str_squish(platform),
    platform_group = classify_platform(platform)
  ) %>%
  filter(
    !is.na(sensor_type), sensor_type != "",
    !is.na(platform),    platform    != ""
  ) %>%
  distinct(study_id, pub_year, platform, sensor_type, platform_group)

denom_cases <- nrow(cases_long)

## 12f) Calculate selected results-text totals --------------------
is_multispec <- str_detect(cases_long$sensor_type, "multi\\s*-\\s*spectral|multispectral")
is_lidar     <- str_detect(cases_long$sensor_type, "\\blidar\\b")
is_stereo_pg <- str_detect(cases_long$sensor_type, "stereo")

ms_n       <- sum(is_multispec)
ms_pct     <- 100 * ms_n / denom_cases

lidar_n    <- sum(is_lidar)
lidar_pct  <- 100 * lidar_n / denom_cases

stereo_n   <- sum(is_stereo_pg)
stereo_pct <- 100 * stereo_n / denom_cases

sat_n      <- sum(cases_long$platform_group == "satellite")
sat_pct    <- 100 * sat_n / denom_cases

sat_ms_n   <- sum(cases_long$platform_group == "satellite" & is_multispec)
sat_ms_pct <- 100 * sat_ms_n / denom_cases

results_totals_cases <- tibble(
  metric = c(
    "Multispectral sensor use (cases)",
    "LiDAR use (cases)",
    "Stereo photogrammetric use (cases)",
    "Satellite platform use (cases)",
    "Satellite + multispectral (cases)"
  ),
  n = c(ms_n, lidar_n, stereo_n, sat_n, sat_ms_n),
  denom_cases = denom_cases,
  pct = round(c(ms_pct, lidar_pct, stereo_pct, sat_pct, sat_ms_pct), 1)
)

## 12g) Print quality checks to the console -----------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n===== SENSOR/PLATFORM COUNT MISMATCHES (IF ANY) =====\n")
if (nrow(sensor_platform_mismatches) > 0) {
  print(sensor_platform_mismatches, n = Inf)
} else {
  cat("No mismatches found.\n")
}

cat("\n====== TOTAL CASES BY SENSOR-PLATFORM COMBO ======\n")
print(sensor_platform_totals, n = Inf)

cat("\n======== TOTAL CASES BY PUBLICATION YEAR =========\n")
print(year_totals, n = Inf)

cat("\n===== RESULTS-TEXT TOTALS (CASES) =====\n")
print(results_totals_cases, n = Inf)

cat("\n===== COMPACT SENSOR-PLATFORM TOTALS STRING =====\n")
cat(totals_str_sp, "\n")

cat("\nFigure export complete.\n")

