# FIG_S3_rs_method_by_disturbance.R
# 
# Figure output:
# This script creates a faceted horizontal stacked bar chart
# showing the number of cases for each remote-sensing method,
# grouped by disturbance category.
#
# Pairing rule used in this figure:
# - Disturbance comes from intervention_category_1/2/3.
# - rs_method can contain multiple values separated by semicolons.
# - rs_method is NOT matched by slot to disturbance.
#   Instead, each disturbance in a study is paired with every
#   rs_method reported for that same study.
#
# Each panel represents one disturbance category. Within each
# panel, the y-axis shows remote-sensing methods and the bars
# show the number of cases.
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
output_file <- file.path(proj_root, "figures", "FIG_S3_rs_method_by_disturbance.png")

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
  "rs_method",
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

## 3b) Standardize disturbance labels ------------------------------
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

## 3c) Standardize RS method labels --------------------------------
# This keeps a few common method names consistent and ensures that
# LiDAR and SAR are always capitalized correctly.
canon_rs_method <- function(x) {
  xl <- str_squish(x)
  xl_low <- str_to_lower(xl)
  
  out <- case_when(
    xl_low == ""                                          ~ "",
    xl_low %in% c("ml", "machine learning")               ~ "Machine learning",
    xl_low %in% c("rf", "random forest", "random forests")~ "Random forest",
    xl_low %in% c("svm", "support vector machine")        ~ "SVM",
    xl_low %in% c("pls", "partial least squares")         ~ "PLS",
    TRUE                                                  ~ str_to_sentence(xl)
  )
  
  out %>%
    str_replace_all(regex("\\blidar\\b", ignore_case = TRUE), "LiDAR") %>%
    str_replace_all(regex("\\bsar\\b",   ignore_case = TRUE), "SAR")
}


# STEP 4: BUILD THE DISTURBANCE × RS METHOD CASE TABLE ================

## 4a) Cross-pair every disturbance with every RS method ----------
# This follows the logic of the original script:
# each disturbance in a row is paired with every RS method in that row.
rs_base <- study_data %>%
  transmute(
    study_id,
    rs_method = split_semis(rs_method),
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
  separate_rows(rs_method, sep = ";") %>%
  mutate(
    rs_method = canon_rs_method(rs_method)
  ) %>%
  filter(rs_method != "") %>%
  distinct(study_id, disturbance, rs_method)

## 4b) Stop if there are no valid rows to plot ---------------------
if (nrow(rs_base) == 0) {
  stop("No valid disturbance × RS method cases were found for plotting.", call. = FALSE)
}


# STEP 5: SUMMARIZE THE DATA FOR PLOTTING =============================

## 5a) Count cases by RS method and disturbance --------------------
tally_DRM <- rs_base %>%
  count(rs_method, disturbance, name = "count") %>%
  mutate(method_dist = paste0(rs_method, "_", disturbance)) %>%
  group_by(method_dist) %>%
  mutate(count_method_dist = sum(count)) %>%
  ungroup() %>%
  arrange(count_method_dist) %>%
  mutate(method_dist = fct_reorder(method_dist, row_number()))

## 5b) Create a custom label table for the y-axis ------------------
custom_labels <- tally_DRM %>%
  distinct(rs_method, method_dist)

## 5c) Calculate totals per panel ----------------------------------
panel_totals <- tally_DRM %>%
  group_by(disturbance, method_dist) %>%
  summarise(total = sum(count), .groups = "drop")

## 5d) Force the same x-axis range in every facet ------------------
max_total <- max(panel_totals$total, na.rm = TRUE)

dummy_max <- panel_totals %>%
  group_by(disturbance) %>%
  summarise(method_dist = dplyr::first(method_dist), .groups = "drop") %>%
  mutate(total = max_total)

## 5e) Order disturbance panels by total number of cases ----------
facet_order <- tally_DRM %>%
  group_by(disturbance) %>%
  summarise(total_cases = sum(count), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  pull(disturbance)

tally_DRM <- tally_DRM %>%
  mutate(disturbance = factor(disturbance, levels = facet_order))

panel_totals <- panel_totals %>%
  mutate(disturbance = factor(disturbance, levels = facet_order))

dummy_max <- dummy_max %>%
  mutate(disturbance = factor(disturbance, levels = facet_order))

custom_labels <- custom_labels %>%
  mutate(method_dist = factor(method_dist, levels = levels(tally_DRM$method_dist)))

## 5f) Make RS method a stable factor for consistent colours -------
method_levels <- tally_DRM %>%
  distinct(rs_method) %>%
  arrange(rs_method) %>%
  pull(rs_method)

tally_DRM <- tally_DRM %>%
  mutate(rs_method = factor(rs_method, levels = method_levels))


# STEP 6: BUILD THE FIGURE ============================================

## 6a) Create the faceted horizontal stacked bar chart ------------
p_figS3 <- ggplot(
  tally_DRM,
  aes(x = count, y = method_dist)
) +
  geom_blank(data = panel_totals, aes(x = total, y = method_dist)) +
  geom_blank(data = dummy_max,    aes(x = total, y = method_dist)) +
  geom_col(aes(fill = rs_method)) +
  facet_grid(
    rows = vars(disturbance),
    scales = "free_y",
    space  = "free",
    labeller = labeller(disturbance = identity)
  ) +
  scale_y_discrete(
    breaks = custom_labels$method_dist,
    labels = custom_labels$rs_method
  ) +
  scale_fill_brewer(palette = "Spectral", na.translate = FALSE) +
  scale_x_continuous(
    limits = c(0, max_total),
    breaks = scales::pretty_breaks(),
    expand = c(0, 0)
  ) +
  labs(
    x    = "Number of cases",
    y    = "Remote sensing method",
    fill = "Remote sensing method"
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
print(p_figS3)


# STEP 7: EXPORT THE FIGURE ===========================================

## 7a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 7b) Save the figure ---------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_figS3,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi,
  units    = "in"
)


# STEP 8: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 8a) Count total cases and unique combination types -------------
total_cases <- nrow(rs_base)
total_cases_from_tally <- sum(tally_DRM$count)

n_unique_triplets <- rs_base %>%
  distinct(rs_method, disturbance) %>%
  nrow()

n_unique_methods <- rs_base %>%
  distinct(rs_method) %>%
  nrow()

n_unique_disturbances <- rs_base %>%
  distinct(disturbance) %>%
  nrow()

## 8b) Create pairwise summary tables ------------------------------
method_disturbance <- rs_base %>%
  count(rs_method, disturbance, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), rs_method, disturbance)

method_totals <- rs_base %>%
  count(rs_method, name = "total_cases") %>%
  arrange(desc(total_cases), rs_method)

disturbance_totals <- rs_base %>%
  count(disturbance, name = "total_cases") %>%
  arrange(desc(total_cases), disturbance)

## 8c) Calculate results-text totals for selected RS methods ------
denom_cases <- nrow(rs_base)

n_spectral <- sum(str_detect(rs_base$rs_method, regex("\\bspectral\\s*analysis\\b", ignore_case = TRUE)))
n_lidar    <- sum(str_detect(rs_base$rs_method, regex("\\blidar\\b", ignore_case = TRUE)))
n_imgdiff  <- sum(str_detect(rs_base$rs_method, regex("\\bimage\\s*differenc", ignore_case = TRUE)))
n_photo    <- sum(str_detect(rs_base$rs_method, regex("photogram", ignore_case = TRUE)))

pct <- function(n) round(100 * n / denom_cases, 1)

rs_method_totals_key <- tibble(
  method = c("Spectral analysis", "LiDAR", "Image differencing", "Photogrammetry"),
  n_cases = c(n_spectral, n_lidar, n_imgdiff, n_photo),
  denom_cases = denom_cases,
  pct_cases = c(pct(n_spectral), pct(n_lidar), pct(n_imgdiff), pct(n_photo))
)

## 8d) Create a consistent summary table ---------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Rows imported",
    "Unique studies imported",
    "Valid disturbance × RS method cases",
    "Total cases from plot tally",
    "Unique RS method × disturbance types",
    "Unique RS methods",
    "Unique disturbance types",
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
    n_unique_methods,
    n_unique_disturbances,
    nlevels(tally_DRM$disturbance),
    output_file
  )
)

## 8e) Confirm that totals are internally consistent --------------
stopifnot(total_cases == total_cases_from_tally)
stopifnot(sum(method_disturbance$n) == total_cases)

## 8f) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n========== TOTAL CASES BY DISTURBANCE ==========\n")
print(disturbance_totals, n = Inf)

cat("\n========== TOTAL CASES BY RS METHOD ============\n")
print(method_totals, n = Inf)

cat("\n===== RS METHOD × DISTURBANCE COUNTS (TOP 30) =====\n")
print(head(method_disturbance, 30), n = 30)

cat("\n===== KEY RS-METHOD TOTALS FOR RESULTS TEXT =====\n")
print(rs_method_totals_key, n = Inf)



