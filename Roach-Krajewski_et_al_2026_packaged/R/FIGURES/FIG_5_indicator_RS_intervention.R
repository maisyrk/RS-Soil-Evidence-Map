# FIG_5_indicator_RS_intervention.R
# ============================================================
# Figure output:
# This script creates a faceted horizontal bar chart showing the
# number of cases for each soil degradation indicator category ×
# remote-sensing method combination, grouped by disturbance type.
#
# This script uses the categorized indicator workbook created by:
# R/5_categorize_indicators.R
#
# Input workbook:
# data/4_final_study_data/evidence_map_data_with_indicator_categories.xlsx
#
# This script should be saved in:
# Roach-Krajewski_et_al_2026_packaged/R/FIGURES/
#
# Expected folder structure:
# Roach-Krajewski_et_al_2026_packaged/
# ├── data/
# │   ├── 4_final_study_data/
# │   │   ├── evidence_map_data.xlsx
# │   │   └── evidence_map_data_with_indicator_categories.xlsx
# │   └── 5_extra_figure_data/
# ├── R/
# │   └── FIGURES/
# └── figures/
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
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(stringr)
  library(scales)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data_with_indicator_categories.xlsx")
sheet_name  <- "study_data_with_indicator_cats"
output_file <- file.path(proj_root, "figures", "FIG_5_indicator_RS_intervention.png")

## 1g) Define plot settings ----------------------------------------
plot_width      <- 10
plot_height     <- 10
plot_dpi        <- 350
base_font_size  <- 13


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Normalize semicolon-separated text --------------------------
split_semis <- function(x) {
  x %>%
    as.character() %>%
    replace_na("") %>%
    str_replace_all("\\s*;\\s*", ";")
}

## 2b) Standardize disturbance labels ------------------------------
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

## 2c) Group a few uncommon RS methods into "Other" ----------------
group_rs_method <- function(x) {
  xl <- str_to_lower(str_squish(x))
  
  case_when(
    xl %in% c("object-based analysis", "structure-from-motion", "time-of-flight") ~ "Other",
    TRUE                                                                           ~ str_squish(x)
  )
}


# STEP 3: IMPORT THE CATEGORIZED STUDY DATA ============================

## 3a) Check that the categorized workbook exists ------------------
if (!file.exists(input_file)) {
  stop(
    paste0(
      "The categorized indicator workbook was not found:\n",
      input_file,
      "\n\nPlease run PREP_1_indicator_categories.R first."
    ),
    call. = FALSE
  )
}

## 3b) Read the categorized study-data sheet -----------------------
study_data <- read_excel(
  path         = input_file,
  sheet        = sheet_name,
  skip         = 0,
  na           = c("", "NA"),
  col_types    = "text",
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))

## 3c) Check that required columns are present ---------------------
required_cols <- c(
  "study_id",
  "rs_method",
  "indicator_cat_1",
  "indicator_cat_2",
  "indicator_cat_3",
  "intervention_category_1",
  "intervention_category_2",
  "intervention_category_3"
)

missing_cols <- setdiff(required_cols, names(study_data))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the categorized study-data sheet:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: PREPARE THE INDICATOR × RS METHOD × DISTURBANCE DATA ========

## 4a) Pair the indicator and disturbance columns by slot ----------
# indicator_cat_n and intervention_category_n share the same slot
# number, so they are pivoted together.
indicator_base <- study_data %>%
  select(
    study_id,
    rs_method,
    starts_with("indicator_cat_"),
    starts_with("intervention_category_")
  ) %>%
  pivot_longer(
    cols = c(starts_with("indicator_cat_"), starts_with("intervention_category_")),
    names_to      = c(".value", "slot"),
    names_pattern = "(indicator_cat|intervention_category)_(\\d+)"
  )

## 4b) Split semicolon-separated values into individual rows -------
indicator_base <- indicator_base %>%
  mutate(
    indicator_cat = split_semis(indicator_cat),
    rs_method     = split_semis(rs_method),
    intervention  = canon_disturbance(intervention_category)
  ) %>%
  separate_rows(indicator_cat, sep = ";") %>%
  separate_rows(rs_method, sep = ";") %>%
  mutate(
    indicator_cat = str_squish(indicator_cat),
    rs_method     = group_rs_method(rs_method)
  ) %>%
  filter(
    indicator_cat != "",
    rs_method     != "",
    !is.na(intervention)
  ) %>%
  distinct(study_id, indicator_cat, rs_method, intervention)

## 4c) Stop if there are no valid rows to plot ---------------------
if (nrow(indicator_base) == 0) {
  stop("No valid indicator × RS method × disturbance cases were found for plotting.", call. = FALSE)
}


# STEP 5: SUMMARIZE THE DATA FOR PLOTTING =============================

## 5a) Count cases by indicator, RS method, and disturbance --------
tally_IRI <- indicator_base %>%
  count(indicator_cat, rs_method, intervention, name = "count") %>%
  mutate(indicator_int = paste0(indicator_cat, "_", intervention)) %>%
  group_by(indicator_int) %>%
  mutate(count_indicator_int = sum(count)) %>%
  ungroup() %>%
  arrange(count_indicator_int, indicator_cat, rs_method)

## 5b) Order the indicator labels within each disturbance panel ----
indicator_levels <- unique(tally_IRI$indicator_int)

tally_IRI <- tally_IRI %>%
  mutate(indicator_int = factor(indicator_int, levels = indicator_levels))

custom_labels_ind <- tally_IRI %>%
  distinct(indicator_cat, indicator_int)

## 5c) Order the RS-method legend by total frequency ---------------
rs_order <- tally_IRI %>%
  group_by(rs_method) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(rs_method)

# Keep "Other" at the end of the legend if it is present.
if ("Other" %in% rs_order) {
  rs_order <- c(setdiff(rs_order, "Other"), "Other")
}

tally_IRI <- tally_IRI %>%
  mutate(rs_method = factor(rs_method, levels = rs_order))

## 5d) Create totals used to control panel widths ------------------
panel_totals_ind <- tally_IRI %>%
  group_by(intervention, indicator_int) %>%
  summarise(total = sum(count), .groups = "drop")

max_total_ind <- max(panel_totals_ind$total, na.rm = TRUE)

dummy_max_ind <- panel_totals_ind %>%
  group_by(intervention) %>%
  summarise(indicator_int = dplyr::first(indicator_int), .groups = "drop") %>%
  mutate(total = max_total_ind)

## 5e) Order disturbance panels by total number of cases -----------
facet_order_ind <- tally_IRI %>%
  group_by(intervention) %>%
  summarise(total_cases = sum(count), .groups = "drop") %>%
  arrange(desc(total_cases)) %>%
  pull(intervention)

tally_IRI <- tally_IRI %>%
  mutate(intervention = factor(intervention, levels = facet_order_ind))

panel_totals_ind <- panel_totals_ind %>%
  mutate(intervention = factor(intervention, levels = facet_order_ind))

dummy_max_ind <- dummy_max_ind %>%
  mutate(intervention = factor(intervention, levels = facet_order_ind))

custom_labels_ind <- custom_labels_ind %>%
  mutate(indicator_int = factor(indicator_int, levels = levels(tally_IRI$indicator_int)))


# STEP 6: BUILD THE FIGURE ============================================

## 6a) Create the faceted horizontal bar chart ---------------------
p_fig5 <- ggplot(
  tally_IRI,
  aes(x = count, y = indicator_int)
) +
  geom_blank(data = panel_totals_ind, aes(x = total, y = indicator_int)) +
  geom_blank(data = dummy_max_ind, aes(x = total, y = indicator_int)) +
  geom_col(aes(fill = rs_method)) +
  facet_grid(
    rows = vars(intervention),
    scales = "free_y",
    space  = "free",
    labeller = labeller(intervention = identity)
  ) +
  scale_y_discrete(
    breaks = custom_labels_ind$indicator_int,
    labels = custom_labels_ind$indicator_cat
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(
    limits = c(0, max_total_ind),
    breaks = scales::pretty_breaks(),
    expand = c(0, 0)
  ) +
  labs(
    x    = "Number of cases",
    y    = "Soil degradation indicator type",
    fill = "Remote-sensing method"
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
print(p_fig5)


# STEP 7: EXPORT THE FIGURE ===========================================

## 7a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 7b) Save the figure ---------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_fig5,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi,
  units    = "in"
)


# STEP 8: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 8a) Count total cases and unique combination types -------------
total_cases <- nrow(indicator_base)
total_cases_from_tally <- sum(tally_IRI$count)

n_unique_triplets <- indicator_base %>%
  distinct(indicator_cat, intervention, rs_method) %>%
  nrow()

n_unique_indicator_rs <- indicator_base %>%
  distinct(indicator_cat, rs_method) %>%
  nrow()

n_unique_indicator_disturbance <- indicator_base %>%
  distinct(indicator_cat, intervention) %>%
  nrow()

## 8b) Create pairwise summary tables ------------------------------
indicator_disturbance <- indicator_base %>%
  count(indicator_cat, intervention, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), indicator_cat, intervention)

indicator_rs <- indicator_base %>%
  count(indicator_cat, rs_method, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), indicator_cat, rs_method)

rs_disturbance <- indicator_base %>%
  count(intervention, rs_method, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), intervention, rs_method)

## 8c) Create the full three-way summary table --------------------
indicator_rs_disturbance <- indicator_base %>%
  count(indicator_cat, intervention, rs_method, name = "n") %>%
  mutate(pct = 100 * n / total_cases) %>%
  arrange(desc(n), indicator_cat, intervention, rs_method)

## 8d) Create totals by disturbance, RS method, and indicator -----
disturbance_totals <- indicator_base %>%
  count(intervention, name = "total_cases") %>%
  arrange(desc(total_cases), intervention)

rs_totals <- indicator_base %>%
  count(rs_method, name = "total_cases") %>%
  arrange(desc(total_cases), rs_method)

indicator_totals <- indicator_base %>%
  count(indicator_cat, name = "total_cases") %>%
  arrange(desc(total_cases), indicator_cat)

## 8e) Create a consistent summary table ---------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input workbook",
    "Input sheet",
    "Rows imported",
    "Unique studies imported",
    "Valid indicator × RS method × disturbance cases",
    "Total cases from plot tally",
    "Unique indicator × disturbance × RS method types",
    "Unique indicator × disturbance types",
    "Unique indicator × RS method types",
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
    n_unique_indicator_disturbance,
    n_unique_indicator_rs,
    nlevels(tally_IRI$intervention),
    output_file
  )
)

## 8f) Confirm that the case totals are internally consistent -----
stopifnot(total_cases == total_cases_from_tally)
stopifnot(sum(indicator_disturbance$n) == total_cases)
stopifnot(sum(indicator_rs$n) == total_cases)
stopifnot(sum(rs_disturbance$n) == total_cases)

## 8g) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n========== TOTAL CASES BY DISTURBANCE ==========\n")
print(disturbance_totals, n = Inf)

cat("\n========== TOTAL CASES BY RS METHOD ============\n")
print(rs_totals, n = Inf)

cat("\n=========== TOTAL CASES BY INDICATOR ===========\n")
print(indicator_totals, n = Inf)

cat("\n===== INDICATOR × DISTURBANCE COUNTS (TOP 20) =====\n")
print(head(indicator_disturbance, 20), n = 20)

cat("\n===== INDICATOR × RS METHOD COUNTS (TOP 20) =====\n")
print(head(indicator_rs, 20), n = 20)

cat("\n===== RS METHOD × DISTURBANCE COUNTS (TOP 20) =====\n")
print(head(rs_disturbance, 20), n = 20)

cat("\n===== TOP THREE-WAY COMBINATIONS (TOP 30) =====\n")
print(head(indicator_rs_disturbance, 30), n = 30)


