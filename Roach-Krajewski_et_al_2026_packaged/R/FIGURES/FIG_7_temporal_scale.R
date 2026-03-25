# FIG_7_temporal_scale.R
# 
# Figure output:
# This script creates a study-by-study temporal timeline showing:
# 1) the sampling period for each study,
# 2) the year(s) of reported disturbance events, and
# 3) the year(s) of remote-sensing data acquisition.
#
# Grey bars or squares show the sampling window.
# Coloured points show disturbance years by disturbance type.
# Black vertical tick marks show RS acquisition years.
#
# This script uses:
# data/5_extra_figure_data/temporal_scale.xlsx
#
# Expected folder structure:
# Roach-Krajewski_et_al_2026_packaged/
# ├── data/
# │   ├── 4_final_study_data/
# │   └── 5_extra_figure_data/
# │       └── temporal_scale.xlsx
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
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(tibble)
  library(scales)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "5_extra_figure_data", "temporal_scale.xlsx")
output_file <- file.path(proj_root, "figures", "FIG_7_temporal_scale.png")

## 1g) Define plot settings ----------------------------------------
plot_width      <- 8
plot_height     <- 10
plot_dpi        <- 350
base_font_size  <- 11


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Create shorter y-axis labels from study_id -----------------
# This converts a study ID like "Smith_2020" to "Smith 2020".
format_study_label <- function(study_id) {
  first_author <- str_extract(study_id, "^[^_]+")
  yr           <- str_sub(study_id, -4)
  paste(first_author, yr)
}

## 2b) Standardize year strings and convert common missing codes ---
clean_year_string <- function(x) {
  x <- str_trim(as.character(x))
  x[x %in% c("NR", "NA", "na", "N/A", "n/a", "")] <- NA_character_
  x
}

## 2c) Percentage helper for QC output -----------------------------
pct1 <- function(x) {
  round(100 * x, 1)
}


# STEP 3: IMPORT THE TEMPORAL DATA ====================================

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

## 3b) Read the temporal-scale workbook ----------------------------
temporal_data <- read_excel(
  path         = input_file,
  col_types    = "text",
  na           = c("", "NA"),
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))

## 3c) Check that required columns are present ---------------------
required_cols <- c(
  "study_id",
  "sample_year_start",
  "sample_year_end",
  "intervention_category_1",
  "intervention_category_2",
  "intervention_category_3",
  "intervention_year_1",
  "intervention_year_2",
  "intervention_year_3",
  "RS_acquisition_year"
)

missing_cols <- setdiff(required_cols, names(temporal_data))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from temporal_scale.xlsx:\n",
      paste(missing_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}


# STEP 4: CLEAN THE YEAR FIELDS =======================================

## 4a) Define the year columns to clean ----------------------------
year_cols <- c(
  "sample_year_start",
  "sample_year_end",
  "intervention_year_1",
  "intervention_year_2",
  "intervention_year_3",
  "RS_acquisition_year"
)

## 4b) Standardize missing values and trim spaces ------------------
temporal_clean <- temporal_data %>%
  mutate(across(all_of(year_cols), clean_year_string))

## 4c) Keep only studies with valid sampling years -----------------
# The figure requires both a sample start and sample end year.
temporal_clean <- temporal_clean %>%
  filter(!is.na(sample_year_start), !is.na(sample_year_end))

## 4d) Keep only studies with at least one disturbance year --------
# Studies with no reported disturbance year are not included in the figure.
temporal_clean <- temporal_clean %>%
  filter(if_any(starts_with("intervention_year_"), ~ !is.na(.x)))

## 4e) Convert sampling years to numeric ---------------------------
temporal_clean <- temporal_clean %>%
  mutate(
    sample_year_start = suppressWarnings(as.numeric(sample_year_start)),
    sample_year_end   = suppressWarnings(as.numeric(sample_year_end))
  ) %>%
  filter(!is.na(sample_year_start), !is.na(sample_year_end))


# STEP 5: BUILD THE SAMPLING TIMELINE DATA ============================

## 5a) Create one row per study for the sampling window ------------
sampling_df <- temporal_clean %>%
  select(study_id, sample_year_start, sample_year_end) %>%
  distinct() %>%
  arrange(sample_year_start, sample_year_end, study_id) %>%
  mutate(
    study_id_factor = factor(study_id, levels = unique(study_id)),
    y_num           = as.integer(study_id_factor)
  )

## 5b) Split sampling windows into ranges versus single years ------
sampling_ranges <- sampling_df %>%
  filter(sample_year_start < sample_year_end)

sampling_points <- sampling_df %>%
  filter(sample_year_start == sample_year_end)

## 5c) Stop if there are no studies left to plot -------------------
if (nrow(sampling_df) == 0) {
  stop("No valid studies remained after temporal data cleaning.", call. = FALSE)
}


# STEP 6: BUILD THE DISTURBANCE EVENT DATA ============================

## 6a) Pivot paired disturbance categories and years to long format
# The category and year columns are paired by slot number.
interventions_long <- temporal_clean %>%
  select(
    study_id,
    sample_year_start,
    sample_year_end,
    starts_with("intervention_category_"),
    starts_with("intervention_year_")
  ) %>%
  pivot_longer(
    cols          = matches("^intervention_(category|year)_"),
    names_to      = c(".value", "set"),
    names_pattern = "intervention_(category|year)_(\\d+)"
  ) %>%
  rename(
    intervention_category = category,
    intervention_year_raw = year
  ) %>%
  filter(!(is.na(intervention_category) & is.na(intervention_year_raw))) %>%
  mutate(
    intervention_category = str_trim(intervention_category),
    intervention_year_raw = str_trim(as.character(intervention_year_raw))
  ) %>%
  filter(!is.na(intervention_category), intervention_category != "") %>%
  separate_rows(intervention_year_raw, sep = ";") %>%
  mutate(
    intervention_year_raw = str_trim(intervention_year_raw),
    intervention_year     = suppressWarnings(as.numeric(intervention_year_raw))
  ) %>%
  filter(!is.na(intervention_year)) %>%
  left_join(
    sampling_df %>% select(study_id, study_id_factor, y_num),
    by = "study_id"
  ) %>%
  distinct(study_id, intervention_category, intervention_year, study_id_factor, y_num)

## 6b) Stop if there are no valid disturbance events ---------------
if (nrow(interventions_long) == 0) {
  stop("No valid disturbance-year records were found for plotting.", call. = FALSE)
}


# STEP 7: BUILD THE RS ACQUISITION DATA ===============================

## 7a) Expand RS acquisition years to one row per study-year -------
rs_long <- temporal_clean %>%
  select(study_id, RS_acquisition_year) %>%
  distinct() %>%
  filter(!is.na(RS_acquisition_year)) %>%
  separate_rows(RS_acquisition_year, sep = ";") %>%
  mutate(
    RS_acquisition_year = str_trim(RS_acquisition_year),
    rs_year             = suppressWarnings(as.numeric(RS_acquisition_year))
  ) %>%
  filter(!is.na(rs_year)) %>%
  left_join(
    sampling_df %>% select(study_id, study_id_factor, y_num),
    by = "study_id"
  ) %>%
  distinct(study_id, rs_year, study_id_factor, y_num)


# STEP 8: DEFINE THE AXIS RANGE =======================================

## 8a) Collect all years shown in the figure -----------------------
all_years <- c(
  sampling_df$sample_year_start,
  sampling_df$sample_year_end,
  interventions_long$intervention_year,
  rs_long$rs_year
)

## 8b) Calculate the plotting range -------------------------------
year_min <- min(all_years, na.rm = TRUE)
year_max <- max(all_years, na.rm = TRUE)

## 8c) Build the y-axis break table --------------------------------
y_breaks <- sampling_df %>%
  distinct(study_id, y_num) %>%
  arrange(y_num)


# STEP 9: BUILD THE FIGURE ============================================

## 9a) Create the temporal timeline figure -------------------------
p_fig7 <- ggplot() +
  geom_segment(
    data = sampling_ranges,
    aes(
      x    = sample_year_start,
      xend = sample_year_end,
      y    = y_num,
      yend = y_num
    ),
    linewidth = 3.2,
    colour    = "grey85"
  ) +
  geom_point(
    data = sampling_points,
    aes(
      x = sample_year_start,
      y = y_num
    ),
    shape  = 15,
    size   = 3.5,
    colour = "grey85"
  ) +
  geom_point(
    data = interventions_long,
    aes(
      x      = intervention_year,
      y      = y_num,
      colour = intervention_category
    ),
    size  = 2.2,
    alpha = 0.97
  ) +
  geom_segment(
    data = rs_long,
    aes(
      x    = rs_year,
      xend = rs_year,
      y    = y_num - 0.28,
      yend = y_num + 0.28
    ),
    colour    = "black",
    linewidth = 0.6
  ) +
  scale_x_continuous(
    name   = "Year",
    limits = c(year_min - 1, year_max + 1),
    breaks = pretty(c(year_min, year_max))
  ) +
  scale_y_continuous(
    name   = "Study",
    breaks = y_breaks$y_num,
    labels = format_study_label(y_breaks$study_id),
    expand = expansion(add = c(0.6, 0.6))
  ) +
  labs(colour = "Disturbance type") +
  theme_bw(base_size = base_font_size) +
  theme(
    axis.text.y        = element_text(size = 7),
    legend.position    = "bottom",
    panel.grid.major.y = element_line(color = "grey65", linewidth = 0.4)
  )

## 9b) Display the figure in the plotting window -------------------
print(p_fig7)


# STEP 10: EXPORT THE FIGURE ==========================================

## 10a) Create the output folder if needed -------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 10b) Save the figure --------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_fig7,
  height   = plot_height,
  width    = plot_width,
  dpi      = plot_dpi,
  units    = "in"
)


# STEP 11: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 11a) Total studies in the source file ---------------------------
n_total_studies_in_file <- temporal_data %>%
  summarise(n = n_distinct(study_id)) %>%
  pull(n)

## 11b) Timing-eligible studies used in the figure -----------------
n_timing_eligible <- temporal_clean %>%
  summarise(n = n_distinct(study_id)) %>%
  pull(n)

## 11c) Define unique disturbance events ---------------------------
# A disturbance event is defined as a unique:
# study_id × disturbance type × event year
events <- interventions_long %>%
  transmute(
    study_id,
    disturbance = str_squish(intervention_category),
    event_year  = intervention_year
  ) %>%
  filter(!is.na(disturbance), disturbance != "", !is.na(event_year)) %>%
  distinct(study_id, disturbance, event_year)

n_events <- nrow(events)

events_by_type <- events %>%
  count(disturbance, name = "n") %>%
  mutate(pct = 100 * n / n_events) %>%
  arrange(desc(n), disturbance)

## 11d) Count studies with multiple events of the same type --------
multi_same_type <- events %>%
  group_by(study_id, disturbance) %>%
  summarise(
    n_events_type = n_distinct(event_year),
    .groups = "drop"
  ) %>%
  filter(n_events_type > 1)

n_studies_multi_same_type <- multi_same_type %>%
  distinct(study_id) %>%
  nrow()

n_studies_multi_wildfire <- multi_same_type %>%
  filter(str_to_lower(disturbance) == "wildfire") %>%
  distinct(study_id) %>%
  nrow()

## 11e) Summarize RS acquisition reporting -------------------------
rs_years <- rs_long %>%
  transmute(study_id, rs_year) %>%
  filter(!is.na(rs_year)) %>%
  distinct(study_id, rs_year)

n_rs_reported_studies <- rs_years %>%
  distinct(study_id) %>%
  nrow()

studies_rs_matches_disturb <- rs_years %>%
  inner_join(events, by = c("study_id" = "study_id", "rs_year" = "event_year")) %>%
  distinct(study_id)

n_rs_match <- nrow(studies_rs_matches_disturb)

## 11f) Calculate post-disturbance lag summaries -------------------
post_lag_per_study <- events %>%
  inner_join(rs_years, by = "study_id") %>%
  mutate(post_lag = rs_year - event_year) %>%
  filter(post_lag >= 0) %>%
  group_by(study_id) %>%
  summarise(
    min_post_lag = min(post_lag),
    .groups = "drop"
  )

if (nrow(post_lag_per_study) > 0) {
  max_min_post_lag <- post_lag_per_study %>%
    arrange(desc(min_post_lag), study_id) %>%
    slice(1)
} else {
  max_min_post_lag <- tibble(
    study_id = NA_character_,
    min_post_lag = NA_real_
  )
}

## 11g) Count studies with pre-disturbance RS ----------------------
earliest_event <- events %>%
  group_by(study_id) %>%
  summarise(
    min_event_year = min(event_year),
    .groups = "drop"
  )

pre_disturb_studies <- rs_years %>%
  inner_join(earliest_event, by = "study_id") %>%
  group_by(study_id) %>%
  summarise(
    any_pre = any(rs_year < min_event_year),
    .groups = "drop"
  ) %>%
  filter(any_pre)

n_pre_disturb_studies <- nrow(pre_disturb_studies)

## 11h) Compare event years to the sampling window -----------------
sampling_only <- sampling_df %>%
  transmute(
    study_id,
    s0 = sample_year_start,
    s1 = sample_year_end
  ) %>%
  distinct()

dist_to_sampling <- events %>%
  inner_join(sampling_only, by = "study_id") %>%
  mutate(
    dist_to_window = case_when(
      event_year < s0 ~ s0 - event_year,
      event_year > s1 ~ event_year - s1,
      TRUE            ~ 0
    )
  ) %>%
  group_by(study_id) %>%
  summarise(
    min_disturb_to_sampling = min(dist_to_window),
    .groups = "drop"
  )

## 11i) Summarize sampling spans ----------------------------------
sampling_span <- sampling_only %>%
  mutate(span_years = s1 - s0)

n_single_year <- sampling_span %>%
  filter(span_years == 0) %>%
  nrow()

max_sampling_span <- if (nrow(sampling_span) > 0) {
  max(sampling_span$span_years, na.rm = TRUE)
} else {
  NA_real_
}

## 11j) Compare RS acquisition years to the sampling window -------
rs_with_sampling <- rs_years %>%
  inner_join(sampling_only, by = "study_id") %>%
  mutate(in_sampling = (rs_year >= s0 & rs_year <= s1))

pct_rs_years_in_sampling <- if (nrow(rs_with_sampling) > 0) {
  mean(rs_with_sampling$in_sampling) * 100
} else {
  NA_real_
}

pct_studies_any_rs_in_sampling <- if (nrow(rs_with_sampling) > 0) {
  rs_with_sampling %>%
    group_by(study_id) %>%
    summarise(any_in = any(in_sampling), .groups = "drop") %>%
    summarise(pct = 100 * mean(any_in)) %>%
    pull(pct)
} else {
  NA_real_
}

rs_offsets <- rs_with_sampling %>%
  filter(!in_sampling) %>%
  mutate(
    relation = case_when(
      rs_year < s0 ~ "precedes_sampling",
      rs_year > s1 ~ "follows_sampling"
    ),
    offset_years = case_when(
      rs_year < s0 ~ s0 - rs_year,
      rs_year > s1 ~ rs_year - s1
    )
  )

## 11k) Create a consistent summary table --------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Rows imported",
    "Unique studies in source file",
    "Timing-eligible studies",
    "Timing-eligible studies (%)",
    "Sampling ranges plotted",
    "Single-year sampling studies plotted",
    "Unique disturbance events",
    "Unique disturbance types",
    "Studies with multiple events of the same type",
    "Studies with multiple wildfire events",
    "Studies with >=1 RS acquisition year",
    "Studies with >=1 RS year matching a disturbance year",
    "Studies with any pre-disturbance RS year",
    "Single-year sampling periods",
    "Longest sampling span (years)",
    "Percent of RS years within sampling windows",
    "Percent of studies with >=1 RS year within sampling window",
    "Longest minimum post-disturbance lag (years)",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    nrow(temporal_data),
    n_total_studies_in_file,
    n_timing_eligible,
    round(100 * n_timing_eligible / n_total_studies_in_file, 1),
    nrow(sampling_ranges),
    nrow(sampling_points),
    n_events,
    n_distinct(events$disturbance),
    n_studies_multi_same_type,
    n_studies_multi_wildfire,
    n_rs_reported_studies,
    n_rs_match,
    n_pre_disturb_studies,
    n_single_year,
    max_sampling_span,
    round(pct_rs_years_in_sampling, 1),
    round(pct_studies_any_rs_in_sampling, 1),
    max_min_post_lag$min_post_lag[1],
    output_file
  )
)

## 11l) Print quality checks to the console ------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n========== DISTURBANCE EVENTS BY TYPE ==========\n")
print(events_by_type, n = Inf)

cat("\n===== STUDIES WITH MULTIPLE EVENTS OF THE SAME TYPE =====\n")
if (nrow(multi_same_type) > 0) {
  print(multi_same_type, n = Inf)
} else {
  cat("No studies had multiple events of the same disturbance type.\n")
}

cat("\n===== LONGEST MINIMUM POST-DISTURBANCE LAG =====\n")
print(max_min_post_lag, n = Inf)

cat("\n===== MINIMUM DISTURBANCE-TO-SAMPLING DISTANCE =====\n")
if (nrow(dist_to_sampling) > 0) {
  cat(
    "Mean:", round(mean(dist_to_sampling$min_disturb_to_sampling), 2),
    " Median:", median(dist_to_sampling$min_disturb_to_sampling),
    " Max:", max(dist_to_sampling$min_disturb_to_sampling), "\n"
  )
  print(
    dist_to_sampling %>%
      arrange(desc(min_disturb_to_sampling), study_id) %>%
      slice_head(n = 10),
    n = 10
  )
} else {
  cat("No disturbance-to-sampling comparisons were available.\n")
}

cat("\n===== LONGEST SAMPLING SPANS =====\n")
print(
  sampling_span %>%
    arrange(desc(span_years), study_id) %>%
    slice_head(n = 10),
  n = 10
)

cat("\n===== NON-OVERLAPPING RS YEARS RELATIVE TO SAMPLING =====\n")
if (nrow(rs_offsets) > 0) {
  print(rs_offsets %>% count(relation, name = "n"), n = Inf)
  print(
    rs_offsets %>%
      arrange(desc(offset_years), study_id, rs_year) %>%
      slice_head(n = 10),
    n = 10
  )
} else {
  cat("All RS acquisition years overlapped sampling windows, or no RS years were available.\n")
}


