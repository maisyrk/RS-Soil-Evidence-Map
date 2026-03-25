# FIG_6_spatial_scale.R
# 
# Figure output:
# This script creates a faceted range plot showing the spatial
# scale of studies (study area in hectares, log scale) across:
# 1) disturbance type,
# 2) platform,
# 3) sensor type,
# 4) sample unit, and
# 5) outcome.
#
# For each category, the figure shows:
# - the minimum and maximum reported study area,
# - the mean study area, and
# - the number of cases (n).
#
# The final figure uses the gradient-style range bars from the
# original script.
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
  library(stringr)
  library(scales)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file  <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
sheet_name  <- "5_study_data"
output_file <- file.path(proj_root, "figures", "FIG_6_spatial_scale.png")

## 1g) Define plot settings ----------------------------------------
plot_width      <- 8
plot_height     <- 10
plot_dpi        <- 350
base_font_size  <- 12
n_gradient_steps <- 100


# STEP 2: DEFINE HELPER FUNCTIONS =====================================

## 2a) Clean category labels for display ---------------------------
# This keeps common acronyms in upper case and standardizes LiDAR.
clean_label <- function(x) {
  x <- str_squish(x)
  x_sentence <- str_to_sentence(x)
  
  case_when(
    str_to_upper(x) == "UAV" ~ "UAV",
    str_to_upper(x) == "RGB" ~ "RGB",
    str_to_upper(x) == "SAR" ~ "SAR",
    str_detect(x, regex("^lidar$", ignore_case = TRUE)) ~ "LiDAR",
    TRUE ~ x_sentence
  )
}

## 2b) Summarize min, max, mean, and n for one semicolon field ----
summarise_range_single <- function(df, col_name, group_name) {
  df %>%
    select(full_aou_ha, label_raw = all_of(col_name)) %>%
    filter(!is.na(label_raw)) %>%
    separate_rows(label_raw, sep = ";") %>%
    mutate(
      label_raw = str_squish(label_raw),
      label_raw = na_if(label_raw, "")
    ) %>%
    filter(!is.na(label_raw)) %>%
    group_by(label_raw) %>%
    summarise(
      xmin  = min(full_aou_ha, na.rm = TRUE),
      xmax  = max(full_aou_ha, na.rm = TRUE),
      xmean = mean(full_aou_ha, na.rm = TRUE),
      n     = n(),
      .groups = "drop"
    ) %>%
    mutate(
      label = clean_label(label_raw),
      group = group_name
    )
}

## 2c) Summarize min, max, mean, and n across multiple columns ----
# This is used for outcomes and disturbance categories because
# they are stored across multiple columns in the study table.
summarise_range_multi <- function(df, cols, value_name, group_name) {
  df %>%
    select(full_aou_ha, all_of(cols)) %>%
    pivot_longer(
      cols      = all_of(cols),
      names_to  = "source_col",
      values_to = value_name
    ) %>%
    filter(!is.na(.data[[value_name]])) %>%
    separate_rows(all_of(value_name), sep = ";") %>%
    mutate(
      label_raw = str_squish(.data[[value_name]]),
      label_raw = na_if(label_raw, "")
    ) %>%
    filter(!is.na(label_raw)) %>%
    group_by(label_raw) %>%
    summarise(
      xmin  = min(full_aou_ha, na.rm = TRUE),
      xmax  = max(full_aou_ha, na.rm = TRUE),
      xmean = mean(full_aou_ha, na.rm = TRUE),
      n     = n(),
      .groups = "drop"
    ) %>%
    mutate(
      label = clean_label(label_raw),
      group = group_name
    )
}

## 2d) Build gradient segments for the range bars ------------------
# The gradient is calculated in log10 space so it looks smooth on
# the log-scaled x-axis.
build_gradient_segments <- function(df, n_steps = 100) {
  
  out_list <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    
    row_i <- df[i, ]
    
    lxmin  <- log10(row_i$xmin)
    lxmax  <- log10(row_i$xmax)
    lxmean <- log10(row_i$xmean)
    
    l_seq   <- seq(lxmin, lxmax, length.out = n_steps + 1)
    l_start <- head(l_seq, -1L)
    l_end   <- tail(l_seq, -1L)
    l_mid   <- (l_start + l_end) / 2
    
    x_start <- 10^l_start
    x_end   <- 10^l_end
    
    left_span  <- lxmean - lxmin
    right_span <- lxmax - lxmean
    
    alpha <- numeric(length(l_mid))
    
    left_idx <- l_mid <= lxmean
    if (left_span > 0) {
      alpha[left_idx] <- (l_mid[left_idx] - lxmin) / left_span
    } else {
      alpha[left_idx] <- 0
    }
    
    right_idx <- l_mid >= lxmean
    if (right_span > 0) {
      alpha[right_idx] <- (lxmax - l_mid[right_idx]) / right_span
    } else {
      alpha[right_idx] <- 0
    }
    
    alpha <- pmax(0, pmin(1, alpha))
    alpha <- alpha^1.5
    
    out_list[[i]] <- tibble(
      id         = row_i$id,
      group      = row_i$group,
      label_plot = row_i$label_plot,
      x_start    = x_start,
      x_end      = x_end,
      alpha      = alpha
    )
  }
  
  bind_rows(out_list)
}

## 2e) Create case tally tables for the QC section ----------------
case_table <- function(df, group_name) {
  
  out <- df %>%
    transmute(
      group    = group_name,
      category = as.character(label),
      n_cases  = as.integer(n)
    ) %>%
    group_by(group, category) %>%
    summarise(n_cases = sum(n_cases), .groups = "drop")
  
  denom <- sum(out$n_cases, na.rm = TRUE)
  
  out %>%
    mutate(
      denom_cases = denom,
      pct_cases   = round(100 * n_cases / denom_cases, 1)
    ) %>%
    arrange(desc(n_cases), category)
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
  na           = "NA",
  col_types    = "text",
  .name_repair = "minimal"
) %>%
  mutate(across(everything(), as.character))

## 3c) Check that required columns are present ---------------------
required_single_cols <- c(
  "study_id",
  "full_aou_ha",
  "sample_unit_clean",
  "platform",
  "sensor_type"
)

missing_single_cols <- setdiff(required_single_cols, names(study_data))

outcome_cols <- grep("^outcome_", names(study_data), value = TRUE)
disturbance_cols <- grep("^intervention_category_", names(study_data), value = TRUE)

if (length(missing_single_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the input sheet:\n",
      paste(missing_single_cols, collapse = ", ")
    ),
    call. = FALSE
  )
}

if (length(outcome_cols) == 0) {
  stop("No outcome columns were found (expected columns starting with 'outcome_').", call. = FALSE)
}

if (length(disturbance_cols) == 0) {
  stop("No disturbance columns were found (expected columns starting with 'intervention_category_').", call. = FALSE)
}


# STEP 4: PREPARE THE STUDY AREA DATA ================================

## 4a) Convert full_aou_ha to numeric ------------------------------
study_area_data <- study_data %>%
  mutate(
    full_aou_ha = na_if(full_aou_ha, "NR"),
    full_aou_ha = suppressWarnings(as.numeric(full_aou_ha))
  )

## 4b) Keep only valid positive study areas ------------------------
# Positive values are required because the final figure uses a
# log-scaled x-axis.
study_area_data <- study_area_data %>%
  filter(!is.na(full_aou_ha), full_aou_ha > 0)

## 4c) Stop if there are no valid study areas ----------------------
if (nrow(study_area_data) == 0) {
  stop("No valid positive values were found in full_aou_ha.", call. = FALSE)
}


# STEP 5: SUMMARIZE SPATIAL SCALE BY CATEGORY =========================

## 5a) Summarize sample unit ---------------------------------------
sample_df <- summarise_range_single(
  df         = study_area_data,
  col_name   = "sample_unit_clean",
  group_name = "Sample unit"
) %>%
  filter(!label %in% c("Nr", "NR", "nr"))

## 5b) Summarize platform ------------------------------------------
platform_df <- summarise_range_single(
  df         = study_area_data,
  col_name   = "platform",
  group_name = "Platform"
)

## 5c) Summarize sensor type ---------------------------------------
sensor_df <- summarise_range_single(
  df         = study_area_data,
  col_name   = "sensor_type",
  group_name = "Sensor type"
)

## 5d) Summarize outcomes ------------------------------------------
outcome_df <- summarise_range_multi(
  df         = study_area_data,
  cols       = outcome_cols,
  value_name = "outcome",
  group_name = "Outcome"
)

## 5e) Summarize disturbance type ----------------------------------
disturbance_df <- summarise_range_multi(
  df         = study_area_data,
  cols       = disturbance_cols,
  value_name = "disturbance",
  group_name = "Disturbance type"
)


# STEP 6: COMBINE AND ORDER THE PLOTTING DATA ========================

## 6a) Combine all category summaries ------------------------------
plot_dat <- bind_rows(
  disturbance_df,
  platform_df,
  sensor_df,
  sample_df,
  outcome_df
) %>%
  filter(
    is.finite(xmin),
    is.finite(xmax),
    is.finite(xmean),
    xmin > 0,
    xmax > 0,
    xmean > 0
  ) %>%
  mutate(
    xmin  = as.numeric(xmin),
    xmax  = as.numeric(xmax),
    xmean = as.numeric(xmean),
    group = factor(
      group,
      levels = c("Disturbance type", "Platform", "Sensor type", "Sample unit", "Outcome")
    ),
    label_plot = paste(label, group, sep = "___")
  )

## 6b) Order labels within each facet by mean study area ----------
# Lowest mean is shown at the top of each facet.
label_levels <- plot_dat %>%
  arrange(group, desc(xmean), label) %>%
  pull(label_plot) %>%
  unique()

plot_dat <- plot_dat %>%
  mutate(
    label_plot = factor(label_plot, levels = label_levels),
    id         = row_number()
  )

custom_labels <- plot_dat %>%
  distinct(label_plot, label)

## 6c) Build the gradient segment data -----------------------------
gradient_dat <- build_gradient_segments(
  df      = plot_dat,
  n_steps = n_gradient_steps
)


# STEP 7: BUILD THE FIGURE ============================================

## 7a) Create the final gradient range plot ------------------------
p_fig6 <- ggplot() +
  geom_segment(
    data = gradient_dat,
    aes(
      x      = x_start,
      xend   = x_end,
      y      = label_plot,
      yend   = label_plot,
      colour = group,
      alpha  = alpha
    ),
    linewidth = 4,
    lineend   = "butt"
  ) +
  scale_alpha(range = c(0, 1), guide = "none") +
  geom_segment(
    data = plot_dat,
    aes(
      x      = xmin,
      xend   = xmax,
      y      = label_plot,
      yend   = label_plot,
      colour = group
    ),
    linewidth = 4,
    alpha     = 0.15,
    lineend   = "round"
  ) +
  geom_point(
    data = plot_dat,
    aes(
      x = xmean,
      y = label_plot
    ),
    size   = 3,
    colour = "black",
    alpha  = 1
  ) +
  geom_point(
    data = plot_dat,
    aes(
      x      = xmean,
      y      = label_plot,
      colour = group
    ),
    size  = 2.6,
    alpha = 1
  ) +
  geom_text(
    data = plot_dat,
    aes(
      x     = (xmin + xmax) / 2,
      y     = label_plot,
      label = paste0("n = ", n)
    ),
    position    = position_nudge(y = -0.25),
    vjust       = 1,
    size        = 3,
    show.legend = FALSE
  ) +
  facet_grid(
    group ~ .,
    scales = "free_y",
    space  = "free_y",
    switch = "y"
  ) +
  scale_y_discrete(
    breaks = custom_labels$label_plot,
    labels = custom_labels$label
  ) +
  scale_x_log10(
    position = "top",
    sec.axis = dup_axis(name = NULL)
  ) +
  labs(
    x      = "Study area (ha, log scale)",
    y      = NULL,
    colour = NULL
  ) +
  theme_bw(base_size = base_font_size) +
  theme(
    strip.placement    = "outside",
    strip.background   = element_rect(fill = "grey90", colour = NA),
    strip.text.y.left  = element_text(angle = 0, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none"
  )

## 7b) Display the figure in the plotting window -------------------
print(p_fig6)


# STEP 8: EXPORT THE FIGURE ===========================================

## 8a) Create the output folder if needed --------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 8b) Save the figure ---------------------------------------------
ggsave(
  filename = output_file,
  plot     = p_fig6,
  width    = plot_width,
  height   = plot_height,
  dpi      = plot_dpi,
  units    = "in"
)


# STEP 9: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ================

## 9a) Count studies with valid study area -------------------------
n_studies_with_area <- study_area_data %>%
  summarise(n = n_distinct(study_id)) %>%
  pull(n)

## 9b) Create case tally tables matching the figure ---------------
platform_cases <- case_table(platform_df, "Platform")
disturbance_cases <- case_table(disturbance_df, "Disturbance type")
sensor_cases <- case_table(sensor_df, "Sensor type")
sampleunit_cases <- case_table(sample_df, "Sample unit")
outcome_cases <- case_table(outcome_df, "Outcome")

all_cases <- bind_rows(
  platform_cases,
  disturbance_cases,
  sensor_cases,
  sampleunit_cases,
  outcome_cases
)

## 9c) Create facet denominators ----------------------------------
denominators <- all_cases %>%
  distinct(group, denom_cases) %>%
  arrange(factor(group, levels = c("Disturbance type", "Platform", "Sensor type", "Sample unit", "Outcome")))

## 9d) Create a consistent summary table ---------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Rows imported",
    "Unique studies imported",
    "Rows with valid positive full_aou_ha",
    "Unique studies with valid positive full_aou_ha",
    "Disturbance categories plotted",
    "Platform categories plotted",
    "Sensor type categories plotted",
    "Sample unit categories plotted",
    "Outcome categories plotted",
    "Overall minimum study area (ha)",
    "Overall maximum study area (ha)",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    sheet_name,
    nrow(study_data),
    n_distinct(study_data$study_id),
    nrow(study_area_data),
    n_studies_with_area,
    nrow(disturbance_df),
    nrow(platform_df),
    nrow(sensor_df),
    nrow(sample_df),
    nrow(outcome_df),
    min(study_area_data$full_aou_ha, na.rm = TRUE),
    max(study_area_data$full_aou_ha, na.rm = TRUE),
    output_file
  )
)

## 9e) Print quality checks to the console -------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n--- DENOMINATORS (TOTAL CASES PER FACET) ---\n")
print(denominators, n = Inf)

cat("\n--- DISTURBANCE TYPE (CASES) ---\n")
print(disturbance_cases, n = Inf)

cat("\n--- PLATFORM (CASES) ---\n")
print(platform_cases, n = Inf)

cat("\n--- SENSOR TYPE (CASES) ---\n")
print(sensor_cases, n = Inf)

cat("\n--- SAMPLE UNIT (CASES) ---\n")
print(sampleunit_cases, n = Inf)

cat("\n--- OUTCOME (CASES) ---\n")
print(outcome_cases, n = Inf)

