# FIG_3_biome_map_w_barchart.R
# 
# Figure output:
# This script creates a two-part figure showing:
# 1) a global map of study locations over biome polygons, and
# 2) a horizontal bar chart of the most represented biomes.
#
# The script uses the finalized Excel study dataset plus biome
# shapefiles stored in the packaged project folder.
#
# Expected folder structure:
# Roach-Krajewski_et_al_2026_packaged/
# ├── data/
# │   ├── 4_final_study_data/
# │   │   └── evidence_map_data.xlsx
# │   └── 5_extra_figure_data/
# │       ├── biomes.shp
# │       └── Ecoregions2017.shp
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
  library(purrr)
  library(sf)
  library(cowplot)
  library(scales)
  library(tibble)
})

## 1f) Define input and output paths -------------------------------
input_file       <- file.path(proj_root, "data", "4_final_study_data", "evidence_map_data.xlsx")
sheet_name       <- "5_study_data"
biomes_path      <- file.path(proj_root, "data", "5_extra_figure_data", "biomes.shp")
ecoregions_path  <- file.path(proj_root, "data", "5_extra_figure_data", "Ecoregions2017.shp")
output_file      <- file.path(proj_root, "figures", "FIG_3_biome_map_w_barchart.png")

## 1g) Define plot settings ----------------------------------------
plot_width   <- 16
plot_height  <- 11
plot_dpi     <- 200
top_panel_h  <- 0.74
bottom_h     <- 1 - top_panel_h
pad_left     <- 0.016
pad_right    <- 0.016
base_font    <- 12

## 1h) Define the image device ------------------------------------
# Use ragg if available for cleaner PNG output.
plot_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"


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
# The original script used skip = 1, so that is kept here.
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
  "study_type",
  "country",
  "location_lat",
  "location_lon",
  "biome"
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


# STEP 3: PREPARE THE STUDY LOCATION POINTS ===========================

## 3a) Keep only the columns needed for the map --------------------
study_location_data <- study_data %>%
  select(study_id, study_type, country, location_lat, location_lon, biome)

## 3b) Split semicolon-separated coordinates into one row per point
# Each row becomes one mapped study location.
study_location_data <- study_location_data %>%
  mutate(
    location_lat = str_squish(location_lat),
    location_lon = str_squish(location_lon)
  ) %>%
  separate_rows(location_lat, location_lon, sep = ";") %>%
  mutate(
    location_lat = suppressWarnings(as.numeric(str_squish(location_lat))),
    location_lon = suppressWarnings(as.numeric(str_squish(location_lon)))
  ) %>%
  filter(!is.na(location_lat), !is.na(location_lon)) %>%
  filter(!(location_lat == 0 & location_lon == 0))

## 3c) Convert the point data to sf format -------------------------
study_points_sf <- st_as_sf(
  study_location_data,
  coords = c("location_lon", "location_lat"),
  crs    = 4326
)

## 3d) Stop if there are no valid study locations ------------------
if (nrow(study_points_sf) == 0) {
  stop("No valid study locations were found after parsing coordinates.", call. = FALSE)
}


# STEP 4: LOAD OR BUILD THE BIOME LAYER ===============================

## 4a) Check which biome shapefile is available --------------------
# The cleaned workflow first looks for biomes.shp.
# If it does not exist, it tries to build it from Ecoregions2017.shp.
if (!file.exists(biomes_path) && !file.exists(ecoregions_path)) {
  stop(
    paste0(
      "Neither biome shapefile was found.\n",
      "Expected one of:\n",
      " - ", biomes_path, "\n",
      " - ", ecoregions_path
    ),
    call. = FALSE
  )
}

## 4b) Read biomes.shp directly if it already exists ---------------
if (file.exists(biomes_path)) {
  
  biomes <- read_sf(biomes_path) %>%
    st_make_valid()
  
} else {
  
  ## 4c) Otherwise build biomes.shp from Ecoregions2017.shp --------
  ecoregions <- read_sf(ecoregions_path) %>%
    st_make_valid()
  
  biomes <- ecoregions %>%
    group_by(BIOME_NAME, COLOR_BIO) %>%
    summarise(.groups = "drop") %>%
    mutate(
      BIOME_NAME = if_else(BIOME_NAME == "N/A", "Rock and Ice", BIOME_NAME)
    )
  
  dir.create(dirname(biomes_path), recursive = TRUE, showWarnings = FALSE)
  write_sf(biomes, biomes_path, delete_dsn = TRUE)
}

## 4d) Adjust a few biome colours for better contrast --------------
biomes <- biomes %>%
  mutate(
    COLOR_BIO = case_when(
      str_detect(str_to_lower(BIOME_NAME), "temperate conifer")   ~ "#2A9D8F",
      str_detect(str_to_lower(BIOME_NAME), "temperate broadleaf") ~ "darkorchid",
      TRUE                                                        ~ COLOR_BIO
    )
  )


# STEP 5: PREPARE THE MAP LAYERS ======================================

## 5a) Create a Robinson-projection ellipse background -------------
bbox <- tibble(
  lon = c(
    rep(-180, 180),
    seq(-180, 180, length.out = 100),
    rep(180, 180),
    seq(180, -180, length.out = 100)
  ),
  lat = c(
    seq(-90, 90, 1),
    rep(90, 99),
    seq(90, -90, -1),
    rep(-90, 99)
  )
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  st_transform("ESRI:54030")

## 5b) Transform the biome and point layers to Robinson projection -
biomes_proj <- biomes %>%
  st_transform("ESRI:54030")

study_points_proj <- study_points_sf %>%
  st_transform("ESRI:54030")

## 5c) Add a simplified biome name field ---------------------------
biomes_proj_named <- biomes_proj %>%
  mutate(
    biome = if_else(BIOME_NAME == "N/A", "Rock and Ice", BIOME_NAME)
  )


# STEP 6: SUMMARIZE BIOME COUNTS FOR THE BAR CHART ====================

## 6a) Build a biome colour lookup table ---------------------------
biome_palette <- biomes %>%
  st_drop_geometry() %>%
  transmute(
    biome = if_else(BIOME_NAME == "N/A", "Rock and Ice", BIOME_NAME),
    COLOR_BIO
  ) %>%
  distinct()

## 6b) Count unique study-biome memberships ------------------------
# This preserves the original counting logic used for the bars:
# one study contributes one count to each biome it is reported in.
biome_counts_all <- study_data %>%
  select(study_id, biome) %>%
  mutate(
    biome = if_else(biome == "N/A", "Rock and Ice", biome)
  ) %>%
  separate_rows(biome, sep = ";") %>%
  mutate(
    biome = str_trim(biome)
  ) %>%
  filter(!is.na(biome), biome != "") %>%
  distinct(study_id, biome) %>%
  count(biome, name = "n") %>%
  right_join(biome_palette, by = "biome") %>%
  mutate(
    n = replace_na(n, 0L)
  ) %>%
  arrange(desc(n))

## 6c) Identify the biomes with non-zero counts --------------------
biomes_with_points <- biome_counts_all %>%
  filter(n > 0) %>%
  pull(biome)

## 6d) Keep the top 7 biomes for the bar chart ---------------------
bar_data <- biome_counts_all %>%
  filter(n > 0) %>%
  arrange(desc(n)) %>%
  slice_head(n = 7) %>%
  mutate(
    biome = forcats::fct_reorder(biome, n)
  )

## 6e) Stop if there is nothing to plot in the bar chart -----------
if (nrow(bar_data) == 0) {
  stop("No biome counts greater than zero were found for the bar chart.", call. = FALSE)
}

## 6f) Set bar chart labels and axis range -------------------------
max_x <- max(bar_data$n, na.rm = TRUE)
xmin  <- 0
xmax  <- max_x * 1.12

bar_x_label <- "Number of locations"
bar_y_label <- "Biomes"


# STEP 7: BUILD THE BIOME BAR CHART ===================================

## 7a) Create the horizontal bar chart -----------------------------
biome_bar <- ggplot(bar_data, aes(y = biome)) +
  geom_col(
    aes(x = n, fill = COLOR_BIO),
    width       = 0.9,
    show.legend = FALSE,
    alpha       = 0.58
  ) +
  geom_text(
    aes(x = n, label = n),
    hjust = -0.15,
    size  = 3.5
  ) +
  scale_fill_identity() +
  coord_cartesian(
    xlim = c(xmin, xmax),
    clip = "off"
  ) +
  labs(
    x = bar_x_label,
    y = bar_y_label
  ) +
  scale_x_continuous(
    breaks = pretty(c(0, max_x))
  ) +
  theme_bw(base_size = base_font) +
  theme(
    plot.margin  = margin(0, 10, 10, 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 6)),
    axis.title.y = element_text(size = 12, margin = margin(r = 6)),
    axis.text.y  = element_text(size = 12)
  )


# STEP 8: BUILD THE STUDY LOCATION MAP ================================

## 8a) Keep only the biomes that have non-zero counts --------------
biomes_colored <- biomes_proj_named %>%
  filter(biome %in% biomes_with_points)

## 8b) Create the study map ----------------------------------------
study_map <- ggplot() +
  geom_sf(
    data      = bbox,
    fill      = "grey95",
    color     = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data  = biomes_proj_named,
    fill  = "grey85",
    color = NA
  ) +
  geom_sf(
    data  = biomes_colored,
    aes(fill = COLOR_BIO),
    color = NA,
    alpha = 0.58
  ) +
  scale_fill_identity(guide = "none") +
  geom_sf(
    data   = study_points_proj,
    fill   = "black",
    color  = "black",
    stroke = 0.3,
    size   = 2
  ) +
  labs(
    title = "Locations of studies included in knowledge map"
  ) +
  theme_bw() +
  theme(
    legend.position  = "none",
    panel.background = element_blank(),
    panel.grid       = element_blank(),
    panel.border     = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    plot.title       = element_text(size = 16, hjust = 0.5),
    text             = element_text(size = 13),
    plot.margin      = margin(0, 0, 0, 0)
  )


# STEP 9: COMBINE THE MAP AND BAR CHART ===============================

## 9a) Stack the two panels into one figure ------------------------
combined_fig3 <- ggdraw() +
  draw_plot(
    study_map,
    x      = 0,
    y      = bottom_h,
    width  = 1,
    height = top_panel_h
  ) +
  draw_plot(
    biome_bar,
    x      = pad_left,
    y      = 0,
    width  = 1 - pad_left - pad_right,
    height = bottom_h
  )

## 9b) Display the figure in the plotting window -------------------
print(combined_fig3)


# STEP 10: EXPORT THE FIGURE ==========================================

## 10a) Create the output folder if needed -------------------------
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

## 10b) Save the figure --------------------------------------------
ggsave(
  filename = output_file,
  plot     = combined_fig3,
  width    = plot_width,
  height   = plot_height,
  units    = "in",
  dpi      = plot_dpi,
  device   = plot_device
)


# STEP 11: RUN QUALITY CHECKS AND PRINT SUMMARY OUTPUT ===============

## 11a) Count studies reported in more than one biome -------------
multi_biome_studies <- study_data %>%
  select(study_id, biome) %>%
  mutate(
    biome = if_else(biome == "N/A", "Rock and Ice", biome)
  ) %>%
  separate_rows(biome, sep = ";") %>%
  mutate(
    biome = str_trim(biome)
  ) %>%
  filter(!is.na(biome), biome != "") %>%
  group_by(study_id) %>%
  summarise(
    n_biomes = n_distinct(biome),
    biomes   = paste(sort(unique(biome)), collapse = "; "),
    .groups  = "drop"
  ) %>%
  filter(n_biomes > 1) %>%
  arrange(desc(n_biomes), study_id)

## 11b) Count mapped locations and biome memberships --------------
n_cases_locations         <- nrow(study_location_data)
n_unique_studies_mapped   <- n_distinct(study_location_data$study_id)
n_cases_biome_memberships <- sum(biome_counts_all$n, na.rm = TRUE)

## 11c) Create a consistent summary table --------------------------
qc_summary <- tibble(
  metric = c(
    "Project root",
    "Input file",
    "Sheet name",
    "Biome shapefile used",
    "Rows imported",
    "Unique studies imported",
    "Mapped study locations",
    "Unique studies with mapped locations",
    "Biomes with non-zero counts",
    "Top biomes shown in bar chart",
    "Total study-biome memberships in bars",
    "Studies spanning more than one biome",
    "Output file"
  ),
  value = c(
    proj_root,
    input_file,
    sheet_name,
    if (file.exists(biomes_path)) biomes_path else ecoregions_path,
    nrow(study_data),
    n_distinct(study_data$study_id),
    n_cases_locations,
    n_unique_studies_mapped,
    length(biomes_with_points),
    nrow(bar_data),
    n_cases_biome_memberships,
    nrow(multi_biome_studies),
    output_file
  )
)

## 11d) Create a biome totals table for the console ----------------
biome_totals <- biome_counts_all %>%
  arrange(desc(n), biome)

## 11e) Print quality checks to the console ------------------------
cat("\n================ QUALITY CHECKS ================\n")
print(qc_summary, n = Inf)

cat("\n========== TOTAL COUNTS BY BIOME ==========\n")
print(biome_totals, n = Inf)

cat("\n===== STUDIES SPANNING MORE THAN ONE BIOME =====\n")
if (nrow(multi_biome_studies) > 0) {
  print(multi_biome_studies, n = Inf)
} else {
  cat("No studies span more than one reported biome.\n")
}
