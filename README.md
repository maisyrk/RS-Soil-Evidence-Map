# Overview

The repository is organized as a packaged workflow so that a user can download the full folder, open the project in R or RStudio, and run the scripts using a consistent directory structure.

The repository contains two broad types of scripts:

1. **Bibliographic parsing and merging scripts** used to process database search exports and snowball-search results.
2. **Figure scripts** used to generate the final manuscript figures from the finalized evidence-map dataset and supporting figure-specific files.

# Important note about the final study dataset

The bibliographic parsing and merging scripts were used to support the search, screening, and record-management workflow. However, the **final evidence-map study dataset used for analysis and plotting was compiled manually**.

After screening and full-text review, information from the included studies was manually extracted and assembled into the final workbook:

`data/4_final_study_data/evidence_map_data.xlsx`

This workbook is the main source used for the final analysis and figure scripts. It contains the finalized coded study variables used in the manuscript.

Thus:

- The early scripts in `R/` were used to process raw search outputs and help manage the evidence-map workflow.
- The final analytical figures were produced from the manually compiled and finalized study-data workbook.

# Repository structure

    Roach-Krajewski_et_al_2026_packaged/
    ├── data/
    │   ├── 0_initial_search/
    │   ├── 1_parsed_databases/
    │   ├── 2_merged_databases/
    │   ├── 3_snowball_search/
    │   ├── 4_final_study_data/
    │   └── 5_extra_figure_data/
    ├── R/
    │   └── FIGURES/
    └── figures/

# Directory descriptions

## `data/0_initial_search`

This folder contains the raw bibliographic exports downloaded from the original database searches. These are the starting inputs for the parsing scripts.

Includes the raw exports from:

- EBSCOhost
- Google Scholar
- NRCan OSTR
- OpenAlex
- Scopus

## `data/1_parsed_databases`

This folder stores cleaned, parsed bibliographic files created from the raw search exports and snowball-search exports:

- `parsed_ebscohost.csv`
- `parsed_googlescholar.csv`
- `parsed_nrcanostr.csv`
- `parsed_openalex.csv`
- `parsed_scopus.csv`
- `parsed_researchrabbit.csv`

## `data/2_merged_databases`

This folder stores merged bibliographic outputs created after combining and de-duplicating the parsed database files.

## `data/3_snowball_search`

This folder contains snowball-search inputs and outputs, including the ResearchRabbit export and the final de-duplicated ResearchRabbit-only subset.

- `3_research_rabbit.csv`
- `researchrabbit_distinct.csv`

## `data/4_final_study_data`

This folder contains the **final manually compiled evidence-map dataset** used for the manuscript analyses and figures:

- `evidence_map_data.xlsx`

This workbook contains the finalized study-level coded variables used for the final figures.

Note: This folder may also contain additional derived files created from that workbook.

## `data/5_extra_figure_data`

This folder contains supporting files used by some figure scripts:

- `biomes.shp`
- `Ecoregions2017.shp`
- `temporal_scale.xlsx`

## `R`

This folder contains the main processing and workflow scripts, including:

- bibliographic parsing scripts
- database merging scripts
- snowball-search processing scripts

## `R/FIGURES`

This folder contains the scripts used to generate the final manuscript figures. Each final figure has its own script.

## `figures`

This folder contains the exported figure image files produced by the figure scripts.

# General script design

The cleaned scripts in this package were standardized so that they:

- automatically detect the project root folder (`Roach-Krajewski_et_al_2026_packaged`)
- set the working directory to that project root
- include a short explanation of script purpose, inputs, and outputs at the top
- export outputs to the appropriate `data/` or `figures/` folder
- print end-of-script quality checks to the console

# Recommended workflow order

## Stage 1: Parse the original database exports

Run the parsing scripts for the raw search outputs:

1. `0_parse_ebscohost.R`
2. `0_parse_googlescholar.R`
3. `0_parse_nrcanostr.R`
4. `0_parse_openalex.R`
5. `0_parse_scopus.R`

These scripts read raw files from `data/0_initial_search/` and write cleaned parsed files to `data/1_parsed_databases/`.

## Stage 2: Merge and de-duplicate the main search databases

Run:

6. `1_merged_dbases.R`

This script combines the parsed databases, applies de-duplication logic, and writes the merged bibliography to `data/2_merged_databases/`.

## Stage 3: Parse and merge snowball-search records

Run:

7. `2_parse_researchrabbit.R`
8. `3_merged_snowball.R`

These scripts process the ResearchRabbit snowball-search export and produce the distinct ResearchRabbit subset in `data/3_snowball_search/`.

## Stage 4: Manual screening, coding, and final dataset assembly

After the bibliographic workflow, the final included studies were screened and coded manually. The resulting final coded dataset was compiled into:

`data/4_final_study_data/evidence_map_data.xlsx`

This is the primary input used by the final analytical and plotting scripts.

## Stage 5: Generate the final figures

Run the figure scripts in `R/FIGURES/` as needed.

# Figure scripts

Each figure script generates one output figure.

## Main figures

- `FIG_1_intervention_count_by_pub_year_cleaned.R`
- `FIG_2_sensor-platform_by_pub_year_cleaned.R`
- `FIG_3_biome_map_w_barchart_cleaned.R`
- `FIG_4_biome_disturbance_sensorType_cleaned.R`
- `FIG_5_indicator_RS_intervention.R`
- `FIG_6_spatial_scale_cleaned.R`
- `FIG_7_temporal_scale_cleaned.R`

## Supplementary figures

- `FIG_S2_platform_sensorType_by_disturbance_cleaned.R`
- `FIG_S3_rs_method_by_disturbance_cleaned.R`

# Summary of figure inputs

## Figures using `evidence_map_data.xlsx`

- Figure 1
- Figure 2
- Figure 3
- Figure 4
- Figure 6
- Figure S2
- Figure S3

## Figures using `evidence_map_data_with_indicator_categories.xlsx`

- Figure 5

## Figures using `data/5_extra_figure_data`

- Figure 3 uses biome shapefiles
- Figure 7 uses `temporal_scale.xlsx`


