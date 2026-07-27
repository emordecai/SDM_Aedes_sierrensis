# *Aedes sierrensis* Species Distribution Models

This repository contains the R code used to develop species distribution models (SDMs) for the western tree hole mosquito, *Aedes sierrensis*, across its range in North America.

The workflow downloads mosquito occurrence records from the Global Biodiversity Information Facility (GBIF), processes and filters these records, associates them with environmental predictors, assigns spatial cross-validation folds, and fits species distribution models using XGBoost. The repository also contains scripts for comparing alternative model specifications based on different environmental variable sets and for generating figures summarizing model performance and predictions.

## Workflow

The analysis consists of the following major steps:

1. Download occurrence records for *Aedes sierrensis* and other mosquito species from GBIF.
2. Clean occurrence records and remove duplicate or low-quality observations.
3. Spatially thin occurrence records to reduce sampling bias.
4. Generate occurrence and background datasets.
5. Extract environmental predictor values for each observation.
6. Assign spatial cross-validation folds.
7. Train species distribution models using XGBoost.
8. Compare alternative model specifications using different environmental predictor sets.
9. Generate summary tables and figures.

## Repository structure

```text
.
├── data/                      # Tabular input and intermediate datasets
├── rasters/                   # Environmental raster datasets (download separately)
├── config.R                   # Loads required packages and project settings
├── plotting_functions.R       # Custom plotting functions
├── 1.gbif_data_download.R     # Downloads mosquito occurrence records from GBIF
├── 2.occ_bg_data_cleaning.R   # Cleans GBIF downloads
├── 3.points_envt_join.R       # Joins occurrence and background points with environmental data from Google Earth Engine
├── 4.create_CV_folds.R        # Assigns occurrence and background points to spatial cross-validation folds and inspects them
├── 5.fit_XGBoost.R            # Fits XGBoost model for a single specification (used for development and troubleshooting)
├── 6.plot_model_outputs.R     # Plots outputs from a fitted model (used for development and troubleshooting)
├── 7.model_comparison.R       # Compares models fit separately (used for development and troubleshooting)
├── 8.parallel_model_fitting.R # Fits multiple model specifications in parallel (used for main analyses)
├── 9.multi-model_plotting.R   # Compares multiple models fit simultaneously (used for main analyses)
├── model1                     # Fits for model1
├── model2                     # Fits for model2
├── model3                     # Fits for model3
├── model4                     # Fits for model4
├── model5                     # Fits for model5
├── model6                     # Fits for model6
├── model7                     # Fits for model7
├── model8                     # Fits for model8
└── README.md
```

All analysis scripts are located in the repository's top-level directory. The file `config.R` loads the required R packages and defines project-wide settings, while `plotting_functions.R` contains custom plotting functions used throughout the analysis.

## Analysis scripts

The scripts are intended to be run sequentially.

| Script                       | Description                                                                                                                            |
| ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `1.gbif_data_download.R`     | Downloads mosquito occurrence records from the Global Biodiversity Information Facility (GBIF).                                        |
| `2.occ_bg_data_cleaning.R`   | Cleans GBIF occurrence records and prepares occurrence and background datasets for analysis.                                           |
| `3.points_envt_join.R`       | Joins occurrence and background points with environmental predictor data from Google Earth Engine.                                     |
| `4.create_CV_folds.R`        | Assigns occurrence and background points to spatial cross-validation folds and provides tools for inspecting the resulting partitions. |
| `5.fit_XGBoost.R`            | Fits an XGBoost model for a single model specification. Primarily used for model development and troubleshooting.                      |
| `6.plot_model_outputs.R`     | Generates plots and summaries for a single fitted model. Primarily used for development and troubleshooting.                           |
| `7.model_comparison.R`       | Compares multiple models fitted separately. Primarily used for development and troubleshooting.                                        |
| `8.parallel_model_fitting.R` | Fits multiple model specifications in parallel. This script was used for the primary analyses.                                         |
| `9.multi-model_plotting.R`   | Compares and visualizes the results of multiple models fitted simultaneously. This script was used for the primary analyses.           |

## Data

### Tabular data

Tabular datasets required for the analyses are included in the `data/` directory.

### Environmental raster data

Environmental and model fit raster datasets are not included in this repository because of their size.

They are archived on Zenodo:

https://doi.org/10.5281/zenodo.21630591

After downloading the archive, extract or copy the raster files into a directory named

```text
rasters/
```

located in the top level of this repository.

## Software requirements

The analyses are written in **R**.

Before running any analysis scripts, source

```r
source("config.R")
source("plotting_functions.R")
```

The `config.R` script loads all required package dependencies and establishes the project configuration used throughout the workflow.

## Reproducing the analysis

1. Download or clone this repository.
2. Confirm that the tabular datasets are located in the `data/` directory.
3. Download the environmental raster datasets from Zenodo and place them in the `rasters/` directory.
4. Open the project in R or RStudio.
5. Source `config.R` and `plotting_functions.R`.
6. Run the numbered analysis scripts sequentially.

## Citation

A preprint and peer-reviewed publication describing these analyses will be added here when available.

If you use this code, please cite both the associated publication (once available) and the accompanying environmental raster dataset:

> Zenodo. Environmental raster datasets for *Aedes sierrensis* species distribution modelling. https://doi.org/10.5281/zenodo.21630591

## License

Please add an appropriate open-source license (for example, MIT or GPL-3.0) before making this repository public.
