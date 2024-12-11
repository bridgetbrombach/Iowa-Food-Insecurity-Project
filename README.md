# Project Documentation

## Overview

This project involves predicting food insecurity proportions and counts for senior populations by PUMA (Public Use Microdata Area) using various machine learning models, including ridge regression, lasso regression, and random forest. The ridge regression model was ultimately chosen based on its superior performance evaluated through ROC curves and AUC. The project also includes the visualization of predictions using both leaflet and ggplot choropleth maps.

## Features and Objectives

- **Data Preparation**: Prepare and clean the input datasets.
- **Machine Learning Models**: Fit and evaluate ridge regression, lasso regression, and random forest models for predicting food insecurity.
- **Visualization**: Create interactive maps and static choropleth maps of food insecurity proportions and counts.
- **Dynamic Customizations**: Add features like titles and captions to visualizations.
- **Data Aggregation**: Aggregate additional dataset, ACS (American Community Survey) data, for predictions.

## Setup Instructions

### Prerequisites

Ensure the following R libraries are installed:

```r
install.packages(c('tidyverse', 'pROC', 'glmnet', 'lubridate',
                   'randomForest', 'ggplot2', 'RColorBrewer',
                   'rpart', 'rpart.plot', 'logistf', 'leaflet',
                   'htmlwidgets'))
```

### Data

Food insecurity data should be provided in CSV format. Additional demographic data for mapping, such as senior population counts, should also be available in CSV format.

Additionally, ACS (American Community Survey) data should be available in sasbdat format to supplement the predictions.

### File Structure

```
project-folder/
|-- data/
|   |-- cps_00007.csv
|   |-- cps_data.csv
|   |-- total_iowa_seniors_by_puma.csv
|   |-- spm_pu_2022.sas7bdat
|   |-- acs_pred_FSSTATUS.csv
|   |-- acs_pred_FSWROUTY.csv
|   |-- acs_pred_FSSTATUS_extreme.csv
|   |-- acs_pred_FSWROUTY_extreme.csv
|-- scripts/
|   |-- clean_acs.R
|   |-- clean_cps.R
|   |-- model_y1.R
|   |-- model_y2.R
|   |-- FSSTATUS_extreme_model.R
|   |-- FSSTATUS_Count_Map.R
|   |-- FSSTATUS_Count_Map.R
|   |-- FSSTATUS_Proportion_Map.R
|   |-- FSSTATUS_extreme_count_Map.R
|   |-- FSSTATUS_extreme_proportion_Map.R
|   |-- FSWROUTY_extreme_model.R
|   |-- FSWROUTY_Count_Map.R
|   |-- FSWROUTY_Count_Map.R
|   |-- FSWROUTY_Proportion_Map.R
|   |-- FSWROUTY_extreme_count_Map.R
|   |-- FSWROUTY_extreme_proportion_Map.R
|   |-- Leaflet_FSWROUTY_Map_Proportions.R
|   |-- Leaflet_FSWROUTY_Map_Count.R
|-- outputs/
|   |-- FSSTATUS_Count_Map.pdf
|   |-- FSSTATUS_Proportion_Map.pdf
|   |-- FSSTATUS_extreme_Count_Map.pdf
|   |-- FSSTATUS_extreme_Proportion_Map.pdf
|-- README.md
```

## Implementation

### Data Preparation

1. **Food Insecurity Data**: Load the food insecurity data using `read.csv`.
2. **ACS Data**: Load ACS data using `read_sas`. The ACS data will be used as additional demographic features for predicting food insecurity.
3. **Data Cleaning**: Use `dplyr` for data cleaning and selecting relevant columns from both datasets.
4. **Missing Values**: Remove missing values in both datasets.
5. **Data Merging**: Merge the food insecurity and ACS datasets based on `PUMA`.
6. **Data Splitting**: Split the combined dataset into training and testing subsets.

### Aggregating ACS Data



To aggregate the ACS data for prediction, the following steps can be followed:

1. **Group by PUMA**: The ACS data can be grouped by PUMA to align with the food insecurity data..
4. **Feature Engineering**: Make sure everything that is present in CPS is also in ACS.

### Model Fitting

#### Ridge Regression

- Use `glmnet` for model fitting with cross-validation.
- Extract the optimal lambda value and fit the final model.
- Evaluate model performance using the `pROC` package.

#### Other Models

- **Lasso Regression**: Fit and evaluate using `glmnet` with `alpha = 1`.
- **Random Forest**: Fit and evaluate using the `randomForest` package.

The ridge regression model was selected for final predictions based on the highest AUC and best ROC curve performance.

### Prediction Aggregation

1. Apply the trained model to ACS Data.
2. Group predictions by PUMA.
3. Summarize results using weighted means to calculate:
   - Proportion of food insecurity.
4. Find counts by multiplying count_seniors from "total_iowa_seniors_by_puma.csv" by predicted proprotion

### Visualization

#### Leaflet Maps
Use "tigris"" package to get PUMA shapefule

```r
iowa_pumas <- tigris::pumas(state = "IA", year = 2022, class = "sf")
```

Use `leaflet` to create interactive maps for visualizing:
- Predicted food insecurity proportions by PUMA.
- Predicted counts of food-insecure seniors by PUMA.

#### GGPlot Choropleth Maps

Use `ggplot2` for static choropleth maps of:
- Predicted food insecurity proportions.
- Predicted food-insecure senior counts.

#### Adding Titles and Captions

- Use `onRender` from `htmlwidgets` to add a centered title to the leaflet map.
- Use `ggtitle` for adding titles to ggplot visualizations.

**Example Code Snippet**:

```r
# Add a title to a leaflet map
map <- leaflet() %>%
  addTiles() %>%
  onRender('
    var title = document.createElement("div");
    title.innerHTML = "<h2 style=\\"text-align:center; font-size: 24px; color: black;\\">Food Insecurity by PUMA</h2>";
    title.style.position = "absolute";
    title.style.top = "10px";
    title.style.left = "50%";
    title.style.transform = "translateX(-50%)";
    title.style.zIndex = "1000";
    document.body.appendChild(title);
  ')

# Create a choropleth map with ggplot
choropleth <- ggplot(data = food_insecurity_data, aes(fill = proportion_of_population, geometry = geometry)) +
  geom_sf() +
  scale_fill_viridis_c() +
  ggtitle("Predicted Food Insecurity Proportion by PUMA") +
  theme_minimal()
```

## Data Variables

Key variables include:

-PUMA
-FSSTATUS
-FSSTATUS_extreme
-FSWROUTY
-FSWROUTY_extreme


For a full list and detailed descriptions of all variables, see `cps_variable_definitions.md`.

## Outputs

- Prediction results aggregated at the PUMA level.
- CSV file containing predicted proportions of food-insecure populations.
- **Visualizations**:
  - Interactive leaflet maps of food insecurity proportions and counts.
  - Static ggplot choropleth maps of food insecurity proportions and counts.


## References

- Dr. Lendie Follett
- https://cps.ipums.org/cps-action/variables/search
- Chat GPT, Choropleth Maps, Leaflet Maps, README formatting

---

**Author**: Bridget Brombach (Project Members: Amelia Burnell, Grace Bero, Ahana Yelagar)
**Date**: 12/11/24