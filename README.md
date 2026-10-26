# Air Quality and Pollution Trend Analysis Using R

## 📌 Project Overview

Air pollution is a major environmental concern that affects human health and the environment. This project uses R programming to analyse air quality data and identify pollution trends, monthly variations, and relationships between different air pollutants.

The project includes data preprocessing, statistical analysis, and data visualization through an R Shiny dashboard. The visualizations help users understand changes in pollution levels over time and explore patterns in the dataset.

## 🎯 Objectives

- Analyse air quality data using R.
- Study daily and monthly variations in pollutant levels.
- Visualize changes in carbon monoxide (CO) and nitrogen dioxide (NO₂) levels.
- Examine the relationship between carbon monoxide and benzene.
- Identify correlations between different air pollutants.
- Present the analysis through visualizations and summary tables in an interactive dashboard.

## 📂 Project Structure

```text
air-quality-pollution-analysis-r/
│
├── app.R
├── AirQualityUCI.csv
└── cleaned_air_quality_2026-10-26.csv
```

### File Description

- **`app.R`** – Contains the R Shiny application code for the dashboard and visualizations.
- **`AirQualityUCI.csv`** – Original air quality dataset.
- **`cleaned_air_quality_2026-10-26.csv`** – Cleaned dataset prepared for analysis.

## 📊 Dataset

The project uses the **Air Quality dataset** from the UCI Machine Learning Repository.

**Dataset source:** https://archive.ics.uci.edu/dataset/360/air+quality

The dataset contains air quality measurements collected over time, along with environmental variables.

Important variables include:

- **CO(GT):** Reference carbon monoxide concentration.
- **C6H6(GT):** Reference benzene concentration.
- **NOx(GT):** Reference nitrogen oxides concentration.
- **NO2(GT):** Reference nitrogen dioxide concentration.
- **T:** Temperature.
- **RH:** Relative humidity.
- **AH:** Absolute humidity.
- **Date and Time:** Used to study changes over time.

## 🛠️ Technologies Used

- **R** – Data analysis and statistical computing.
- **Shiny** – Interactive web dashboard.
- **ggplot2** – Data visualization.
- **dplyr** – Data manipulation and preprocessing.
- **readr / read.csv()** – Dataset loading, depending on the implementation.

## 📈 Visualizations

The project includes the following visualizations:

1. Daily CO Trend – Shows changes in carbon monoxide levels over time.
2. Daily NO₂ Trend – Displays variations in nitrogen dioxide levels.
3. Monthly CO Distribution – Compares the distribution of CO across months using a boxplot.
4. CO and Benzene Relationship – Examines the relationship between CO and benzene using a scatter plot.
5. Correlation Heatmap – Displays correlations between selected numerical variables.
6. Monthly Average CO – Compares average CO levels across months using a bar chart.
7. Distribution of CO – Shows the frequency distribution of CO values using a histogram.
8. Daily Average CO – Displays daily average CO values using a scatter plot.
9. Daily Summary Table – Presents daily air quality summary information.
10. Monthly Summary Table – Presents monthly statistical summaries.

## ⚙️ Data Processing

The project uses the original dataset and a cleaned dataset for analysis. Data preprocessing prepares the data for visualization and statistical analysis.

The exact cleaning operations depend on the implementation in `app.R`. The cleaned dataset is provided separately to support analysis using the processed data.

## 🚀 How to Run the Project

### Prerequisites

Install the following software:

- R: https://cran.r-project.org/
- RStudio (recommended): https://posit.co/download/rstudio-desktop/

### Steps

1. Download or clone this repository.
2. Open RStudio.
3. Open the project folder.
4. Open `app.R`.
5. Install any required packages that are missing.
6. Run the application using the **Run App** button in RStudio.

Alternatively, run the following command in the R console from the project directory:

```r
shiny::runApp()
```

Make sure the CSV filenames and file paths referenced in `app.R` match the files in the project folder.

## 🔍 Key Findings

The visualizations help examine:

- Changes in CO and NO₂ levels over time.
- Differences in average CO levels between months.
- The distribution and variability of CO measurements.
- Relationships between CO and benzene.
- Correlations between selected air quality variables.

The results describe patterns in the available dataset and do not establish the exact causes of pollution changes.

## 🎓 Project Information

**Project Title:** Air Quality and Pollution Trend Analysis Using R

**Course:** Advanced Data Visualization

**Project Type:** Data Analysis and Visualization

## 📝 Conclusion

This project uses R to analyse and visualize air quality data. Different charts help identify pollution trends, monthly variations, and relationships between pollutants. Overall, the analysis helps us understand air pollution patterns and changes over time, but it does not prove their exact causes.

## 📚 References

1. UCI Machine Learning Repository – Air Quality Dataset: https://archive.ics.uci.edu/dataset/360/air+quality
2. R Project: https://www.r-project.org/
3. Shiny: https://shiny.posit.co/
4. ggplot2: https://ggplot2.tidyverse.org/
