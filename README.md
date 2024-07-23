# README

## Analysis of Clinical Data with Logistic Regression

### Overview
This analysis aims to explore the factors influencing the probability of brain hemorrhage in newborns using logistic regression models. The dataset consists of six variables: systolic blood pressure (`sbp`), sex (`sex`), pre-eclampsia experience (`tox`), brain hemorrhage experience (`grmhem`), gestational age (`gestage`), and Apgar score at 5 minutes (`apgar5`).

### Data Preparation
1. **Library Imports:**
   - Various libraries are used for data manipulation, visualization, and statistical analysis.

2. **Data Import and Inspection:**
   - Data is loaded from an Excel file and inspected for structure and missing values.

3. **Handling Missing Data:**
   - Missing values are visualized using a missing map.

4. **Data Type Conversion:**
   - Categorical variables are converted to factors for proper analysis.

### Descriptive Statistics and Visualizations
- **Systolic Blood Pressure (sbp):**
  - Descriptive statistics, boxplot, and histogram show the distribution and central tendency.
- **Gender (sex):**
  - Counts, percentages, bar chart, and pie chart visualize the gender distribution.
- **Toxemia (tox):**
  - Counts, percentages, bar chart, and pie chart illustrate the distribution of pre-eclampsia experience.
- **Gestational Age (gestage):**
  - Descriptive statistics, boxplot, and histogram highlight the distribution.
- **Apgar Score at 5 minutes (apgar5):**
  - Counts, percentages, bar chart, and pie chart show the score distribution.
- **Correlation Analysis:**
  - Correlation matrix and plots identify relationships between numerical variables.

### Logistic Regression Analysis
1. **Univariate Logistic Regression:**
   - Each variable's impact on brain hemorrhage is analyzed using individual logistic regression models.

2. **Multivariate Logistic Regression:**
   - A full model including all variables and a reduced model with significant variables (`sbp` and `apgar5`) are developed.
   - Models are compared using ANOVA and Likelihood Ratio Test.

3. **Model Diagnostics:**
   - Multicollinearity is checked using VIF.
   - Stepwise model selection identifies the best fitting model.

### Prediction
- Predictions are made for fake patients based on the reduced multivariate model, providing probabilities of brain hemorrhage.

### Result Tables
- Summary tables of the univariate and multivariate regression results are created and exported.

### File Descriptions
- `LOWBWT.xlsx`: The dataset file.
- `patient_table.png`: Table of predictions for fake patients.
- `mini_data_table.png`: Sample data table.

### Requirements
- R programming environment with the following libraries: `dplyr`, `readr`, `readxl`, `psych`, `ggplot2`, `Amelia`, `corrplot`, `corrgram`, `epiDisplay`, `car`, `gtsummary`, `ggpubr`, `gridExtra`.

### Conclusion
The analysis identifies significant predictors of brain hemorrhage in newborns and develops a predictive model. The provided scripts and explanations can be used to replicate the analysis or adapt it for similar datasets.

