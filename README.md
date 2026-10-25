# DATA301-Individual-Project by Joshua Simmons

## Running the Workflow

The analysis must be executed sequentially to ensure that prerequisites, engineered data files, and visualizations are correctly generated before compiling the final document. 

### 1. Data Initialization & Acquisition
Place the source CSV file into your local project tree. For details on downloading or scraping the source data parameters, follow the secondary guide located at `data/raw/README.md`.
* **Required target path:** `data/raw/games_march2025_cleaned.csv`

### 2. Preprocessing & Initial Exploratory Data Analysis
Run the baseline cleaning script to audit raw columns and filter missing artifacts. Then, execute the EDA plotting file to generate distribution charts and scatterplot matrices.
```bash
Rscript analysis/01_preprocessing.R
Rscript analysis/02_eda_plotting.R
```

### 3. Feature Engineering & Target Composite Scaling
Execute the primary feature engineering pipeline. This script cleans review anomalies, constructs the five strategic genre metrics, partitions data (80/20 train/test split), bakes selective log-transformations, and outputs processed datasets to the `data/enriched/` directory.
```bash
Rscript analysis/03_feature_engineering.R
```

### 4. Advanced Machine Learning Interpretability
Open and run the Jupyter Notebook to evaluate feature importance and output the three-paneled MDI, Permutation, and game-theoretic SHAP visualisations.
```bash
# Ensure your local environment tools or Jupyter kernels are active
jupyter nbconvert --to notebook --execute analysis/04_importance_analysis.ipynb
```

### 5. Parametric Regression & Evaluation
Run the final OLS regression module across the combined dataset to calculate parameter coefficients, compute robust standard errors, and verify model uncertainty metrics.
```bash
Rscript analysis/05_overall_index_analysis.R
```

### 6. Final Report Document Compilation
Once all figures are cleanly populated within the `outputs/figures/` path, render the main R Markdown file to generate your polished, submission-ready report.
```text
Open "report_Joshua_Simmons.rmd" in RStudio and click 'Knit', or execute:
rmarkdown::render("report_Joshua_Simmons.rmd")
```
## Codebase Repository Structure

```text
├── data/                  # Storage directory for the source CSV tables
├── outputs/
│   └── figures/           # Generated feature importance and distribution charts
├── analysis/              # Core data processing and modeling source scripts
├── report.Rmd             # Primary R Markdown document for report compilation
└── README.md              # Current pipeline documentation file
```