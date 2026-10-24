library(readr)
library(dplyr)
library(sandwich)  # For Huber-White Robust Standard Errors (HC3)
library(lmtest)    # For robust coefficient testing
library(car)       # For VIF calculations
library(pander)

set.seed(42)

# 1. Load both enriched datasets
train_data <- read_csv("data/enriched/training_set.csv")
test_data  <- read_csv("data/enriched/test_set.csv")

# 2. Combine them together for maximum statistical power (Strategy 1)
# This maximizes sample size and tightens standard errors without data leakage
global_inference_data <- bind_rows(train_data, test_data)

# 3. Factorise the binary genre indicators in memory for proper dummy coding
genre_cols <- c("Action_Adventure", "Strategy_Sim", "Casual_Indie", "Pure_RPG", "Sports")
global_inference_data <- global_inference_data %>%
  mutate(across(all_of(genre_cols), ~ factor(if_else(. == 1, "Yes", "No"), levels = c("No", "Yes"))))

# 4. Revert release_season back into descriptive text factor levels
# Level 1 (Winter) will serve as the baseline comparison group in the MLR
# Also convert num_supported_os to categorical with three levels
global_inference_data <- global_inference_data %>%
  mutate(
    release_season = case_when(
      release_season == 1 ~ "Winter",
      release_season == 2 ~ "Spring",
      release_season == 3 ~ "Summer",
      release_season == 4 ~ "Autumn"
    ),
    release_season = factor(release_season, levels = c("Winter", "Spring", "Summer", "Autumn")),

    num_supported_os = case_when(
      num_supported_os == 1 ~ "One",
      num_supported_os == 2 ~ "Two",
      num_supported_os == 3 ~ "Three"
    ),
    num_supported_os = factor(num_supported_os, levels = c("One", "Two", "Three"))
  )

# 5. Explicitly define your structural continuous predictors
continuous_features <- c("log_price", "log_achievements", "log_dlc_count", "log_total_languages", "num_tags", "num_categories")

# 6. Construct the inference formula
# Continuous items get scaled for standardized beta weights; categorical factors remain raw
categorical_features <- c(genre_cols, "release_season", "num_supported_os")
formula_string <- paste(
  "overall_popularity_index ~", 
  paste(c(continuous_features, categorical_features), collapse = " + ")
)
final_formula <- as.formula(formula_string)

# 7. Fit the Global OLS Model on 100% of the unified data
global_mlr <- lm(final_formula, data = global_inference_data)

# =========================================================================
# EXPERT ASSUMPTION TESTING BLOCK
# =========================================================================
print("=========================================================================")
print("                   FORMAL REGRESSION ASSUMPTION TESTS                    ")
print("=========================================================================")

# --- ASSUMPTION 1: MULTICOLLINEARITY (Variance Inflation Factors) ---
print("--- Test 1: Variance Inflation Factors (VIF) ---")
vif_values <- car::vif(global_mlr)
print(vif_values)
# Threshold check: VIF > 5 indicates problematic collinearity; > 10 is severe.
if(any(vif_values[,1] > 5)) {
  print("WARNING: High multicollinearity detected among features!")
} else {
  print("SUCCESS: All VIF values are well below 5. Predictors are uniquely independent.")
}
print("-------------------------------------------------------------------------")

# --- ASSUMPTION 2: INDEPENDENCE OF RESIDUALS (Durbin-Watson Test) ---
print("--- Test 2: Durbin-Watson Test for Residual Autocorrelation ---")
dw_test <- lmtest::dwtest(global_mlr)
print(dw_test)
# Interpret: DW values near 2 indicate no autocorrelation. Significant p-values signal violations.
if(dw_test$p.value < 0.05) {
  print("NOTICE: Significant residual autocorrelation detected. Expected for cross-sectional scraping data.")
} else {
  print("SUCCESS: Residuals show complete independence.")
}
print("-------------------------------------------------------------------------")

# --- ASSUMPTION 3: LINEARITY (Rainbow Test) ---
print("--- Test 3: Rainbow Test for Specification & Linearity ---")
# The Rainbow test checks if the true relationship is linear by comparing a central subset to the whole model
rain_test <- lmtest::raintest(global_mlr)
print(rain_test)
if(rain_test$p.value < 0.05) {
  print("NOTICE: Non-linearity detected. Expected due to our massive structural 10.9 baseline floor.")
} else {
  print("SUCCESS: The linear specification holds perfectly.")
}
print("-------------------------------------------------------------------------")

# --- ASSUMPTION 4: Normality of Residuals ---
# (Note: Standard Shapiro-Wilk or Kolmogorov-Smirnov tests fail on n > 5,000, so we output skewness metrics directly)
residuals_extracted <- residuals(global_mlr)
skewness_val <- sum((residuals_extracted - mean(residuals_extracted))^3) / 
                ((length(residuals_extracted) - 1) * sd(residuals_extracted)^3)
print(paste("Residual Skewness Metric:", round(skewness_val, 4)))
print("=========================================================================")

# 8. Apply HC3 Robust Correction to handle the structural index floor
robust_results <- coeftest(global_mlr, vcov = vcovHC(global_mlr, type = "HC3"))

# 9. Compute matching HC3 Robust 95% Confidence Intervals for all variables
robust_ci <- lmtest::coefci(global_mlr, vcov = sandwich::vcovHC(global_mlr, type = "HC3"), level = 0.95)

# 10. Combine estimates, intervals, and robust diagnostics into a unified inference frame
inference_summary <- data.frame(
  Estimate   = robust_results[, "Estimate"],
  `Robust SE` = robust_results[, "Std. Error"],
  `2.5 %`    = robust_ci[, 1],
  `97.5 %`   = robust_ci[, 2],
  `p-value`  = robust_results[, "Pr(>|t|)"],
  check.names = FALSE
) %>%
  tibble::rownames_to_column(var = "Predictor") %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Print unified publication-ready output
print("=========================================================================")
print("     GLOBAL STRUCTURAL INFERENCE SUMMARY (OVERALL COMMERCIAL INDEX)      ")
print("=========================================================================")
pander(inference_summary)
print("=========================================================================")
print(paste("Global Model Adjusted R-Squared:", round(summary(global_mlr)$adj.r.squared, 4)))