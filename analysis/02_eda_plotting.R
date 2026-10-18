library(ggplot2)
library(dplyr)
library(pander)
library(psych)
library(flextable)
library(tidyverse)

# Read csv file 

df_path <- "data/preprocessed/final_predictors.csv"

df <- read.csv(df_path)

# Create a summary table

df_summary <- summary(df)
pander(df_summary)
# A count table of NA values for each column

na_table <- stack(colSums(is.na(df)))
names(na_table) <- c("NA_Count", "Column")
pander(na_table)

# Plot genre counts to show occurances across the steam game population

df_genres = df %>% select(c("genre_Action", "genre_Adventure", "genre_Strategy", "genre_Indie", "genre_RPG", "genre_Casual", "genre_Simulation", "genre_Sports"))

df_long <- df_genres %>%
  pivot_longer(
    cols = starts_with("genre_"), 
    names_to = "genre_type", 
    values_to = "present"
  ) %>%
  group_by(genre_type) %>%
  summarize(count = sum(present == 1, na.rm = TRUE))

ggplot(df_long, aes(x = reorder(genre_type, count), y = count, fill = genre_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  labs(
    title = "Count of Occurance for each Genre",
    x = "Genres Columns",
    y = "Total Count"
  ) +
  theme_minimal()

# Conduct a Chi-Square test of Indepedence to see if any of these genres are heaviliy associated with another

genre_colnames <- colnames(df_genres)
n_cols <- length(genre_colnames)

p_matrix <- matrix(NA, nrow = n_cols, ncol = n_cols)
rownames(p_matrix) <- genre_colnames
colnames(p_matrix) <- genre_colnames

for (i in 1:(n_cols - 1)) {
  for (j in (i + 1):n_cols) {
    
    var1 <- genre_colnames[i]
    var2 <- genre_colnames[j]
    
    contingency_table <- table(df_genres[[var1]], df_genres[[var2]])
    
    # Run Chi-square test
    chi_test <- suppressWarnings(chisq.test(contingency_table))
    
    # Assign the p-value symmetrically to both triangles of the matrix
    p_matrix[i, j] <- chi_test$p.value
    p_matrix[j, i] <- chi_test$p.value
  }
}

diag(p_matrix) <- 1.0000

panderOptions('digits', 4)
panderOptions('keep.trailing.zeros', TRUE)

pander(p_matrix, caption = "Chi-Square Test Matrix of P-Values Across Genres")
  cat("No statistically significant pairs found.\n")

# Use a scatterplot matrix across all continous dependent variables to see if multicollinearity occurs and how strong

df_subset <- df %>% select(c("price", "achievements", "dlc_count", "total_languages", "num_tags", "num_categories"))

df_log_subset <- as.data.frame(lapply(df_subset, log1p))

pairs.panels(df_subset, 
             main = "Scatterplot Matrix of Continous Features",
             method = "pearson", # correlation method
             hist.col = "green",  # histogram color
             density = TRUE,     # show density plots
             ellipses = TRUE)    # show correlation ellipses

pairs.panels(df_log_subset, 
             main = "Scatterplot Matrix of Continous Features Log Transformed",
             method = "pearson", # correlation method
             hist.col = "gold",  # histogram color
             density = TRUE,     # show density plots
             ellipses = TRUE)    # show correlation ellipses

# Exploring the picked subset of target variables that will be converted and derived into three seperate popularity indexes: Lifetime Engagement, Recent Velocity and User Sentiment

df_targets <- df %>% select(c("owners_mid", "peak_ccu", "average_playtime_forever", "average_playtime_2weeks", "median_playtime_forever", "median_playtime_2weeks","num_reviews_total", "positive", "negative"))

head(df_targets)

# -------------------------------------------------------------------------
# Group 1: Lifetime Engagement Variables (Log Scale due to extreme skew)
# -------------------------------------------------------------------------
engagement_vars <- c("owners_mid", "peak_ccu")

df_targets %>%
  select(all_of(engagement_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  # Using log10 transformation because Steam metrics span multiple orders of magnitude
  scale_x_continuous(trans = "log1p", labels = scales::comma_format())  +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Lifetime Engagement Variables",
       subtitle = "X-axis is Log10 transformed to handle high skewness",
       x = "Value (Log Scale)", y = "Count")

# -------------------------------------------------------------------------
# Group 2: Recent Velocity & Engagement (Handling Zeros + Log Scale)
# -------------------------------------------------------------------------
playtime_vars <- c("median_playtime_forever", "median_playtime_2weeks")

df_targets %>%
  select(all_of(playtime_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  # log1p (log(x + 1)) handles the absolute 0 values safely without dropping rows
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  scale_x_continuous(trans = "log1p", labels = scales::comma_format()) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Playtime Metrics",
       subtitle = "X-axis uses log1p transformation to accommodate 0-minute values",
       x = "Playtime in Minutes (Log+1 Scale)", y = "Count")

df_targets %>%
  select(all_of(playtime_vars)) %>%
  filter(median_playtime_2weeks != 0, median_playtime_forever != 0) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  # log1p (log(x + 1)) handles the absolute 0 values safely without dropping rows
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  scale_x_continuous(trans = "log1p", labels = scales::comma_format()) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Playtime Metrics (Without Zero Values)",
       subtitle = "X-axis uses log1p transformation to accommodate 0-minute values",
       x = "Playtime in Minutes (Log+1 Scale)", y = "Count")

# -------------------------------------------------------------------------
# Group 3: User Sentiment (Derived Ratios for better EDA value)
# -------------------------------------------------------------------------
# Absolute review counts are identical to engagement. For sentiment, 
# it is much more useful to plot the positive review ratio.
df_targets %>%
  mutate(positive_ratio = positive / (positive + negative)) %>%
  ggplot(aes(x = positive_ratio)) +
  geom_histogram(bins = 20, fill = "forestgreen", color = "white", boundary = 0) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_minimal() +
  labs(title = "Distribution of User Sentiment",
       subtitle = "Percentage of positive reviews out of total reviews",
       x = "Positive Review Ratio", y = "Count")