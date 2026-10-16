library(ggplot2)
library(dplyr)
library(pander)
library(psych)
library(flextable)
library(tidyverse)

# Read csv file 

df_path <- "data/preprocessed/final_predictors2.csv"

df <- read.csv(df_path)

# Create a summary table

df_summary <- summary(df)
pander(df_summary)
# A count table of NA values for each column

na_table <- stack(colSums(is.na(df)))
names(na_table) <- c("NA_Count", "Column")
pander(na_table)

# Plot genre counts to show occurances across the steam game population

df_genres = df %>% select(c("genre_Action", "genre_Adventure", "genre_Strategy", "genre_Indie", "genre_RPG", "genre_Casual", "genre_Simulation", "genre_Sports", "genre_Other"))

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

df_subset <- df %>% select(c("price", "achievements", "dlc_count", "total_languages"))

df_log_subset <- as.data.frame(lapply(df_subset, log1p))

pairs.panels(df_subset, 
             main = "Scatterplot Matrix of Continous Features",
             method = "pearson", # correlation method
             hist.col = "gold",  # histogram color
             density = TRUE,     # show density plots
             ellipses = TRUE)    # show correlation ellipses

pairs.panels(df_log_subset, 
             main = "Scatterplot Matrix of Continous Features Log Transformed",
             method = "pearson", # correlation method
             hist.col = "gold",  # histogram color
             density = TRUE,     # show density plots
             ellipses = TRUE)    # show correlation ellipses

