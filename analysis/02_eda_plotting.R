library(ggplot2)
library(dplyr)
library(pander)
library(psych)
library(flextable)

# Read csv file 

df_path <- "data/preprocessed/final_predictors.csv"

df <- read.csv(df_path)

# Summarise dataframe

numeric_df <- df %>% select_if(is.numeric)

numeric_df <- numeric_df %>% select(-any_of(c("appid", "owners_mid", "no.supported_OS")))

head(numeric_df)

png(filename = "outputs/figures/scatterplot_matrix.png", width = 1200, height = 800, res = 100)

matrix = pairs.panels(numeric_df, method = "pearson", hist.col = "#00AFBB", density = TRUE, ellipses = TRUE)   

dev.off()

# Create a summary table

df_summary <- summary(df)
pander(df_summary)
# A count table of NA values for each column

na_table <- stack(colSums(is.na(df)))
names(na_table) <- c("NA_Count", "Column")
pander(na_table)

# Top Ten games by Peak Concurrent Players:

top_ten <- df %>%
  arrange(desc(peak_ccu)) %>%
  slice_head(n = 10)

top_ten_plot <- ggplot(top_ten, aes(x = reorder(name, peak_ccu), y = peak_ccu)) + 
  geom_col(fill = "steelblue") + 
  coord_flip() + 
  labs(
    title = "Top Ten Games by Peak Concurrent Players",
    x = "Game Name",
    y = "Peak Concurrent Players"
  ) +
  theme_minimal()

ggsave( "outputs/figures/top_ten.png", plot = top_ten_plot)

# Count of each level of number of supported OS

os_table <- df %>%
  count(no.supported_OS, sort = TRUE) %>%
  mutate(percentage = (n/sum(n)) * 100) %>%
  rename(Count = n)
pander(os_table)

# Scatter plot of Price vs Player_Count

price_v_player <- ggplot(df, aes(x = price, y = peak_ccu, color = owners_tier)) +
  geom_point(alpha = 0.3) + # alpha helps see overlapping dots
  scale_y_log10(labels = scales::comma) + # Converts Y-axis to a readable log scale
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter plot of Price vs Peak CCU (Log Scale)",
       x = "Price ($)",
       y = "Peak CCU (Log 10 Scale)") + 
  theme_minimal()

ggsave("outputs/figures/price_vs_peak_ccu.png")

# Scatter plot of DLC Count vs Price

dlc_vs_price <- ggplot(df, aes(x = dlc_count, y = price, color = owners_tier)) +
  geom_point(alpha=0.3) +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10(labels = scales::comma) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter plot of DLC Count vs Price",
       x = "DLC Count (Log-10 Scale)",
       y = "Price ($) (Log-10 Scale)") +
  theme_minimal()

ggsave("outputs/figures/dlc_vs_price.png")
# scatter plot of Positive vs Negative Reviews colour coded by owner_tiers

ggplot(data = df, aes(x = positive, y = negative, color = owners_tier)) +
  geom_point() +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10(labels = scales::comma) +
  geom_smooth(method = "lm") +
  labs(title = "Scatter plot of Positive vs Negative Reviews",
       x = "Positive Reviews (Log-10 Scale)",
       y = "Negative Reviews (Log-10 Scale)") +
  theme_minimal()

# Box plot of each no. of supported OS and the spread of Peak CCU

os_df <- df %>% select(c("no.supported_OS", "owners_mid", "peak_ccu"))
os_df$no.supported_OS <- as.factor(os_df$no.supported_OS)

os_v_owners_boxplot <- ggplot(data = os_df, aes(x = no.supported_OS, y = owners_mid, color = no.supported_OS)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Box plot of each no. of supported OS and the spread of Owners Mid",
       x = "Number of Supported OS",
       y = "Owners Mid (Log-10 Scale)") +
  theme_minimal()

os_v_owners_boxplot

ggsave("outputs/figures/os_v_owners.png", plot = os_v_owners_boxplot)

os_v_ccu_boxplot <- ggplot(data = os_df, aes(x = no.supported_OS, y = peak_ccu, color = no.supported_OS)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Box plot of each no. of supported OS and the spread of Peak CCU",
       x = "Number of Supported OS",
       y = "CCU (Log-10 Scale)") +
  theme_minimal()

os_v_ccu_boxplot

ggsave("outputs/figures/os_v_peakccu.png", plot = os_v_ccu_boxplot)

# Longevity plot: no. of years vs peak_ccu colored by owners tier

years_v_ccu <- ggplot(data = df, aes(x = years_since_release, y = peak_ccu, color = owners_tier)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Longevity plot: no. of years vs peak_ccu colored by owners tier",
       x = "Number of Years Since Release",
       y = "Peak CCU") +
  theme_minimal()

ggsave("outputs/figures/years_v_ccu.png", plot = years_v_ccu)

# Histogram of peak CCU

ccu_hist <- ggplot(data = df, aes(x = peak_ccu)) + 
  geom_histogram(fill = "steelblue", colour = "black") + 
  labs(title = "Game Peak CCU Frequency", x = "Peak CCU (Log-10 Scale)", y = "Game Count") +
  scale_x_log10(labels = scales::comma) + 
  theme_minimal()

ggsave("outputs/figures/ccu_hist.png", plot = ccu_hist)

# Histogram of price  
  
price_hist <- ggplot(data = df, aes(x = price)) + 
  geom_histogram(binwidth = 2, fill = "steelblue", colour = "black") + 
  labs(title = "Game Price Frequency", x = "Price ($)", y = "Game Count") +
  coord_cartesian(xlim = c(0, 250)) +
  theme_minimal()

ggsave("outputs/figures/price_hist.png", plot = price_hist)

# Bar chart of ownership tier 

tier_bar <- ggplot(data = df, aes(x = owners_tier)) + 
  geom_bar(fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Game Count by Ownership Tier", x = "Ownership Tier", y = "Game Count")

ggsave("outputs/figures/tier_bar.png", plot = tier_bar)

# Histogram of positive reviews

pos_hist <- ggplot(data = df, aes(x = positive)) + 
  geom_histogram(fill = "steelblue", colour = "black") + 
  labs(title = "Positive Review Frequency", x = "Number of Positive Reviews (Log-10 Scale)", y = "Game Count") + 
  scale_x_log10(labels = scales::comma) +
  theme_minimal()

ggsave("outputs/figures/pos_hist.png", plot = pos_hist)

# Histogram of negative reviews

neg_hist <- ggplot(data = df, aes(x = negative)) + 
  geom_histogram(fill = "steelblue", colour = "black") + 
  labs(title = "Negative Review Frequency", x = "Number of Negative Reviews (Log-10 Scale)", y = "Game Count") + 
  scale_x_log10(labels = scales::comma) +
  theme_minimal()

ggsave("outputs/figures/neg_hist.png", plot=neg_hist)

review_hist <- ggplot(data = df, aes(x = num_reviews_total)) + 
  geom_histogram(fill = "steelblue", colour = "black") + 
  labs(title = "Total Reviews Frequency", x = "Number of Reviews (Log-10 Scale)", y = "Game Count") + 
  scale_x_log10(labels = scales::comma) +
  theme_minimal()

ggsave("outputs/figures/reviews_hist.png", plot=review_hist)