library(dplyr)
library(rsample)
library(recipes)
library(readr)

set.seed(42)

# Load data
df <- read_csv("data/preprocessed/final_predictors.csv")

# -------------------------------------------------------------------------
# Clean User Sentiment Data & Feature Engineering 
# -------------------------------------------------------------------------
df <- df %>%
  mutate(
    # Force all negative placeholder artifacts down to absolute zero
    positive = if_else(positive == -1, 0, positive),
    negative = if_else(negative == -1, 0, negative),
    engineered_total_reviews = positive + negative
  ) %>%
  # Isolate active games first so dead games do not compress the smoothing distribution
  filter(engineered_total_reviews >= 10) %>%
  mutate(
    # Compute the Laplace-smoothed ratio against active, reliable total
    smoothed_sentiment_ratio = (positive) / (engineered_total_reviews),

    # Engineer Developer-Controlled Genre Combinations
    # Action-Adventure Hybrid (Both tags present)
    Action_Adventure = as.integer(genre_Action == 1 & genre_Adventure == 1),

    # Strategy & Simulation Hub (Either tag present, as they share design DNA)
    Strategy_Sim = as.integer(genre_Strategy == 1 | genre_Simulation == 1),

    # Casual Indie Market (Both tags present, isolating small-scale casual titles)
    Casual_Indie = as.integer(genre_Casual == 1 & genre_Indie == 1),

    # Pure RPG (Tagged as RPG, but NOT diluted by mainstream Action)
    Pure_RPG = as.integer(genre_RPG == 1 & genre_Action == 0),

    # Dedicated Sports 
    Sports = as.integer(genre_Sports)
  ) %>%
  # Clean up: Drop old genre columns and the untrustworthy total column
    select(-c("num_reviews_total", "genre_Action", "genre_Adventure", "genre_Strategy", 
            "genre_Simulation", "genre_Casual", "genre_Indie", 
            "genre_Sports", "genre_RPG"))

# -------------------------------------------------------------------------
# Data Partitioning (Split-Safe Strategy)
# -------------------------------------------------------------------------
steam_split <- initial_split(df, prop = 0.8)
train_raw   <- training(steam_split)
test_raw    <- testing(steam_split)

# -------------------------------------------------------------------------
# Index 1 Parameters: Recent Velocity Index
# -------------------------------------------------------------------------
compute_raw_ratio <- function(data) {
  data %>% mutate(
    velocity_ratio = case_when(
      median_playtime_forever == 0 ~ 0,
      median_playtime_2weeks == 0  ~ 0,
      TRUE ~ pmin(median_playtime_2weeks / median_playtime_forever, 1)
    )
  )
}

train_active_vel <- compute_raw_ratio(train_raw) %>% 
  filter(velocity_ratio > 0) %>%
  mutate(log_ratio = log(velocity_ratio))

min_log_vel_train <- min(train_active_vel$log_ratio, na.rm = TRUE)
max_log_vel_train <- max(train_active_vel$log_ratio, na.rm = TRUE)

# -------------------------------------------------------------------------
# Index 2 Parameters: Lifetime Engagement Index
# -------------------------------------------------------------------------
train_active_eng <- train_raw %>%
  filter(owners_mid > 0 & peak_ccu > 0) %>%
  mutate(
    combined_engagement = owners_mid * peak_ccu,
    log_engagement = log(combined_engagement)
  )

min_log_eng_train <- min(train_active_eng$log_engagement, na.rm = TRUE)
max_log_eng_train <- max(train_active_eng$log_engagement, na.rm = TRUE)

# -------------------------------------------------------------------------
# Index 3 Parameters: User Sentiment Index
# -------------------------------------------------------------------------
min_sent_train <- min(train_raw$smoothed_sentiment_ratio, na.rm = TRUE)
max_sent_train <- max(train_raw$smoothed_sentiment_ratio, na.rm = TRUE)

# -------------------------------------------------------------------------
# Transformation Function for All Three Target Indices
# -------------------------------------------------------------------------
transform_target_indices <- function(data, min_vel, max_vel, min_eng, max_eng, min_sent, max_sent) {
  data %>%
    compute_raw_ratio() %>%
    mutate(
      # Transform Index 1: Recent Velocity
      recent_velocity_index = case_when(
        velocity_ratio == 0 ~ 1,
        TRUE ~ {
          log_val <- log(velocity_ratio)
          scaled  <- 1.01 + 98.99 * ((log_val - min_vel) / (max_vel - min_vel))
          pmax(1.01, pmin(scaled, 100))
        }
      ),
      # Transform Index 2: Lifetime Engagement
      lifetime_engagement_index = case_when(
        owners_mid == 0 | peak_ccu == 0 ~ 1,
        TRUE ~ {
          combined <- owners_mid * peak_ccu
          log_val  <- log(combined)
          scaled   <- 1.01 + 98.99 * ((log_val - min_eng) / (max_eng - min_eng))
          pmax(1.01, pmin(scaled, 100))
        }
      ),
      # Transform Index 3: User Sentiment
      user_sentiment_index = {
        if (max_sent == min_sent) {
          50.5
        } else {
          scaled <- 1 + 99 * ((smoothed_sentiment_ratio - min_sent) / (max_sent - min_sent))
          pmax(1, pmin(scaled, 100))
        }
      }
    )
}

# Apply all index transformations safely to our raw splits
train_transformed <- transform_target_indices(train_raw, min_log_vel_train, max_log_vel_train, 
                                               min_log_eng_train, max_log_eng_train, 
                                               min_sent_train, max_sent_train)

test_transformed  <- transform_target_indices(test_raw, min_log_vel_train, max_log_vel_train, 
                                               min_log_eng_train, max_log_eng_train, 
                                               min_sent_train, max_sent_train)

# -------------------------------------------------------------------------
# Explicit Factor Ordering
# -------------------------------------------------------------------------
# This locks the calendar order in place so step_integer() maps them predictably:
# 1 = Winter, 2 = Spring, 3 = Summer, 4 = Autumn
train_transformed <- train_transformed %>%
  mutate(release_season = factor(release_season, levels = c("Winter", "Spring", "Summer", "Autumn")))

test_transformed <- test_transformed %>%
  mutate(release_season = factor(release_season, levels = c("Winter", "Spring", "Summer", "Autumn")))

# -------------------------------------------------------------------------
# Feature Recipe Normalization & Baking (Selective Log Transformations)
# -------------------------------------------------------------------------
columns_to_log <- c("price", "achievements", "dlc_count", "total_languages")

recipe_spec <- recipe(~ ., data = train_transformed) %>%
  step_integer(release_season, zero_based = FALSE) %>%
  # SELECTIVE TRANSFORMATION: Log1p only applied to heavily right-skewed columns.
  # num_tags and num_categories are intentionally bypassed to preserve linear structure.
  step_log(all_of(columns_to_log), offset = 1) %>%
 # Explicit renaming: prepends log_ prefix to the transformed columns
  step_rename_at(all_of(columns_to_log), fn = ~ paste0("log_", .))

trained_recipe <- prep(recipe_spec, training = train_transformed)

train_final <- bake(trained_recipe, new_data = train_transformed)
test_final  <- bake(trained_recipe, new_data = test_transformed)

# -------------------------------------------------------------------------
# Weighted Overall Popularity Index
# -------------------------------------------------------------------------
# Commercial Blockbuster Weight Definition (Weights sum to exactly 1.0)
w_eng  <- 0.50
w_vel  <- 0.30
w_sent <- 0.20

train_final <- train_final %>%
  mutate(
    overall_popularity_index = (lifetime_engagement_index * w_eng) + 
                               (recent_velocity_index * w_vel) + 
                               (user_sentiment_index * w_sent)
  )

test_final <- test_final %>%
  mutate(
    overall_popularity_index = (lifetime_engagement_index * w_eng) + 
                               (recent_velocity_index * w_vel) + 
                               (user_sentiment_index * w_sent)
  )


# -------------------------------------------------------------------------
# Drop previous Dependent Variables used to Engineer the Indexes
# -------------------------------------------------------------------------

cols_to_remove <- c(
  "estimated_owners",
  "average_playtime_2weeks",
  "average_playtime_forever",
  "median_playtime_forever",
  "median_playtime_2weeks",
  "peak_ccu",
  "positive", 
  "negative",
  "owners_tier",
  "owners_mid",
  "engineered_total_reviews",
  "smoothed_sentiment_ratio",
  "velocity_ratio"
)

train_final <- train_final %>% select(-all_of(cols_to_remove))
test_final <- test_final %>% select(-all_of(cols_to_remove))

# -------------------------------------------------------------------------
# Data Output & Verification
# -------------------------------------------------------------------------
write.csv(train_final, "data/enriched/training_set.csv", row.names = FALSE)
write.csv(test_final, "data/enriched/test_set.csv", row.names = FALSE)

print("--- TRAINING SET INDEX HEAD ---")
head(train_final[,c("recent_velocity_index", "lifetime_engagement_index", "user_sentiment_index")], n = 6)

print("--- TEST SET INDEX HEAD ---")
head(test_final[,c("recent_velocity_index", "lifetime_engagement_index", "user_sentiment_index")], n = 6)