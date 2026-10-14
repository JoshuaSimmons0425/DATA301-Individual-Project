library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)

############## load raw data ###############################
raw_file <- "data/raw/games_march2025_cleaned.csv"

steam_raw <- read_csv(raw_file)

############### Scientific question ###########################
# Among the Steam games in the March 2025 dataset,
# which observable characteristics — genre, price, years
# since release, platform availability, language coverage,
# achievements and DLC — are associated with greater
# popularity, measured primarily by estimated owner tier
# and corroborated by total review count?
##############################################################

######################################################################################
################---------------- basic structure checks ------------------------------
######################################################################################

# dimensions
n_rows <- nrow(steam_raw)
n_columns <- ncol(steam_raw)
cat("rows:", n_rows, " cols:", n_columns, "\n")

tibble(
  column = names(steam_raw),
  type = sapply(steam_raw, class)
)

# duplicate cols names
duplicated_col_names <- anyDuplicated(names(steam_raw))
print(duplicated_col_names)

##################### col selection #########################
# team, please correct me if more/less cols relevant

# cols for our scientific question
core_cols <- c(   
  "release_date",         
  "genres",                     
  "price",            
  "windows",   
  "mac",                     
  "linux",           
  "supported_languages",
  "full_audio_languages",  
  "achievements",         
  "dlc_count",            
  "estimated_owners",     
  "num_reviews_total",
  "average_playtime_forever",
  "average_playtime_2weeks",
  "median_playtime_forever",
  "median_playtime_2weeks",
  "peak_ccu"   
)

# additional cols
add_cols <- c("positive", "negative")
key_cols <- c(core_cols, add_cols)

# new df 
steam <- steam_raw[key_cols]
tibble(
  column = names(steam),
  type = sapply(steam, class)
)
################################################################

####### 1. Duplicate Column Clean-up ###################################
# Remove duplicate column names if they exist from the initial data pull
steam <- steam[, !duplicated(colnames(steam))]


####### 2. Date Processing & Season Extraction #########################
# Check for missing dates
sum(is.na(steam$release_date)) 

ref_date <- as.Date("2025-03-31") 

steam <- steam %>%
  mutate(
    release_month = month(release_date),
    release_season = case_when(
      is.na(release_date) ~ "Unknown",
      release_month %in% c(12, 1, 2)  ~ "Winter",
      release_month %in% c(3, 4, 5)   ~ "Spring",
      release_month %in% c(6, 7, 8)   ~ "Summer",
      release_month %in% c(9, 10, 11) ~ "Autumn"
    ),
    release_season = factor(release_season, levels = c("Spring", "Summer", "Autumn", "Winter", "Unknown")),
    years_since_release = as.numeric(ref_date - release_date) / 365.25
  ) %>%
  select(-release_month)

# Impute missing years_since_release with the median value
if (any(is.na(steam$years_since_release))) {
  median_years <- median(steam$years_since_release, na.rm = TRUE)
  steam$years_since_release[is.na(steam$years_since_release)] <- median_years
}

####### 3. DLC Count to Binary (has_dlc) ###############################
steam$dlc_count[is.na(steam$dlc_count)] <- 0
steam <- steam %>%
  mutate(has_dlc = if_else(dlc_count > 0, 1, 0))


####### 4. Log Transformations (Price & Achievements) ##################
steam$price[is.na(steam$price)] <- 0 
steam$achievements[is.na(steam$achievements)] <- 0

steam <- steam %>%
  mutate(
    log_price = log1p(price),
    log_achievements = log1p(achievements)
  )


####### 5. Language Column Parsing & Counts ############################
count_languages <- function(lang_col) {
  parsed <- str_split(str_remove_all(lang_col, "\\|\\]|'"), ", ")
  lengths(parsed) * (lang_col != "[]" & lang_col != "" & !is.na(lang_col))
}

steam <- steam %>%
  mutate(
    supported_languages = if_else(is.na(supported_languages) | supported_languages == "", "[]", supported_languages),
    full_audio_languages = if_else(is.na(full_audio_languages) | full_audio_languages == "", "[]", full_audio_languages),
    
    n_languages = count_languages(supported_languages),
    n_audio_languages = count_languages(full_audio_languages),
    total_languages = n_languages + n_audio_languages
  )


####### 6. Genres Multi-Label (Filtered Core Genres) ##################
# Handle empty or missing values safely first
steam$genres <- if_else(is.na(steam$genres) | steam$genres == "" | steam$genres == "[]", "['Other']", steam$genres)

# Clean and split the genre strings into vector lists per row
parsed_genres_list <- str_split(str_remove_all(steam$genres, "\\[|\\]|'"), ",\\s*")

# Define your 8 specific core genres
core_genres <- c("Action", "Adventure", "Strategy", "Indie", "RPG", "Casual", "Simulation", "Sports")

# Create binary columns for the 8 core genres using vectorised row-wise checks
genre_matrix <- map_dfc(core_genres, function(g) {
  tibble(!!paste0("genre_", g) := as.integer(map_lgl(parsed_genres_list, ~ g %in% .x)))
})

# Add an 'Other' column for games that do not contain any of the 8 core genres
genre_matrix <- genre_matrix %>%
  mutate(genre_Other = as.integer(rowSums(.) == 0))

# Bind the clean core genre columns back to the primary dataframe
steam <- bind_cols(steam, genre_matrix)


####### 7. Number of Owners & Target Metrics ###########################
steam$estimated_owners[steam$estimated_owners == "0 - 0" | steam$estimated_owners == ""] <- NA

owner_tiers <- c(
  "0 - 20000", "20000 - 50000", "50000 - 100000", "100000 - 200000",
  "200000 - 500000", "500000 - 1000000", "1000000 - 2000000",
  "2000000 - 5000000", "5000000 - 10000000", "10000000 - 20000000",
  "20000000 - 50000000", "50000000 - 100000000", "100000000 - 200000000",
  "200000000 - 500000000"
)

steam$owners_tier <- factor(steam$estimated_owners, levels = owner_tiers, ordered = TRUE)

# Parse mid-points for evaluation scripts
owners_split <- str_split(steam$estimated_owners, " - ")
owners_lower <- as.numeric(sapply(owners_split, `[`, 1))
owners_upper <- as.numeric(sapply(owners_split, `[`, 2))
steam$owners_mid <- (owners_lower + owners_upper) / 2


####### 8. Data Export #################################################
# Auto-compile variable names for correlation screening (excluding non-numeric/raw items)
genre_cols <- paste0("genre_", unique_genres)
final_correlation_vars <- c(
  "years_since_release", "price", "log_price", "windows", "mac", "linux",
  "achievements", "log_achievements", "has_dlc", "total_languages", 
  "owners_mid", "num_reviews_total", genre_cols
)

# Remove no longer necessary columns for upcoming analysis
drop_columns <- c("release_date", "genres", "supported_languages", "full_audio_languages", "n_languages", "n_audio_langauges")
steam <- steam %>% select(-any_of(drop_columns))

# Export complete configuration output
write.csv(
  steam,
  "data/preprocessed/final_predictors2.csv",
  row.names = FALSE
)