library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(lubridate)
library(readr)

############## load raw data ###############################
raw_file <- "data/raw/games_march2025_cleaned.csv"
steam_raw <- read_csv(raw_file)

############### Scientific question ###########################
# Among the Steam games in the March 2025 dataset,
# which observable characteristics — genre, price, years
# since release, platform availability, language coverage,
# achievements and DLC — are associated with greater
# popularity?
##############################################################

######################################################################
################---------------- basic structure checks --------------
######################################################################

n_rows <- nrow(steam_raw)
n_columns <- ncol(steam_raw)
cat("rows:", n_rows, " cols:", n_columns, "\n")

tibble(
  column = names(steam_raw),
  type = sapply(steam_raw, class)
)

duplicated_col_names <- anyDuplicated(names(steam_raw))
print(duplicated_col_names)

##################### col selection #########################
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
  "tags",
  "categories",            
  "estimated_owners",     
  "num_reviews_total",
  "average_playtime_forever",
  "average_playtime_2weeks",
  "median_playtime_forever",
  "median_playtime_2weeks",
  "peak_ccu"   
)

add_cols <- c("positive", "negative")
key_cols <- c(core_cols, add_cols)

steam <- steam_raw[key_cols]

####### 1. Duplicate Column Clean-up ###################################
steam <- steam[, !duplicated(colnames(steam))]


####### 2. Date Processing & Season Extraction (Derivation) ############
sum(is.na(steam[["release_date"]])) 

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
  ) %>%
  select(-release_month)


####### 3. Convert OS columns into counts of each row ###########################

os.columns <- c("windows", "mac", "linux")

steam <- steam %>%
  mutate(
    # Calculate the row sums across the specified OS columns
    num_supported_os = rowSums(across(all_of(os.columns))),
    
    # Convert the column to a factor 
    # but here we use 'num_supported_os' to keep it consistent.
    num_supported_os = as.factor(num_supported_os)
  )

####### 4. Raw Numerical Handling (No Transforms) ######################
# Standardising structural NAs to zero for safe analytical operations
steam[["price"]][is.na(steam[["price"]])] <- 0 
steam[["achievements"]][is.na(steam[["achievements"]])] <- 0


####### 5. Language, Categories and Tags Column Parsing & Counts ###############
parse_languages <- function(lang_col) {
  # Helper to parse string representation of list into a vector
  str_split(str_remove_all(lang_col, "\\[|\\]|'"), ", ")
}

steam <- steam %>%
  mutate(
    supported_languages = if_else(is.na(supported_languages) | supported_languages == "", "[]", supported_languages),
    full_audio_languages = if_else(is.na(full_audio_languages) | full_audio_languages == "", "[]", full_audio_languages),
    
    # Parse both columns into list-columns of vectors
    parsed_supported = parse_languages(supported_languages),
    parsed_audio     = parse_languages(full_audio_languages),
    
    # Combine vectors row-by-row, filter out empty strings, and count unique items
    total_languages = purrr::map2_int(parsed_supported, parsed_audio, function(lang, audio) {
      combined <- c(lang, audio)
      combined <- combined[combined != "" & !is.na(combined)]
      length(unique(combined))
    })
  ) %>%
  # Clean up temporary helper columns
  select(-parsed_supported, -parsed_audio)

steam <- steam %>%
  mutate(
    num_tags = if_else(
      tags == "{}" | tags == "", 
      0, 
      str_count(tags, ",") + 1
    )
  ) %>%
  # Clean up temporary helper columns
  select(-tags)

steam <- steam %>%
  mutate(
    num_categories = if_else(
      categories == "[]" | categories == "", 
      0, 
      str_count(categories, ",") + 1
    )
  ) %>%
  # Clean up temporary helper columns
  select(-categories)

####### 6. Genres Multi-Label (Filtered Core Genres Binary) ############
steam[["genres"]] <- if_else(
  is.na(steam[["genres"]]) | steam[["genres"]] == "" | steam[["genres"]] == "[]", 
  "['Other']", 
  steam[["genres"]]
)

parsed_genres_list <- str_split(str_remove_all(steam[["genres"]], "\\[|\\]|'"), ",\\s*")

core_genres <- c("Action", "Adventure", "Strategy", "Indie", "RPG", "Casual", "Simulation", "Sports")

genre_matrix <- map_dfc(core_genres, function(g) {
  tibble(!!paste0("genre_", g) := as.integer(map_lgl(parsed_genres_list, ~ g %in% .x)))
})

steam <- bind_cols(steam, genre_matrix)


####### 7. Number of Owners & Target Metrics (Derivation) ##############

owner_tiers <- c(
  "0 - 0", "0 - 20000", "20000 - 50000", "50000 - 100000", "100000 - 200000",
  "200000 - 500000", "500000 - 1000000", "1000000 - 2000000",
  "2000000 - 5000000", "5000000 - 10000000", "10000000 - 20000000",
  "20000000 - 50000000", "50000000 - 100000000", "100000000 - 200000000",
  "200000000 - 500000000"
)

steam[["owners_tier"]] <- factor(steam[["estimated_owners"]], levels = owner_tiers, ordered = TRUE)

owners_split <- str_split(steam[["estimated_owners"]], " - ")
owners_lower <- as.numeric(sapply(owners_split, `[`, 1))
owners_upper <- as.numeric(sapply(owners_split, `[`, 2))
steam[["owners_mid"]] <- (owners_lower + owners_upper) / 2


####### 8. Dropping Structural Parent Columns & Clean Export ###########
# Retaining dlc_count, price, and achievements in raw form for EDA evaluation
drop_columns <- c("release_date", "genres", "supported_languages", "full_audio_languages", "n_languages", "n_audio_languages", "windows", "linux", "mac")
steam <- steam %>% select(-any_of(drop_columns))

write.csv(
  steam,
  "data/preprocessed/final_predictors.csv",
  row.names = FALSE
)
