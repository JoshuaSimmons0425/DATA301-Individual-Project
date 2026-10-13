library(tidyverse)

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
################################ basic structure checks ##############################
######################################################################################

# dimensions
n_rows <- nrow(steam_raw)
n_columns <- ncol(steam_raw)
cat("rows:", n_rows, " cols:", n_columns, "\n")

tibble(column = names(steam_raw),
  type = sapply(steam_raw, class)
)

# duplicate cols names
duplicated_col_names <- anyDuplicated(names(steam_raw))
print(duplicated_col_names)

##################### col selection #########################
#team, please correct me if more/less cols relevant

# cols for our scientific question
core_cols <- c(
  "appid",                
  "name",                 
  "release_date",         
  "genres",               
  "price",                
  "windows",              # game available on OSs
  "mac",                  
  "linux",                
  "supported_languages",  
  "achievements",         
  "dlc_count",            
  "estimated_owners",     
  "num_reviews_total",
  "peak_ccu"    
)

# additional cols
add_cols <- c("full_audio_languages",  "positive","negative")
key_cols <- c(core_cols, add_cols)

# new df 
steam <- steam_raw[key_cols]
tibble(column = names(steam),
       type = sapply(steam, class)
)
################################################################

####### date check & calculate years since release #############
max(steam$release_date)
sum(is.na(steam$release_date)) # missing dates


#data is from march, so 31st used as as a reference date
ref_date <- as.Date("2025-03-31") 

# calculate years since release
steam$years_since_release <-
  as.numeric(ref_date - steam$release_date) / 365.25

summary(steam$years_since_release)
################################################################

####### duplicate check ########################################
sum(duplicated(steam))
sum(duplicated(steam$appid)) # duplicate appid - should be 0, appid = unique key

######################################################################################
######################## basic structure checks COMPLETE #############################
######################################################################################

######################################################################################
################################ data quality checks ##################################
######################################################################################

################### missing values ##############################
colSums(is.na(steam))

text_cols <- c("name", "genres", "supported_languages",
               "estimated_owners", "full_audio_languages")
colSums(steam[text_cols] == "", na.rm = TRUE)

#################### zero values #################################
numeric_cols <- c("price", "achievements", "dlc_count",
                  "num_reviews_total", "positive", "negative")
colSums(steam[numeric_cols] == 0, na.rm = TRUE)

# 75388 games have no DLC 
range(steam$dlc_count)
table(steam$dlc_count, useNA = "ifany")

steam %>%
  filter(dlc_count == 0) %>%
  select(name, dlc_count) %>%
  slice_sample(n = 10)
# seems to be valid 0 - no downloadable content, not a missing data

#concurrent players
summary(steam$peak_ccu)
sum(is.na(steam$peak_ccu))
sum(steam$peak_ccu == 0, na.rm = TRUE)
sum(steam$peak_ccu < 0, na.rm = TRUE)

steam %>%
  filter(peak_ccu == 0) %>%
  select(name, peak_ccu, num_reviews_total, positive, negative) %>%
  slice_sample(n = 10)

70698 / nrow(steam) * 100 #around 80% missing/invalid (peak_ccu = 0)


summary(steam[numeric_cols])



#################### sorting out negative values ###################
colSums(steam[numeric_cols] < 0, na.rm = TRUE)
# only num_reviews_total has negative values
#and the only neg value is -1
unique(steam$num_reviews_total[steam$num_reviews_total < 0])
sum(steam$num_reviews_total == -1)

# it does seem fair to assume those are missing values
steam$num_reviews_total[steam$num_reviews_total == -1] <- NA
sum(is.na(steam$num_reviews_total))

# double check if any other negatives left
sum(steam$num_reviews_total < 0, na.rm = TRUE) # 0 left

# conclusion/note for the team: 36419 missing values in num_reviews_total

# check if positive/negative are available for these rows
missing_reviews <- steam[is.na(steam$num_reviews_total), c("positive", "negative")]
sum(missing_reviews$positive > 0 | missing_reviews$negative > 0)

summary(steam[numeric_cols])
# there are potential high outliers in total review counts, positive and negative.. although it may represent genuinely very popular games
# will look into log transform or similar later

#???????????????????????????????????????????????????????????????????????????????????????
################UP to team to decide whether we go with the removal, or keep all the rows
#???????????????????????????????????????????????????????????????????????????????????????

# removing rows that don't have neither of three: positive, negative and num_review_total
no_review_data_at_all <- is.na(steam$num_reviews_total) &
  steam$positive == 0 & steam$negative == 0

sum(no_review_data_at_all) # should be 36419 - 28515 = 7904(number of rows removed)

steam <- steam[!no_review_data_at_all, ]

nrow(steam) # making sure I didn't  remove more than intended
sum(is.na(steam$num_reviews_total)) # remaining NAs: rows with positive/negative but no total

summary(steam[numeric_cols])

######################################################################################
######################## data quality checks COMPLETE ################################
######################################################################################


######################################################################################
######################## text cols preprocessing #####################################
######################################################################################

head(steam$genres, 3)
head(steam$supported_languages, 3)
head(steam$full_audio_languages, 3)

#cleaning genres, sup lang and full lang
parse_genres <- str_split(str_remove_all(steam$genres, "\\[|\\]|'"),", ")
#parse_genres

steam$n_genres <- lengths(parse_genres)

#if empty genre = 0
sum(steam$genres == "[]")
steam$n_genres[steam$genres == "[]"] <- 0

summary(steam$n_genres)

#####################################################################
############################### languages ###########################
#####################################################################

############################### game languages ######################
#check all languages
all_languages <- unlist(str_split(
  str_remove_all(steam$supported_languages, "\\[|\\]|'"),
  ", "
))
all_languages <- unique(all_languages[all_languages != ""])
sort(all_languages)

parse_languages <- str_split(str_remove_all(steam$supported_languages, "\\[|\\]|'"), ", ")

#create new col (Number of all languages that the game supports)
steam$n_languages <- lengths(parse_languages)

#if there is no language listed, assign 0
steam$n_languages[steam$supported_languages == "[]"] <- 0
summary(steam$n_languages)
steam[steam$n_languages == 103, "name"] #949 games has 103 language cover
length(all_languages) #108 languages in total
#additional checks to see whether it makes sense
sample_langs <- steam$supported_languages[steam$n_languages == 103] #different sets of 103 (not the same list of langauges)
length(unique(sample_langs)) 


################################# audio languages ###############################
sum(steam$full_audio_languages == "[]") #55% has no audio language

parse_audio_languages <- str_split(str_remove_all(steam$full_audio_languages, "\\[|\\]|'"), ", ")
steam$n_audio_languages <- lengths(parse_audio_languages)

steam$n_audio_languages[steam$full_audio_languages == "[]"] <- 0

summary(steam$n_audio_languages)


############################# NUmber of owners ##################################
unique(steam$estimated_owners)
sum(steam$estimated_owners == "0 - 0")
head(steam[steam$estimated_owners == "0 - 0", c("name", "price", "num_reviews_total")])
#0-0 seems to be a placeholder for unknown
steam$estimated_owners[steam$estimated_owners == "0 - 0"] <- NA

sum(is.na(steam$estimated_owners))

# using range, instead of creating categories (excluded what seems to be NA 0-0)
owner_tiers <- c(
  "0 - 20000",
  "20000 - 50000",
  "50000 - 100000",
  "100000 - 200000",
  "200000 - 500000",
  "500000 - 1000000",
  "1000000 - 2000000",
  "2000000 - 5000000",
  "5000000 - 10000000",
  "10000000 - 20000000",
  "20000000 - 50000000",
  "50000000 - 100000000",
  "100000000 - 200000000",
  "200000000 - 500000000"
)

steam$owners_tier <- factor(steam$estimated_owners, levels = owner_tiers, ordered = TRUE)

table(steam$owners_tier, useNA = "always")

# for correlation analysis
owners_split <- str_split(steam$estimated_owners, " - ")
owners_lower <- as.numeric(sapply(owners_split, `[`, 1))
owners_upper <- as.numeric(sapply(owners_split, `[`, 2))
steam$owners_mid <- (owners_lower + owners_upper) / 2

summary(steam$owners_mid)

# > summary(steam$owners_mid)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 10000     10000     10000    119392     35000 350000000      6285 
#again right skewed, will use log later for this as well

##concurrent players in comparison with mid_owner
steam %>%
  filter(peak_ccu == 0) %>%
  select(name, peak_ccu, owners_mid) %>%
  slice_sample(n = 10)

#####################################################################
################### text cols preprocessing COMPLETE ################
#####################################################################


#####################################################################
#########data transformation, validation, multicolinearity ##########
#####################################################################

# new column to count no. or supported os
os.columns <- c("windows", "mac", "linux")
steam$no.supported_OS <- rowSums(steam[os.columns])
steam$no.supported_OS <- as.factor(steam$no.supported_OS)

print(length(which(steam$no.supported_OS == 1)))
print(length(which(steam$no.supported_OS == 2)))
print(length(which(steam$no.supported_OS == 3)))

#log transform for 
steam$log_n_reviews <- log1p(steam$num_reviews_total)
summary(steam$log_n_reviews)

steam$log_positive <- log1p(steam$positive)
summary(steam$log_positive)

steam$log_negative <- log1p(steam$negative)
summary(steam$log_negative)

steam$log_owners_mid <- log1p(steam$owners_mid)
summary(steam$log_owners_mid)

steam$log_peak_ccu <- log1p(steam$peak_ccu)
summary(steam$log_peak_ccu)


print(tibble(column = names(steam), type = sapply(steam, function(x) class(x)[1])), n = Inf)
#multicolinearity

correlation_vars <- c(
  "price", "years_since_release", "achievements", "dlc_count",
  "windows", "mac", "linux",
  "n_genres", "n_languages", "n_audio_languages"
)

#pearson correlation for linear relationships
cor_matrix_pearson <- cor(steam[correlation_vars], use = "pairwise.complete.obs", method = "pearson")
round(cor_matrix_pearson, 2)

#spearman correlation 
cor_matrix_spearman <- cor(steam[correlation_vars], use = "pairwise.complete.obs", method = "spearman")
round(cor_matrix_spearman, 2)

#compare two methods, check for non linearity
round(cor_matrix_pearson - cor_matrix_spearman, 2)


# # log transform achievements and dlc_count
# steam$log_achievements <- log1p(steam$achievements)
# summary(steam$log_achievements)

# steam$log_dlc_count <- log1p(steam$dlc_count)
# summary(steam$log_dlc_count)

# check if log changes correlation
correlation_vars_v2 <- c(
  "price", "years_since_release",
  "windows", "mac", "linux",
  "n_genres", "n_languages", "n_audio_languages"
)

cor_matrix_pearson_v2 <- cor(steam[correlation_vars_v2], use = "pairwise.complete.obs", method = "pearson")
round(cor_matrix_pearson_v2, 2)


####### final predictor set (dropping redundant/superseded variables) #########
final_correlation_vars <- c(
  "price", "years_since_release",
  "windows", "mac", "linux",
  "n_genres"
)

cor_matrix_final <- cor(steam[final_correlation_vars], use = "pairwise.complete.obs", method = "pearson")
round(cor_matrix_final, 2)
head(steam[, c("name", "num_reviews_total", "positive", "negative")], 5)

# check number of rows which: num_reviews_total has value, but pos & neg - missing
sum(!is.na(steam$num_reviews_total) & steam$positive == 0 & steam$negative == 0)
#9151

steam[!is.na(steam$num_reviews_total) & steam$positive == 0 & steam$negative == 0, 
      c("name", "num_reviews_total", "positive", "negative")]
# 9151 games (titles like Overwatch 2, Marvel Rivals) show
# positive=0 and negative=0, but still have a real num_reviews_total in the
# hundreds of thousands - so these zeros can't be real, they're likely missing
# data that just wasn't marked as NA. Same pattern as the "0-0" estimated_owners case.

#???????????????????????????????????????????????????????????????????????????????????????
# TEAM DECISION NEEDED:
# 1) should positive/negative be set to NA for these 9151 rows (instead of 0)?
# 2) which review-based measure should we use going forward - log_n_reviews,
#    or log_positive/log_negative
#???????????????????????????????????????????????????????????????????????????????????????

colSums(is.na(steam[final_correlation_vars]))
# conclusions

# platform: mac and linux moderately correlated (0.56) (not strong, so keeping both)

# language variables are highly correlated with each other (multicollinearity) -
# keeping only n_world_languages (weak correlation with popularity .29, but strongest among language variables), 
# dropping n_languages and n_audio_languages

# achievements and dlc_count were skewed, used log to transform

# owners and reviews are strongly correlated with each other (Pearson ~.75 and Spearman ~.60), so they agree as measures of popularity

final_predictors <- steam["name", final_correlation_vars]

write.csv(
  steam,
  "data/preprocessed/final_predictors2.csv",
  row.names = FALSE
)
