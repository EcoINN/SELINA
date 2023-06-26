#' ---
#' Title: "Twitter spatial analysis"
#' Author: EcoINN
#' Date: "September 2022"
#' Output: Database
#' ---


# Parallel computing
library(future)
library(furrr)

# APIs and web scraping
library(academictwitteR)
library(jsonlite)
library(rjson)
library(tidygeocoder)

# Data manipulation
library(tidyverse)
library(readxl)
library(tidytext)
library(textreuse)
library(dplyr)

# Text analysis
library(stringi)
library(stringdist)
library(textstem)
library(SnowballC)

# Visualization
library(ggraph)
library(igraph)
library(leaflet)

# xlsx
library("openxlsx")


# Prepare script ------------------------------
# Set up parallel processing
plan(multisession)

# Loading supporting r-scripts
invisible(sapply(list.files('./src', full.names = T), source))

# Define data and output directories
datdir <- 'data/'
outdir <- 'output/'

# Data needed for Twitter
keys <- "./keys/TwitterKeys.json" 
keywords <- fromJSON(file = "./keys/Keywords.json")
country <- "MT"
start_date <- "2015-01-01T00:00:00Z"
end_date <- "2022-12-31T00:00:00Z"


# Connect to the twitter API ------------------
# Mining tweets
tweets <- get_tweets(keys, keywords, country, start_date, end_date, datdir)
View(tweets)

# Read json files
tweets <- rjson("./data", "^data.*$")


# Data processing ------------------------------
# df preparation
df <- process_tweets(tweets)

# Filter URLs
df <- process_urls(df)

# Tokenisation, lemmatisation and remove short tweets (<3)
lemma <- lemmatize_and_filter(df)


# Filtering data that have coordinates ------------------------------
# Remove rows with NA values in the longitude column
df_coord <- remove_na_rows(lemma, "longitude")

# Filter using the hashtags and the lemmatized_text columns
dfc <- filter_by_word_triples(df_coord, 1, "lemmatized_text")
dfc <- filter_by_word_pairs(dfc, 1, "lemmatized_text")
dfc <- filter_dataframe(dfc)

# Filter df into 4 different dfs based on a set of keywords
# Define the keyword sets
keyword_sets <- list(
  land = c("maltacountryside", "landscape", "bees", "butterflies", "insects", 
           "agriculture", "bird", "soil", "garden", "maltawalks", 
           "wildlifephotography", "hiking", "beachphotography", "trekking", 
           "birdwatching", "birdphotography", "agritourism"),
  sea = c("sea", "beach", "seascape", "sealife", "seagrass", "marinelife", 
          "coast", "coral", "fish", "diving", "scubadiving",
          "underwaterphotography", "fishing", "parasailing"),
  #places = c("malta", "gozo", "comino", "island", "visitmalta", "mymalta"),
  other = c("tourism", "biodiversity", "travelphoto", "explore", "photography",
            "ecology", "winter", "summer", "nature", "naturephotography", 
            "ecoturism", "culture", "history")
)

# Columns to filter on
column_names <- c("hashtags", "lemmatized_text")

# Filter the dataframes
filtered_dfs <- groups_df(dfc, column_names, keyword_sets)

# Access the individual dataframes
land_df <- filtered_dfs[["land"]]
sea_df <- filtered_dfs[["sea"]]
other_df <- filtered_dfs[["other"]]
places_df <- filtered_dfs[["places"]]

# Filter 
land <- filter_by_word_pairs(land_df, 1, "lemmatized_text")
sea <- filter_by_word_triples(sea_df, 1, "lemmatized_text")
other <- filter_by_word_triples(other_df, 1, "lemmatized_text")
places <- filter_by_word_triples(places_df, 1, "lemmatized_text")
places <- filter_dataframe(places_df)

# Concatenate dataframes vertically
combined_df <- bind_rows(land, sea, other, places)

# Final filter
tweets_df <- filter_dataframe(combined_df)


# Filtering data that dont have coordinates ------------------------------
# Keep only rows containing NA values in the longitude column
df_nocoord <- lemma[is.na(lemma[['longitude']]), ]

# Read the JSON file containing sites
sites <- read.csv(file ="./data/Malta_sites.json")
  
# Filter by df with a list of coordinates of all sites in malta
df_clean <- clean_text_function(df_nocoord, 'text')
df_sites <- filter_df_by_list(df_clean, sites)

df_sites_mt <- modify_dataframe(df_sites)

# Write df to an Excel file
write.xlsx(df_sites_mt, "Sites.xlsx")


# Fix tweet_df------------------------------------------------------------
# Filter by df with a list of coordinates of all sites in malta
df_clean2 <- clean_text_function(tweets_df, 'text')
df_tweets <- filter_df_by_list(df_clean2, sites)

# Remove the rows in df2 that have the same 'text' as the rows in df1
df_subset <- anti_join(tweets_df, df_tweets, by = "text")

# Add the unique rows from df2 to df1
df_combined <- bind_rows(df_subset, df_tweets)


# Merge data frames-------------------------------------------------------
# Merge the two dataframes
merged_df <- bind_rows(tweets_df, df_sites_mt)

# Write df to an Excel file
write.xlsx(merged_df, "Mt_tweets.xlsx")


# Plot data --------------------------------------------------------------
maps(df_tweets, "latitude.x", "longitude.x")
maps(df_tweets, "latitude.y", "longitude.y")


# Analysis ---------------------------------------------------------------

analyse_seasons <- function(data) {
  # Convert created_at column to date-time format
  data$created_at <- ymd_hms(data$created_at)
  
  # Create season column
  data$season <- ifelse(month(data$created_at) %in% c(4:9), "Dry Season", "Wet Season")
  
  # Count rows per year and season
  rows_per_season <- table(year(data$created_at), data$season)
  
  # Return the data frame with season column
  return(data)
}

result <- analyse_seasons(merged_df)


# Filter data for the dry season
dry_season_data <- result[result$season == "Dry Season", ]

# Create a Leaflet map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 0, lat = 0, zoom = 2)

# Add markers for places in the dry season
map <- map %>%
  addMarkers(data = dry_season_data, lng = ~longitude, lat = ~latitude, 
             popup = ~paste("Tweet:", text))

# Display the map
map


# Calculate row counts per year in the dry season
row_counts <- table(year(dry_season_data$created_at))

# Find the year with the maximum and minimum row counts
year_max <- names(row_counts)[which.max(row_counts)]
year_min <- names(row_counts)[which.min(row_counts)]

# Print the results
cat("Year with the most rows in the dry season:", year_max, "\n")
cat("Year with the fewest rows in the dry season:", year_min, "\n")