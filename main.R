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

# Text analysis
library(stringdist)
library(textstem)
library(SnowballC)

# Visualization
library(ggraph)
library(igraph)
library(leaflet)


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
df_mt <- lemmatize_and_filter(df)

# Remove rows with NA values in the longitude column
df_coord <- remove_na_rows(df_mt, "longitude")

# Filter using the hashtags and the lemmatized_text columns
dfc <- filter_by_word_pairs(df_coord, 1, "lemmatized_text")
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
filtered_dfs <- groups_df(df_mt, column_names, keyword_sets)

# Access the individual dataframes
land_df <- filtered_dfs[["land"]]
sea_df <- filtered_dfs[["sea"]]
other_df <- filtered_dfs[["other"]]
places_df <- filtered_dfs[["places"]]

# Filter using the hashtags and the lemmatized_text columns
land <- filter_by_word_pairs(land_df, 1, "lemmatized_text")
land <- filter_dataframe(land)



plot_common_words(df_mt, "lemmatized_text", n = 20)
plot_common_words(df_mt, "hashtags", n = 20)


land_map <- leaflet(land_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    fillColor = "blue",
    fillOpacity = 0.8,
    stroke = FALSE
  )

land_map






