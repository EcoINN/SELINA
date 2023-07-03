#' Tweets preprocessing and analysis
#' 
#' @author EcoINN
#' @date "September 2022"
#' @return data frame and analysis


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



#' Prepare script
#'
#' This section sets up parallel processing and defines the data and output directories.
#' It also defines the necessary data for connecting to the Twitter API.
 

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



#' Connect to the twitter API
#'
#' This section connects to the Twitter API and mines tweets using the defined keywords.
#' The mined tweets are then saved to the specified data directory.
 

# Mining tweets
tweets <- get_tweets(keys, keywords, country, start_date, end_date, datdir)
View(tweets)

# Read json files
tweets <- rjson("./data", "^data.*$")



#' Data processing
#'
#' This section processes the tweets. This includes preparing the data,
#' filtering URLs, tokenizing and lemmatizing the text, and removing short tweets.


# df preparation
df <- process_tweets(tweets)

# Filter URLs
df <- process_urls(df)

# Tokenisation, lemmatisation and remove short tweets (<3)
lemma <- lemmatise(df)



#' Filtering data that have coordinates
#'
#' This section filters the data to remove rows without coordinates. It then further filters
#' the data using hashtags and the lemmatized text. The data is then divided into four dataframes
#' based on a set of defined keywords.


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



#' Filtering data that dont have coordinates
#'
#' This section filters the data to only include rows without coordinates. It then cleans
#' the text and filters the data using a list of sites.
 

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



#' Merge data frames
#'
#' This section merges the two dataframes into one.


# Merge the two dataframes
merged_df <- bind_rows(tweets_df, df_sites_mt)

# Write df to an Excel file
write.xlsx(merged_df, "Mt_tweets.xlsx")



#' Plot data
#'
#' This section reads the merged data from the .xlsx file and plots it.


# Read the .xlsx file
malta <- read_excel("./output/Mt_tweets.xlsx")

maps(malta, "latitude", "longitude")


# Analysis ---------------------------------------------------------------

words <- common_words(malta)
plot_words(malta, "lemmatized_text", 10)
plot_bigrams(malta, "lemmatized_text", 10)
plot_trigrams(malta, "lemmatized_text", 10)
sentiment_analysis(malta, "lemmatized_text", 10)

library(spatstat)
library(sp)
spatial_analysis(malta, "longitude", "latitude", 99)

