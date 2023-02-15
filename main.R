#' ---
#' Title: "Twitter spatial analysis"
#' Author: EcoINN
#' Date: "September 2022"
#' Output: Database
#' ---


# Libraries
library(rjson) 
library(jsonlite) 
library(academictwitteR)
library(tidytext)
library(ggraph)
library(tidyverse)
library(leaflet)


# Prepare script
# Loading supporting r-scripts
invisible(sapply(list.files('./src', full.names = T), source))

# Define data and output directories
datdir <- 'data/'
outdir <- 'output/'

# Data needed
json_file <- "./keys/TwitterKeys.json" # Twitter keys
keywords <- fromJSON(file = "./keys/Keywords.json") # Twitter keywords
country <- "MT"
start_date <- "2015-01-01T00:00:00Z"
end_date <- "2022-12-31T00:00:00Z"


# Connect to the twitter API and
# Get tweets
tweets <- get_tweets(json_file, keywords, country, start_date, end_date, datdir)
View(tweets)

# Save as df
df <- data.frame(tweets)

# df preprocessing
df_mt <- preprocess(df)
df_mt <- df_clean(df_mt)

# save as xls
write.xlsx(df_mt, "./output/tweets_df.xlsx")


# Twitter analysis
# Explore common words found on tweets, and plot
c_words <- common_words(df_mt)
plot_words(c_words, x = 'Count', y = 'Unique words' ,
           title = 'Count of unique words found in tweets', 20)

# Paired word analysis, and plot
p_analysis <- tweet_bigrams(df_mt)
plot_paired_words(p_analysis, title="Paired word analysis", 
                  subtitle="Twitter ", 20)

# plot coordinates
tw_map <- maps(df_mt)
tw_map