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
library(readxl)
library(tidytext)
library(ggraph)
library(igraph)
library(tidyverse)
library(leaflet)


# Prepare script ---------------------------
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


# Connect to the twitter API ---------------------------
# Get tweets
tweets <- get_tweets(keys, keywords, country, start_date, end_date, datdir)
View(tweets)

# Read json files
tweets <- rjson("./data", "^data.*$")
View(tweets)


# Pre-processing ---------------------------
# df preparation
df <- process_tweets(tweets)
View(df)

# plot georeferenced tweets
tw_map <- maps(df)
tw_map


# Twitter filtering ---------------------------




# Twitter analysis ---------------------------
# Explore common words found on tweets, and plot
c_words <- common_words(df)
plot_words(c_words, x = 'Count', y = 'Unique words' ,
           title = 'Count of unique words found in tweets', 20)

# Paired word analysis, and plot
p_analysis <- tweet_bigrams(df)
plot_paired_words(p_analysis, title="Paired word analysis", 
                  subtitle="Twitter ", 20)




