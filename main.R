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
library(ggplot2) 
library(dplyr) 
library(tidyr) 
library(tidytext) 
library(tm) 
library(widyr) 
library(igraph) 
library(ggraph) 
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)


#### Preparing script ####
# Loading supporting r-scripts
invisible(sapply(list.files('./src', full.names = T), source))

# Define data and output directories
datdir <- 'data/'
outdir <- 'output/'

# Data needed
json_file <- "./keys/TwitterKeys.json" # Twitter keys
country <- "MT"
keywords <- c('malta', 'gozo', 'comino', 'island', 'visitmalta', 'mymalta',
              'sea', 'beach', 'seascape', 'sealife', 'seagrass', 'marinelife',
              'coast', 'coral', 'fish', 'maltacountryside', 'lanscape', 'bees',
              'butterflies', 'insects', 'agriculture', 'bird', 'soil', 'diving',
              'scubadiving', 'unserwaterphotography', 'fishing', 'maltawalks',
              'wildlifephotography', 'hiking', 'beachphotography', 'trekking',
              'birdwatching', 'birdphotography', 'agritourism', 'tourism', 
              'biodiversity', 'travelphoto', 'explore', 'photography', 'winter',
              'ecology', 'summer', 'nature', 'naturephotography', 'ecoturism',
              'culture', 'history') # Twitter keywords
start_date <- "2015-01-01T00:00:00Z"
end_date <- "2022-12-31T00:00:00Z"


#### Get tweets ####
# Connect to twitter
tokens <- fromJSON(json_file)
bearer_token <- tokens$Bearer

# Build a query
query <- build_query(query = keywords, 
                     country = country,
                     is_retweet = FALSE,
                     has_media = TRUE,
                     has_images = NULL,
                     has_videos = NULL,
                     has_geo = NULL)

# Get tweets
tweets <-  get_all_tweets(query = query,
                          start_tweets = start_date,
                          end_tweets = end_date,
                          data_path = datdir,
                          n = Inf,
                          bearer_token = bearer_token)

View(tweets)

# Save as df
df_tweets <- data.frame(tweets)

test <- common_words(df_tweets)














#### JSON to tidy and raw format ####
# convert json into a tidy format 
tw_tidy <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/data", 
                       user = TRUE, 
                       output_format = "tidy")

# convert json into a raw format
tw_raw <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/data", 
                      user = TRUE, 
                      output_format = "raw")

# to csv
# write.csv(tw_json, "C:/Ecostack/Selina/selina/output/tweets.csv")



#### Create a data frame ####
# delay re-encoding of strings
options(stringsAsFactors = FALSE)

# file path
tweet_data <- c("C:/Ecostack/Selina/selina/data/data_1080223137348964357.json",
               "C:/Ecostack/Selina/selina/data/data_1101073181899673600.json",
               "C:/Ecostack/Selina/selina/data/data_1582049800144683008.json",
               "C:/Ecostack/Selina/selina/data/data_1582160362870542336.json",
               "C:/Ecostack/Selina/selina/data/data_1582268491591487492.json")


# This function creates a df
import_json <- function(json_file){
  # import json 
  file_json <- stream_in(file(json_file))
  # create new df 
  tweet_data <- data.frame(geo = file_json$geo,
                           date = file_json$created_at,
                           tweet_text = file_json$text,
                           source_id = file_json$source
                           #url = file_json$entities$urls
                           #hashtag = file_json$entities$hashtags
  )
}


df1 <- import_json(tweet_data[1])
df2 <- import_json(tweet_data[2])
df3 <- import_json(tweet_data[3])
df4 <- import_json(tweet_data[4])
df5 <- import_json(tweet_data[5])

write.csv(, "C:/Ecostack/Selina/selina/output/df4.csv")


# import json 
file_json <- stream_in(file(tweet_data[5]))
# create new df 
tweet_data <- data.frame(geo = file_json$geo,
                         date = file_json$created_at,
                         tweet_text = file_json$text,
                         source_id = file_json$source)





tables <- list()
test <- for (t in tweet_data){
  df <- import_json(t)
  
  print(t)
}


df <- dMerged <- do.call("rbind", list(df1, df2, df3, df4))

#### Explore common words ####




#### Paired word analysis ####



#### Create maps ####

options(stringsAsFactors = FALSE)

