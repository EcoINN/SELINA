#' ---
#' title: "Twitter spatial analysis"
#' author: EcoINN
#' date: "September 2022"
#' output: 
#' ---



#### preparation of the script ####


# set folder
setwd("C:/Ecostack/Selina/selina/output")

# load the required packages

if( !("rjson" %in% installed.packages()[,1]))
  install.packages("rjson")
library(rjson)

if( !("academictwitteR" %in% installed.packages()[,1]))
  install.packages("academictwitteR")
library(academictwitteR)


#### connect to twitter ####


# json file 
tokens <- fromJSON(file= "C:/Ecostack/Selina/selina/keys/TwitterKeys.json")

# set bearer toke
bearer_token <- tokens$Bearer


#### search tweets ####


# query
query <- build_query(query = "#malta #nature #travel", 
                     country = 'Malta',
                     is_retweet = FALSE,
                     remove_promoted = TRUE,
                     has_media = NULL,
                     has_images = NULL,
                     has_videos = NULL,
                     has_geo = NULL)

# get tweets
tweets <-  get_all_tweets(query = query,
                          start_tweets = "2021-01-01T00:00:00Z",
                          end_tweets = "2021-12-31T00:00:00Z",
                          data_path = "C:/Ecostack/Selina/selina/output",
                          n = 1000,
                          bearer_token = bearer_token)

View(tweets)


#### inspect data ####

tw_json <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/output", 
                       user = TRUE, 
                       output_format = "tidy")
