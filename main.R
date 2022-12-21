#' ---
#' Title: "Twitter spatial analysis"
#' Author: EcoINN
#' Date: "September 2022"
#' Output: 
#' ---



#### Load libraries ####
# json support
library(rjson)
library(jsonlite)
# twitter API
library(academictwitteR)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tidyr)
# text mining library
library(tidytext)
library(tm)
# coupled words analysis
library(widyr)
# plotting packages
library(igraph)
library(ggraph)
# to install: devtools::install_github("dgrtwo/gganimate")
# note this required imagemagick to be installed
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)


#### Preparing script ####
# Loading supporting R-scripts
invisible(sapply(list.files('./R', full.names = T), source))

# Define data and output directories
datdir <- 'data/'
outdir <- 'output/'

# Connect to twitter
json_file <- "C:/Ecostack/Selina/selina/keys/TwitterKeys.json"
connect <- connect_twitter(json_file)



#### Search tweets ####
# build a query
query <- build_query(query = c('malta', 'nature', 'gozo', 
                               'beach', 'visitmalta', 'comino'), 
                     country = "MT",
                     #point_radius = c(14.37672500, 35.92161111, 25),
                     is_retweet = FALSE,
                     #remove_promoted = TRUE,
                     has_media = NULL,
                     has_images = NULL,
                     has_videos = NULL,
                     has_geo = NULL)

# get tweets
tweets <-  get_all_tweets(query = query,
                          start_tweets = "2019-01-01T00:00:00Z",
                          end_tweets = "2022-10-31T00:00:00Z",
                          data_path = "C:/Ecostack/Selina/selina/data",
                          #n = 10,
                          bearer_token = bearer_token)

View(tweets)



#### JSON to tidy and raw format ####
# convert json into a tidy format 
tw_json <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/output", 
                       user = TRUE, 
                       output_format = "tidy2")

# convert json into a raw format
tw_raw <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/output", 
                      user = TRUE, 
                      output_format = "raw")

# to csv
# write.csv(tw_json, "C:/Ecostack/Selina/selina/output/tweets.csv")



#### Create a data frame ####
# delay re-encoding of strings
options(stringsAsFactors = FALSE)

# file path
tweet_data <- c("C:/Ecostack/Selina/selina/output/data_1080223137348964357.json",
               "C:/Ecostack/Selina/selina/output/data_1101073181899673600.json",
               "C:/Ecostack/Selina/selina/output/data_1582049800144683008.json",
               "C:/Ecostack/Selina/selina/output/data_1582160362870542336.json")

twitter_df <- for (package in neededPackages){pkgTest(package)}



#### Explore common words ####




#### Paired word analysis ####



#### Create maps ####

options(stringsAsFactors = FALSE)

