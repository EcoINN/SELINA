#' ---
#' Title: "Twitter spatial analysis"
#' Author: EcoINN
#' Date: "September 2022"
#' Output: 
#' ---



#### Preparation of the script ####


# Set folder
setwd("C:/Ecostack/Selina/selina/output")

# Load the required packages

if( !("rjson" %in% installed.packages()[,1]))
  install.packages("rjson")
library(rjson)

if( !("academictwitteR" %in% installed.packages()[,1]))
  install.packages("academictwitteR")
library(academictwitteR)


#### Connect to twitter ####


# Call JSON file 
tokens <- fromJSON(file= "C:/Ecostack/Selina/selina/keys/TwitterKeys.json")

# Set bearer toke
bearer_token <- tokens$Bearer


#### Search tweets ####


# Build a query
query <- build_query(query = c('malta', 'nature', 'gozo', 'beach', 'visitmalta', 'comino'), 
                     country = "MT",
                     #point_radius = c(14.37672500, 35.92161111, 25),
                     is_retweet = FALSE,
                     #remove_promoted = TRUE,
                     has_media = NULL,
                     has_images = NULL,
                     has_videos = NULL,
                     has_geo = NULL)

# Get tweets
tweets <-  get_all_tweets(query = query,
                          start_tweets = "2019-01-01T00:00:00Z",
                          end_tweets = "2022-10-31T00:00:00Z",
                          data_path = "C:/Ecostack/Selina/selina/output",
                          #n = 10,
                          bearer_token = bearer_token)

View(tweets)


#### JSON to data frame ####


# convert json into a tidy format 
tw_json <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/output", 
                       user = TRUE, 
                       output_format = "tidy2")

# convert json into a raw format
tw_raw <- bind_tweets(data_path = "C:/Ecostack/Selina/selina/output", 
                      user = TRUE, 
                      output_format = "raw")

write.csv(tw_json, "C:/Ecostack/Selina/selina/output/tweets.csv")






#### tweets analysis ####


# json support
library(rjson)
library(jsonlite)

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

options(stringsAsFactors = FALSE)


# create file path
json_file <- "C:/Ecostack/Selina/selina/output/data_1080223137348964357.json"

# import json file line by line to avoid syntax errors
# this takes a few seconds
boulder_flood_tweets <- stream_in(file(json_file))

# check coordinates
boulder_flood_tweets$geo

# create new df 
tweet_data <- data.frame(geo = boulder_flood_tweets$geo,
                         date = boulder_flood_tweets$created_at,
                         tweet_text = boulder_flood_tweets$text,
                         source_id = boulder_flood_tweets$source
                         #url = boulder_flood_tweets$entities$urls
                         #hashtag = boulder_flood_tweets$entities$hashtags
                         )
head(tweet_data)


## explore common words
# get a list of words
tweet_data_messages <- tweet_data %>%
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text)

head(tweet_data_messages)

# plot the top 15 words
tweet_data_messages %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# remove stop words
data("stop_words")
# how many words do you have including the stop words?
nrow(tweet_data_messages)

tweet_clean <- tweet_data_messages %>%
  anti_join(stop_words) %>%
  filter(!word == "rt")

# how many words after removing the stop words?
nrow(tweet_clean)

# plot the top 15 words -- notice any issues?
tweet_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

# remove https
# cleanup
tweet_clean <- tweet_data %>%
  mutate(tweet_text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                           "", tweet_text)) %>%  
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text) %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt") # remove all rows that contain "rt" or retweet

# plot the top 15 words -- notice any issues?
tweet_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets, ")



## paired word analysis 
# clean up the data by removing stop words
tweets_paired <- tweet_data %>%
  dplyr::select(tweet_text) %>%
  mutate(tweet_text = removeWords(tweet_text, stop_words$word)) %>%
  mutate(tweet_text = gsub("\\brt\\b|\\bRT\\b", "", tweet_text)) %>%
  mutate(tweet_text = gsub("http://*", "", tweet_text)) %>%
  unnest_tokens(paired_words, tweet_text, token = "ngrams", n = 2)

tweets_paired %>%
  count(paired_words, sort = TRUE)

# separate words into columns and count the unique combinations of words
tweets_separated <- tweets_paired %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

# new bigram counts:
word_counts <- tweets_separated %>%
  count(word1, word2, sort = TRUE)
word_counts

# plot 
word_counts %>%
  filter(n >= 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Paired word analysis",
       subtitle = "Twitter ",
       x = "", y = "") +
  theme_void()


#### Create maps ####

# animated maps
# to install: devtools::install_github("dgrtwo/gganimate")
# note this required imagemagick to be installed
library(leaflet)
library(gganimate)
library(lubridate)
library(maps)
library(ggthemes)

options(stringsAsFactors = FALSE)

# test
l1 <- sapply(tweet_data$geo.coordinates, length)
unlist.col1 <- rep(tweet_data$geo.coordinates, l1)
unlist.col1



# cleanup & and filter
mt_tweets <- tweet_data %>%
  mutate(geo.coordinates = gsub("\\)|c\\(", "", geo.coordinates)) %>%
  separate(geo.coordinates, c("long", "lat"), sep = ", ") %>%
  mutate_at(c("lat", "long"), as.numeric) 

# create basemap of the globe
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80")

world_basemap

# remove na values
tweet_locations <- tweet_data %>%
  na.omit()

head(tweet_locations)

# plot the data
world_basemap +
  geom_point(data = tweet_locations, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "Tweet Locations")


# plot points on top of a leaflet basemap
site_locations <- leaflet(tweet_locations) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, popup = ~tweet_text,
                   radius = 3, stroke = FALSE)

site_locations