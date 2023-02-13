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
df <- data.frame(tweets)


#### Twitter analysis ####

# Extract the URLs from the text column
df <- df %>% 
  mutate(url = stringr::str_extract(text, "(https?://t\\.co/[^[:space:]]+)")) %>% 
  mutate(text = gsub("(https?://t\\.co/[^[:space:]]+)", "", text))


# Clean and preprocess the data
df_text <- df %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% c("rt", "&amp"))

# remove https
tweet_clean <- df %>%
  mutate(tweet_text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                           "", text)) %>%  
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt") # remove all rows that contain "rt" or retweet

# Plot words
plot_words(tweet_clean)


