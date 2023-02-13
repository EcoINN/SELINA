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

# Connect to twitter and get tweets
tweets <- get_tweets(json_file, keywords, country, start_date, end_date, datdir)
View(tweets)

# Save as df
df <- data.frame(tweets)



# Twitter analysis
# df preprocessing
df_mt <- common_words(df)

# Plot words
plot_words(df_mt, x = 'Count', y = 'Unique words' , 
           title = 'Count of unique words found in tweets')


